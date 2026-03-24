# app.R
# Shiny battle engine for classroom use
# Students provide pick_opponent() and you "battle" live.

library(shiny)
library(jsonlite)

# -----------------------------
# PokeAPI helper + caching
# -----------------------------
.poke_cache <- new.env(parent = emptyenv())

poke_get <- function(name_or_id) {
  key <- tolower(as.character(name_or_id))
  
  if (exists(key, envir = .poke_cache, inherits = FALSE)) {
    return(get(key, envir = .poke_cache, inherits = FALSE))
  }
  
  url <- paste0("https://pokeapi.co/api/v2/pokemon/", key)
  out <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  
  assign(key, out, envir = .poke_cache)
  out
}

poke_name <- function(p) if (is.null(p)) NA_character_ else p$name

poke_types <- function(p) {
  if (is.null(p)) return(character())
  unique(tolower(p$types$type$name))
}

poke_stat <- function(p, stat = "hp") {
  if (is.null(p)) return(NA_real_)
  i <- match(tolower(stat), tolower(p$stats$stat$name))
  if (is.na(i)) return(NA_real_)
  as.numeric(p$stats$base_stat[i])
}

poke_sprite <- function(p) {
  if (is.null(p)) return(NA_character_)
  # Prefer official artwork if available; fall back to front_default
  art <- tryCatch(p$sprites$other$`official-artwork`$front_default, error = function(e) NA_character_)
  if (!is.na(art) && nzchar(art)) return(art)
  spr <- p$sprites$front_default
  if (is.null(spr) || !nzchar(spr)) NA_character_ else spr
}

# Simple power heuristic (tune if you want)
poke_power <- function(p) {
  if (is.null(p)) return(NA_real_)
  sum(
    poke_stat(p, "hp"),
    poke_stat(p, "attack"),
    poke_stat(p, "defense"),
    poke_stat(p, "special-attack"),
    poke_stat(p, "special-defense"),
    poke_stat(p, "speed"),
    na.rm = TRUE
  )
}

poke_random_move <- function(p) {
  if (is.null(p) || is.null(p$moves) || length(p$moves$move$name) == 0) return("Struggle")
  sample(tolower(p$moves$move$name), 1)
}

# tiny type edge table: attacker -> defender types it beats
.type_edges <- list(
  fire     = c("grass", "ice", "bug", "steel"),
  water    = c("fire", "rock", "ground"),
  grass    = c("water", "rock", "ground"),
  electric = c("water", "flying"),
  ground   = c("electric", "fire", "rock", "steel", "poison"),
  flying   = c("grass", "bug", "fighting")
)

type_multiplier <- function(attacker_types, defender_types) {
  # simplistic: if any attacker type beats any defender type => 1.5
  # if defender beats attacker (reverse) => 0.75
  a <- tolower(attacker_types); d <- tolower(defender_types)
  super <- any(sapply(a, function(t) any(.type_edges[[t]] %in% d)), na.rm = TRUE)
  notvery <- any(sapply(d, function(t) any(.type_edges[[t]] %in% a)), na.rm = TRUE)
  if (super && !notvery) return(1.5)
  if (notvery && !super) return(0.75)
  1
}

# -----------------------------
# Default opponent picker (fallback)
# -----------------------------
pick_opponent_default <- function(player, candidates, ...) {
  player_key <- tolower(as.character(player))
  cands <- unique(tolower(as.character(candidates)))
  cands <- setdiff(cands, player_key)   # <-- key line
  
  if (!length(cands)) return(NA_character_)
  
  p <- poke_get(player_key)
  if (is.null(p)) return(sample(cands, 1))
  p_pow <- poke_power(p)
  
  cand_pow <- sapply(cands, function(x) poke_power(poke_get(x)))
  cand_pow[is.na(cand_pow)] <- Inf
  
  # still "fair fight" (closest power)
  idx <- which.min(abs(cand_pow - p_pow))
  best <- which(abs(cand_pow - p_pow) == abs(cand_pow[idx] - p_pow))
  sample(cands[best], 1)
}

# Global placeholder; can be replaced by student code
pick_opponent <- pick_opponent_default

# -----------------------------
# Battle mechanics (simple, visible, fun)
# -----------------------------
calc_damage <- function(attacker, defender) {
  base <- sample(8:16, 1)
  
  # miss / crit
  miss <- runif(1) < 0.10
  crit <- runif(1) < 0.125
  
  atk <- poke_stat(attacker, "attack")
  def <- poke_stat(defender, "defense")
  if (is.na(atk)) atk <- 60
  if (is.na(def)) def <- 60
  
  dmg <- base + round((atk - def) / 12)
  dmg <- max(1, min(25, dmg))
  
  if (miss) dmg <- 0
  if (crit) dmg <- dmg * 2
  
  mult <- type_multiplier(poke_types(attacker), poke_types(defender))
  dmg <- round(dmg * mult)
  dmg <- min(dmg, 35)  # or 40, whatever pacing you like
  
  list(dmg = dmg, miss = miss, crit = crit, mult = mult)
}

max_hp <- function(p) {
  hp <- poke_stat(p, "hp")
  if (is.na(hp)) 60 else max(30, min(200, hp))
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .battle-row { display:flex; justify-content:space-between; align-items:flex-end; gap:20px; }
      .poke-card { width:48%; padding:12px; border:1px solid #ddd; border-radius:12px; }
      .poke-name { font-size:22px; font-weight:700; text-transform:capitalize; }
      .poke-sub { color:#555; }
      .sprite { width:200px; height:200px; object-fit:contain; image-rendering:pixelated; }
      .logbox { height:80px; overflow:auto; background:#0b0f14; color:#d7e3f4; padding:10px; border-radius:12px; font-family:ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace; }
      .muted { color:#777; }
      /* Make left control panel text smaller */
      .well { 
      font-size: 8px; 
      line-height: 1;
      }

/* Optional: also shrink input labels a bit */
.well label, .well .control-label { 
  font-size: 11px; 
}
    "))
  ),
  
  titlePanel("Pokémon Battle Engine (Teacher Mode)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1) Load a student function (optional)"),
      fileInput("student_file", "Upload student's .R file (must define pick_opponent)", accept = c(".R")),
      actionButton("use_default", "Revert to default pick_opponent()"),
      
      tags$hr(),
      
      h4("2) Choose player & candidate pool"),
      textInput("player", "Your Pokémon (name or id)", value = "pikachu"),
      textAreaInput(
        "candidates",
        "Candidate opponents (comma-separated names or ids)",
        value = "bulbasaur, charmander, squirtle, gengar, snorlax, eevee, dragonite, alakazam, machamp, gyarados",
        rows = 3
      ),
      
      checkboxInput("auto_pick", "Autopick opponent using pick_opponent()", value = TRUE),
      textInput("opponent_manual", "Manual opponent (used if Autopick OFF)", value = "bulbasaur"),
      
      tags$hr(),
      
      h4("3) Battle controls"),
      sliderInput("tick_ms", "Animation speed (ms per turn)", min = 250, max = 1500, value = 700, step = 50),
      actionButton("start", "Start battle"),
      actionButton("step", "Single step"),
      actionButton("reset", "Reset"),
      
      tags$hr(),
      h4("Debug / transparency"),
      verbatimTextOutput("picker_status")
    ),
    
    mainPanel(
      fluidRow(
        column(
          12,
          div(class = "battle-row",
              div(class = "poke-card",
                  div(class = "poke-name", textOutput("player_name")),
                  div(class = "poke-sub", textOutput("player_types")),
                  div(style = "margin:10px 0;",
                      uiOutput("player_sprite")),
                  uiOutput("player_hpbar"),
                  div(class = "muted", textOutput("player_stats"))
              ),
              div(class = "poke-card",
                  div(class = "poke-name", textOutput("opponent_name")),
                  div(class = "poke-sub", textOutput("opponent_types")),
                  div(style = "margin:10px 0;",
                      uiOutput("opponent_sprite")),
                  uiOutput("opponent_hpbar"),
                  div(class = "muted", textOutput("opponent_stats"))
              )
          )
        )
      ),
      
      tags$hr(),
      
      h4("Battle Log"),
      div(class = "logbox", htmlOutput("battle_log"))
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  # Track whether we have a student picker loaded
  picker_meta <- reactiveValues(source = "default", last_error = NULL)
  
  # Helper: parse candidates
  candidates_vec <- reactive({
    x <- trimws(unlist(strsplit(input$candidates, ",")))
    x <- x[nzchar(x)]
    unique(tolower(x))
  })
  
  # Load student file: expects pick_opponent defined
  observeEvent(input$student_file, {
    req(input$student_file$datapath)
    picker_meta$last_error <- NULL
    
    # Evaluate into a clean environment then pull out pick_opponent
    env <- new.env(parent = baseenv())
    ok <- tryCatch({
      sys.source(input$student_file$datapath, envir = env)
      TRUE
    }, error = function(e) {
      picker_meta$last_error <- paste("Error sourcing file:", conditionMessage(e))
      FALSE
    })
    
    if (!ok) return()
    
    if (!exists("pick_opponent", envir = env, inherits = FALSE) || !is.function(env$pick_opponent)) {
      picker_meta$last_error <- "File did not define a function named pick_opponent()."
      return()
    }
    
    # Swap global picker for this session
    pick_opponent <<- env$pick_opponent
    picker_meta$source <- paste("student:", input$student_file$name)
  })
  
  observeEvent(input$use_default, {
    pick_opponent <<- pick_opponent_default
    picker_meta$source <- "default"
    picker_meta$last_error <- NULL
  })
  
  output$picker_status <- renderText({
    c(
      paste("pick_opponent source:", picker_meta$source),
      if (!is.null(picker_meta$last_error)) paste("last error:", picker_meta$last_error) else "last error: <none>"
    )
  })
  
  # Resolve player and opponent selections
  player_poke <- reactive({
    poke_get(input$player)
  })
  
  opponent_choice <- reactive({
    cands <- candidates_vec()
    if (!length(cands)) return(NA_character_)
    
    if (!isTRUE(input$auto_pick)) {
      return(tolower(trimws(input$opponent_manual)))
    }
    
    # Call pick_opponent() safely
    out <- tryCatch(
      pick_opponent(input$player, cands),
      error = function(e) {
        picker_meta$last_error <- paste("pick_opponent() error:", conditionMessage(e))
        NA_character_
      }
    )
    
    if (is.null(out) || length(out) == 0 || is.na(out)) return(NA_character_)
    tolower(as.character(out[[1]]))
  })
  
  opponent_poke <- reactive({
    x <- opponent_choice()
    if (is.na(x) || !nzchar(x)) return(NULL)
    poke_get(x)
  })
  
  # Battle state
  state <- reactiveValues(
    running = FALSE,
    turn = 0L,
    phase = 0L,            # 0 = decide order, 1 = first attack, 2 = second attack
    first_actor = NULL,    # "p1" or "p2"
    player_hp = NA_real_,
    opp_hp = NA_real_,
    player_hp_max = NA_real_,
    opp_hp_max = NA_real_,
    log = character(),
    winner = NULL
  )
  
  init_battle <- function() {
    p1 <- player_poke()
    p2 <- opponent_poke()
    
    if (is.null(p1)) {
      state$log <- c("Player Pokémon not found. Check spelling / id.")
      state$running <- FALSE
      return()
    }
    if (is.null(p2)) {
      state$log <- c("Opponent Pokémon not found OR pick_opponent() returned something invalid.")
      state$running <- FALSE
      return()
    }
    state$phase <- 0L
    state$first_actor <- NULL
    state$turn <- 0L
    state$player_hp_max <- max_hp(p1)
    state$opp_hp_max <- max_hp(p2)
    state$player_hp <- state$player_hp_max
    state$opp_hp <- state$opp_hp_max
    state$winner <- NULL
    
    state$log <- c(
      sprintf("Battle begins: %s vs %s", poke_name(p1), poke_name(p2)),
      sprintf("Arena announcer: \"No lawsuits, only vibes.\""),
      "—"
    )
  }
  
  do_step <- function() {
    p1 <- player_poke()
    p2 <- opponent_poke()
    if (is.null(p1) || is.null(p2)) return()
    
    if (state$player_hp <= 0 || state$opp_hp <= 0 || !is.null(state$winner)) {
      state$running <- FALSE
      return()
    }
    
    # Phase 0: decide who goes first and announce the turn
    if (state$phase == 0L) {
      state$turn <- state$turn + 1L
      s1 <- poke_stat(p1, "speed")
      s2 <- poke_stat(p2, "speed")
      state$first_actor <- if (is.na(s1) || is.na(s2)) sample(c("p1","p2"), 1) else if (s1 > s2) "p1" else if (s2 > s1) "p2" else sample(c("p1","p2"), 1)
      state$log <- c(state$log, sprintf("— Turn %d —", state$turn))
      state$phase <- 1L
      return()
    }
    
    take_hit <- function(attacker) {
      if (attacker == "p1") {
        move <- poke_random_move(p1)
        res  <- calc_damage(p1, p2)
        state$log <- c(state$log, sprintf("%s used %s!", poke_name(p1), gsub("-", " ", move)))
        
        if (res$miss) {
          state$log <- c(state$log, "…but it missed. (skill issue)")
          return()
        }
        if (res$mult > 1) state$log <- c(state$log, "It's super effective!")
        if (res$mult < 1) state$log <- c(state$log, "It's not very effective…")
        if (res$crit) state$log <- c(state$log, "CRITICAL HIT!!")
        
        state$opp_hp <- max(0, state$opp_hp - res$dmg)
        state$log <- c(state$log, sprintf("%s took %d damage.", poke_name(p2), res$dmg))
        
        if (state$opp_hp <= 0) state$winner <- "player"
      } else {
        move <- poke_random_move(p2)
        res  <- calc_damage(p2, p1)
        state$log <- c(state$log, sprintf("%s used %s!", poke_name(p2), gsub("-", " ", move)))
        
        if (res$miss) {
          state$log <- c(state$log, "…but it missed. Tragic.")
          return()
        }
        if (res$mult > 1) state$log <- c(state$log, "It's super effective!")
        if (res$mult < 1) state$log <- c(state$log, "It's not very effective…")
        if (res$crit) state$log <- c(state$log, "CRITICAL HIT!!")
        
        state$player_hp <- max(0, state$player_hp - res$dmg)
        state$log <- c(state$log, sprintf("%s took %d damage.", poke_name(p1), res$dmg))
        
        if (state$player_hp <= 0) state$winner <- "opponent"
      }
    }
    
    # Phase 1: first attack
    if (state$phase == 1L) {
      take_hit(state$first_actor)
      if (!is.null(state$winner)) {
        state$running <- FALSE
        state$log <- c(state$log,
                       if (state$winner == "player") sprintf("KO! %s wins.", poke_name(p1)) else sprintf("KO! %s wins.", poke_name(p2)),
                       "— end —")
        return()
      }
      state$phase <- 2L
      return()
    }
    
    # Phase 2: second attack (if still alive), then move to next turn
    if (state$phase == 2L) {
      second <- if (state$first_actor == "p1") "p2" else "p1"
      take_hit(second)
      
      if (!is.null(state$winner)) {
        state$running <- FALSE
        state$log <- c(state$log,
                       if (state$winner == "player") sprintf("KO! %s wins.", poke_name(p1)) else sprintf("KO! %s wins.", poke_name(p2)),
                       "— end —")
        return()
      }
      
      state$phase <- 0L
      return()
    }
  }
  
  # Buttons
  observeEvent(input$reset, {
    state$running <- FALSE
    init_battle()
  })
  
  observeEvent(input$start, {
    init_battle()
    state$running <- TRUE
  })
  
  observeEvent(input$step, {
    if (is.null(state$log) || !length(state$log)) init_battle()
    do_step()
  })
  
  # Animation loop
  observe({
    req(state$running)
    invalidateLater(input$tick_ms, session)
    do_step()
  })
  
  # Render player/opponent panels
  output$player_name <- renderText({
    p <- player_poke()
    if (is.null(p)) "Player: <not found>" else paste("Player:", poke_name(p))
  })
  output$opponent_name <- renderText({
    p <- opponent_poke()
    if (is.null(p)) "Opponent: <not selected>" else paste("Opponent:", poke_name(p))
  })
  
  output$player_types <- renderText({
    p <- player_poke()
    if (is.null(p)) "" else paste("Types:", paste(poke_types(p), collapse = " / "))
  })
  output$opponent_types <- renderText({
    p <- opponent_poke()
    if (is.null(p)) "" else paste("Types:", paste(poke_types(p), collapse = " / "))
  })
  
  output$player_stats <- renderText({
    p <- player_poke()
    if (is.null(p)) return("")
    sprintf("HP %d | Atk %d | Def %d | SpA %d | SpD %d | Spe %d",
            poke_stat(p,"hp"), poke_stat(p,"attack"), poke_stat(p,"defense"),
            poke_stat(p,"special-attack"), poke_stat(p,"special-defense"), poke_stat(p,"speed"))
  })
  output$opponent_stats <- renderText({
    p <- opponent_poke()
    if (is.null(p)) return("")
    sprintf("HP %d | Atk %d | Def %d | SpA %d | SpD %d | Spe %d",
            poke_stat(p,"hp"), poke_stat(p,"attack"), poke_stat(p,"defense"),
            poke_stat(p,"special-attack"), poke_stat(p,"special-defense"), poke_stat(p,"speed"))
  })
  
  output$player_sprite <- renderUI({
    p <- player_poke()
    src <- poke_sprite(p)
    if (is.na(src)) span(class="muted", "No sprite available") else tags$img(src = src, class="sprite")
  })
  output$opponent_sprite <- renderUI({
    p <- opponent_poke()
    src <- poke_sprite(p)
    if (is.na(src)) span(class="muted", "No sprite available") else tags$img(src = src, class="sprite")
  })
  
  hp_bar <- function(current, maximum, label) {
    if (is.na(current) || is.na(maximum) || maximum <= 0) return(div(class="muted", "HP: n/a"))
    pct <- round(100 * current / maximum)
    div(
      tags$div(style="display:flex; justify-content:space-between;",
               tags$span(strong(label)),
               tags$span(sprintf("%d / %d", current, maximum))),
      tags$div(style="width:100%; background:#eee; border-radius:8px; height:16px; overflow:hidden;",
               tags$div(style=paste0("width:", pct, "%; background:#2ecc71; height:16px;")))
    )
  }
  
  output$player_hpbar <- renderUI({
    hp_bar(state$player_hp, state$player_hp_max, "HP")
  })
  output$opponent_hpbar <- renderUI({
    hp_bar(state$opp_hp, state$opp_hp_max, "HP")
  })
  
  output$battle_log <- renderUI({
    # show last ~30 lines
    lines <- tail(state$log, 30)
    HTML(paste0(paste0(lines, collapse = "<br/>"), "<script>var el=document.querySelector('.logbox'); if(el){ el.scrollTop=el.scrollHeight; }</script>"))
  })
  
  # Initialize once
  observeEvent(TRUE, {
    init_battle()
  }, once = TRUE)
}

shinyApp(ui, server)