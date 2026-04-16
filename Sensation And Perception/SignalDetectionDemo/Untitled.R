install.packages("av")
library(av)

wrap01 <- function(z) {
  z[z < 0] <- z[z < 0] + 1
  z[z > 1] <- z[z > 1] - 1
  z
}

make_dot_mp4 <- function(
    filename,
    n_dots = 8,
    n_frames = 24,
    base_speed = 0.03,
    target_speed = NULL,
    signal_present = FALSE,
    width = 600,
    height = 600,
    fps = 12,
    dot_cex = 2.5,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  # Initial positions
  x <- runif(n_dots, 0.15, 0.85)
  y <- runif(n_dots, 0.15, 0.85)
  
  # Random directions
  theta <- runif(n_dots, 0, 2 * pi)
  
  # Speeds
  speeds <- rep(base_speed, n_dots)
  if (signal_present) {
    target_idx <- sample.int(n_dots, 1)
    speeds[target_idx] <- target_speed
  }
  
  vx <- speeds * cos(theta)
  vy <- speeds * sin(theta)
  
  # Capture one plot per frame into a video
  av_capture_graphics(
    {
      for (t in seq_len(n_frames)) {
        par(mar = c(0, 0, 0, 0), bg = "black")
        plot.new()
        plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
        
        # Draw ONLY current positions
        points(x, y, pch = 16, cex = dot_cex, col = "white")
        
        # Move after drawing
        x <- wrap01(x + vx)
        y <- wrap01(y + vy)
      }
    },
    output = filename,
    width = width,
    height = height,
    framerate = fps
  )
}

dir.create("stimuli", showWarnings = FALSE)

# Noise
for (i in 1:20) {
  make_dot_mp4(
    filename = sprintf("stimuli/noise_%d.mp4", i),
    signal_present = FALSE,
    base_speed = 0.03,
    n_frames = 24,
    fps = 12,
    seed = 100 + i
  )
}

wrap01 <- function(z) {
  z[z < 0] <- z[z < 0] + 1
  z[z > 1] <- z[z > 1] - 1
  z
}

make_dot_gif <- function(
    filename,
    n_dots = 8,
    n_frames = 24,
    base_speed = 0.03,
    target_speed = NULL,
    signal_present = FALSE,
    width = 600,
    height = 600,
    fps = 12,
    dot_cex = 2.5,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  x <- runif(n_dots, 0.15, 0.85)
  y <- runif(n_dots, 0.15, 0.85)
  theta <- runif(n_dots, 0, 2 * pi)
  
  speeds <- rep(base_speed, n_dots)
  if (signal_present) {
    target_idx <- sample.int(n_dots, 1)
    speeds[target_idx] <- target_speed
  }
  
  vx <- speeds * cos(theta)
  vy <- speeds * sin(theta)
  
  frames <- vector("list", n_frames)
  
  for (t in seq_len(n_frames)) {
    img <- image_graph(width = width, height = height, res = 96, bg = "black")
    par(mar = c(0, 0, 0, 0), bg = "black")
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
    points(x, y, pch = 16, cex = dot_cex, col = "white")
    dev.off()
    
    frames[[t]] <- img
    
    x <- wrap01(x + vx)
    y <- wrap01(y + vy)
  }
  
  anim <- image_join(frames)
  anim <- image_animate(anim, fps = fps)
  image_write(anim, path = filename)
}

# Baseline signal
for (i in 1:20) {
  make_dot_gif(
    filename = sprintf("stimuli/signal_base_%d.mp4", i),
    signal_present = TRUE,
    base_speed = 0.03,
    target_speed = 0.04,
    n_frames = 24,
    fps = 12,
    seed = 200 + i
  )
}

# Easy signal
for (i in 1:20) {
  make_dot_gif(
    filename = sprintf("stimuli/signal_easy_%d.mp4", i),
    signal_present = TRUE,
    base_speed = 0.03,
    target_speed = 0.06,
    n_frames = 24,
    fps = 12,
    seed = 300 + i
  )
}