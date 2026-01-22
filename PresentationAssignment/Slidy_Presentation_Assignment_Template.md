---
title: "Data Science Presentation"
subtitle: "Data Science for Psychologists — Slidy Presentation Assignment"
author: "Your Name"
date: "January 04, 2026"
output:
  slidy_presentation:
    incremental: true
    keep_md: true
    widescreen: true
    theme: "darkly" #cerulean , spacelab, flatly, journal, lumen, readable, cosmo, united, yeti, sandstone, paper, simplex
    highlight: "kate" #, pygments, tango, monochrome, default,
---



## Assignment overview (read this slide first)

**Goal:** Deliver a ~10-minute presentation that demonstrates a reproducible data science workflow on a psychology-relevant dataset.

Your presentation must:

1. Use **Slidy** (`output: slidy_presentation`).
2. Demonstrate how you made your data **tidy** (show the *before* and *after*).
3. Include **at least three (3)** of the analyses listed below.
4. Use **ggplot2** for all visualizations.

**Target length:** ~10 minutes (typically **8–12 slides**).

---

## Required analyses (choose ≥ 3)

Include **at least three** of the following (you may include more):

- **General Linear Model (continuous outcomes)**  
  *(ANOVA, t-test, regression; e.g., `aov()`, `t.test()`, `lm()`)*

- **Logistic regression**  
  *(binary outcome; e.g., `glm(..., family = binomial)`)*

- **Bootstrapping and/or model evaluation**  
  *(e.g., bootstrap CIs, train/test split, cross-validation, accuracy/AUC)*

- **Cluster analysis**  
  *(e.g., k-means, hierarchical clustering, cluster validation/interpretation)*

---

## Deliverables and submission

Submit **two files**:

1. Your `.Rmd` Slidy deck (named `Lastname_Firstname_Presentation.Rmd`)
2. The knitted HTML output (named `Lastname_Firstname_Presentation.html`)

Also include:
- the dataset (or a link and access instructions), and
- any helper scripts you used.

---

## Evaluation criteria (rubric summary)

You will be assessed on:

- **Reproducibility:** knits without errors; code and narrative are coherent
- **Tidying demonstration:** clear before/after; tidy principles applied correctly
- **Analyses (≥3):** correct rationale, implementation, and interpretation
- **Visualization (ggplot2):** clarity, labeling, correct mapping to research question
- **Communication:** fits ~10 minutes; logical flow; conclusions match results

---

## Slidy basics (what creates a new slide)

In Slidy, **each level-2 header** starts a new slide.

Example:

```markdown
## This is a slide title

This text appears on that slide.
```

Use `---` for a visual separator *within* a slide.

Code chunks are inserted into slides the same way they are in any other Rmarkdown document.

---

## Slide 1: Title / research question

**Your job:** Introduce your dataset and your research question(s).

- What is the psychological construct or phenomenon?
- What are your key variables?
- What is your hypothesis or exploratory goal?

---

## Slide 2: Data description

Include:

- Data source (study, repository, simulated, class dataset, etc.)
- Sample size (N) and key variables
- Outcome variable(s) and predictors
- Any exclusions or filtering decisions


```r
# Example:
# library(tidyverse)
# dat_raw <- readr::read_csv("data_raw/your_data.csv")
# glimpse(dat_raw)
```

---

## Slide 3: Tidying demonstration (REQUIRED)

**Show the data "before"** (untidy) and explain what is wrong:
- variables stored as values (wide vs long)
- multiple variables in one column
- one variable spread across multiple columns
- inconsistent names / coding

**Then show the "after"** (tidy) and explain your steps.


```r
# Example:
# dat_tidy <- dat_raw %>%
#   janitor::clean_names() %>%
#   tidyr::pivot_longer(cols = starts_with("trial_"),
#                       names_to = "trial",
#                       values_to = "rt_ms") %>%
#   mutate(condition = factor(condition))

# Show:
# head(dat_raw)
# head(dat_tidy)
```

---

## Slide 4: Descriptive statistics + first ggplot (REQUIRED)

Include at least one ggplot that:
- answers a basic question about the distribution or group differences, and
- is clearly labeled (title, x/y labels, legend if needed).


---

## Slide 5: Analysis 1 (choose from required list)

### Analysis 1: Title and rationale
- What question does this test?
- Why is this method appropriate?


```r
# Example: t-test
# t.test(outcome ~ condition, data = dat_tidy)
```

**Interpretation (in plain language):**  
- Include polished ggplot, and
- 1–3 sentences explaining the result and what it means substantively.

---

## Slide 6: Analysis 2 (choose from required list)

Include a ggplot that supports or illustrates the model result (as appropriate).

---

## Slide 7: Analysis 3 (choose from required list)


If logistic regression: interpret coefficients as **odds** (or predicted probabilities).

---

## Optional analysis slides (if you include 4+ analyses)

Duplicate the structure from slides 5–7.

---

## Slide: Bootstrapping / model evaluation (if chosen)

If you chose bootstrapping or model evaluation, include:

- What is being evaluated (accuracy, RMSE, AUC, etc.)?
- What resampling approach did you use (bootstrap, train/test, CV)?
- A clear plot or table summarizing performance/uncertainty.

---

## Slide: Cluster analysis (if chosen)

If you chose clustering, include:

- Variables used for clustering (and why)
- Scaling/standardization decisions
- How you chose the number of clusters (if applicable)
- Interpretation of clusters in psychological terms

---

## Slide 8: Key takeaways

Summarize:

- What did you learn from tidying?
- What did your analyses collectively suggest?
- What limitations should we keep in mind?
- What would you do next with more time/data?

---

## Slide 9 (optional): Reproducibility checklist

Confirm that:

- Your deck knits without errors on a clean run
- You set seeds where randomness is used (`set.seed()`)
- All plots are created with **ggplot2**
- Your data tidying steps are shown clearly
- You included **≥ 3** required analyses

---

## Slide 10 (optional): References

Provide citations for:

- Data source
- Packages/methods (if required)
- Any external figures or scales used
