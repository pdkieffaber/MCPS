# --- INSTRUCTOR-ONLY SCRIPT (No tidymodels) ---
# This script creates the "checker_6_no_tidy.rds" file.

# 1. Load all required libraries
library(mlmRev)
library(tidyverse)
library(lme4)
library(lmerTest)
library(car)
library(digest)

# 2. Define the new 6-point "check_my_work" function (Base-R/dplyr version)
check_my_work_6 <- function(your_name,
                            task1_t_stat,
                            task2_ses_coef,
                            task3_f_stat,
                            task4_intercept_var,
                            task5_median_ci_upper,
                            task6_cv_rmse) {
  
  # --- 1. Re-generate this student's unique seed ---
  hex_hash <- digest::digest(your_name, algo = "crc32", serialize = FALSE)
  hex_substr <- substr(hex_hash, 1, 7)
  student_seed <- strtoi(hex_substr, base = 16L)
  
  if (is.na(student_seed)) {
    stop("Error: 'your_name' does not match the one used in Part 1. Please check spelling.")
  }
  
  # Set the seed for reproducible data generation
  set.seed(student_seed)
  
  # --- 2. Re-generate this student's unique data ---
  data("Hsb82", package = "mlmRev", envir = environment())
  my_data_key <- Hsb82 %>% dplyr::sample_frac(0.95)
  
  cat("--- CHECKING ANSWERS FOR:", your_name, "---\n")
  cat("Seed:", student_seed, "\nData Rows:", nrow(my_data_key), "\n\n")
  
  # --- 3. Check Task 1 (t-test) ---
  true_t_stat <- t.test(MathAch ~ Sex, data = my_data_key)$statistic
  if (abs(task1_t_stat - true_t_stat) < 0.01) {
    cat("✅ TASK 1 (t-test statistic): CORRECT!\n")
  } else {
    cat("❌ TASK 1 (t-test statistic): Incorrect. Expected", round(true_t_stat, 3), "\n")
  }
  
  # --- 4. Check Task 2 (lm coefficient) ---
  true_ses_coef <- coef(lm(MathAch ~ SES, data = my_data_key))[2]
  if (abs(task2_ses_coef - true_ses_coef) < 0.01) {
    cat("✅ TASK 2 (lm SES coefficient): CORRECT!\n")
  } else {
    cat("❌ TASK 2 (lm SES coefficient): Incorrect. Expected", round(true_ses_coef, 3), "\n")
  }
  
  # --- 5. Check Task 3 (ANOVA F-stat) ---
  model_t3 <- lm(MathAch ~ SchoolType * SES, data = my_data_key)
  true_f_stat <- car::Anova(model_t3, type = "III")["SchoolType:SES", "F"]
  if (abs(task3_f_stat - true_f_stat) < 0.01) {
    cat("✅ TASK 3 (ANOVA Interaction F-stat): CORRECT!\n")
  } else {
    cat("❌ TASK 3 (ANOVA Interaction F-stat): Incorrect. Expected", round(true_f_stat, 3), "\n")
  }
  
  # --- 6. Check Task 4 (LMM Variance) ---
  lmm_t4 <- lmer(MathAch ~ SES + (1 | School), data = my_data_key,
                 control = lme4::lmerControl(optimizer = "bobyqa"))
  true_intercept_var <- VarCorr(lmm_t4)$School[1,1]
  if (abs(task4_intercept_var - true_intercept_var) < 0.1) {
    cat("✅ TASK 4 (LMM Intercept Variance): CORRECT!\n")
  } else {
    cat("❌ TASK 4 (LMM Intercept Variance): Incorrect. Expected", round(true_intercept_var, 3), "\n")
  }
  
  # --- 7. Check Task 5 (Bootstrap CI) - Manual Loop ---
  set.seed(student_seed) 
  boot_medians <- numeric(1000)
  for (i in 1:1000) {
    boot_data <- my_data_key[sample(1:nrow(my_data_key), replace = TRUE), ]
    boot_medians[i] <- median(boot_data$SES)
  }
  true_median_ci <- quantile(boot_medians, probs = c(0.025, 0.975))
  true_median_ci_upper <- true_median_ci[2]
  
  if (abs(task5_median_ci_upper - true_median_ci_upper) < 0.1) {
    cat("✅ TASK 5 (Bootstrap Median CI): CORRECT!\n")
  } else {
    cat("❌ TASK 5 (Bootstrap Median CI): Incorrect. Expected", round(true_median_ci_upper, 3), "\n")
  }
  
  # --- 8. Check Task 6 (Cross-Validation RMSE) - Manual Loop ---
  set.seed(student_seed)
  my_data_key$fold <- sample(1:10, size = nrow(my_data_key), replace = TRUE)
  fold_rmses <- numeric(10)
  
  for (i in 1:10) {
    train_data <- my_data_key %>% dplyr::filter(fold != i)
    test_data  <- my_data_key %>% dplyr::filter(fold == i)
    
    # Check for empty folds (can happen with small N)
    if (nrow(test_data) == 0) {
      fold_rmses[i] <- NA # Skip this fold
      next
    }
    
    model <- lm(MathAch ~ SES + SchoolType, data = train_data)
    preds <- predict(model, newdata = test_data)
    
    # Calculate RMSE
    rmse <- sqrt(mean((test_data$MathAch - preds)^2, na.rm = TRUE))
    fold_rmses[i] <- rmse
  }
  true_cv_rmse <- mean(fold_rmses, na.rm = TRUE)
  
  if (abs(task6_cv_rmse - true_cv_rmse) < 0.1) {
    cat("✅ TASK 6 (Cross-Validation RMSE): CORRECT!\n")
  } else {
    cat("❌ TASK 6 (Cross-Validation RMSE): Incorrect. Expected", round(true_cv_rmse, 3), "\n")
  }
  
  cat("\n--- CHECK COMPLETE ---")
}

# 3. Save the *function itself* to a binary file
saveRDS(check_my_work_6, file = "RandomSamplingChecker.rds")

cat("\n'checker_6_no_tidy.rds' file has been created. Give this file to your students.")