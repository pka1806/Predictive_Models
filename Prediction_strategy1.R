#------AMPM-----------------
library(lattice)
library(lme4)
library(tidyverse)
library(Metrics)
library(lubridate)
library(caret)

data_am = read.csv("Dataset_ampm.csv")
 
data_am = data_am %>%
  rename(
    PARITY = PARITYAM, AM_YIELD = MILK_YIELDAM, SCC = SCCAM, HERD = HERDAM, 
    AM_FAT = ACT_FATAM, AM_PRO = ACT_PROAM, MILK_YIELD = TOTAL_MILK_YIELD
  ) %>%
  mutate(
    PARITY_GROUP = case_when(
      PARITY == 1 ~ 'Parity 1',
      PARITY == 2 ~ 'Parity 2',
      PARITY >= 3 ~ 'Parity 3+'
    ),
    STAGE_LACTATION = cut(LACTATION_PERIOD, breaks = c(0, 50, 100, 150, 200, 250, 300, Inf), labels = FALSE),
    LACTATION_PERIOD_W = LACTATION_PERIOD / 7,
    PARITY_GROUP = as.factor(PARITY_GROUP),
    MILK_INTERVAL = as.numeric(MILK_INTERVAL),
    MONTH = month(ymd_hms(MILK_WEIGHING_DTMAM)),
    YEAR = year(ymd_hms(MILK_WEIGHING_DTMAM)),
    Herd_yr = paste(HERD, YEAR, sep = "_"),
    Total_Pro_kg = Pam + Ppm,  
    Total_Fat_kg = Fam + Fpm  
  )


data_am$MONTH = factor(data_am$MONTH, levels = 1:12)


data_am = data_am %>%
  mutate(SUBDIVISION = interaction(PARITY_GROUP, STAGE_LACTATION))


train_data = data_am %>% filter(YEAR %in% c(2020, 2021, 2022))
test_data = data_am %>% filter(YEAR == 2023)


test_data$MONTH = factor(test_data$MONTH, levels = levels(train_data$MONTH))


mape = function(actual, predicted) {
  return(mean(abs((actual - predicted) / actual)) * 100)
}

wmape = function(actual, predicted) {
  return(sum(abs(actual - predicted)) / sum(actual) * 100)
}


subdivisions = unique(train_data$SUBDIVISION)
results = data.frame(
  SUBDIVISION = character(),
  Train_MAPE_MILK = numeric(), Test_MAPE_MILK = numeric(), 
  Train_WMAPE_MILK = numeric(), Test_WMAPE_MILK = numeric(),
  Train_MAPE_FAT = numeric(), Test_MAPE_FAT = numeric(),
  Train_WMAPE_FAT = numeric(), Test_WMAPE_FAT = numeric(),
  Train_MAPE_PRO = numeric(), Test_MAPE_PRO = numeric(),
  Train_WMAPE_PRO = numeric(), Test_WMAPE_PRO = numeric()
)

final_results = data.frame(
  SUBDIVISION = character(),
  Test_MAPE_MILK = numeric(), Test_WMAPE_MILK = numeric(),
  Test_MAPE_FAT = numeric(), Test_WMAPE_FAT = numeric(),
  Test_MAPE_PRO = numeric(), Test_WMAPE_PRO = numeric()
)

# Cross-validation setup
k = 5
folds = createFolds(train_data$Herd_yr, k = k, list = TRUE, returnTrain = TRUE)

for (subdivision in subdivisions) {
  train_subset = train_data %>% filter(SUBDIVISION == subdivision)
  test_subset = test_data %>% filter(SUBDIVISION == subdivision)
  
  if (nrow(train_subset) > 0 && nrow(test_subset) > 0) {
    train_mape_milk_values = c()
    test_mape_milk_values = c()
    train_wmape_milk_values = c()
    test_wmape_milk_values = c()
    
    train_mape_fat_values = c()
    test_mape_fat_values = c()
    train_wmape_fat_values = c()
    test_wmape_fat_values = c()
    
    train_mape_pro_values = c()
    test_mape_pro_values = c()
    train_wmape_pro_values = c()
    test_wmape_pro_values = c()
    
    # Perform Stratified Cross-Validation on 2020-2022 data
    for (i in 1:k) {
      fold_train_indices = folds[[i]]
      fold_train_subset = train_subset[fold_train_indices, ]
      fold_test_indices = setdiff(1:nrow(train_subset), fold_train_indices)
      fold_test_subset = train_subset[fold_test_indices, ]
      
      # Fit mixed-effects models
      model_milk = lmer(MILK_YIELD ~ AM_YIELD + Pam + Fam + MILK_INTERVAL + (1 | Herd_yr), data = fold_train_subset)
      model_fat = lmer(Total_Fat_kg ~ AM_YIELD + Pam + Fam + MILK_INTERVAL + (1 | Herd_yr), data = fold_train_subset)
      model_pro = lmer(Total_Pro_kg ~ AM_YIELD + Pam + Fam + MILK_INTERVAL + (1 | Herd_yr), data = fold_train_subset)
      
  
      fold_train_subset$PREDICTED_TOTAL_MILK_YIELD = predict(model_milk, newdata = fold_train_subset, allow.new.levels = TRUE)
      fold_train_subset$PREDICTED_TOTAL_FAT_KG = predict(model_fat, newdata = fold_train_subset, allow.new.levels = TRUE)
      fold_train_subset$PREDICTED_TOTAL_PRO_KG = predict(model_pro, newdata = fold_train_subset, allow.new.levels = TRUE)
      
      fold_test_subset$PREDICTED_TOTAL_MILK_YIELD = predict(model_milk, newdata = fold_test_subset, allow.new.levels = TRUE)
      fold_test_subset$PREDICTED_TOTAL_FAT_KG = predict(model_fat, newdata = fold_test_subset, allow.new.levels = TRUE)
      fold_test_subset$PREDICTED_TOTAL_PRO_KG = predict(model_pro, newdata = fold_test_subset, allow.new.levels = TRUE)

      fold_train_subset$PREDICTED_TOTAL_PRO_PCT = (fold_train_subset$PREDICTED_TOTAL_PRO_KG / fold_train_subset$PREDICTED_TOTAL_MILK_YIELD) * 100
      fold_train_subset$PREDICTED_TOTAL_FAT_PCT = (fold_train_subset$PREDICTED_TOTAL_FAT_KG / fold_train_subset$PREDICTED_TOTAL_MILK_YIELD) * 100
      
      fold_test_subset$PREDICTED_TOTAL_PRO_PCT = (fold_test_subset$PREDICTED_TOTAL_PRO_KG / fold_test_subset$PREDICTED_TOTAL_MILK_YIELD) * 100
      fold_test_subset$PREDICTED_TOTAL_FAT_PCT  =  (fold_test_subset$PREDICTED_TOTAL_FAT_KG / fold_test_subset$PREDICTED_TOTAL_MILK_YIELD) * 100
      
   
      train_mape_milk_values = c(train_mape_milk_values, mape(fold_train_subset$MILK_YIELD, fold_train_subset$PREDICTED_TOTAL_MILK_YIELD))
      test_mape_milk_values = c(test_mape_milk_values, mape(fold_test_subset$MILK_YIELD, fold_test_subset$PREDICTED_TOTAL_MILK_YIELD))
      train_wmape_milk_values = c(train_wmape_milk_values, wmape(fold_train_subset$MILK_YIELD, fold_train_subset$PREDICTED_TOTAL_MILK_YIELD))
      test_wmape_milk_values = c(test_wmape_milk_values, wmape(fold_test_subset$MILK_YIELD, fold_test_subset$PREDICTED_TOTAL_MILK_YIELD))
      
     
      train_mape_pro_values = c(train_mape_pro_values, mape(fold_train_subset$Total_Pro, fold_train_subset$PREDICTED_TOTAL_PRO_PCT))
      test_mape_pro_values = c(test_mape_pro_values, mape(fold_test_subset$Total_Pro, fold_test_subset$PREDICTED_TOTAL_PRO_PCT))
      train_wmape_pro_values = c(train_wmape_pro_values, wmape(fold_train_subset$Total_Pro, fold_train_subset$PREDICTED_TOTAL_PRO_PCT))
      test_wmape_pro_values = c(test_wmape_pro_values, wmape(fold_test_subset$Total_Pro, fold_test_subset$PREDICTED_TOTAL_PRO_PCT))
      
      train_mape_fat_values = c(train_mape_fat_values, mape(fold_train_subset$Total_fat, fold_train_subset$PREDICTED_TOTAL_FAT_PCT))
      test_mape_fat_values = c(test_mape_fat_values, mape(fold_test_subset$Total_fat, fold_test_subset$PREDICTED_TOTAL_FAT_PCT))
      train_wmape_fat_values = c(train_wmape_fat_values, wmape(fold_train_subset$Total_fat, fold_train_subset$PREDICTED_TOTAL_FAT_PCT))
      test_wmape_fat_values = c(test_wmape_fat_values, wmape(fold_test_subset$Total_fat, fold_test_subset$PREDICTED_TOTAL_FAT_PCT))
    }
    
    avg_train_mape_milk = mean(train_mape_milk_values)
    avg_test_mape_milk = mean(test_mape_milk_values)
    avg_train_wmape_milk = mean(train_wmape_milk_values)
    avg_test_wmape_milk = mean(test_wmape_milk_values)
    
    avg_train_mape_fat = mean(train_mape_fat_values)
    avg_test_mape_fat = mean(test_mape_fat_values)
    avg_train_wmape_fat = mean(train_wmape_fat_values)
    avg_test_wmape_fat = mean(test_wmape_fat_values)
    
    avg_train_mape_pro = mean(train_mape_pro_values)
    avg_test_mape_pro = mean(test_mape_pro_values)
    avg_train_wmape_pro = mean(train_wmape_pro_values)
    avg_test_wmape_pro = mean(test_wmape_pro_values)
    
    results = rbind(results, data.frame(
      SUBDIVISION = subdivision,
      Train_MAPE_MILK = avg_train_mape_milk, Test_MAPE_MILK = avg_test_mape_milk,
      Train_WMAPE_MILK = avg_train_wmape_milk, Test_WMAPE_MILK = avg_test_wmape_milk,
      Train_MAPE_FAT = avg_train_mape_fat, Test_MAPE_FAT = avg_test_mape_fat,
      Train_WMAPE_FAT = avg_train_wmape_fat, Test_WMAPE_FAT = avg_test_wmape_fat,
      Train_MAPE_PRO = avg_train_mape_pro, Test_MAPE_PRO = avg_test_mape_pro,
      Train_WMAPE_PRO = avg_train_wmape_pro, Test_WMAPE_PRO = avg_test_wmape_pro
    ))
    

    full_model_milk = lmer(MILK_YIELD ~ AM_YIELD + Pam + Fam + MILK_INTERVAL + (1 | Herd_yr), data = train_subset)
    full_model_fat = lmer(Total_Fat_kg ~ AM_YIELD + Pam + Fam + MILK_INTERVAL + (1 | Herd_yr), data = train_subset)
    full_model_pro = lmer(Total_Pro_kg ~ AM_YIELD + Pam + Fam + MILK_INTERVAL + (1 | Herd_yr), data = train_subset)
    

    test_subset$PREDICTED_TOTAL_MILK_YIELD = predict(full_model_milk, newdata = test_subset, allow.new.levels = TRUE)
    test_subset$PREDICTED_TOTAL_FAT_KG = predict(full_model_fat, newdata = test_subset, allow.new.levels = TRUE)
    test_subset$PREDICTED_TOTAL_PRO_KG = predict(full_model_pro, newdata = test_subset, allow.new.levels = TRUE)
    
    test_subset$PREDICTED_TOTAL_PRO_PCT  =  (test_subset$PREDICTED_TOTAL_PRO_KG / test_subset$PREDICTED_TOTAL_MILK_YIELD) * 100
    test_subset$PREDICTED_TOTAL_FAT_PCT  =  (test_subset$PREDICTED_TOTAL_FAT_KG / test_subset$PREDICTED_TOTAL_MILK_YIELD) * 100
    

    test_mape_milk = mape(test_subset$MILK_YIELD, test_subset$PREDICTED_TOTAL_MILK_YIELD)
    test_wmape_milk = wmape(test_subset$MILK_YIELD, test_subset$PREDICTED_TOTAL_MILK_YIELD)
    
    test_mape_fat = mape(test_subset$Total_fat, test_subset$PREDICTED_TOTAL_FAT_PCT)
    test_wmape_fat = wmape(test_subset$Total_fat, test_subset$PREDICTED_TOTAL_FAT_PCT)
    
    test_mape_pro = mape(test_subset$Total_Pro, test_subset$PREDICTED_TOTAL_PRO_PCT)
    test_wmape_pro = wmape(test_subset$Total_Pro, test_subset$PREDICTED_TOTAL_PRO_PCT)
    
    final_results = rbind(final_results, data.frame(
      SUBDIVISION = subdivision,
      Test_MAPE_MILK = test_mape_milk, Test_WMAPE_MILK = test_wmape_milk,
      Test_MAPE_FAT = test_mape_fat, Test_WMAPE_FAT = test_wmape_fat,
      Test_MAPE_PRO = test_mape_pro, Test_WMAPE_PRO = test_wmape_pro
    ))
    
  }
}

print(results)
print(final_results)

overall_final_test_mape_milk = mean(final_results$Test_MAPE_MILK)
overall_final_test_wmape_milk = mean(final_results$Test_WMAPE_MILK)
overall_final_test_mape_fat = mean(final_results$Test_MAPE_FAT)
overall_final_test_wmape_fat = mean(final_results$Test_WMAPE_FAT)
overall_final_test_mape_pro = mean(final_results$Test_MAPE_PRO)
overall_final_test_wmape_pro = mean(final_results$Test_WMAPE_PRO)

cat("Final Test MAPE for MILK_YIELD in 2023:", overall_final_test_mape_milk, "\n")
cat("Final Test WMAPE for MILK_YIELD in 2023:", overall_final_test_wmape_milk, "\n")
cat("Final Test MAPE for TOTAL_FAT in 2023:", overall_final_test_mape_fat, "\n")
cat("Final Test WMAPE for TOTAL_FAT in 2023:", overall_final_test_wmape_fat, "\n")
cat("Final Test MAPE for TOTAL_PRO in 2023:", overall_final_test_mape_pro, "\n")
cat("Final Test WMAPE for TOTAL_PRO in 2023:", overall_final_test_wmape_pro, "\n")



#---------PMAM-------------------------------------------------------------------------

library(lme4)
library(tidyverse)
library(caret)
library(Metrics)
library(lubridate)


data_pm = read.csv("Dataset_pmam.csv")
data_pm = data_pm %>%
  rename(
    PARITY = PARITYPM, PM_YIELD = MILK_YIELDPM, SCC = SCCPM, HERD = HERDPM, 
    PM_FAT = ACT_FATPM, PM_PRO = ACT_PROPM, MILK_YIELD = TOTAL_MILK_YIELD
  ) %>%
  mutate(
    PARITY_GROUP = case_when(
      PARITY == 1 ~ 'Parity 1',
      PARITY == 2 ~ 'Parity 2',
      PARITY >= 3 ~ 'Parity 3+'
    ),
    STAGE_LACTATION = cut(LACTATION_PERIOD, breaks = c(0, 50, 100, 150, 200, 250, 300, Inf), labels = FALSE),
    LACTATION_PERIOD_W = LACTATION_PERIOD / 7,
    PARITY_GROUP = as.factor(PARITY_GROUP),
    MILK_INTERVAL = as.numeric(MILK_INTERVAL),
    MONTH = month(ymd_hms(MILK_WEIGHING_DTMPM)),
    YEAR = year(ymd_hms(MILK_WEIGHING_DTMPM)),
    Herd_yr = paste(HERD, YEAR, sep = "_"),
    Total_Pro_kg = Pam + Ppm,  
    Total_Fat_kg = Fam + Fpm  
  )


data_pm$MONTH = factor(data_pm$MONTH, levels = 1:12)


data_pm = data_pm %>%
  mutate(SUBDIVISION = interaction(PARITY_GROUP, STAGE_LACTATION))


train_data = data_pm %>% filter(YEAR %in% c(2020, 2021, 2022))
test_data = data_pm %>% filter(YEAR == 2023)


mape = function(actual, predicted) {
  return(mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100)
}

wmape = function(actual, predicted) {
  return(sum(abs(actual - predicted), na.rm = TRUE) / sum(actual, na.rm = TRUE) * 100)
}


subdivisions = unique(train_data$SUBDIVISION)
results = data.frame(
  SUBDIVISION = character(),
  Train_MAPE_MILK = numeric(), Test_MAPE_MILK = numeric(), 
  Train_WMAPE_MILK = numeric(), Test_WMAPE_MILK = numeric(),
  Train_MAPE_FAT = numeric(), Test_MAPE_FAT = numeric(),
  Train_WMAPE_FAT = numeric(), Test_WMAPE_FAT = numeric(),
  Train_MAPE_PRO = numeric(), Test_MAPE_PRO = numeric(),
  Train_WMAPE_PRO = numeric(), Test_WMAPE_PRO = numeric()
)

# Cross-validation setup
k = 5
set.seed(42)
folds = createFolds(train_data$Herd_yr, k = k, list = TRUE, returnTrain = TRUE)

for (subdivision in subdivisions) {
  train_subset = train_data %>% filter(SUBDIVISION == subdivision)
  test_subset = test_data %>% filter(SUBDIVISION == subdivision)
  
  if (nrow(train_subset) > 0 && nrow(test_subset) > 0) {
    train_mape_milk_values = c()
    test_mape_milk_values = c()
    train_wmape_milk_values = c()
    test_wmape_milk_values = c()
    
    train_mape_fat_values = c()
    test_mape_fat_values = c()
    train_wmape_fat_values = c()
    test_wmape_fat_values = c()
    
    train_mape_pro_values = c()
    test_mape_pro_values = c()
    train_wmape_pro_values = c()
    test_wmape_pro_values = c()
    
    # Perform Stratified Cross-Validation on 2020-2022 data
    for (i in 1:k) {
      fold_train_indices = folds[[i]]
      fold_train_subset = train_subset[fold_train_indices, ]
      fold_test_indices = setdiff(1:nrow(train_subset), fold_train_indices)
      fold_test_subset = train_subset[fold_test_indices, ]
      
      # Fit mixed-effects models
      model_milk = lmer(MILK_YIELD ~ PM_YIELD + Ppm + Fpm + MILK_INTERVAL + (1 | Herd_yr), data = fold_train_subset)
      model_fat = lmer(Total_Fat_kg ~ PM_YIELD + Ppm + Fpm + MILK_INTERVAL + (1 | Herd_yr), data = fold_train_subset)
      model_pro = lmer(Total_Pro_kg ~ PM_YIELD + Ppm + Fpm + MILK_INTERVAL + (1 | Herd_yr), data = fold_train_subset)
      

      fold_test_subset$PREDICTED_TOTAL_MILK_YIELD = predict(model_milk, newdata = fold_test_subset, allow.new.levels = TRUE)
      fold_test_subset$PREDICTED_TOTAL_FAT_KG = predict(model_fat, newdata = fold_test_subset, allow.new.levels = TRUE)
      fold_test_subset$PREDICTED_TOTAL_PRO_KG = predict(model_pro, newdata = fold_test_subset, allow.new.levels = TRUE)
      
      fold_test_subset$PREDICTED_TOTAL_PRO_PCT = fold_test_subset$PREDICTED_TOTAL_PRO_KG / fold_test_subset$PREDICTED_TOTAL_MILK_YIELD) * 100
      fold_test_subset$PREDICTED_TOTAL_FAT_PCT = fold_test_subset$PREDICTED_TOTAL_FAT_KG / fold_test_subset$PREDICTED_TOTAL_MILK_YIELD) * 100
      

      train_mape_milk_values = c(train_mape_milk_values, mape(fold_train_subset$MILK_YIELD, fold_test_subset$PREDICTED_TOTAL_MILK_YIELD))
      test_mape_milk_values = c(test_mape_milk_values, mape(fold_test_subset$MILK_YIELD, fold_test_subset$PREDICTED_TOTAL_MILK_YIELD))
      train_wmape_milk_values = c(train_wmape_milk_values, wmape(fold_train_subset$MILK_YIELD, fold_test_subset$PREDICTED_TOTAL_MILK_YIELD))
      test_wmape_milk_values = c(test_wmape_milk_values, wmape(fold_test_subset$MILK_YIELD, fold_test_subset$PREDICTED_TOTAL_MILK_YIELD))
      

      train_mape_pro_values = c(train_mape_pro_values, mape(fold_train_subset$Total_Pro_kg, fold_test_subset$PREDICTED_TOTAL_PRO_KG))
      test_mape_pro_values = c(test_mape_pro_values, mape(fold_test_subset$Total_Pro_kg, fold_test_subset$PREDICTED_TOTAL_PRO_KG))
      train_wmape_pro_values = c(train_wmape_pro_values, wmape(fold_train_subset$Total_Pro_kg, fold_test_subset$PREDICTED_TOTAL_PRO_KG))
      test_wmape_pro_values = c(test_wmape_pro_values, wmape(fold_test_subset$Total_Pro_kg, fold_test_subset$PREDICTED_TOTAL_PRO_KG))
      
      train_mape_fat_values = c(train_mape_fat_values, mape(fold_train_subset$Total_Fat_kg, fold_test_subset$PREDICTED_TOTAL_FAT_KG))
      test_mape_fat_values = c(test_mape_fat_values, mape(fold_test_subset$Total_Fat_kg, fold_test_subset$PREDICTED_TOTAL_FAT_KG))
      train_wmape_fat_values = c(train_wmape_fat_values, wmape(fold_train_subset$Total_Fat_kg, fold_test_subset$PREDICTED_TOTAL_FAT_KG))
      test_wmape_fat_values = c(test_wmape_fat_values, wmape(fold_test_subset$Total_Fat_kg, fold_test_subset$PREDICTED_TOTAL_FAT_KG))
    }

    avg_train_mape_milk = mean(train_mape_milk_values, na.rm = TRUE)
    avg_test_mape_milk = mean(test_mape_milk_values, na.rm = TRUE)
    avg_train_wmape_milk = mean(train_wmape_milk_values, na.rm = TRUE)
    avg_test_wmape_milk = mean(test_wmape_milk_values, na.rm = TRUE)
    
    avg_train_mape_fat = mean(train_mape_fat_values, na.rm = TRUE)
    avg_test_mape_fat = mean(test_mape_fat_values, na.rm = TRUE)
    avg_train_wmape_fat = mean(train_wmape_fat_values, na.rm = TRUE)
    avg_test_wmape_fat = mean(test_wmape_fat_values, na.rm = TRUE)
    
    avg_train_mape_pro = mean(train_mape_pro_values, na.rm = TRUE)
    avg_test_mape_pro = mean(test_mape_pro_values, na.rm = TRUE)
    avg_train_wmape_pro = mean(train_wmape_pro_values, na.rm = TRUE)
    avg_test_wmape_pro = mean(test_wmape_pro_values, na.rm = TRUE)
    
    results = rbind(results, data.frame(
      SUBDIVISION = subdivision,
      Train_MAPE_MILK = avg_train_mape_milk, Test_MAPE_MILK = avg_test_mape_milk,
      Train_WMAPE_MILK = avg_train_wmape_milk, Test_WMAPE_MILK = avg_test_wmape_milk,
      Train_MAPE_FAT = avg_train_mape_fat, Test_MAPE_FAT = avg_test_mape_fat,
      Train_WMAPE_FAT = avg_train_wmape_fat, Test_WMAPE_FAT = avg_test_wmape_fat,
      Train_MAPE_PRO = avg_train_mape_pro, Test_MAPE_PRO = avg_test_mape_pro,
      Train_WMAPE_PRO = avg_train_wmape_pro, Test_WMAPE_PRO = avg_test_wmape_pro
    ))
  }
}

print(results)

final_results = data.frame(
  SUBDIVISION = character(),
  Test_MAPE_MILK = numeric(), Test_WMAPE_MILK = numeric(),
  Test_MAPE_FAT = numeric(), Test_WMAPE_FAT = numeric(),
  Test_MAPE_PRO = numeric(), Test_WMAPE_PRO = numeric()
)

for (subdivision in subdivisions) {
  train_subset = train_data %>% filter(SUBDIVISION == subdivision)
  test_subset = test_data %>% filter(SUBDIVISION == subdivision)
  
  if (nrow(train_subset) > 0 && nrow(test_subset) > 0) {
   
    model_milk = lmer(MILK_YIELD ~ PM_YIELD + Ppm + Fpm + MILK_INTERVAL + (1 | Herd_yr), data = train_subset)
    model_fat = lmer(Total_Fat_kg ~ PM_YIELD + Ppm + Fpm + MILK_INTERVAL + (1 | Herd_yr), data = train_subset)
    model_pro = lmer(Total_Pro_kg ~ PM_YIELD + Ppm + Fpm + MILK_INTERVAL + (1 | Herd_yr), data = train_subset)

    test_subset$PREDICTED_TOTAL_MILK_YIELD = predict(model_milk, newdata = test_subset, allow.new.levels = TRUE)
    test_subset$PREDICTED_TOTAL_FAT_KG = predict(model_fat, newdata = test_subset, allow.new.levels = TRUE)
    test_subset$PREDICTED_TOTAL_PRO_KG = predict(model_pro, newdata = test_subset, allow.new.levels = TRUE)
    
    test_subset$PREDICTED_TOTAL_PRO_PCT = (test_subset$PREDICTED_TOTAL_PRO_KG / test_subset$PREDICTED_TOTAL_MILK_YIELD) * 100
    test_subset$PREDICTED_TOTAL_FAT_PCT  =  (test_subset$PREDICTED_TOTAL_FAT_KG / test_subset$PREDICTED_TOTAL_MILK_YIELD) * 100
    
  
    test_mape_milk = mape(test_subset$MILK_YIELD, test_subset$PREDICTED_TOTAL_MILK_YIELD)
    test_wmape_milk = wmape(test_subset$MILK_YIELD, test_subset$PREDICTED_TOTAL_MILK_YIELD)
    
    test_mape_fat = mape(test_subset$Total_fat, test_subset$PREDICTED_TOTAL_FAT_PCT)
    test_wmape_fat = wmape(test_subset$Total_fat, test_subset$PREDICTED_TOTAL_FAT_PCT)
    
    test_mape_pro = mape(test_subset$Total_Pro, test_subset$PREDICTED_TOTAL_PRO_PCT)
    test_wmape_pro = wmape(test_subset$Total_Pro, test_subset$PREDICTED_TOTAL_PRO_PCT)
    
    final_results = rbind(final_results, data.frame(
      SUBDIVISION = subdivision,
      Test_MAPE_MILK = test_mape_milk, Test_WMAPE_MILK = test_wmape_milk,
      Test_MAPE_FAT = test_mape_fat, Test_WMAPE_FAT = test_wmape_fat,
      Test_MAPE_PRO = test_mape_pro, Test_WMAPE_PRO = test_wmape_pro
    ))
  }
}

print(final_results)

overall_final_test_mape_milk = mean(final_results$Test_MAPE_MILK)
overall_final_test_wmape_milk = mean(final_results$Test_WMAPE_MILK)
overall_final_test_mape_fat = mean(final_results$Test_MAPE_FAT)
overall_final_test_wmape_fat = mean(final_results$Test_WMAPE_FAT)
overall_final_test_mape_pro = mean(final_results$Test_MAPE_PRO)
overall_final_test_wmape_pro = mean(final_results$Test_WMAPE_PRO)

cat("Final Test MAPE for MILK_YIELD in 2023:", overall_final_test_mape_milk, "\n")
cat("Final Test WMAPE for MILK_YIELD in 2023:", overall_final_test_wmape_milk, "\n")
cat("Final Test MAPE for TOTAL_FAT in 2023:", overall_final_test_mape_fat, "\n")
cat("Final Test WMAPE for TOTAL_FAT in 2023:", overall_final_test_wmape_fat, "\n")
cat("Final Test MAPE for TOTAL_PRO in 2023:", overall_final_test_mape_pro, "\n")
cat("Final Test WMAPE for TOTAL_PRO in 2023:", overall_final_test_wmape_pro, "\n")


