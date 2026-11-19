mydata <- read.csv("G:/Chrome downloads/IDS_MID_PROJECT/archive/healthcare-dataset-stroke-data.csv",
                   header = TRUE, sep = ",")
print(mydata)
dim(mydata)

nrow(mydata)

ncol(mydata)

names(mydata)

str(mydata)

head(mydata)

summary(mydata)

summary(mydata[c("age", "avg_glucose_level", "bmi")])

table(mydata$stroke)

table(mydata$stroke, useNA = "ifany")

round(prop.table(table(mydata$stroke)) * 100, 2)




mydata <- read.csv("G:/Chrome downloads/IDS_MID_PROJECT/archive/healthcare-dataset-stroke-data.csv",
                   header=TRUE, sep=",", na.strings="N/A")
sum(is.na(mydata))

colSums(is.na(mydata))

head(which(is.na(mydata$bmi)))

sum(is.na(mydata$bmi))

round(sum(is.na(mydata$bmi)) / nrow(mydata) * 100, 2)

mydata_remove <- na.omit(mydata)

dim(mydata_remove)

nrow(mydata_remove)

sum(is.na(mydata_remove))

bmi_mean <- mean(mydata$bmi, na.rm=TRUE)

bmi_median <- median(mydata$bmi, na.rm=TRUE)

bmi_mean

bmi_median

mydata_mean <- mydata

mydata_mean$bmi[is.na(mydata_mean$bmi)] <- bmi_mean

sum(is.na(mydata_mean$bmi))

mydata_median <- mydata

mydata_median$bmi[is.na(mydata_median$bmi)] <- bmi_median

sum(is.na(mydata_median$bmi))

dim(mydata)

dim(mydata_remove)

dim(mydata_mean)

dim(mydata_median)

sd(mydata$bmi, na.rm=TRUE)

sd(mydata_mean$bmi)

sd(mydata_median$bmi)

final_data <- mydata_median

dim(final_data)

sum(is.na(final_data$bmi))




final_data_outlier <- final_data

numeric_columns <- c("id", "age", "hypertension", "heart_disease",
                     "avg_glucose_level", "bmi", "stroke")

numeric_columns

par(mfrow = c(2, 2))

for (col in numeric_columns) {
  boxplot(final_data_outlier[[col]], main = col)
}

selected_columns <- c("age", "avg_glucose_level", "bmi")

outlier_results <- data.frame()

for (col in selected_columns) {
  
  x <- final_data_outlier[[col]]
  
  q1 <- quantile(x, 0.25)
  
  q3 <- quantile(x, 0.75)
  
  iqr_value <- IQR(x)
  
  lower_limit <- q1 - 1.5 * iqr_value
  
  upper_limit <- q3 + 1.5 * iqr_value
  
  before <- sum(x < lower_limit | x > upper_limit)
  
  x[x < lower_limit] <- lower_limit
  
  x[x > upper_limit] <- upper_limit
  
  final_data_outlier[[col]] <- x
  
  after <- sum(x < lower_limit | x > upper_limit)
  
  outlier_results <- rbind(outlier_results,
                           data.frame(Attribute = col,
                                      Before = before,
                                      After = after))
}

outlier_results

par(mfrow = c(2, 2))

for (col in selected_columns) {
  boxplot(final_data_outlier[[col]], main = col)
}

dim(final_data_outlier)

sum(is.na(final_data_outlier))




final_data_type <- final_data_outlier

table(final_data_type$ever_married)
table(final_data_type$Residence_type)

str(final_data_type[c("ever_married", "Residence_type")])

final_data_type$ever_married <- ifelse(final_data_type$ever_married == "Yes", 1, 0)

final_data_type$Residence_type <- ifelse(final_data_type$Residence_type == "Urban", 1, 0)

head(final_data_type[c("ever_married", "Residence_type")])

str(final_data_type[c("ever_married", "Residence_type")])

table(final_data_type$hypertension)
table(final_data_type$heart_disease)
table(final_data_type$stroke)

str(final_data_type[c("hypertension", "heart_disease", "stroke")])

final_data_type$hypertension <- ifelse(final_data_type$hypertension == 1, "Yes", "No")

final_data_type$heart_disease <- ifelse(final_data_type$heart_disease == 1, "Yes", "No")

final_data_type$stroke <- ifelse(final_data_type$stroke == 1, "Stroke", "No Stroke")

head(final_data_type[c("hypertension", "heart_disease", "stroke")])

str(final_data_type[c("hypertension", "heart_disease", "stroke")])

dim(final_data_type)

sum(is.na(final_data_type))



final_data_normalized <- final_data_type

continuous_columns <- c("age", "avg_glucose_level")

for (col in continuous_columns) {
  
  min_value <- min(final_data_normalized[[col]])
  
  max_value <- max(final_data_normalized[[col]])
  
  final_data_normalized[[col]] <- 
    (final_data_normalized[[col]] - min_value) / (max_value - min_value)
}

summary(final_data_normalized[continuous_columns])

dim(final_data_normalized)

sum(is.na(final_data_normalized))





final_data_unique <- final_data_type

sum(duplicated(final_data_unique))

final_data_unique <- final_data_unique[!duplicated(final_data_unique), ]

sum(duplicated(final_data_unique))

dim(final_data_type)

dim(final_data_unique)




final_data_filter <- final_data_unique

final_data_filter <- final_data_filter[final_data_filter$age >= 50, ]

head(final_data_filter)

dim(final_data_filter)

summary(final_data_filter$age)




final_data_valid <- final_data_unique

sum(final_data_valid$age < 0 | final_data_valid$age > 120)

sum(final_data_valid$avg_glucose_level <= 0)

sum(final_data_valid$bmi <= 0)

invalid_rows <- final_data_valid$age < 0 |
  final_data_valid$age > 120 |
  final_data_valid$avg_glucose_level <= 0 |
  final_data_valid$bmi <= 0

sum(invalid_rows)

final_data_valid <- final_data_valid[!invalid_rows, ]

dim(final_data_valid)

sum(is.na(final_data_valid))




final_data_feature <- final_data_valid

final_data_feature$age_group <- ifelse(final_data_feature$age < 18, "Child",
                                       ifelse(final_data_feature$age < 40, "Young Adult",
                                              ifelse(final_data_feature$age < 60, "Middle-aged",
                                                     "Senior")))

head(final_data_feature[c("age", "age_group")])

table(final_data_feature$age_group)

str(final_data_feature[c("age", "age_group")])

dim(final_data_feature)




table(final_data_feature$stroke)

stroke_data <- final_data_feature[final_data_feature$stroke == "Stroke", ]

no_stroke_data <- final_data_feature[final_data_feature$stroke == "No Stroke", ]

set.seed(123)

minority_count <- nrow(stroke_data)

majority_count <- nrow(no_stroke_data)

under_sampled <- no_stroke_data[sample(nrow(no_stroke_data), minority_count), ]

under_data <- rbind(stroke_data, under_sampled)

table(under_data$stroke)

set.seed(123)

over_sampled <- stroke_data[sample(nrow(stroke_data), majority_count, replace = TRUE), ]

over_data <- rbind(no_stroke_data, over_sampled)

table(over_data$stroke)

dim(under_data)

dim(over_data)




set.seed(123)

data_size <- nrow(final_data_normalized)

train_index <- sample(1:data_size, size = floor(0.7 * data_size))

training_data <- final_data_normalized[train_index, ]

testing_data <- final_data_normalized[-train_index, ]

dim(training_data)

dim(testing_data)

dim(final_data_normalized)




imputation_data <- mydata

sum(is.na(imputation_data$bmi))

bmi_mean <- mean(imputation_data$bmi, na.rm = TRUE)

bmi_median <- median(imputation_data$bmi, na.rm = TRUE)

bmi_mean

bmi_median

mean_imputed <- imputation_data

mean_imputed$bmi[is.na(mean_imputed$bmi)] <- bmi_mean

median_imputed <- imputation_data

median_imputed$bmi[is.na(median_imputed$bmi)] <- bmi_median

sum(is.na(mean_imputed$bmi))

sum(is.na(median_imputed$bmi))

sd(imputation_data$bmi, na.rm = TRUE)

sd(mean_imputed$bmi)

sd(median_imputed$bmi)

summary(mean_imputed$bmi)

summary(median_imputed$bmi)

par(mfrow = c(1, 2))

boxplot(mean_imputed$bmi, main = "Mean Imputation")

boxplot(median_imputed$bmi, main = "Median Imputation")




aggregate(cbind(age, avg_glucose_level, bmi) ~ stroke,
          data = final_data_outlier,
          FUN = mean)

aggregate(cbind(age, avg_glucose_level, bmi) ~ stroke,
          data = final_data_outlier,
          FUN = median)

aggregate(cbind(age, avg_glucose_level, bmi) ~ stroke,
          data = final_data_outlier,
          FUN = sd)

aggregate(cbind(age, avg_glucose_level, bmi) ~ stroke,
          data = final_data_outlier,
          FUN = min)

aggregate(cbind(age, avg_glucose_level, bmi) ~ stroke,
          data = final_data_outlier,
          FUN = max)




table(final_data_outlier$ever_married)

aggregate(avg_glucose_level ~ ever_married,
          data = final_data_outlier,
          FUN = mean)

aggregate(avg_glucose_level ~ ever_married,
          data = final_data_outlier,
          FUN = median)




table(final_data_outlier$smoking_status)

variability_results <- aggregate(
  bmi ~ smoking_status,
  data = final_data_outlier,
  FUN = function(x) c(
    IQR = IQR(x),
    SD = sd(x),
    Variance = var(x),
    Range = diff(range(x))
  )
)

variability_results

