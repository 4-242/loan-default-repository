library(dplyr)
library(ggplot2)
library(naniar)
library(VIM)
loan_default <- read.csv("C:/Users/User/Desktop/R BOOKS/Loan_Default.csv",header=TRUE)
str(loan_default)
summary(loan_default)
gg_miss_upset(loan_default)


loan_default$Status <- factor(loan_default$Status, levels = c(0,1), labels = c("NOT","Defaulter"))
loan_default <- loan_default %>% select(loan_amount,rate_of_interest,Interest_rate_spread,Upfront_charges,property_value,income,Credit_Score,age,dtir1,Status,credit_type,lump_sum_payment,co.applicant_credit_type,Gender,Region)
summary(loan_default)

library(imputeMissings)
loan_imp <- impute(loan_default, method = "median/mode")
summary(loan_imp)
numeric_data <- loan_imp[,sapply(loan_imp,is.numeric)]
cor(numeric_data)

ggplot(loan_imp, aes(x = Gender, fill = factor(Status)))+
  geom_density(alpha = 0.5)+
  facet_grid(age ~ Region)+
  labs(title = "Proportion of Defaulters by Age, Region, and Gender", x = "Gender", fill = "Status")+
  scale_fill_manual(values = c("NOT"="blue", "Defaulter"= "red"), labels = c("Non-Defaulters","Defaulters"))+
  theme_minimal()

ggplot(loan_imp, aes(x = factor(Status), y = income, fill = factor(Status)))+
  geom_boxplot()+
  labs(title = "Income Distribution by Default Status", x = "Status", y = "income", fill = "Status")+
  scale_fill_manual(values = c("NOT" = "blue", "Defaulter" = "red"),
                    labels = c("Non-Defaulters","Defaulters"))+
  theme_minimal()

ggplot(loan_imp, aes(x = Region, fill = factor(Status)))+
  geom_bar(position = "fill")+
  labs(title = "Proportion of Defaulters by Region", x = "Region", y = "Proportion", fill = "Status")+
  scale_fill_manual(values = c("NOT"="blue","Defaulter"="red"),
                    labels = c("Non-Defaulters","Defaulters"))+
  theme_minimal()

ggplot(loan_imp, aes(x = age, fill = factor(Status)))+
  geom_bar(position = "fill")+
  labs(title = "Proportion of Defaulters by Age", x = "age", y = "Proportion", fill = "Status")+
  scale_fill_manual(values = c("NOT"="blue","Defaulter"="red"),
                    labels = c("Non-Defaulters","Defaulters"))+
  theme_minimal()



library(reshape2)
melted_cor <- melt(cor(numeric_data))
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value))+
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0)+
  labs(title = "Correlation Heatmap of Numeric Variables")+
  theme_minimal()



#library(GGally)
#ggpairs(loan_imp, columns = c("loan_amount","rate_of_interest","Interest_rate_spread","Upfront_charges","property_value","income","Credit_Score","age","dtir1","Status","credit_type","lump_sum_payment","co.applicant_credit_type","Gender","Region"),
#aes(color = factor(Status), alpha = 0.6)
#)+
 # labs(title = "Scatterplot matrix for Default Status")+
  #theme_minimal()

matrixplot(loan_imp, main = "Matrix Plot After Imputation")


str(unique(loan_imp$age))
loan_imp <- loan_imp %>%
  mutate(age = case_when(
    age == "<25" ~ "1-24",
    age == ">74" ~ "75-100",
    TRUE ~ age
  ))



library(rsample)
library(caret)
library(vip)
library(dplyr)
library(broom)

df <- loan_imp %>% mutate_if(is.ordered, factor, ordered = FALSE)

set.seed(123)
loan_split <- initial_split(df, prop = 0.7, strata = "Status")
loan_train <- training(loan_split)
loan_test <- testing(loan_split)

logistic <- glm(
  Status ~ loan_amount + rate_of_interest + Interest_rate_spread + Upfront_charges + property_value + income + Credit_Score + age + dtir1 + credit_type + lump_sum_payment + co.applicant_credit_type + Gender + Region,
  family = "binomial",
  data = loan_train
  )

tidy(logistic)
exp(coef(logistic))

set.seed(123)

cv_logistic <- train(
  Status ~ loan_amount + rate_of_interest + Interest_rate_spread + Upfront_charges + property_value + income + Credit_Score + age + dtir1 + credit_type + lump_sum_payment + co.applicant_credit_type + Gender + Region,
  data = loan_train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 5, summaryFunction = twoClassSummary, classProbs = TRUE),
  metric = "ROC"
)



pred_class <- predict(cv_logistic, loan_test)

confusionMatrix(
  data = relevel(pred_class, ref = "Defaulter"),
  reference = relevel(loan_test$Status, ref = "Defaulter")
)





conf_matrix <- confusionMatrix(
  data = relevel(pred_class, ref = "Defaulter"),
  reference = relevel(loan_test$Status, ref = "Defaulter")
)

conf_table <- as.data.frame(conf_matrix$table)
ggplot(conf_table, aes(x=Prediction, y = Reference, fill= Freq)) +
  geom_tile(color = "white") + scale_fill_gradient(low="white", high = "darkorange1")+
  geom_text(aes(label = Freq), color = "black", size = 5) +
  labs(title = "Logistic Regression Heatmap", x = "Predicted", y = "Actual") +
  theme_minimal()


set.seed(123)
cv_decision <- train(
  Status ~ loan_amount + rate_of_interest + Interest_rate_spread + Upfront_charges + property_value + income + Credit_Score + age + dtir1 + credit_type + lump_sum_payment + co.applicant_credit_type + Gender + Region,
  data = loan_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5)
)

pred_class_b <- predict(cv_decision, loan_test)

confusionMatrix(
  data = relevel(pred_class_b, ref = "Defaulter"),
  reference = relevel(loan_test$Status, ref = "Defaulter")
)

conf_matrix_b <- confusionMatrix(
  data = relevel(pred_class_b, ref = "Defaulter"),
  reference = relevel(loan_test$Status, ref = "Defaulter")
)


conf_table_b <- as.data.frame(conf_matrix_b$table)
ggplot(conf_table_b, aes(x=Prediction, y = Reference, fill= Freq)) +
  geom_tile(color = "white") + scale_fill_gradient(low="white", high = "olivedrab3")+
  geom_text(aes(label = Freq), color = "black", size = 5) +
  labs(title = "Decision Tree Heatmap", x = "Predicted", y = "Actual") +
  theme_minimal()

library(rpart.plot)
rpart.plot(cv_decision$finalModel, main = "Decision Tree")

library(pROC)
logistic_probs <- predict(cv_logistic, loan_test, type = "prob")[,2] 
actual_labels <- ifelse(loan_test$Status == "Defaulter", 1, 0)
roc_curve_logistic <- roc(actual_labels, logistic_probs)
plot(roc_curve_logistic, main = "ROC Curve for Logistic Regression and Decision Tree", col = "darkorange1", lwd = 2)


decision_probs <- predict(cv_decision, loan_test, type = "prob")[,2]
roc_curve_decision <- roc(actual_labels, decision_probs)
plot(roc_curve_decision, col = "olivedrab3",lwd = 2, add = TRUE)
legend("bottomright", legend = c("Logistic Regression", "Decision Tree"), col = c("darkorange1","olivedrab3"),bg = "transparent" ,box.lty = 0 ,lwd = 2)




#######TWENTIES
twenties_data <- filter(df, age == "1-24")

set.seed(123)
cv_twenties_decision <- train(
  Status ~ loan_amount + rate_of_interest + Interest_rate_spread + Upfront_charges + property_value + income + Credit_Score + dtir1 + credit_type + lump_sum_payment + co.applicant_credit_type + Gender + Region,
  data = twenties_data,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 2)
)

library(vip)
vip(cv_twenties_decision, num_features = 14)


######THIRTIES
thirties_data <- filter(df, age == "25-34")

set.seed(123)
cv_thirties_decision <- train(
  Status ~ loan_amount + rate_of_interest + Interest_rate_spread + Upfront_charges + property_value + income + Credit_Score  + dtir1 + credit_type + lump_sum_payment + co.applicant_credit_type + Gender + Region,
  data = thirties_data,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 2)
)

vip(cv_thirties_decision, num_features = 14)



######FORTIES
forties_data <- filter(df, age == "35-44")

set.seed(123)
cv_forties_decision <- train(
  Status ~ loan_amount + rate_of_interest + Interest_rate_spread + Upfront_charges + property_value + income + Credit_Score  + dtir1 + credit_type + lump_sum_payment + co.applicant_credit_type + Gender + Region,
  data = forties_data,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 2)
)

vip(cv_forties_decision, num_features = 14)



######FIFTIES
fifties_data <- filter(df, age == "45-54")

set.seed(123)
cv_fifties_decision <- train(
  Status ~ loan_amount + rate_of_interest + Interest_rate_spread + Upfront_charges + property_value + income + Credit_Score + dtir1 + credit_type + lump_sum_payment + co.applicant_credit_type + Gender + Region,
  data = fifties_data,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 2)
)

vip(cv_fifties_decision, num_features = 14)



######SIXTIES
sixties_data <- filter(df, age == "55-64")

set.seed(123)
cv_sixties_decision <- train(
  Status ~ loan_amount + rate_of_interest + Interest_rate_spread + Upfront_charges + property_value + income + Credit_Score + dtir1 + credit_type + lump_sum_payment + co.applicant_credit_type + Gender + Region,
  data = sixties_data,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 2)
)

vip(cv_sixties_decision, num_features = 14)



######SEVENTIES
seventies_data <- filter(df, age == "65-74")

set.seed(123)
cv_seventies_decision <- train(
  Status ~ loan_amount + rate_of_interest + Interest_rate_spread + Upfront_charges + property_value + income + Credit_Score + dtir1 + credit_type + lump_sum_payment + co.applicant_credit_type + Gender + Region,
  data = seventies_data,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 2)
)

vip(cv_seventies_decision, num_features = 14)



######EIGHTTIES
eighties_data <- filter(df, age == "75-100")

set.seed(123)
cv_eighties_decision <- train(
  Status ~ loan_amount + rate_of_interest + Interest_rate_spread + Upfront_charges + property_value + income + Credit_Score + dtir1 + credit_type + lump_sum_payment + co.applicant_credit_type + Gender + Region,
  data = eighties_data,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 2)
)

vip(cv_eighties_decision, num_features = 14)



