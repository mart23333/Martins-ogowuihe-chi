install.packages("car")
install.packages("ResourceSelection")

# Load libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(pROC)


 # Load data
   hr_data <- read.csv("/Users/septa/Documents/Applied stats/Employee Data.csv")
 
 # Check structure & missing values
   str(hr_data)

 sum(is.na(hr_data)) 


# Convert Attrition and other categorical variables to factors
   hr_data_clean <- hr_data %>%
       mutate(
             Attrition       = factor(Attrition, levels = c("No", "Yes")),
            Business.Travel  = factor(Business.Travel),
             Department      = factor(Department),
             EducationField  = factor(EducationField),
            Gender           = factor(Gender),
             Satisfaction    = factor(Satisfaction),
             Salary          = factor(Salary),
          Home.Office        = factor(Home.Office)
              )

 #check if data is clean 
   
  str(hr_data_clean)

 
 # 1. Check distribution of Attrition
  attrition_counts <- table(hr_data_clean$Attrition)
 attrition_props  <- prop.table(attrition_counts)
 print(attrition_counts)
 print(attrition_props)

 
 
# Plot Attrition distribution
   ggplot(hr_data_clean, aes(x = Attrition, fill = Attrition)) +
       geom_bar() 
      labs(title = "Employee Attrition Distribution") +
       theme_minimal()


 
 # Comparing  categorical variables by Attrition
   hr_data_clean %>%
      group_by(Attrition) %>%
       summarise( 
            Female_Count = sum(Gender == "Female"),
           Male_Count   = sum(Gender == "Male"),
            High_Sat     = sum(Satisfaction == "High"),
            Medium_Sat   = sum(Satisfaction == "Medium"),
            Low_Sat      = sum(Satisfaction == "Low"),
            Near_Home    = sum(Home.Office == "Near"),
             Far_Home     = sum(Home.Office == "Far")
       )
#  Cross-tabulations for other categorical variables
table(hr_data_clean$Business.Travel, hr_data_clean$Attrition)
table(hr_data_clean$Department, hr_data_clean$Attrition)
table(hr_data_clean$EducationField, hr_data_clean$Attrition)
table(hr_data_clean$Salary, hr_data_clean$Attrition)


# Logistic regression model
model <- glm(Attrition ~ Salary + Satisfaction + Business.Travel,
             data = hr_data_clean,
             family = binomial)

# Model summary 
summary(model)

# Odds ratios
exp(coef(model))

# 95% Confidence Intervals for Odds Ratios
exp(confint(model))

# Likelihood Ratio Test for overall model significance
anova(model, test = "Chisq")



# Load the package
library(car)

# Now run VIF
vif(model)


# Check multicollinearity 
library(car)
vif(model)



# Load the package
library(ResourceSelection)

# Run the Hosmer-Lemeshow goodness-of-fit test
hoslem.test(hr_data_clean$Attrition, fitted(model))






# Model fit and predictive accuracy
library(ResourceSelection)
hoslem.test(hr_data_clean$Attrition, fitted(model))

# Recode Attrition as numeric (1 = Yes, 0 = No)
hr_data_clean$Attrition_num <- ifelse(hr_data_clean$Attrition == "Yes", 1, 0)
hoslem.test(hr_data_clean$Attrition_num, fitted(model))
hoslem.test(hr_data_clean$Attrition_num, fitted(model), g = 5)





# Create ROC curve using fitted probabilities from the logistic regression
roc_curve <- roc(hr_data_clean$Attrition_num, fitted(model))

# Plot ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Attrition Model")

# Get AUC (Area Under Curve)
auc(roc_curve)

library(pROC)
roc_curve <- roc(hr_data_clean$Attrition, fitted(model))
plot(roc_curve)
auc(roc_curve)

