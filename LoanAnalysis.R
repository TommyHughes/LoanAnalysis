# Script Name : LoanAnalysis.R
# Created On: April 14, 2019
# Author: Thomas Hughes
# Purpose: Initial examination of loanbook
# Version: 1.0
# Execution: Must be run as a Source to await user input. Visit kaggle Lending Club Loan Data for .csv file.

library(ggplot2)
library(tidyverse)
library(data.table)
library(DescTools)
library(caTools)

  ## IMPORT----

  # Load the loan file.
loans <- fread(input = "loan.csv", select = c("loan_amnt", "term", "int_rate", "grade",
                                              "emp_title", "emp_length", "home_ownership",
                                              "annual_inc", "issue_d", "loan_status", "purpose",
                                              "title", "dti", "delinq_2yrs"))

  ## Cleaning----

  # Let's get rid of any NA values.
loans <- na.omit(loans, invert = F)

  # We assume that any dti > 100 is reported in error, and thus remove them.
loans <- loans[dti < 100 | dti == 100,]

  # We convert the issue_d column from a chr object to a Date object. It is
  # missing day information so we insert that ourselves
loans <- loans[, issue_d := as.Date(stringr::str_replace(loans[, issue_d], "^", "01-"), format = "%d-%b-%Y")]

  # We'll factor grade
loans <- loans[, grade := as.factor(grade)]
  
  # We create a column which indicates whether a loan is good or bad depending on its status:
  # First we create a vector, badLoans, of bad loan types.
badLoans <- c("Charged Off", "Default", "Does not meet the credit policy. Status:Charged Off",
               "In Grace Period", "Late (16-30 days)", "Late (31-120 days)")

  # Next we write a function which classifies the loan using badLoans. 
classifyLoan <- function(s){
  if(s %in% badLoans){
    return(0)
  }else{
    return(1)
  }
}

  # Now, we add a new column, loan_status_simple, to loans indicating the loan type.
loans <- loans[, loan_status_simple := sapply(loans[,loan_status], classifyLoan, USE.NAMES = F)]

  # Finally, we factor our simple status.
loans <- loans[, loan_status_simple := factor(loans[,loan_status_simple], labels = c("Bad", "Good"))]

  ## Exploratory Analysis----

  # Create a violin plot of dti vs. grade
dtiVsGradePlot <- ggplot(loans[,dti, by = grade],aes(x = grade, y = dti, color = grade)) +
  geom_violin() +
  stat_summary(fun.y = "mean", geom = "point") + 
  labs(x = "Grade", y = "Debt-to-Income",
       title = "Debt-to-Income Vs Grade") +
  theme_bw()

print(dtiVsGradePlot)
  # Clearly larger dti is associated with greater risk.  

  # Let's plot loan purposes.
loanPurposesPlot <- Desc(loans[,purpose], main = "Loan Purposes", plotit = T) 

print(loanPurposesPlot)
  # Debt consolidation was the most popular reason for taking out a loan.

  # Create density plot of distribution of annual_inc
annualIncomeDistribution <- Desc(loans[annual_inc < 500000,annual_inc], main = "Annual Income", plotit = T)

print(annualIncomeDistribution)
  # The mode is 50000

  # Time series for loan amounts:

  # First we look at totals
loanAmntTsPlot <- ggplot(loans[,.(Amount = sum(loan_amnt)), by = issue_d], aes(x = issue_d, y = Amount)) +
  geom_line() +
  theme_bw() +
  labs(x = "Date",
       title = "Loan Amount")
print(loanAmntTsPlot)

  # Next we take a closer look at loan amounts by term.
loanAmntTermTsPlot <- ggplot(loans[,.(Amount = sum(loan_amnt)), by = .(issue_d, term)], aes(x = issue_d, y = Amount, color = term)) +
  geom_line() +
  theme_bw() +
  labs(x = "Date",
       title = "Loan Amounts by Term")
print(loanAmntTermTsPlot)


  # Then we take a closer look at loan amounts by grade. 
loanAmntGradeTsPlot <- ggplot(loans[,.(Amount = sum(loan_amnt)), by = .(issue_d, grade)], aes(x = issue_d, y = Amount, color = grade)) +
  geom_line() +
  facet_wrap(vars(grade)) +
  theme_bw() +
  labs(x = "Date",
       title = "Loan Amounts by Grade")
print(loanAmntGradeTsPlot)

  # Finally we take a closer look at loan amounts by status.
loanAmntStatusTsPlot <- ggplot(loans[,.(Amount = sum(loan_amnt)), by = .(issue_d, loan_status_simple)], aes(x = issue_d, y = Amount, color = loan_status_simple)) +
  geom_line() +
  theme_bw() +
  labs(x = "Date",
       title = "Loan Amounts by Status")
print(loanAmntStatusTsPlot)

  # Machine Learning----

  # We would like to predict when a loan will be good or bad. To that end, we will build a 
  # logistic model to try to predict when a loan will perform well or poorly.

  # First we'll split our data.table into training and testing data groups.
split <- caTools::sample.split(loans$loan_status_simple, SplitRatio = 0.50)
logisticTraining <- subset(loans, split == T)
logisticTesting <- subset(loans, split == F)

  # Next we'll create our logistic model.
logisticModel <- glm(loan_status_simple ~ loan_amnt + int_rate + grade + annual_inc +
                       dti + delinq_2yrs, family = binomial(link = "logit"),
                     data = logisticTraining)

  # Now we'll test our model against testing data.
fittedProbabilites <- predict(logisticModel, logisticTesting, type = 'response')
fittedResults <- ifelse(fittedProbabilites>0.5, "Good", "Bad")

  # Let's see how well model is performing.
error <- mean(fittedResults != logisticTesting$loan_status_simple)
print(1-error)
table(logisticTesting$loan_status_simple, fittedResults)
