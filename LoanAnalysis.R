library(tidyverse)
library(data.table)
library(DescTools)

  ## IMPORT----
  
  # Load the loan file.
loans <- fread(input = "loan.csv", drop = c("id", "member_id"), nrows = 1000000)

  ## Cleaning----

  # We assume that any dti > 100 is reported in error, and thus remove them.
loans <- loans[dti < 100 | dti == 100,]


  ## Exploratory Analysis----

  # Create a violin plot of dti vs. grade
dtiVsGradePlot <- ggplot(loans[application_type == "Individual",dti, by = grade],aes(x = grade, y = dti)) +
  geom_violin(color = "black" , fill = "red", alpha = 1) +
  stat_summary(fun.y = "mean", geom = "point", alpha = 0.5) + 
  theme_gray()

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
