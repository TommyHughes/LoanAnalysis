library(tidyverse)
library(data.table)

  ## IMPORT----
  
  # Load the loan file.
loans <- fread(input = "loan.csv", drop = c("id", "member_id"), nrows = 1000000)

  ## Cleaning----

  # We assume that any dti > 100 is reported in error, and thus remove them.
loans <- loans[dti < 100 | dti == 100,]


  ## Exploratory Analysis----

  # Create a violin plot of dti vs grade
dtiVsGrade <- ggplot(loans[application_type == "Individual",.(dti, grade)],aes(x = grade, y = dti)) +
  geom_violin()

plot(dtiVsGrade)
  
