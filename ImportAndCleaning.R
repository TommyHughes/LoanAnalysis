library(tidyverse)
library(data.table)

# IMPORT ----------------------------------------------------------------------------------------
# Let's begin by importing our 'loan.csv' file. This is a rather large file (1.1 GB)
# with roughly 2 million rows. For this reason we'll use data.table.

# Since the "id" and "member_id" columns are entirely NA (presumably for privacy purposes),
# we will exclude them from our data.table.

loans <- data.table::fread(input = "loan.csv", drop = c("id", "member_id"))


# CLEANING --------------------------------------------------------------------------------------

