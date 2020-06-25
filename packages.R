## library() calls go here
library(conflicted)
library(dotenv)
library(drake)
# data management
library(haven)
library(janitor)
library(dataPreparation)
library(labelled)
# data analysis
library(tidyverse)
library(rsample)
library(recipes)
library(embed)
library(yardstick)
library(xgboost)
# reporting
library(glue)
library(rmarkdown)


conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("slice",  "dplyr")
conflicted::conflict_prefer("gather", "tidyr")
