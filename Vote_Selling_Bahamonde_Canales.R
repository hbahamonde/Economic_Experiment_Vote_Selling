############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())


# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(readxl)

dat1 <- read_excel("/Users/hectorbahamonde/RU/research/Economic_Experiment_Vote_Selling/data/120619.xls")

