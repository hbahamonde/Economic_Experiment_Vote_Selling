############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())



# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(readxl)

setwd("./Economic_Experiment_Vote_Selling")
dir = getwd() # set dir
setwd(dir) # set dir
dat <-read_excel(paste(dir, "/data/Edit/dat.xlsx", sep="")) # read data







############################## 
# Ideological Distance and Vote-Buying/Selling
##############################

# VD: sold?
# VI: rango [Distancia Votante Partido A], [Distancia Votante Partido B]

