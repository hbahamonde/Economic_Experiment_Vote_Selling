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
# Question 1: When do Parties buy?
############################## 

# 1. "Oferta que acepta el votante": generate var that selects whether voter sells or not (it does correct for bad coding: cuando no recibe oferta y si acepta ofertas, ambas son 0!)
# Remember: "Oferta que acepta el votante" [0 si acepto oferta del partido A, 1 si acepto oferta del partido B, 2 ninguna oferta. * OJO cuando no recibe oferta, tb es 0 (reemplazar ese valor por un NA). ]

# 1. Generate var to see if party targets (Stage 1)
dat$is.targeted = as.numeric(ifelse(dat$Elección == 1,"1","0"))





############################## 
# Question 2: When do voters decide to take Partie's offer?
############################## 
# Heckman: vende [si/no] // A o B

# 2. Generate var to see if voter accepts party's/ies offer (Stage 2)
p_load(plyr)
dat.vote.buying.offered = dat %>% group_by(Period,Group) %>% mutate(vende = ifelse(Elección %in% 1,"1","0"))
dat.vote.buying.offered = data.frame(dat.vote.buying.offered)




# dat$vende = as.numeric(ifelse(dat$'Oferta que acepta el votante'==0 | dat$'Oferta que acepta el votante'==1 & dat$Elección == 1 ,"0","1"))  # introduces right word into the text








# 2. Split data (vote-buying data): These data consist of voters-only who are targeted (parties offered to buy votes).
# It will help answer the question: Under what conditions do voters decide to accept the party/ies offer?






############################## 
# Ideological Distance and Vote-Buying (partidos salen a comprar: entonces observamos por parte de los votantes un Heckman Selection type of model, because la primera etapa seria ofertar el voto (stage 1; variable "eleccion"), aceptar oferta de los partidos (stage 2, variable "Oferta que acepta el votante")). 
##############################

# 0) base debe ser solo con votantes
# VD: sold? "Oferta que acepta el votante" [0 si acepto oferta del partido A, 1 si acepto oferta del partido B, 2 ninguna oferta. * OJO cuando no recibe oferta, tb es 0 (reemplazar ese valor por un NA). ]

## (*) Variable "Oferta que acepta el votante": partirla en dos = acepto oferta / no acepto ofera. (STAGE 1)
## (*) Variable "Oferta que acepta el votante": partirla en dos = 

## (*) si es 0 y partidos no ofertan, cambiar por 

# VI: 
        # 1) rango [Distancia Votante Partido A], [Distancia Votante Partido B]: ambas son ortogonales
        # 2) 

