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
names(dat) <- sub(" ", ".", names(dat))


################################################ 
# ************** VOTE BUYING DATA **************
################################################ 


############################## 
# Generating Variables
############################## 

# Generate var to see if party targets: Question 1
p_load(dplyr)
dat.vote.buying.offered = dat %>% group_by(Period,Group) %>% mutate(is.targeted = ifelse(Elección == 1 & 'Oferta.que.acepta.el.votante'!= 2,"0","1"))
dat = data.frame(dat.vote.buying.offered)

# Generate var to see if voter accepts party's/ies offer (Question 2)
dat$vende = as.numeric(ifelse(dat$'Oferta.que.acepta.el.votante'!= 2 & dat$is.targeted == 1,"1","0"))

############################## 
# Question 1: Who do Parties target to? Core supporters? Swing voters?
############################## 

# Data partitioning: dat$'Voto Participante' == "Votante"
core.swinger.d = dat[dat$'Voto Participante' == "Votante",]

# Possible ID's: 
## (1) distancia partido-votante
## (2) competencia partidaria (3's o 5's).
## (3) Party's budget

# DV
## dat$is.targeted





############################## 
# Question 2: When do voters decide to take Partie's offer?
############################## 

# Data partitioning: only subjects with dat$is.targeted == 1

# Remember: "Oferta que acepta el votante" [0 si acepto oferta del partido A, 1 si acepto oferta del partido B, 2 ninguna oferta. * OJO cuando no recibe oferta, tb es 0 (reemplazar ese valor por un NA). ]

# DV:
## dat$vende







############################## 
# Question 3: What does determine the vote-buying price?
############################## 

# Data partitioning: possible spliting the data into Pa and Pb, separately.
## Oferta Partido A
## Oferta Partido B


# Possible DV:
## Oferta Partido A
## Oferta Partido B

# Possible IV:
## Cantidad en el grupo
## Distancia Votante Partido A/B
## Presupuesto Partido



################################################ 
# ************** VOTE SELLING DATA **************
################################################ 

# Question 1: WHen do voters sell? 

## It's a two-stage process.
### Heckman: STAGE 1 = vende [si/no] // STAGE 2: for the ones who decide to sell, do they take A's or B's offer?



