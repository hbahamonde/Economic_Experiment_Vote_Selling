############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())

## ---- loadings:d ----

# function to to recover mode
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(rio,tibble)
dat <- rio::import("https://github.com/hbahamonde/Economic_Experiment_Vote_Selling/raw/master/data/data.xlsx")

# dropping obs
# drop if payoff is 0
dat <- subset(dat, participant.payoff > 0)

## ID VARS
id.vars = c(
    "participant.code",
    "session.code",
    "participant.payoff"
)


## VOTE BUYING VARS
v.buying.vars = c(
    "vote_b.1.player.votanteOpartido" ,
    "vote_b.1.player.tipoAoB",
    "vote_b.1.player.nuevotipoAoB",
    "vote_b.1.player.p_oferta_choice" ,
    "vote_b.1.player.p_oferta_amount",
    "vote_b.1.player.p_oferta_acepta",
    # "vote_b.1.player.win_lose",
    "vote_b.1.player.puntos",
    # "vote_b.1.player.payoff",
    # "vote_b.1.group.id_in_subsession",
    "vote_b.1.group.presupuesto",
    "vote_b.1.group.n_votantes",
    # "vote_b.1.group.partido_elegido" ,
    "vote_b.1.group.tipo_votante" ,
    "vote_b.1.group.n_votantes_A",
    "vote_b.1.group.n_votantes_B"  ,
    "vote_b.1.group.ubicacion_pA",
    "vote_b.1.group.ubicacion_pB"   ,
    "vote_b.1.group.pje_win_cA",
    "vote_b.1.group.pje_win_cB"  ,
    "vote_b.2.player.votanteOpartido",
    "vote_b.2.player.tipoAoB",
    "vote_b.2.player.nuevotipoAoB",
    "vote_b.2.player.p_oferta_choice",
    "vote_b.2.player.p_oferta_amount",
    "vote_b.2.player.p_oferta_acepta",
    # "vote_b.2.player.win_lose",
    "vote_b.2.player.puntos",
    # "vote_b.2.player.payoff" ,
    # "vote_b.2.group.id_in_subsession",
    "vote_b.2.group.presupuesto" ,
    "vote_b.2.group.n_votantes" ,
    # "vote_b.2.group.partido_elegido" ,
    "vote_b.2.group.tipo_votante",
    "vote_b.2.group.n_votantes_A",
    "vote_b.2.group.n_votantes_B",
    "vote_b.2.group.ubicacion_pA",
    "vote_b.2.group.ubicacion_pB",
    "vote_b.2.group.pje_win_cA",
    "vote_b.2.group.pje_win_cB",
    "vote_b.3.player.votanteOpartido",
    "vote_b.3.player.tipoAoB",
    "vote_b.3.player.nuevotipoAoB",
    "vote_b.3.player.p_oferta_choice",
    "vote_b.3.player.p_oferta_amount",
    "vote_b.3.player.p_oferta_acepta",
    # "vote_b.3.player.win_lose",
    "vote_b.3.player.puntos",
    # "vote_b.3.player.payoff" ,
    # "vote_b.3.group.id_in_subsession",
    "vote_b.3.group.presupuesto" ,
    "vote_b.3.group.n_votantes" ,
    # "vote_b.3.group.partido_elegido" ,
    "vote_b.3.group.tipo_votante",
    "vote_b.3.group.n_votantes_A",
    "vote_b.3.group.n_votantes_B",
    "vote_b.3.group.ubicacion_pA",
    "vote_b.3.group.ubicacion_pB",
    "vote_b.3.group.pje_win_cA",
    "vote_b.3.group.pje_win_cB")

## VOTE SELLING VARS
v.selling.vars = c(
    "vote_s.1.player.votanteOpartido",
    "vote_s.1.player.tipoAoB",
    "vote_s.1.player.p_oferta_choice_A",
    "vote_s.1.player.p_oferta_choice_B",
    "vote_s.1.player.p_oferta_amount_A",
    "vote_s.1.player.p_oferta_amount_B",
    "vote_s.1.player.win_lose",
    "vote_s.1.player.win_losev",
    "vote_s.1.player.puntos",
    "vote_s.1.player.payoff",
    "vote_s.1.group.presupuesto",
    "vote_s.1.group.n_votantes",
    "vote_s.1.group.partido_elegido",
    "vote_s.1.group.tipo_votante",
    "vote_s.1.group.ubicacion_pA",
    "vote_s.1.group.ubicacion_pB",
    "vote_s.1.group.pje_win_cA",
    "vote_s.1.group.pje_win_cB",
    "vote_s.2.player.votanteOpartido",
    "vote_s.2.player.tipoAoB",
    "vote_s.2.player.p_oferta_choice_A",
    "vote_s.2.player.p_oferta_choice_B",
    "vote_s.2.player.p_oferta_amount_A",
    "vote_s.2.player.p_oferta_amount_B",
    "vote_s.2.player.win_lose",
    "vote_s.2.player.win_losev",
    "vote_s.2.player.puntos",
    "vote_s.2.player.payoff",
    "vote_s.2.group.presupuesto",
    "vote_s.2.group.n_votantes",
    "vote_s.2.group.partido_elegido",
    "vote_s.2.group.tipo_votante",
    "vote_s.2.group.ubicacion_pA",
    "vote_s.2.group.ubicacion_pB",
    "vote_s.2.group.pje_win_cA",
    "vote_s.2.group.pje_win_cB",
    "vote_s.3.player.votanteOpartido",
    "vote_s.3.player.tipoAoB",
    "vote_s.3.player.p_oferta_choice_A",
    "vote_s.3.player.p_oferta_choice_B",
    "vote_s.3.player.p_oferta_amount_A",
    "vote_s.3.player.p_oferta_amount_B",
    "vote_s.3.player.win_lose",
    "vote_s.3.player.win_losev",
    "vote_s.3.player.puntos",
    "vote_s.3.player.payoff",
    "vote_s.3.group.presupuesto",
    "vote_s.3.group.n_votantes",
    "vote_s.3.group.partido_elegido",
    "vote_s.3.group.tipo_votante",
    "vote_s.3.group.ubicacion_pA",
    "vote_s.3.group.ubicacion_pB",
    "vote_s.3.group.pje_win_cA",
    "vote_s.3.group.pje_win_cB")


# SOCIO-DEMO VARS
socio.dem.vars = c(
    "survey.1.player.q3",
    "survey.1.player.q4" ,
    "survey.1.player.q6",
    "survey.1.player.q7",
    "survey.1.player.q8",
    "survey.1.player.q9",
    "survey.1.player.q10")

# litle codebooking
# "survey.1.player.q3"  # gender
# "survey.1.player.q4" # Salario:  (1=Les alcanza bien y pueden ahorrar, 2=Les alcanza justo y sin grandes dificultades, 3=No les alcanza y tienen dificultades, 4=No les alcanza y tienen grandes dificultades)
# "survey.1.player.q5" # Ingresos: (1= Menos de $288.800, 2= Entre $288.801 - $312.001, 3=Entre $312.002 - $361.002, 4= Entre $361.003 - $410.003, 5=Entre $410.004 - $459.004, 6=Entre $459.005 - $558.005, 7= Entre $558.006 - $657.006, 8=Entre $657.007 - $756.007, 9= Entre $756.008 - $1.005.008, 10= Más de $1.005.008)                         
# "survey.1.player.q6" # Simpatiza con partido político 1=sí, 2=no
# "survey.1.player.q7" # Qué partido? (1=Partido Socialista de Chile, 2= Unión Demócrata Independiente, 3= Renovación Nacional, 4=Partido Demócrata Cristiano, 5= Partido Comunistica de Chile, 6= Revolución Democrática, 7= Evolución Política, 8= Otro, 9=No me siento representado)                         
# "survey.1.player.q8" # Escala tendencia política (1=izq a 10=derecha)
# "survey.1.player.q9" # Voto ultima elección 1=sí, 0=no)                    
# "survey.1.player.q10" # Piensa votar proxima elección 1=sí, 0=no

# ID df
dat.v.b.ID = dat[c(id.vars, socio.dem.vars)]
names(dat.v.b.ID) <- c(
    "participant.code",
    "session.code",
    "participant.payoff",
    "gender", # "survey.1.player.q3",
    "salary.enough", # "survey.1.player.q4",
    "party.like", #"survey.1.player.q6", 
    "party.id", #"survey.1.player.q7",
    "left.right", #"survey.1.player.q8",
    "vote.last.election", #"survey.1.player.q9",
    "vote.next.election" #"survey.1.player.q10"
)

# remove some columns
dat.v.b.ID = subset(dat.v.b.ID, select = -c(session.code, participant.payoff))


################################################ 
# ************** VOTE BUYING DATA **************
################################################ 

# subsetting vars
v.buying.dat = dat[c(id.vars, v.buying.vars, socio.dem.vars)]
# dropping obs that dont belong to the vote buying exp
v.buying.dat <- subset(v.buying.dat, !is.na(vote_b.1.player.votanteOpartido))

# party ID before offer (voter)
v.buying.dat$party.id.before.voter.b.1 = v.buying.dat$vote_b.1.player.tipoAoB
v.buying.dat$party.id.before.voter.b.2 = v.buying.dat$vote_b.2.player.tipoAoB
v.buying.dat$party.id.before.voter.b.3 = v.buying.dat$vote_b.3.player.tipoAoB

# gen var that marks if what offer was taken by voter, if any (game 1)
p_load(dplyr,tidyverse)
v.buying.dat <- v.buying.dat %>% dplyr::group_by(session.code,vote_b.1.group.ubicacion_pA,vote_b.1.group.ubicacion_pB) %>% mutate(value_tmp = vote_b.1.player.p_oferta_acepta) %>% fill(value_tmp)
#v.buying.dat$value_tmp[is.na(v.buying.dat$value_tmp)] <- 0
v.buying.dat$offer.taken.voter.b.1 = ifelse(v.buying.dat$vote_b.1.player.votanteOpartido!="votantes", NA, v.buying.dat$value_tmp)

# gen var that marks if what offer was taken by voter, if any (game 2)
p_load(dplyr,tidyverse)
v.buying.dat <- v.buying.dat %>% dplyr::group_by(session.code,vote_b.2.group.ubicacion_pA,vote_b.2.group.ubicacion_pB) %>% mutate(value_tmp = vote_b.2.player.p_oferta_acepta) %>% fill(value_tmp)
#v.buying.dat$value_tmp[is.na(v.buying.dat$value_tmp)] <- 0
v.buying.dat$offer.taken.voter.b.2 = ifelse(v.buying.dat$vote_b.2.player.votanteOpartido!="votantes", NA, v.buying.dat$value_tmp)

# gen var that marks if what offer was taken by voter, if any (game 3)
p_load(dplyr,tidyverse)
v.buying.dat <- v.buying.dat %>% dplyr::group_by(session.code,vote_b.3.group.ubicacion_pA,vote_b.3.group.ubicacion_pB) %>% mutate(value_tmp = vote_b.3.player.p_oferta_acepta) %>% fill(value_tmp)
#v.buying.dat$value_tmp[is.na(v.buying.dat$value_tmp)] <- 0
v.buying.dat$offer.taken.voter.b.3 = ifelse(v.buying.dat$vote_b.3.player.votanteOpartido!="votantes", NA, v.buying.dat$value_tmp)

# voter's ideological position
v.buying.dat$voter.ideology.b.1 = ifelse(v.buying.dat$vote_b.1.player.votanteOpartido != "votantes", NA, v.buying.dat$vote_b.1.group.tipo_votante)
v.buying.dat$voter.ideology.b.2 = ifelse(v.buying.dat$vote_b.2.player.votanteOpartido != "votantes", NA, v.buying.dat$vote_b.2.group.tipo_votante)
v.buying.dat$voter.ideology.b.3 = ifelse(v.buying.dat$vote_b.3.player.votanteOpartido != "votantes", NA, v.buying.dat$vote_b.3.group.tipo_votante)

# if voter is swinger
v.buying.dat$swing.voter.b.1 = ifelse(v.buying.dat$vote_b.1.player.tipoAoB != v.buying.dat$vote_b.1.player.nuevotipoAoB, 1, 0)
v.buying.dat$swing.voter.b.2 = ifelse(v.buying.dat$vote_b.2.player.tipoAoB != v.buying.dat$vote_b.2.player.nuevotipoAoB, 1, 0)
v.buying.dat$swing.voter.b.3 = ifelse(v.buying.dat$vote_b.3.player.tipoAoB != v.buying.dat$vote_b.3.player.nuevotipoAoB, 1, 0)

# dropping rows that are voters
# v.buying.dat <- subset(v.buying.dat, vote_b.1.player.votanteOpartido!="votantes")
# v.buying.dat <- subset(v.buying.dat, vote_b.2.player.votanteOpartido!="votantes")
# v.buying.dat <- subset(v.buying.dat, vote_b.3.player.votanteOpartido!="votantes")

# characterizing offer type of party (game 1)
v.buying.dat$offer.type.party.v.b.1 = ifelse(v.buying.dat$vote_b.1.player.votanteOpartido == "Partido A" &
                                                 v.buying.dat$vote_b.1.player.p_oferta_choice == 1, "A",
                                             ifelse(v.buying.dat$vote_b.1.player.votanteOpartido == "Partido B" &
                                                        v.buying.dat$vote_b.1.player.p_oferta_choice == 1, "B", NA
                                             )
)

p_load(dplyr)
v.buying.dat = v.buying.dat %>%
    group_by(session.code, vote_b.1.group.ubicacion_pA,vote_b.1.group.ubicacion_pB) %>%
    mutate(n.offers.made.to.voter = n_distinct(offer.type.party.v.b.1, na.rm = T))

p_load(dplyr)
v.buying.dat = v.buying.dat %>%
    group_by(session.code, vote_b.1.group.ubicacion_pA,vote_b.1.group.ubicacion_pB) %>%
    mutate(offer.party.type.b.1 = offer.type.party.v.b.1) %>% 
    fill(offer.party.type.b.1, .direction = "downup")

v.buying.dat$offer.party.type.b.1 = as.character(v.buying.dat$offer.party.type.b.1)
v.buying.dat$offer.party.type.b.1[v.buying.dat$n.offers.made.to.voter == 2] <- "AB"
v.buying.dat$offer.party.type.b.1 = v.buying.dat$offer.party.type.b.1 %>% replace_na("None")
v.buying.dat$offer.party.type.b.1 = as.factor(v.buying.dat$offer.party.type.b.1)

# characterizing offer type of party (game 2)
v.buying.dat$offer.type.party.v.b.2 = ifelse(v.buying.dat$vote_b.2.player.votanteOpartido == "Partido A" &
                                                 v.buying.dat$vote_b.2.player.p_oferta_choice == 1, "A",
                                             ifelse(v.buying.dat$vote_b.2.player.votanteOpartido == "Partido B" &
                                                        v.buying.dat$vote_b.2.player.p_oferta_choice == 1, "B", NA
                                             )
)
p_load(dplyr)
v.buying.dat = v.buying.dat %>%
    group_by(session.code, vote_b.2.group.ubicacion_pA,vote_b.2.group.ubicacion_pB) %>%
    mutate(n.offers.made.to.voter = n_distinct(offer.type.party.v.b.2, na.rm = T))

p_load(dplyr)
v.buying.dat = v.buying.dat %>%
    group_by(session.code, vote_b.2.group.ubicacion_pA,vote_b.2.group.ubicacion_pB) %>%
    mutate(offer.party.type.b.2 = offer.type.party.v.b.2) %>% 
    fill(offer.party.type.b.2, .direction = "downup")

v.buying.dat$offer.party.type.b.2 = as.character(v.buying.dat$offer.party.type.b.2)
v.buying.dat$offer.party.type.b.2[v.buying.dat$n.offers.made.to.voter == 2] <- "AB"
v.buying.dat$offer.party.type.b.2 = v.buying.dat$offer.party.type.b.2 %>% replace_na("None")
v.buying.dat$offer.party.type.b.2 = as.factor(v.buying.dat$offer.party.type.b.2)

# characterizing offer type of party (game 3)
v.buying.dat$offer.type.party.v.b.3 = ifelse(v.buying.dat$vote_b.3.player.votanteOpartido == "Partido A" &
                                                 v.buying.dat$vote_b.3.player.p_oferta_choice == 1, "A",
                                             ifelse(v.buying.dat$vote_b.3.player.votanteOpartido == "Partido B" &
                                                        v.buying.dat$vote_b.3.player.p_oferta_choice == 1, "B", NA)
)
p_load(dplyr)
v.buying.dat = v.buying.dat %>%
    group_by(session.code, vote_b.3.group.ubicacion_pA,vote_b.3.group.ubicacion_pB) %>%
    mutate(n.offers.made.to.voter = n_distinct(offer.type.party.v.b.3, na.rm = T))

p_load(dplyr)
v.buying.dat = v.buying.dat %>%
    group_by(session.code, vote_b.3.group.ubicacion_pA,vote_b.3.group.ubicacion_pB) %>%
    mutate(offer.party.type.b.3 = offer.type.party.v.b.3) %>% 
    fill(offer.party.type.b.3, .direction = "downup")

v.buying.dat$offer.party.type.b.3 = as.character(v.buying.dat$offer.party.type.b.3)
v.buying.dat$offer.party.type.b.3[v.buying.dat$n.offers.made.to.voter == 2] <- "AB"
v.buying.dat$offer.party.type.b.3 = v.buying.dat$offer.party.type.b.3 %>% replace_na("None")
v.buying.dat$offer.party.type.b.3 = as.factor(v.buying.dat$offer.party.type.b.3)

# Game 1
v.buying.dat.1 = data.frame(
    v.buying.dat$participant.code,
    v.buying.dat$session.code, 
    v.buying.dat$vote_b.1.player.votanteOpartido,
    v.buying.dat$party.id.before.voter.b.1,
    v.buying.dat$offer.party.type.b.1,
    v.buying.dat$swing.voter.b.1,
    v.buying.dat$participant.payoff,
    v.buying.dat$vote_b.1.player.puntos,
    v.buying.dat$vote_b.1.group.presupuesto, 
    v.buying.dat$vote_b.1.player.p_oferta_amount, 
    #v.buying.dat$vote_b.1.player.p_oferta_acepta,
    v.buying.dat$offer.taken.voter.b.1,
    v.buying.dat$vote_b.1.group.n_votantes_A,
    v.buying.dat$vote_b.1.group.n_votantes_B, 
    v.buying.dat$vote_b.1.group.ubicacion_pA, 
    v.buying.dat$vote_b.1.group.ubicacion_pB, 
    v.buying.dat$voter.ideology.b.1,
    v.buying.dat$vote_b.1.group.pje_win_cA, 
    v.buying.dat$vote_b.1.group.pje_win_cB,
    v.buying.dat$vote_b.1.player.tipoAoB,
    v.buying.dat$vote_b.1.group.n_votantes
)


v.buying.dat.1$vote.intention.party = ifelse(
    v.buying.dat.1$v.buying.dat.vote_b.1.player.votanteOpartido=="Partido A", 
    v.buying.dat.1$v.buying.dat.vote_b.1.group.n_votantes_A, 
    ifelse(v.buying.dat.1$v.buying.dat.vote_b.1.player.votanteOpartido=="Partido B", 
           v.buying.dat.1$v.buying.dat.vote_b.1.group.n_votantes_B, 
           NA)
)


# vote.intention.voter.before.offer
v.buying.dat.1$vote.intention.voter.before.offer = ifelse(
    v.buying.dat.1$v.buying.dat.vote_b.1.player.tipoAoB=="A", 
    v.buying.dat.1$v.buying.dat.vote_b.1.group.n_votantes_A, 
    ifelse(v.buying.dat.1$v.buying.dat.vote_b.1.player.tipoAoB=="B", 
           v.buying.dat.1$v.buying.dat.vote_b.1.group.n_votantes_B, 
           NA)
)



v.buying.dat.1$ideo.distance = ifelse(
    v.buying.dat.1$v.buying.dat.vote_b.1.player.votanteOpartido=="Partido A", 
    v.buying.dat.1$v.buying.dat.vote_b.1.group.ubicacion_pA, 
    ifelse(v.buying.dat.1$v.buying.dat.vote_b.1.player.votanteOpartido=="Partido B", v.buying.dat.1$v.buying.dat.vote_b.1.group.ubicacion_pB, 
           ifelse(v.buying.dat.1$v.buying.dat.vote_b.1.player.votanteOpartido=="votantes", v.buying.dat$voter.ideology.b.1, NA ))
)

v.buying.dat.1$voters.elect.payoff = ifelse(
    v.buying.dat.1$v.buying.dat.vote_b.1.player.votanteOpartido=="Partido A", 
    v.buying.dat.1$v.buying.dat.vote_b.1.group.pje_win_cA, 
    ifelse(v.buying.dat.1$v.buying.dat.vote_b.1.player.votanteOpartido=="Partido B", v.buying.dat.1$v.buying.dat.vote_b.1.group.pje_win_cB, 
           NA)
)



v.buying.dat.1 = subset(v.buying.dat.1, select = -c(
    v.buying.dat.vote_b.1.group.n_votantes_A,
    v.buying.dat.vote_b.1.group.n_votantes_B,
    v.buying.dat.vote_b.1.group.ubicacion_pA,
    v.buying.dat.vote_b.1.group.ubicacion_pB,
    v.buying.dat.vote_b.1.group.pje_win_cA,
    v.buying.dat.vote_b.1.group.pje_win_cB,
    v.buying.dat.voter.ideology.b.1,
    v.buying.dat.vote_b.1.player.tipoAoB)
)

# change names
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.participant.code"]<- "participant.code"
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.session.code"] <- "session.code"
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.vote_b.1.player.votanteOpartido"] <- "player.votanteOpartido"
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.party.id.before.voter.b.1"] <- "party.id.before.voter"   
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.offer.party.type.b.1"] <- "offer.party.type"    
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.swing.voter.b.1"] <- "swing.voter"             
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.participant.payoff"] <- "participant.payoff"         
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.vote_b.1.player.puntos"] <- "points.this.round"       
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.vote_b.1.group.presupuesto"] <- "group.presupuesto"  
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.vote_b.1.player.p_oferta_amount"] <- "player.p_oferta_amount"
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.offer.taken.voter.b.1"] <- "offer.taken.voter"      
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "vote.intention.party"] <- "vote.intention.party"
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "vote.intention.voter.before.offer"] <- "vote.intention.voter.before.offer"     
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "ideo.distance"] <- "ideo.distance"
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "voters.elect.payoff"] <- "voters.elect.payoff"
colnames(v.buying.dat.1)[colnames(v.buying.dat.1) == "v.buying.dat.vote_b.1.group.n_votantes"] <- "pivotal.3.5"




# Game 2
v.buying.dat.2 = data.frame(
    v.buying.dat$participant.code,
    v.buying.dat$session.code, 
    v.buying.dat$vote_b.2.player.votanteOpartido,
    v.buying.dat$party.id.before.voter.b.2,
    v.buying.dat$offer.party.type.b.2,
    v.buying.dat$swing.voter.b.2,
    v.buying.dat$participant.payoff,
    v.buying.dat$vote_b.2.player.puntos,
    v.buying.dat$vote_b.2.group.presupuesto, 
    v.buying.dat$vote_b.2.player.p_oferta_amount, 
    #v.buying.dat$vote_b.2.player.p_oferta_acepta,
    v.buying.dat$offer.taken.voter.b.2,
    v.buying.dat$vote_b.2.group.n_votantes_A,
    v.buying.dat$vote_b.2.group.n_votantes_B, 
    v.buying.dat$vote_b.2.group.ubicacion_pA, 
    v.buying.dat$vote_b.2.group.ubicacion_pB, 
    v.buying.dat$voter.ideology.b.2,
    v.buying.dat$vote_b.2.group.pje_win_cA, 
    v.buying.dat$vote_b.2.group.pje_win_cB,
    v.buying.dat$vote_b.2.player.tipoAoB,
    v.buying.dat$vote_b.2.group.n_votantes
)


v.buying.dat.2$vote.intention.party = ifelse(
    v.buying.dat.2$v.buying.dat.vote_b.2.player.votanteOpartido=="Partido A", 
    v.buying.dat.2$v.buying.dat.vote_b.2.group.n_votantes_A, 
    ifelse(v.buying.dat.2$v.buying.dat.vote_b.2.player.votanteOpartido=="Partido B", 
           v.buying.dat.2$v.buying.dat.vote_b.2.group.n_votantes_B, 
           NA)
)

# vote.intention.voter.before.offer
v.buying.dat.2$vote.intention.voter.before.offer = ifelse(
    v.buying.dat.2$v.buying.dat.vote_b.2.player.tipoAoB=="A", 
    v.buying.dat.2$v.buying.dat.vote_b.2.group.n_votantes_A, 
    ifelse(v.buying.dat.2$v.buying.dat.vote_b.2.player.tipoAoB=="B", 
           v.buying.dat.2$v.buying.dat.vote_b.2.group.n_votantes_B, 
           NA)
)


v.buying.dat.2$ideo.distance = ifelse(
    v.buying.dat.2$v.buying.dat.vote_b.2.player.votanteOpartido=="Partido A", 
    v.buying.dat.2$v.buying.dat.vote_b.2.group.ubicacion_pA, 
    ifelse(v.buying.dat.2$v.buying.dat.vote_b.2.player.votanteOpartido=="Partido B", v.buying.dat.2$v.buying.dat.vote_b.2.group.ubicacion_pB, 
           ifelse(v.buying.dat.2$v.buying.dat.vote_b.2.player.votanteOpartido=="votantes", v.buying.dat$voter.ideology.b.2, NA ))
)

v.buying.dat.2$voters.elect.payoff = ifelse(
    v.buying.dat.2$v.buying.dat.vote_b.2.player.votanteOpartido=="Partido A", 
    v.buying.dat.2$v.buying.dat.vote_b.2.group.pje_win_cA, 
    ifelse(v.buying.dat.2$v.buying.dat.vote_b.2.player.votanteOpartido=="Partido B", v.buying.dat.2$v.buying.dat.vote_b.2.group.pje_win_cB, 
           NA)
)

v.buying.dat.2 = subset(v.buying.dat.2, select = -c(
    v.buying.dat.vote_b.2.group.n_votantes_A,
    v.buying.dat.vote_b.2.group.n_votantes_B,
    v.buying.dat.vote_b.2.group.ubicacion_pA,
    v.buying.dat.vote_b.2.group.ubicacion_pB,
    v.buying.dat.vote_b.2.group.pje_win_cA,
    v.buying.dat.vote_b.2.group.pje_win_cB,
    v.buying.dat.voter.ideology.b.2,
    v.buying.dat.vote_b.2.player.tipoAoB)
)

# change names
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.participant.code"]<- "participant.code"
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.session.code"] <- "session.code"
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.vote_b.2.player.votanteOpartido"] <- "player.votanteOpartido"
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.party.id.before.voter.b.2"] <- "party.id.before.voter"   
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.offer.party.type.b.2"] <- "offer.party.type"    
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.swing.voter.b.2"] <- "swing.voter"             
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.participant.payoff"] <- "participant.payoff"         
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.vote_b.2.player.puntos"] <- "points.this.round"       
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.vote_b.2.group.presupuesto"] <- "group.presupuesto"  
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.vote_b.2.player.p_oferta_amount"] <- "player.p_oferta_amount"
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.offer.taken.voter.b.2"] <- "offer.taken.voter"      
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "vote.intention.party"] <- "vote.intention.party"
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "vote.intention.voter.before.offer"] <- "vote.intention.voter.before.offer"     
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "ideo.distance"] <- "ideo.distance"
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "voters.elect.payoff"] <- "voters.elect.payoff"
colnames(v.buying.dat.2)[colnames(v.buying.dat.2) == "v.buying.dat.vote_b.2.group.n_votantes"] <- "pivotal.3.5"


# Game 3
v.buying.dat.3 = data.frame(
    v.buying.dat$participant.code,
    v.buying.dat$session.code, 
    v.buying.dat$vote_b.3.player.votanteOpartido,
    v.buying.dat$party.id.before.voter.b.3,
    v.buying.dat$offer.party.type.b.3,
    v.buying.dat$swing.voter.b.3,
    v.buying.dat$participant.payoff,
    v.buying.dat$vote_b.3.player.puntos,
    v.buying.dat$vote_b.3.group.presupuesto, 
    v.buying.dat$vote_b.3.player.p_oferta_amount, 
    #v.buying.dat$vote_b.3.player.p_oferta_acepta,
    v.buying.dat$offer.taken.voter.b.3,
    v.buying.dat$vote_b.3.group.n_votantes_A,
    v.buying.dat$vote_b.3.group.n_votantes_B, 
    v.buying.dat$vote_b.3.group.ubicacion_pA, 
    v.buying.dat$vote_b.3.group.ubicacion_pB, 
    v.buying.dat$voter.ideology.b.3,
    v.buying.dat$vote_b.3.group.pje_win_cA, 
    v.buying.dat$vote_b.3.group.pje_win_cB,
    v.buying.dat$vote_b.3.player.tipoAoB,
    v.buying.dat$vote_b.3.group.n_votantes
    
)


v.buying.dat.3$vote.intention.party = ifelse(
    v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="Partido A", 
    v.buying.dat.3$v.buying.dat.vote_b.3.group.n_votantes_A, 
    ifelse(v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="Partido B", 
           v.buying.dat.3$v.buying.dat.vote_b.3.group.n_votantes_B, 
           NA)
)

# vote.intention.voter.before.offer
v.buying.dat.3$vote.intention.voter.before.offer = ifelse(
    v.buying.dat.3$v.buying.dat.vote_b.3.player.tipoAoB=="A", 
    v.buying.dat.3$v.buying.dat.vote_b.3.group.n_votantes_A, 
    ifelse(v.buying.dat.3$v.buying.dat.vote_b.3.player.tipoAoB=="B", 
           v.buying.dat.3$v.buying.dat.vote_b.3.group.n_votantes_B, 
           NA)
)

v.buying.dat.3$ideo.distance = ifelse(
    v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="Partido A", 
    v.buying.dat.3$v.buying.dat.vote_b.3.group.ubicacion_pA, 
    ifelse(v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="Partido B", v.buying.dat.3$v.buying.dat.vote_b.3.group.ubicacion_pB, 
           ifelse(v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="votantes", v.buying.dat$voter.ideology.b.3, NA ))
)

v.buying.dat.3$voters.elect.payoff = ifelse(
    v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="Partido A", 
    v.buying.dat.3$v.buying.dat.vote_b.3.group.pje_win_cA, 
    ifelse(v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="Partido B", v.buying.dat.3$v.buying.dat.vote_b.3.group.pje_win_cB, 
           NA)
)

v.buying.dat.3 = subset(v.buying.dat.3, select = -c(
    v.buying.dat.vote_b.3.group.n_votantes_A,
    v.buying.dat.vote_b.3.group.n_votantes_B,
    v.buying.dat.vote_b.3.group.ubicacion_pA,
    v.buying.dat.vote_b.3.group.ubicacion_pB,
    v.buying.dat.vote_b.3.group.pje_win_cA,
    v.buying.dat.vote_b.3.group.pje_win_cB,
    v.buying.dat.voter.ideology.b.3,
    v.buying.dat.vote_b.3.player.tipoAoB)
)

# change names
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.participant.code"]<- "participant.code"
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.session.code"] <- "session.code"
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.vote_b.3.player.votanteOpartido"] <- "player.votanteOpartido"
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.party.id.before.voter.b.3"] <- "party.id.before.voter"   
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.offer.party.type.b.3"] <- "offer.party.type"    
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.swing.voter.b.3"] <- "swing.voter"             
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.participant.payoff"] <- "participant.payoff"         
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.vote_b.3.player.puntos"] <- "points.this.round"       
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.vote_b.3.group.presupuesto"] <- "group.presupuesto"  
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.vote_b.3.player.p_oferta_amount"] <- "player.p_oferta_amount"
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.offer.taken.voter.b.3"] <- "offer.taken.voter"      
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "vote.intention.party"] <- "vote.intention.party"
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "vote.intention.voter.before.offer"] <- "vote.intention.voter.before.offer"     
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "ideo.distance"] <- "ideo.distance"
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "voters.elect.payoff"] <- "voters.elect.payoff"
colnames(v.buying.dat.3)[colnames(v.buying.dat.3) == "v.buying.dat.vote_b.3.group.n_votantes"] <- "pivotal.3.5"


# Stack up 3 games
dat.v.b = data.frame(rbind(v.buying.dat.1,v.buying.dat.2,v.buying.dat.3))

# round count
p_load(dplyr)
dat.v.b = dat.v.b %>% group_by(participant.code) %>% mutate(round = row_number())

# Merging with ID df
dat.v.b = merge(dat.v.b, dat.v.b.ID, by=c("participant.code"))

# renaming
colnames(dat.v.b)[colnames(dat.v.b)=="participant.code"] <- "participant.code"
colnames(dat.v.b)[colnames(dat.v.b)=="session.code"] <- "session.code"
colnames(dat.v.b)[colnames(dat.v.b)=="player.votanteOpartido"] <- "role"
colnames(dat.v.b)[colnames(dat.v.b)=="party.id.before.voter"] <- "party.id.before.voter"
colnames(dat.v.b)[colnames(dat.v.b)=="offer.party.type"] <- "offer.party.type" 
colnames(dat.v.b)[colnames(dat.v.b)=="swing.voter"] <- "swing.voter"
colnames(dat.v.b)[colnames(dat.v.b)=="participant.payoff"] <- "payoff"
colnames(dat.v.b)[colnames(dat.v.b)=="points.this.round"] <- "points.cumul"
colnames(dat.v.b)[colnames(dat.v.b)=="group.presupuesto"] <- "budget"
colnames(dat.v.b)[colnames(dat.v.b)=="player.p_oferta_amount"] <- "offer.made.party"
colnames(dat.v.b)[colnames(dat.v.b)=="offer.taken.voter"] <- "offer.taken.voter"
colnames(dat.v.b)[colnames(dat.v.b)=="vote.intention.party"] <- "vote.intention.party"
colnames(dat.v.b)[colnames(dat.v.b)=="ideo.distance"] <- "ideo.distance"
colnames(dat.v.b)[colnames(dat.v.b)=="voters.elect.payoff"] <- "voters.elect.payoff"
colnames(dat.v.b)[colnames(dat.v.b)=="round"] <- "round"
colnames(dat.v.b)[colnames(dat.v.b)=="gender"] <- "gender"
colnames(dat.v.b)[colnames(dat.v.b)=="salary.enough"] <- "salary.enough"
colnames(dat.v.b)[colnames(dat.v.b)=="party.like"] <- "party.like"
colnames(dat.v.b)[colnames(dat.v.b)=="party.id"] <- "party.id"
colnames(dat.v.b)[colnames(dat.v.b)=="left.right"] <- "left.right"
colnames(dat.v.b)[colnames(dat.v.b)=="vote.last.election"] <- "vote.last.election"
colnames(dat.v.b)[colnames(dat.v.b)=="vote.next.election"] <-  "vote.next.election"


# voters don't make offers, so it's NA for them.
dat.v.b$offer.made.party[dat.v.b$role=="votantes"] <- NA
# offer made NA now is 0
#dat.v.b$offer.made.party[is.na(dat.v.b$offer.made.party)] <- 0

# "calculates" the offer made to the voter
p_load(dplyr)
dat.v.b = dat.v.b %>% group_by(session.code,budget) %>% mutate(offer.made.voter = sum(offer.made.party, na.rm = T))
dat.v.b$offer.made.voter = ifelse(dat.v.b$role=="votantes", dat.v.b$offer.made.voter, NA)

# change in payoff
dat.v.b = dat.v.b %>%
    group_by(participant.code) %>%
    mutate(points.cumul.delta = points.cumul - lag(points.cumul))

# offer.made Dummy made to voter
dat.v.b$offer.made.voter.d = ifelse(dat.v.b$offer.made.voter > 0, 1, 0)
dat.v.b$offer.made.voter.d[dat.v.b$role!="votantes"] <- NA

# offer.made Dummy made by party
dat.v.b$offer.made.party.d = ifelse(dat.v.b$offer.made.party > 0, 1, 0)
dat.v.b$offer.made.party.d[dat.v.b$role=="votantes"] <- NA

# offer.taken.voter Dummy
dat.v.b$offer.taken.voter.d = ifelse(dat.v.b$offer.taken.voter > 0, 1, 0)
dat.v.b$offer.taken.voter.d[dat.v.b$role!="votantes"] <- NA

# as factor offer.taken.voter
dat.v.b$offer.taken.voter = as.factor(dat.v.b$offer.taken.voter)
levels(dat.v.b$offer.taken.voter) <- c("A", "B", "Pending")

# party ID after offer
dat.v.b$party.id.after.voter = ifelse(
    dat.v.b$party.id.before.voter=="A" & dat.v.b$swing.voter == 1, "B",
    ifelse(dat.v.b$party.id.before.voter=="B" & dat.v.b$swing.voter == 1, "A", 
           ifelse(dat.v.b$swing.voter == 0, dat.v.b$party.id.before.voter, NA)
    )
)

# competitive.offers
dat.v.b$competitive.offers.party = ifelse(dat.v.b$offer.party.type=="AB", 1, 0)
dat.v.b$competitive.offers.party[dat.v.b$role=="votantes"] <- NA # if voter, is NA (this var is only for parties)

# transforming vars.
dat.v.b$role = as.factor(dat.v.b$role)
dat.v.b$participant.code = as.factor(dat.v.b$participant.code)
dat.v.b$swing.voter = as.numeric(dat.v.b$swing.voter)
dat.v.b$payoff = as.numeric(dat.v.b$payoff)
dat.v.b$points.cumul = as.numeric(dat.v.b$points.cumul)
dat.v.b$budget = as.numeric(dat.v.b$budget)
dat.v.b$offer.made.voter = as.numeric(dat.v.b$offer.made.voter)
dat.v.b$offer.taken.voter = as.numeric(dat.v.b$offer.taken.voter)
dat.v.b$offer.taken.voter = as.factor(ifelse(dat.v.b$offer.taken.voter == 1, "A", ifelse(dat.v.b$offer.taken.voter == 2, "B", NA)))
dat.v.b$offer.party.type = relevel(dat.v.b$offer.party.type, ref = "None")
dat.v.b$vote.intention.party = as.numeric(dat.v.b$vote.intention.party)
dat.v.b$ideo.distance = as.numeric(dat.v.b$ideo.distance)
dat.v.b$voters.elect.payoff = as.numeric(dat.v.b$voters.elect.payoff)
dat.v.b$voters.elect.payoff = as.numeric(dat.v.b$voters.elect.payoff)
dat.v.b$points.cumul.delta = as.numeric(dat.v.b$points.cumul.delta)

dat.v.b = dat.v.b %>% select(session.code,
                             participant.code,
                             round,
                             role,
                             pivotal.3.5,
                             vote.intention.party,
                             party.id.before.voter,
                             vote.intention.voter.before.offer,
                             offer.party.type,
                             competitive.offers.party,
                             party.id.after.voter,
                             swing.voter,
                             offer.made.party,
                             offer.made.party.d,
                             offer.made.voter,
                             offer.made.voter.d,
                             offer.taken.voter,
                             offer.taken.voter.d,
                             points.cumul,
                             points.cumul.delta,
                             payoff, 
                             everything())
# recode role in English
p_load(dplyr)

dat.v.b = dat.v.b %>% mutate(role=recode(role, 
                               "Partido A" = "Party A",
                               "Partido B" = "Party B",
                               "votantes" = "Voter"))

## ----


################################################################################## 
# ***** Q      U       E       S       T       I       O       N       S *********
##################################################################################

## plot
p_load(lattice) # most parties offer something: conditions?
lattice::histogram(as.factor(dat.v.b$offer.made.party.d))

## plot
p_load(lattice) # all voters accept offer
lattice::histogram(as.factor(dat.v.b$offer.taken.voter.d))

## plot
p_load(lattice) # however, not all voters become swing voters
## first impressions: most vote-buying offers come from the same party (as they would HATE to loose)
## it's not the opposing party the one that buys more: winning it's great, but losing is the WORST
lattice::histogram(as.factor(dat.v.b$swing.voter))

## plot
plot(dat.v.b$offer.party.type)
## most offers come from situations where both parties attempt to buy 
## since the dataset is relational, values across subtypes (parties and voters) are the same.

## plot
plot(dat.v.b$offer.party.type[dat.v.b$party.id.before.voter=="A"])
plot(dat.v.b$offer.party.type[dat.v.b$party.id.before.voter=="B"])
## for voters A and B, parties in almost the same proportion aim at buying votes,
## most of the times, at the same time.


# payoff plot SAVE BY HAND
p_load(lattice) 
options(scipen=9999999) # turn off sci not
payoffplot = densityplot(~payoff | role, 
            #scales=list(relation="free", rot=0),
            scales=list(x=list(at=c(
                round(as.numeric(quantile(dat.v.b$payoff)[1]),-2),
                round(as.numeric(quantile(dat.v.b$payoff)[2]),-2),
                round(as.numeric(quantile(dat.v.b$payoff)[3]),-2),
                round(as.numeric(quantile(dat.v.b$payoff)[4]),-2),
                round(as.numeric(quantile(dat.v.b$payoff)[5]),-2)
            ), 
            labels=c(
                round(as.numeric(quantile(dat.v.b$payoff)[1]),-2),
                round(as.numeric(quantile(dat.v.b$payoff)[2]),-2),
                round(as.numeric(quantile(dat.v.b$payoff)[3]),-2),
                round(as.numeric(quantile(dat.v.b$payoff)[4]),-2),
                round(as.numeric(quantile(dat.v.b$payoff)[5]),-2)
            )),
            rot=90),
            data=dat.v.b, 
            aspect = 1,
            xlab = " ", 
            ylab = "Payoffs by Role (actual currency)", 
            layout = c(3, 1)
)# columns, rows

# saving plot
png(filename="payoffplot.png", 
    type="cairo",
    units="in", 
    width=4, 
    height=2, 
    pointsize=10, 
    res=1000)

print(payoffplot)
dev.off()



######################################################################### 
# ************** M      O       D       E       L       S **************
#########################################################################

## ---- models-d ----
# for clustered std errors
p_load(sandwich,lmtest,DAMisc,lattice,latticeExtra)

#########################################################################
##### Amount Offered Model
#########################################################################

# Subsetting Data
m1.d = dat.v.b %>% select(offer.made.party, vote.intention.party, points.cumul.delta, ideo.distance, budget, participant.code, pivotal.3.5) %>% drop_na()
m1.d = as.data.frame(m1.d)

# Model (with participant FEs)
m1 = lm(offer.made.party ~ vote.intention.party + points.cumul.delta + ideo.distance + budget + pivotal.3.5 + participant.code, m1.d)
# options(scipen=9999999) # turn off sci not
# summary(m1)

# Clustered Std Errors and Model info
options(scipen=9999999) # turn off sci not
m1.clst.std.err = as.numeric(coeftest(m1, vcov. = vcovCL(m1, cluster = m1.d$participant.code, type = "HC0"))[,2])[1:5]
m1.clst.t.test = c(as.numeric(coeftest(m1, vcov. = vcovCL(m1, cluster = m1.d$participant.code, type = "HC0"))[,3])[1:5])
m1.clst.p.value = c(as.numeric(coeftest(m1, vcov. = vcovCL(m1, cluster = m1.d$participant.code, type = "HC0"))[,4])[1:5])
custom.model.names.m1 = "Amount of Vote-Buying Offer"

#########################################################################
##### Competitive Offers Model
#########################################################################

m2 = glm(competitive.offers.party ~ 
             points.cumul.delta +
             budget + 
             #vote.intention.party + 
             ideo.distance,
         data = dat.v.b, 
         family = binomial(link = "logit")
)

# Clustered Std Errors and Model info
m2.clst.std.err = as.numeric(coeftest(m2, vcov. = vcovCL(m2, cluster = dat.v.b$participant.code, type = "HC0"))[,2])[1:4]
m2.clst.t.test = c(as.numeric(coeftest(m2, vcov. = vcovCL(m2, cluster = dat.v.b$participant.code, type = "HC0"))[,3])[1:4])
m2.clst.p.value = c(as.numeric(coeftest(m2, vcov. = vcovCL(m2, cluster = dat.v.b$participant.code, type = "HC0"))[,4])[1:4])
custom.model.names.m2 = "Competitive Vote-Buying Offer"


# summary(m2)
# p_load(effects)
# plot(predictorEffects(m2))
## Comments: 
## 1. Competitive Offers are more likely when lost previous game at t-1
## 2. Competitive Offers are more likely when I have accumulated enough wealth throughout the game
## 3. Competitive Offers are NOT related to the imemdiate perception of risk (vote.intention.party)

## Reg Table
p_load(texreg)
reg.table = texreg::texreg( # screenreg
    list(m1),
    custom.model.names = c(custom.model.names.m1),
    #custom.coef.names = NULL,
    omit.coef = "participant",
    custom.coef.names = c("Intercept", "Vote Share", "Points Accumulated (delta)", "Spatial Distance", "Party Budget"),
    override.se = list(c(m1.clst.std.err,rep(0.0, length(unique(m1.d$participant.code))-1))),
    override.pvalues = list(c(m1.clst.p.value,rep(0.0, length(unique(m1.d$participant.code))-1))),
    custom.header = list( "OLS" = 1),
    stars = c(0.001, 0.01, 0.05, 0.1),
    symbol = "\\cdot",
    label = "reg:t",
    caption = "Statistical Model (OLS): Amount of Vote-Buying Offer.",
    float.pos="H",
    use.packages = FALSE,
    threeparttable = TRUE,
    custom.note = "\\item %stars. \\item Robust standard errors in parentheses. \\item Fixed effects parameteres omitted in table."
    )
## ----

## ---- reg:table:t ----
reg.table
## ----



## MODEL 1 PLOTS
#mientras mas pierdo ayer, mas caro compro hoy
m1.p1.d = data.frame(ggeffects::ggpredict(
    model=m1,
    terms=c("points.cumul.delta [all]"), 
    vcov.fun = "vcovHC", 
    vcov.type = "HC0")
); m1.p1.d$group = "Points Cumul (delta)"


#mientras mas votos a favor tengo, mas ofrezco
m1.p2.d = data.frame(ggeffects::ggpredict(
    model=m1,
    terms=c("vote.intention.party [all]"), 
    vcov.fun = "vcovHC", 
    vcov.type = "HC0")
); m1.p2.d$group = "Vote Share"

# no importa la distancia ideologica
m1.p3.d = data.frame(ggeffects::ggpredict(
    model=m1,
    terms=c("ideo.distance [all]"), 
    vcov.fun = "vcovHC", 
    vcov.type = "HC0")
); m1.p3.d$group = "Spatial Distance (left-right)"

# no importa el budget del partido
m1.p4.d = data.frame(ggeffects::ggpredict(
    model=m1,
    terms=c("budget [all]"), 
    vcov.fun = "vcovHC", 
    vcov.type = "HC0")
); m1.p4.d$group = "Party's Budget"


# pivotal voter
m1.p5.d = data.frame(ggeffects::ggpredict(
  model=m1,
  terms=c("pivotal.3.5 [all]"), 
  vcov.fun = "vcovHC", 
  vcov.type = "HC0")
); m1.p5.d$group = "Pivotal Voter"

# plot (export by hand)
m1.p.d = as.data.frame(rbind(m1.p1.d,m1.p2.d,m1.p3.d,m1.p4.d,m1.p5.d))
m1.p.d$group = factor(m1.p.d$group, 
                      levels = c("Vote Share", 
                                 "Points Cumul (delta)", 
                                 "Spatial Distance (left-right)", 
                                 "Party's Budget",
                                 "Pivotal Voter"))

#m1.p.d$group = as.factor(m1.p.d$group)
#m1.p.d$group <- relevel(m1.p.d$group, "Points Cumul (delta)")

p_load(lattice, latticeExtra, DAMisc)
m1plot = xyplot(predicted ~ x | group, 
                scales=list(relation="free", rot=0),
                data=m1.p.d, 
                aspect = 1,
                xlab = " ", 
                ylab = "Amount of Vote-Buying Offer (points)", 
                lower=m1.p.d$conf.low,
                upper=m1.p.d$conf.high,
                panel = panel.ci, 
                zl=F, 
                prepanel=prepanel.ci,
                layout = c(5, 1) # columns, rows
                )

# saving plot
png(filename="m1plot.png", 
    type="cairo",
    units="in", 
    width=12, 
    height=5, 
    pointsize=10, 
    res=1000)

print(m1plot)
dev.off()



m1.p.d.1 = m1.p.d[m1.p.d$group=="Vote Share",]
m1.p.d.2 = m1.p.d[m1.p.d$group=="Points Cumul (delta)",]
m1.p.d.3 = m1.p.d[m1.p.d$group=="Spatial Distance (left-right)",]
m1.p.d.4 = m1.p.d[m1.p.d$group=="Party's Budget",]

# m1.p.d.1
m1.p.d.1.p = xyplot(predicted ~ x | group, 
                    scales=list(relation="free", rot=0),
                    data=m1.p.d.1, 
                    aspect = 1,
                    xlab = " ", 
                    ylab = "Amount of Vote-Buying Offer (points)", 
                    lower=m1.p.d.1$conf.low,
                    upper=m1.p.d.1$conf.high,
                    panel = panel.ci, 
                    zl=F, 
                    prepanel=prepanel.ci,
                    layout = c(1, 1) # columns, rows
                    )
# saving plot
png(filename="m1plot_1.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=5, 
    pointsize=10, 
    res=1000)

print(m1.p.d.1.p)
dev.off()


# m1.p.d.2
m1.p.d.2.p = xyplot(predicted ~ x | group, 
                    scales=list(relation="free", rot=0),
                    data=m1.p.d.2, 
                    aspect = 1,
                    xlab = " ", 
                    ylab = "Amount of Vote-Buying Offer (points)", 
                    lower=m1.p.d.2$conf.low,
                    upper=m1.p.d.2$conf.high,
                    panel = panel.ci, 
                    zl=F, 
                    prepanel=prepanel.ci,
                    layout = c(1, 1) # columns, rows
)
# saving plot
png(filename="m1plot_2.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=5, 
    pointsize=10, 
    res=1000)

print(m1.p.d.2.p)
dev.off()


# m1.p.d.3
m1.p.d.3.p = xyplot(predicted ~ x | group, 
                    scales=list(relation="free", rot=0),
                    data=m1.p.d.3, 
                    aspect = 1,
                    xlab = " ", 
                    ylab = "Amount of Vote-Buying Offer (points)", 
                    lower=m1.p.d.3$conf.low,
                    upper=m1.p.d.3$conf.high,
                    panel = panel.ci, 
                    zl=F, 
                    prepanel=prepanel.ci,
                    layout = c(1, 1) # columns, rows
)
# saving plot
png(filename="m1plot_3.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=5, 
    pointsize=10, 
    res=1000)

print(m1.p.d.3.p)
dev.off()


# m1.p.d.4
m1.p.d.4.p = xyplot(predicted ~ x | group, 
                    scales=list(relation="free", rot=0),
                    data=m1.p.d.4, 
                    aspect = 1,
                    xlab = " ", 
                    ylab = "Amount of Vote-Buying Offer (points)", 
                    lower=m1.p.d.4$conf.low,
                    upper=m1.p.d.4$conf.high,
                    panel = panel.ci, 
                    zl=F, 
                    prepanel=prepanel.ci,
                    layout = c(1, 1) # columns, rows
)
# saving plot
png(filename="m1plot_4.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=5, 
    pointsize=10, 
    res=1000)

print(m1.p.d.4.p)
dev.off()

## MODEL 2 PLOTS
m2.p1.d = data.frame(ggeffects::ggpredict(
    model=m2,
    terms=c("points.cumul.delta [all]"), 
    vcov.fun = "vcovHC", 
    vcov.type = "HC0")
); m2.p1.d$group = "Points Cumul (delta)"


m2.p2.d = data.frame(ggeffects::ggpredict(
    model=m2,
    terms=c("ideo.distance [all]"), 
    vcov.fun = "vcovHC", 
    vcov.type = "HC0")
); m2.p2.d$group = "Spatial Distance (left-right)"


m2.p3.d = data.frame(ggeffects::ggpredict(
    model=m2,
    terms=c("budget [all]"), 
    vcov.fun = "vcovHC", 
    vcov.type = "HC0")
); m2.p3.d$group = "Party's Budget"


# plot (export by hand)
m2.p.d = as.data.frame(rbind(m2.p1.d,m2.p2.d,m2.p3.d))
m2.p.d$group = as.factor(m2.p.d$group)
m2.p.d$group <- relevel(m2.p.d$group, "Points Cumul (delta)")

p_load(lattice, latticeExtra, DAMisc)
m2plot = xyplot(predicted ~ x | group, 
                scales=list(relation="free", rot=0),
                data=m2.p.d, 
                aspect = 1,
                xlab = " ", 
                ylab = "Competitive Vote-Buying Offer", 
                lower=m2.p.d$conf.low,
                upper=m2.p.d$conf.high,
                panel = panel.ci, 
                zl=F, 
                prepanel=prepanel.ci,
                layout = c(3, 1) # columns, rows
)

# saving plot
png(filename="m2plot.png", 
    type="cairo",
    units="in", 
    width=8, 
    height=5, 
    pointsize=10, 
    res=1000)

print(m2plot)
dev.off()

# plotting dep variable plot BY HAND
p_load(gridExtra,lattice)
m1.dep.var = histogram(~m1.d$offer.made.party, 
                       aspect = 1,
                       xlab = "Amount of Vote-Buying Offer (points)"
                       )

# m2.dep.var = histogram(~as.factor(dat.v.b$competitive.offers.party), 
#                       aspect = 1,
#                       xlab = "Competitive Vote-Buying Offer"
#                       )

png(filename="depvarplot.png", 
    type="cairo",
    units="in", 
    width=5, 
    height=5, 
    pointsize=10, 
    res=1000)

m1.dep.var
#grid.arrange(m1.dep.var,m2.dep.var, ncol=2)
dev.off()


################################################ 
# ************** Summary Stats **************
################################################ 

## ---- summary:stats:d ----
p_load(ggpubr)
summary.stats <- dat.v.b %>%  
    mutate(male =  if_else(gender == "Hombre", 1, 0)) %>%
    select(names(dat.v.b.ID), role, male, payoff) %>% 
    group_by(role) %>%
    distinct(participant.code, .keep_all = TRUE) %>% 
    get_summary_stats(type = "common")
## ----

## ---- summary:stats:t ----
p_load(xtable)
xtable(summary.stats[order(summary.stats$variable),], 
       caption = "Summary Statistics.", 
       digits = 0,
       label = "summary:stats:t",
       align = rep("c",12))
## ----



################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
abstract.c = as.character(c("Leveraging on the expected utility theory framework, most research asserts that parties in need of securing electoral support invest in vote buying. We consider this framework is limited in a number of ways. First, it assumes that losses and gains affect party's decision-making process in a comparable way---i.e., winning elections feels good as losing one hurts. Second, it assumes that the decision-making process of clientelist political parties focuses only on absolute levels of utilities while overlooking changes in outcomes respect to a reference point. Whether these assumptions hold is very important for understanding why parties buy votes. By introducing prospect theory in the clientelism literature, we hypothesize that parties are risk averse in the domain of gains and risk-seeking in the domain of losses---i.e., losing an election hurts more than winning an election pleases. This explains why clientelism is most likely when parties are probable winners or have experienced important losses in the past. These results are invariant to the political identity of voters (i.e., whether voters are swing or core voters). Unfortunately, the expected utility theory (wrongly) predicts that under these scenarios clientelism should not occur. After formalizing a theory of vote buying and vote selling within the expected utility theory, we tested it in the lab by designing an economic experiment. The voting experiment was carefully designed to capture different domains of gains/losses as well as varying reference points. Exploiting these novel experimental data, we show that prospect theory provides a better explanation of clientelism than do other theories based on the expected-utility theory. As the statistical analyses suggest, because of risk-seeking with respect to losses, experimental subjects adopt a more risky alternative buying votes in a way that is unpredicted by standard expected-value calculations."))
writeLines(abstract.c, fileConn)
close(fileConn)
## ----




## ---- abstract.length ----
abstract.c.l = sapply(strsplit(abstract.c, " "), length)
## ----
