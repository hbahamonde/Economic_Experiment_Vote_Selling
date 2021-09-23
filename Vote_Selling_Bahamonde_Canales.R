############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())

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
        # "vote_b.1.group.n_votantes",
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
        # "vote_b.2.group.n_votantes" ,
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
        # "vote_b.3.group.n_votantes" ,
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
        v.buying.dat$vote_b.1.player.tipoAoB
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
        v.buying.dat$vote_b.2.player.tipoAoB
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
        v.buying.dat$vote_b.3.player.tipoAoB
        )


v.buying.dat.3$vote.intention.party = ifelse(
        v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="Partido A", 
        v.buying.dat.3$v.buying.dat.vote_b.3.group.n_votantes_A, 
        ifelse(v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="Partido B", 
               v.buying.dat.3$v.buying.dat.vote_b.3.group.n_votantes_B, 
               NA)
        )

# HERE vote.intention.voter.before.offer
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
                             party.id.before.voter,
                             vote.intention.voter.before.offer,
                             offer.party.type,
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


######################################################################### 
# ************** M      O       D       E       L       S **************
#########################################################################

# for clustered std errors
p_load(sandwich,lmtest)


#########################################################################
## SWING VOTER
m3.d = dat.v.b %>% select(swing.voter, offer.made.voter, participant.code, ideo.distance, vote.intention.voter.before.offer, points.cumul.delta) %>% drop_na()
m3 = glm(swing.voter ~ vote.intention.voter.before.offer + points.cumul.delta + offer.made.voter, 
         data = m3.d, family = binomial(link = "logit")
         )


options(scipen=9999999) # turn off sci not
p_load(texreg)
screenreg(m3)
p_load(sandwich,lmtest)
coeftest(m3, vcov. = vcovCL(m3, cluster = m3.d$participant.code, type = "HC0"))
coefci(m3, level = 0.95, vcov. = vcovCL(m3, cluster = m3.d$participant.code, type = "HC0"))
p_load(effects)
plot(predictorEffects(m3))


# https://stackoverflow.com/questions/49161198/predicted-probability-plot-with-robust-errors-for-logit-model
p_load(ggeffects)
plot(ggeffects::ggpredict(
        model=m3,
        terms="offer.made.voter [all]", 
        vcov.fun = "vcovHC", 
        vcov.type = "HC0"
)
)

#########################################################################
## OFFER TYPE: competitive.offers.party
m5 = glm(competitive.offers.party ~ 
                 #budget + 
                 points.cumul.delta +
                 points.cumul +
                 # participant.code +
                 vote.intention.party,
         #ideo.distance,
         data = dat.v.b, 
         family = binomial(link = "logit")
)

summary(m5)
# p_load(effects)
# plot(predictorEffects(m5))
## Comments: 
## 1. Competitive Offers are more likely when lost previous game at t-1
## 2. Competitive Offers are more likely when I have accumulated enough wealth throughout the game
## 3. Competitive Offers are NOT related to the imemdiate perception of risk (vote.intention.party)


p_load(sandwich,lmtest)
test = coeftest(m5, vcov. = vcovCL(m5, cluster = dat.v.b$participant.code, type = "HC0"))
as.numeric(test[,2])


p_load(ggeffects)
dev.off()
m5.p1 = plot(ggeffects::ggpredict(
        model=m5,
        terms=c("points.cumul.delta [all]"), 
        vcov.fun = "vcovHC", 
        vcov.type = "HC0"
        )
     ) + labs(
             x = bquote("Experimental Points"[t-1]), 
             y = "Competitive Vote-Buying Offer", 
             title = "Predicted Probabilities of Competitive Vote-Buying Offers"
     )

p_load(ggeffects)
dev.off()
m5.p2 = plot(ggeffects::ggpredict(
        model=m5,
        terms=c("points.cumul [all]"), 
        vcov.fun = "vcovHC", 
        vcov.type = "HC0")
     ) + labs(
             x = bquote("Cumulated Experimental Points"[t]), 
             y = "", 
             title = ""
             )
     

p_load(ggeffects)
dev.off()
m5.p3 = plot(ggeffects::ggpredict(
        model=m5,
        terms=c("vote.intention.party [all]"), 
        vcov.fun = "vcovHC", 
        vcov.type = "HC0"
        )
     ) + labs(
             x = "Subject's Party Electoral Support", 
             y = "", 
             title = ""
     )


p_load(patchwork)
m5.p1|m5.p2|m5.p3

#########################################################################
##### Vote Intention: Risk of Losing the Election
p_load(lattice)
lattice::histogram(dat.v.b$vote.intention.party)

formula.m8 = as.formula(offer.made.party ~ vote.intention.party + points.cumul.delta)


m8.d = dat.v.b %>% select(offer.made.party, vote.intention.party, points.cumul.delta, participant.code) %>% drop_na()


m8 = lm(offer.made.party ~ vote.intention.party + points.cumul.delta + participant.code, m8.d)
coeftest(m8, vcov. = vcovCL(m8, cluster = m8.d$participant.code, type = "HC0"))

# mientras mas votos a favor tengo, mas ofrezco
p_load(ggeffects)
plot(ggeffects::ggpredict(
        model=m8,
        terms=c("vote.intention.party [all]"), 
        vcov.fun = "vcovHC", 
        vcov.type = "HC0"
        )
     )

# mientras mas he perdido, mas ofrezco 
p_load(ggeffects)
plot(ggeffects::ggpredict(
        model=m8,
        terms=c("points.cumul.delta [all]"), 
        vcov.fun = "vcovHC", 
        vcov.type = "HC0"
        )
     )


summary(m8)
texreg::screenreg(
        list(m8, m3, m5),
        omit.coef = "participant"
        )

















################################################ 
# ************** VOTE SELLING DATA **************
################################################ 

# Question 1: WHen do voters sell? 

## It's a two-stage process.
### Heckman: STAGE 1 = vende [si/no] // STAGE 2: for the ones who decide to sell, do they take A's or B's offer?



