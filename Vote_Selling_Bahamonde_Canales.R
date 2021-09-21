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
                                                            v.buying.dat$vote_b.3.player.p_oferta_choice == 1, "B", NA
                                             )
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
        v.buying.dat$vote_b.1.group.pje_win_cB
)


v.buying.dat.1$vote.intention = ifelse(
        v.buying.dat.1$v.buying.dat.vote_b.1.player.votanteOpartido=="Partido A", 
        v.buying.dat.1$v.buying.dat.vote_b.1.group.n_votantes_A, 
        ifelse(v.buying.dat.1$v.buying.dat.vote_b.1.player.votanteOpartido=="Partido B", 
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
        v.buying.dat.voter.ideology.b.1
        
) )

# change names
colnames(v.buying.dat.1) <- c("participant.code",
                              "session.code",
                              "player.votanteOpartido",
                              "party.id.before.voter",
                              "offer.party.type",
                              "swing.voter",
                              "participant.payoff",
                              "points.this.round",
                              "group.presupuesto",
                              "player.p_oferta_amount",
                              "offer.taken.voter",
                              "vote.intention",
                              "ideo.distance",
                              "voters.elect.payoff")


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
        v.buying.dat$vote_b.2.group.pje_win_cB
)


v.buying.dat.2$vote.intention = ifelse(
        v.buying.dat.2$v.buying.dat.vote_b.2.player.votanteOpartido=="Partido A", 
        v.buying.dat.2$v.buying.dat.vote_b.2.group.n_votantes_A, 
        ifelse(v.buying.dat.2$v.buying.dat.vote_b.2.player.votanteOpartido=="Partido B", 
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
        v.buying.dat.voter.ideology.b.2
        
) )

# change names
colnames(v.buying.dat.2) <- c("participant.code",
                              "session.code",
                              "player.votanteOpartido",
                              "party.id.before.voter",
                              "offer.party.type",
                              "swing.voter",
                              "participant.payoff",
                              "points.this.round",
                              "group.presupuesto",
                              "player.p_oferta_amount",
                              "offer.taken.voter",
                              "vote.intention",
                              "ideo.distance",
                              "voters.elect.payoff")



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
        v.buying.dat$vote_b.3.group.pje_win_cB
)


v.buying.dat.3$vote.intention = ifelse(
        v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="Partido A", 
        v.buying.dat.3$v.buying.dat.vote_b.3.group.n_votantes_A, 
        ifelse(v.buying.dat.3$v.buying.dat.vote_b.3.player.votanteOpartido=="Partido B", 
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
        v.buying.dat.voter.ideology.b.3
        
) )

# change names
colnames(v.buying.dat.3) <- c("participant.code",
                              "session.code",
                              "player.votanteOpartido",
                              "party.id.before.voter",
                              "offer.party.type",
                              "swing.voter",
                              "participant.payoff",
                              "points.this.round",
                              "group.presupuesto",
                              "player.p_oferta_amount",
                              "offer.taken.voter",
                              "vote.intention",
                              "ideo.distance",
                              "voters.elect.payoff")




# Stack up 3 games
dat.v.b = data.frame(rbind(v.buying.dat.1,v.buying.dat.2,v.buying.dat.3))

# Merging with ID df
dat.v.b = merge(dat.v.b, dat.v.b.ID, by=c("participant.code"))

# renaming
colnames(dat.v.b) <- c(
        "participant.code",
        "session.code",
        "role",
        "party.id.before.voter",
        "offer.party.type" ,
        "swing.voter",
        "payoff",
        "points.cumul",
        "budget",
        "offer.made.party",
        "offer.taken.voter",
        "vote.intention",
        "ideo.distance",
        "voters.elect.payoff",
        "gender",
        "salary.enough",
        "party.like",
        "party.id",
        "left.right",
        "vote.last.election",
        "vote.next.election"
)

# voters don't make offers, so it's NA for them.
dat.v.b$offer.made.party[dat.v.b$role=="votantes"] <- NA
# offer made NA now is 0
#dat.v.b$offer.made.party[is.na(dat.v.b$offer.made.party)] <- 0

# "calculates" the offer made to the voter
p_load(dplyr)
dat.v.b = dat.v.b %>% group_by(session.code,budget) %>% mutate(x_max = max(offer.made.party, na.rm = T))
dat.v.b$offer.made.voter = ifelse(dat.v.b$role=="votantes", dat.v.b$x_max, NA)
dat.v.b = subset(dat.v.b, select = -c(x_max))

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

# party ID after offer
dat.v.b$party.id.after.voter = ifelse(
        dat.v.b$party.id.before.voter=="A" & dat.v.b$swing.voter == 1, "B",
        ifelse(dat.v.b$party.id.before.voter=="B" & dat.v.b$swing.voter == 1, "A", 
               ifelse(dat.v.b$swing.voter == 0, dat.v.b$party.id.before.voter, NA)
               )
        )


# transforming vars.
dat.v.b$role = as.factor(dat.v.b$role)
dat.v.b$participant.code = as.factor(dat.v.b$participant.code)
dat.v.b$swing.voter = as.numeric(dat.v.b$swing.voter)
dat.v.b$payoff = as.numeric(dat.v.b$payoff)
dat.v.b$points.cumul = as.numeric(dat.v.b$points.cumul)
dat.v.b$budget = as.numeric(dat.v.b$budget)
dat.v.b$offer.made.voter = as.numeric(dat.v.b$offer.made.voter)
dat.v.b$offer.taken.voter = as.numeric(dat.v.b$offer.taken.voter)
dat.v.b$vote.intention = as.numeric(dat.v.b$vote.intention)
dat.v.b$ideo.distance = as.numeric(dat.v.b$ideo.distance)
dat.v.b$voters.elect.payoff = as.numeric(dat.v.b$voters.elect.payoff)
dat.v.b$voters.elect.payoff = as.numeric(dat.v.b$voters.elect.payoff)
dat.v.b$points.cumul.delta = as.numeric(dat.v.b$points.cumul.delta)

# round count
p_load(dplyr)
dat.v.b = dat.v.b %>% group_by(participant.code) %>% mutate(round = row_number())

dat.v.b = dat.v.b %>% select(session.code,
                             participant.code,
                             round,
                             role,
                             party.id.before.voter,
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


######################################################################### 
# ************** M      O       D       E       L       S **************
#########################################################################


#########################################################################
# function that does clustered SEs
vcovCluster <- function(
        model,
        cluster
)
{
        require(sandwich)
        require(lmtest)
        if(nrow(model.matrix(model))!=length(cluster)){
                stop("check your data: cluster variable has different N than model")
        }
        M <- length(unique(cluster))
        N <- length(cluster)           
        K <- model$rank   
        if(M<50){
                warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
        }
        dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
        uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
        rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
        return(rcse.cov)
}

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(lmtest,sandwich,msm)
#########################################################################



m1 = lm(offer.made ~ 
                budget + 
                vote.intention + 
                # payoff +
                # voters.elect.payoff +
                ideo.distance,
        data = dat.v.b[dat.v.b$role != "votantes",])

coeftest(m1, vcov = vcovCluster(m1, cluster = dat.v.b[dat.v.b$role != "votantes",]$participant.code))



m2 = glm(offer.taken ~ 
                 budget +
                 vote.intention +
                 offer.made +
                 #payoff +
                 ideo.distance + 
                 voters.elect.payoff, 
         data = dat.v.b[dat.v.b$role != "votantes",], family = binomial(link = "logit"))

options(scipen=9999999) # turn off sci not
coeftest(m2, vcov = vcovCluster(m2, cluster = dat.v.b[dat.v.b$role != "votantes",]$participant.code))


m3 = glm(swing.voter ~ 
                 budget +
                 offer.made.voter +
                 ideo.distance,
         #offer.made +
         #payoff +
         data = dat.v.b, family = binomial(link = "logit"))


options(scipen=9999999) # turn off sci not
summary(m3)

p_load(effects)
plot(predictorEffects(m3))



m4 = lm(offer.made ~ 
                budget + 
                points.cumul.delta +
                #points.cumul +
                participant.code +
                vote.intention,
        #ideo.distance,
        data = dat.v.b)

summary(m4)


m5 = lm(offer.made ~ 
                budget + 
                #points.cumul.delta +
                points.cumul +
                participant.code +
                vote.intention,
        #ideo.distance,
        data = dat.v.b)

summary(m5)

m6 = glm(offer.taken ~ 
                 budget + 
                 points.cumul.delta +
                 participant.code +
                 vote.intention,
         data = dat.v.b, family = binomial(link = "logit"))


options(scipen=9999999) # turn off sci not
summary(m6)
p_load(effects)
plot(predictorEffects(m6, "points.cumul.delta"))





m7 = glm(offer.taken ~ 
                 budget + 
                 #points.cumul.delta +
                 points.cumul +
                 participant.code +
                 vote.intention,
         #ideo.distance,
         data = dat.v.b, family = binomial(link = "logit"))


options(scipen=9999999) # turn off sci not
summary(m7)
p_load(effects)
plot(predictorEffects(m7, "points.cumul"))






############################## 
# Question 1: Who do Parties target to? Core supporters? Swing voters?
# Question 2: When do voters decide to take Partie's offer?
# Question 3: What does determine the vote-buying price?
############################## 



################################################ 
# ************** VOTE SELLING DATA **************
################################################ 

# Question 1: WHen do voters sell? 

## It's a two-stage process.
### Heckman: STAGE 1 = vende [si/no] // STAGE 2: for the ones who decide to sell, do they take A's or B's offer?



