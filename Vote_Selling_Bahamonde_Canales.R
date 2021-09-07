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
dat <- subset(dat, participant.payoff > 0 )


# keeping vars

## ID VARS
id.vars = c(
        "participant.code",
        "session.code",
        "participant.payoff"
        )


## VOTE BUYING VARS
v.buying.vars = c(
        "vote_b.1.player.votanteOpartido" ,
        # "vote_b.1.player.tipoAoB",
        # "vote_b.1.player.nuevotipoAoB",
        # "vote_b.1.player.p_oferta_choice" ,
        "vote_b.1.player.p_oferta_amount",
        "vote_b.1.player.p_oferta_acepta",
        # "vote_b.1.player.win_lose",
        # "vote_b.1.player.puntos",
        # "vote_b.1.player.payoff",
        # "vote_b.1.group.id_in_subsession",
        "vote_b.1.group.presupuesto",
        # "vote_b.1.group.n_votantes",
        # "vote_b.1.group.partido_elegido" ,
        # "vote_b.1.group.tipo_votante" ,
        "vote_b.1.group.n_votantes_A",
        "vote_b.1.group.n_votantes_B"  ,
        "vote_b.1.group.ubicacion_pA",
        "vote_b.1.group.ubicacion_pB"   ,
        "vote_b.1.group.pje_win_cA",
        "vote_b.1.group.pje_win_cB"  ,
        "vote_b.2.player.votanteOpartido",
        # "vote_b.2.player.tipoAoB",
        # "vote_b.2.player.nuevotipoAoB",
        "vote_b.2.player.p_oferta_choice",
        "vote_b.2.player.p_oferta_amount",
        "vote_b.2.player.p_oferta_acepta",
        # "vote_b.2.player.win_lose",
        # "vote_b.2.player.puntos",
        # "vote_b.2.player.payoff" ,
        # "vote_b.2.group.id_in_subsession",
        "vote_b.2.group.presupuesto" ,
        # "vote_b.2.group.n_votantes" ,
        # "vote_b.2.group.partido_elegido" ,
        # "vote_b.2.group.tipo_votante",
        "vote_b.2.group.n_votantes_A",
        "vote_b.2.group.n_votantes_B",
        "vote_b.2.group.ubicacion_pA",
        "vote_b.2.group.ubicacion_pB",
        "vote_b.2.group.pje_win_cA",
        "vote_b.2.group.pje_win_cB",
        "vote_b.3.player.votanteOpartido",
        # "vote_b.3.player.tipoAoB",
        # "vote_b.3.player.nuevotipoAoB",
        "vote_b.3.player.p_oferta_choice",
        "vote_b.3.player.p_oferta_amount",
        "vote_b.3.player.p_oferta_acepta",
        # "vote_b.3.player.win_lose",
        # "vote_b.3.player.puntos",
        # "vote_b.3.player.payoff" ,
        # "vote_b.3.group.id_in_subsession",
        "vote_b.3.group.presupuesto" ,
        # "vote_b.3.group.n_votantes" ,
        # "vote_b.3.group.partido_elegido" ,
        # "vote_b.3.group.tipo_votante",
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
"survey.1.player.q3"  # gender
"survey.1.player.q4" # Salario:  (1=Les alcanza bien y pueden ahorrar, 2=Les alcanza justo y sin grandes dificultades, 3=No les alcanza y tienen dificultades, 4=No les alcanza y tienen grandes dificultades)
"survey.1.player.q5" # Ingresos: (1= Menos de $288.800, 2= Entre $288.801 - $312.001, 3=Entre $312.002 - $361.002, 4= Entre $361.003 - $410.003, 5=Entre $410.004 - $459.004, 6=Entre $459.005 - $558.005, 7= Entre $558.006 - $657.006, 8=Entre $657.007 - $756.007, 9= Entre $756.008 - $1.005.008, 10= Más de $1.005.008)                         
"survey.1.player.q6" # Simpatiza con partido político 1=sí, 2=no
"survey.1.player.q7" # Qué partido? (1=Partido Socialista de Chile, 2= Unión Demócrata Independiente, 3= Renovación Nacional, 4=Partido Demócrata Cristiano, 5= Partido Comunistica de Chile, 6= Revolución Democrática, 7= Evolución Política, 8= Otro, 9=No me siento representado)                         
"survey.1.player.q8" # Escala tendencia política (1=izq a 10=derecha)
"survey.1.player.q9" # Voto ultima elección 1=sí, 0=no)                    
"survey.1.player.q10" # Piensa votar proxima elección 1=sí, 0=no







################################################ 
# ************** VOTE BUYING DATA **************
################################################ 

# subsetting vars
v.buying.dat = dat[c(id.vars, v.buying.vars, socio.dem.vars)]
# dropping obs that dont belong to the vote buying exp
v.buying.dat <- subset(v.buying.dat, !is.na(vote_b.1.player.votanteOpartido))

# gen var that marks if what offer was taken by voter, if any (game 1)
p_load(dplyr,tidyverse)
v.buying.dat <- v.buying.dat %>% dplyr::group_by(session.code,vote_b.1.group.ubicacion_pA,vote_b.1.group.ubicacion_pB) %>%
        mutate(value_tmp = vote_b.1.player.p_oferta_acepta)%>%
        fill(value_tmp)
v.buying.dat$offer.taken.b.1 = ifelse(v.buying.dat$vote_b.1.player.votanteOpartido=="Partido A" & v.buying.dat$value_tmp==1, 1, 
                                 ifelse(v.buying.dat$vote_b.1.player.votanteOpartido=="Partido B" & v.buying.dat$value_tmp==2, 1, 0))
v.buying.dat$offer.taken.b.1[is.na(v.buying.dat$offer.taken.b.1 )] <- 0

# gen var that marks if what offer was taken by voter, if any (game 2)
p_load(dplyr,tidyverse)
v.buying.dat <- v.buying.dat %>% dplyr::group_by(session.code,vote_b.2.group.ubicacion_pA,vote_b.2.group.ubicacion_pB) %>%
        mutate(value_tmp = vote_b.2.player.p_oferta_acepta)%>%
        fill(value_tmp)
v.buying.dat$offer.taken.b.2 = ifelse(v.buying.dat$vote_b.2.player.votanteOpartido=="Partido A" & v.buying.dat$value_tmp==1, 1, 
                                      ifelse(v.buying.dat$vote_b.2.player.votanteOpartido=="Partido B" & v.buying.dat$value_tmp==2, 1, 0))
v.buying.dat$offer.taken.b.2[is.na(v.buying.dat$offer.taken.b.2 )] <- 0


# gen var that marks if what offer was taken by voter, if any (game 3)
p_load(dplyr,tidyverse)
v.buying.dat <- v.buying.dat %>% dplyr::group_by(session.code,vote_b.3.group.ubicacion_pA,vote_b.3.group.ubicacion_pB) %>%
        mutate(value_tmp = vote_b.3.player.p_oferta_acepta)%>%
        fill(value_tmp)
v.buying.dat$offer.taken.b.3 = ifelse(v.buying.dat$vote_b.3.player.votanteOpartido=="Partido A" & v.buying.dat$value_tmp==1, 1, 
                                      ifelse(v.buying.dat$vote_b.3.player.votanteOpartido=="Partido B" & v.buying.dat$value_tmp==2, 1, 0))
v.buying.dat$offer.taken.b.3[is.na(v.buying.dat$offer.taken.b.3 )] <- 0


# dropping rows that are voters
v.buying.dat <- subset(v.buying.dat, vote_b.1.player.votanteOpartido!="votantes")

# Game 1
v.buying.dat.1 = data.frame(
        v.buying.dat$participant.code,
        v.buying.dat$session.code, 
        v.buying.dat$vote_b.1.player.votanteOpartido, 
        v.buying.dat$participant.payoff,
        v.buying.dat$vote_b.1.group.presupuesto, 
        v.buying.dat$vote_b.1.player.p_oferta_amount, 
        #v.buying.dat$vote_b.1.player.p_oferta_acepta,
        v.buying.dat$offer.taken.b.1,
        v.buying.dat$vote_b.1.group.n_votantes_A,
        v.buying.dat$vote_b.1.group.n_votantes_B, 
        v.buying.dat$vote_b.1.group.ubicacion_pA, 
        v.buying.dat$vote_b.1.group.ubicacion_pB, 
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
               NA)
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
        v.buying.dat.vote_b.1.group.pje_win_cB
        
) )



############################## 
# Generating Variables
############################## 

# Generate var to see if party targets: Question 1
p_load(dplyr)
dat <- data.frame(dat %>% group_by(Period,Group) %>% mutate(is.targeted = as.numeric(ifelse(Elección == 1 & 'Oferta.que.acepta.el.votante'!= 2,"0","1"))))

# Generate var to see if voter accepts party's/ies offer (Question 2)
dat$vende = as.numeric(ifelse(dat$'Oferta.que.acepta.el.votante'!= 2 & dat$is.targeted == 1,"1","0"))

# dist.part.mas.cercano
dat <- transform(dat, dist.part.mas.cercano = pmin(dat$'Distancia.Votante.Partido.A', dat$'Distancia.Votante.Partido.B'))

# dist.part.mas.lejano
dat <- transform(dat, dist.part.mas.lejano = pmax(dat$'Distancia.Votante.Partido.A', dat$'Distancia.Votante.Partido.B'))

# generate varianle to see what offer accepts the voter, if any
## If accepts party A's offer
ifelse(is.targeted = 1 & )

############################## 
# Question 1: Who do Parties target to? Core supporters? Swing voters?
############################## 

# Data partitioning: solo "Votante"
core.swinger.d = dat[dat$'Voto.Participante' == "Votante",]

# Bayesian Model
## 1. Install Clang (clang-8.0.0.pkg) and GNU Fortran (gfortran-6.1.pkg.dmg) from the CRAN tools directory (http://cran.r-project.org/bin/macosx/tools/)
## 2. Now install JAGS version 4.3.0 (JAGS-4.3.0.dmg) from Martyn Plummer's repository (http://sourceforge.net/projects/mcmc-jags/files/JAGS/).
## 3. Start the Terminal and type jags to see if you receive the message: Welcome to JAGS 4.3.0.
## 4. Open R and install the packages R2jags, coda, R2WinBUGS, lattice, and rjags, by typing install.packages(c("R2jags", "coda", "R2WinBUGS", "lattice", "rjags"))

# Possible ID's: 
## (1) distancia partido-votante
## (2) competencia partidaria (3's o 5's).
## (3) Party's budget

# DV
## dat$is.targeted


# model (sourced from https://github.com/jkarreth/Bayes/blob/master/mlm.state.instructions.R)
is.targeted.mod <- function() {
        for (i in 1:n){
                y[i] ~dbern(p[i]) ## Bernoulli distribution of y_i, and p[i] is latent probability 
                logit(p[i]) <- mu[i] ## Logit link function
                mu[i] <- 
                        b.dist.cercano*dist.cercano[i] + 
                        b.dist.lejano*dist.lejano[i] + 
                        b.cantidad.votantes*cantidad.votantes[i] + 
                        b.presup.partido*presup.partido[i]
        }
        b.dist.cercano ~ dnorm (0, .01)
        b.dist.lejano ~ dnorm (0, .01)
        b.cantidad.votantes ~ dnorm (0, .01)
        b.presup.partido ~ dnorm (0, .01)
}

# declare DV
y <- core.swinger.d$is.targeted
# declare IV's
dist.cercano <- core.swinger.d$dist.part.mas.cercano
dist.lejano <- core.swinger.d$dist.part.mas.lejano
cantidad.votantes <- core.swinger.d$Cantidad.en.el.grupo
presup.partido <- core.swinger.d$Presupuesto.Partido
# declare n
# declare others
n.chains = 4 # 4
n.iter   = 200000 # 200000
n.burnin = 5000 # 5000
# declare data
core.swinger.d.jags <- list(y = y, 
                            dist.cercano = dist.cercano,
                            dist.lejano = dist.lejano, 
                            cantidad.votantes = cantidad.votantes, 
                            presup.partido = presup.partido,
                            n = length(y)
                            )


# Monitor the following
is.targeted.mod.params = c("b.dist.cercano", "b.dist.lejano", "b.cantidad.votantes", "b.presup.partido", "p")
# run the model
p_load(R2jags, coda, R2WinBUGS, lattice, rjags)
set.seed(123)
core.swinger.d.jags.fit <- jags(
        data=core.swinger.d.jags,
        parameters.to.save = is.targeted.mod.params,
        inits=NULL,
        n.chains=n.chains,
        n.iter=n.iter,
        n.burnin=n.burnin, 
        model.file=is.targeted.mod,
        progress.bar = "none"
)


plot(core.swinger.d.jags.fit)

p_load(BayesPostEst)
mcmcCoefPlot(core.swinger.d.jags.fit, pars = NULL, pointest = "mean", ci = 0.95, hpdi = FALSE, sort = FALSE, plot = TRUE)

# table 1 (for paper)
mcmcReg(core.swinger.d.jags.fit)

# table 2 (quick table for analyses)
p_load(devtools)
source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R")
mcmctab(as.mcmc(core.swinger.d.jags.fit))




# ROC Curve
# p_load(ggmcmc)
# swiss.p.gg <- ggs(as.mcmc(core.swinger.d.jags.fit), family = "p")
# swiss.p.gg2 <- swiss.p.gg[with(swiss.p.gg, order(Chain, nchar(as.character(Parameter)))), ]
# ggs_rocplot(swiss.p.gg2, outcome = core.swinger.d$is.targeted)



# Predicted Prob's
core.swinger.sim.dat <- as.matrix(data.frame(constant = 1, 
                                             cantidad.votantes = median(core.swinger.d$Cantidad.en.el.grupo), 
                                             dist.cercano = median(core.swinger.d$dist.part.mas.cercano), 
                                             dist.lejano = median(core.swinger.d$dist.part.mas.lejano), 
                                             presup.partido = seq(
                                                     from = min(core.swinger.d$Presupuesto.Partido), 
                                                     to = max(core.swinger.d$Presupuesto.Partido), by = 20)
                                             ))

core.swinger.coefs <- as.matrix(as.mcmc(core.swinger.d.jags.fit))[, c(1:ncol(core.swinger.sim.dat))] 

Xb <- t(core.swinger.sim.dat %*% t(core.swinger.coefs))

core.swinger.pp <- exp(Xb) / (1 + exp(Xb))

p_load(reshape2,dplyr)

core.swinger.pp <- melt(core.swinger.pp, varnames = c("Iteration", "presup.partido"))

core.swinger.pp$presup.partido <- core.swinger.pp$presup.partido + min(core.swinger.d.jags$presup.partido) - 1

core.swinger.pp.sum <- summarise(group_by(core.swinger.pp, presup.partido), 
                                 mean.pp = mean(value), 
                                 lower.pp = quantile(value, probs = c(0.05), na.rm = T), 
                                 upper.pp = quantile(value, probs = c(0.95),  na.rm = T)
                                 )


ggplot(data = core.swinger.pp.sum, aes(x = presup.partido, y = mean.pp)) + geom_ribbon(aes(ymin = lower.pp, ymax = upper.pp), alpha = 0.2) + geom_line() + theme_bw() + ylab("Pr(Voted)")

############################## 
# Question 2: When do voters decide to take Partie's offer?
############################## 

# Data partitioning: only subjects with dat$is.targeted == 1
is.targeted.d = dat[dat$is.targeted == 1,]

# Stage 1: dat$vende
# Stage 2: 

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



