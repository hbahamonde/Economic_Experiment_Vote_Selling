############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())



# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(readxl,tibble)

setwd("./research/Economic_Experiment_Vote_Selling")
dir = getwd() # set dir
setwd(dir) # set dir
dat <-read_excel(paste(dir, "/data/data.xlsx", sep="")) # read data
names(dat) <- sub(" ", ".", names(dat))

# keeping vars
"participant.id_in_session" 
"participant.code"
"participant._current_app_name" 
"participant.payoff" 
"session.code"





"vote_b.1.player.votanteOpartido" 
"vote_b.1.player.tipoAoB"  
"vote_b.1.player.nuevotipoAoB"
"vote_b.1.player.p_oferta_choice" 
"vote_b.1.player.p_oferta_amount"             
"vote_b.1.player.p_oferta_acepta"             
"vote_b.1.player.win_lose"                    
"vote_b.1.player.puntos"  
"vote_b.1.player.payoff"  
"vote_b.1.group.id_in_subsession"
"vote_b.1.group.presupuesto"   
"vote_b.1.group.n_votantes"
"vote_b.1.group.partido_elegido" 
"vote_b.1.group.tipo_votante" 
"vote_b.1.group.n_votantes_A"                 
"vote_b.1.group.n_votantes_B"  
"vote_b.1.group.ubicacion_pA"                 
"vote_b.1.group.ubicacion_pB"   
"vote_b.1.group.pje_win_cA"                   
"vote_b.1.group.pje_win_cB"  


"vote_b.2.player.votanteOpartido"             
"vote_b.2.player.tipoAoB"                     
"vote_b.2.player.nuevotipoAoB"                
"vote_b.2.player.p_oferta_choice"             
"vote_b.2.player.p_oferta_amount"             
"vote_b.2.player.p_oferta_acepta"             
"vote_b.2.player.win_lose"                    
"vote_b.2.player.puntos"    
"vote_b.2.player.payoff" 
"vote_b.2.group.id_in_subsession"
"vote_b.2.group.presupuesto" 
"vote_b.2.group.n_votantes" 
"vote_b.2.group.partido_elegido" 
"vote_b.2.group.tipo_votante"                 
"vote_b.2.group.n_votantes_A"                 
"vote_b.2.group.n_votantes_B"     
"vote_b.2.group.ubicacion_pA"                 
"vote_b.2.group.ubicacion_pB"                 
"vote_b.2.group.pje_win_cA"                   
"vote_b.2.group.pje_win_cB"    


"vote_b.3.player.votanteOpartido"             
"vote_b.3.player.tipoAoB"                     
"vote_b.3.player.nuevotipoAoB"                
"vote_b.3.player.p_oferta_choice"             
"vote_b.3.player.p_oferta_amount"             
"vote_b.3.player.p_oferta_acepta"             
"vote_b.3.player.win_lose"                    
"vote_b.3.player.puntos"    
"vote_b.3.player.payoff" 
"vote_b.3.group.id_in_subsession"
"vote_b.3.group.presupuesto" 
"vote_b.3.group.n_votantes" 
"vote_b.3.group.partido_elegido" 
"vote_b.3.group.tipo_votante"                 
"vote_b.3.group.n_votantes_A"                 
"vote_b.3.group.n_votantes_B"     
"vote_b.3.group.ubicacion_pA"                 
"vote_b.3.group.ubicacion_pB"                 
"vote_b.3.group.pje_win_cA"                   
"vote_b.3.group.pje_win_cB"     


################################################ 
# ************** VOTE BUYING DATA **************
################################################ 


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



