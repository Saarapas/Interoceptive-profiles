setwd("R:\\IMI-PROMPT\\Reetta\\Saara\\LOPULLISET AINEISTOT\\Tunnedata\\Tunne1\\R_paketit\\")
pkgs <- list.files()

install.packages(c(print(as.character(pkgs), collapse="\",\"")), repos = NULL, type="binary")

Sys.setenv(PATH=paste(Sys.getenv("PATH"),"R:\\IMI-PROMPT\\Reetta\\Saara\\LOPULLISET AINEISTOT\\Tunnedata\\Tunne1\\R_paketit\\",sep=":"))

library(stringr)
library(tidyverse)
library(psych)
library(plyr)
library(dplyr)
library(labeling)
library(farver)
library(yarrr)
library(knitr)
library(table1)
library(ltm)
library(car)
library(boot)
library(devtools)
library(expm)
library(msm)
library(ltm)



tunnedata_reliabiliteetit <- read.csv2("R:/IMI-PROMPT/Reetta/Saara/VARSINAISET/raakadata_9_26.CSV", 
                                       header = TRUE, 
                                       dec = ",", 
                                       sep = ";", 
                                       stringsAsFactors = TRUE,
                                       na='NaN')

#EHP-30 sum scores
tunnedata_reliabiliteetit <- tunnedata_reliabiliteetit %>% dplyr::mutate(EHP_pain = ((EHP301 + EHP302 + EHP303 + EHP304 + EHP305 + EHP306 + EHP307 + EHP308 + EHP309 + EHP3010 + EHP3011)/44)*100)
tunnedata_reliabiliteetit <- tunnedata_reliabiliteetit %>% dplyr::mutate(EHP_control = ((EHP3012 + EHP3013 + EHP3014 + EHP3015 + EHP3016 + EHP3017)/24)*100)
tunnedata_reliabiliteetit <- tunnedata_reliabiliteetit %>% dplyr::mutate(EHP_emotional = ((EHP3018 + EHP3019 + EHP3020 + EHP3021 + EHP3022 + EHP3023)/24)*100)
tunnedata_reliabiliteetit <- tunnedata_reliabiliteetit %>% dplyr::mutate(EHP_social = ((EHP3024 + EHP3025 + EHP3026 + EHP3027)/16)*100)
tunnedata_reliabiliteetit <- tunnedata_reliabiliteetit %>% dplyr::mutate(EHP_self = ((EHP3028 + EHP3029 + EHP3030)/12)*100)

#patients with confirmed endo
puuttuu_AFS <- is.na(tunnedata_reliabiliteetit$type)
sum(puuttuu_AFS)

tunnedata_reliabiliteetit_endo <- subset(tunnedata_reliabiliteetit,
                                         subset = !puuttuu_AFS & rAFSscore > 0)


#how many have EHP & MAIA
tunnedata_reliabiliteetit_ehp_maia <- tunnedata_reliabiliteetit_endo %>% drop_na("EHP_pain", "EHP_self", "MAIA_noticing")


#EHP reliability
ehp_pain_alpha <- data.frame(tunnedata_reliabiliteetit_ehp_maia[,150:160])
alpha(ehp_pain_alpha, check.keys = T)

ehp_control_alpha <- data.frame(tunnedata_reliabiliteetit_ehp_maia[,161:166])
alpha(ehp_control_alpha, check.keys = T)

ehp_emotional_alpha <- data.frame(tunnedata_reliabiliteetit_ehp_maia[,167:172])
alpha(ehp_emotional_alpha, check.keys = T)

ehp_social_alpha <- data.frame(tunnedata_reliabiliteetit_ehp_maia[,173:176])
alpha(ehp_social_alpha, check.keys = T)

ehp_self_alpha <- data.frame(tunnedata_reliabiliteetit_ehp_maia[,177:179])
alpha(ehp_self_alpha, check.keys = T)

#HADS reliability
HADS_reliability_a <- tunnedata_reliabiliteetit_ehp_maia %>% dplyr::select (c('AN1E', 'AN3E', 'AN5E', 'AN7E', 'AN9E', 'AN11E', 'AN13E'))
HADS_reliability_d <- tunnedata_reliabiliteetit_ehp_maia %>% dplyr::select (c('DE2E', 'DE4E', 'DE6E', 'DE8E', 'DE10E', 'DE12E', 'DE14E'))

hads_a_alpha <- data.frame(HADS_reliability_a)
alpha(hads_a_alpha, check.keys = T)

hads_d_alpha <- data.frame(HADS_reliability_d)
alpha(hads_d_alpha, check.keys = T)