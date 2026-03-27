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



tunnedata_reliabiliteetit <- read.csv2("R:/IMI-PROMPT/Reetta/Saara/VARSINAISET/raakadata_5_25.CSV", 
                                       header = TRUE, 
                                       dec = ",", 
                                       sep = ";", 
                                       stringsAsFactors = TRUE,
                                       na='NaN')

#patients with confirmed endo
puuttuu_AFS <- is.na(tunnedata_reliabiliteetit$type)
sum(puuttuu_AFS)

tunnedata_reliabiliteetit_endo <- subset(tunnedata_reliabiliteetit,
                                         subset = !puuttuu_AFS & rAFSscore > 0)

tunnedata_reliabiliteetit_endo <- tunnedata_reliabiliteetit_endo %>% dplyr::mutate(MAIA_trusting_uusi = (MAIA30 + MAIA31 + MAIA32)/3)

#how many have EHP & MAIA
tunnedata_reliabiliteetit_ehp_maia <- tunnedata_reliabiliteetit_endo %>% drop_na("EHP_pain", "MAIA_noticing")


#EHP reliability
ehp_pain_alpha <- data.frame(tunnedata_reliabiliteetit_ehp_maia[,141:151])
alpha(ehp_pain_alpha, check.keys = T)

ehp_control_alpha <- data.frame(tunnedata_reliabiliteetit_ehp_maia[,152:157])
alpha(ehp_control_alpha, check.keys = T)

ehp_emotional_alpha <- data.frame(tunnedata_reliabiliteetit_ehp_maia[,158:163])
alpha(ehp_emotional_alpha, check.keys = T)

ehp_social_alpha <- data.frame(tunnedata_reliabiliteetit_ehp_maia[,164:167])
alpha(ehp_social_alpha, check.keys = T)

ehp_self_alpha <- data.frame(tunnedata_reliabiliteetit_ehp_maia[,168:170])
alpha(ehp_self_alpha, check.keys = T)

#HADS reliability
HADS_reliability_a <- tunnedata_reliabiliteetit_ehp_maia %>% dplyr::select (c('AN1E', 'AN3E', 'AN5E', 'AN7E', 'AN9E', 'AN11E', 'AN13E'))
HADS_reliability_d <- tunnedata_reliabiliteetit_ehp_maia %>% dplyr::select (c('DE2E', 'DE4E', 'DE6E', 'DE8E', 'DE10E', 'DE12E', 'DE14E'))

hads_a_alpha <- data.frame(HADS_reliability_a)
alpha(hads_a_alpha, check.keys = T)

hads_d_alpha <- data.frame(HADS_reliability_d)
alpha(hads_d_alpha, check.keys = T)