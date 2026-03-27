setwd("R:\\IMI-PROMPT\\Reetta\\Saara\\LOPULLISET AINEISTOT\\Tunnedata\\Tunne1\\R_paketit\\")

pkgs <- list.files()

install.packages(c(print(as.character(pkgs), collapse="\",\"")), repos = NULL, type="binary", dependencies = TRUE, INSTALL_opts = '--no-lock')

Sys.setenv(PATH=paste(Sys.getenv("PATH"),"R:\\IMI-PROMPT\\Reetta\\Saara\\LOPULLISET AINEISTOT\\Tunnedata\\Tunne1\\R_paketit\\",sep=":"))



library(tidyverse)
library(psych)
library(labeling)
library(farver)
library(yarrr)
library(knitr)
library(table1)
library(ltm)
library(car)
library(gtsummary)
library(gt)
library(factoextra)
library(ggrepel)
library(cluster)
library(ggpubr)
library(ggsignif)
library(rstatix)
library(ggplot2)
library(ggcorrplot)
library(maptree)
library(fossil)
library(dendextend)
library(rcompanion)
library(expm)
library(msm)
library(ltm)
library(tidyselect)
library(tidyr)
library(NbClust)
library(purrr)
library(dplyr)
library(FSA)
library(effectsize)


#seuraavalla rivillä haetaan funktioita tästä lokaatiosta, joita käytetään: joskus windows vaatii tuplakenoviivoja
source('R:\\IMI-PROMPT\\Reetta\\Saara\\LOPULLISET AINEISTOT\\Tunnedata\\Tunne1\\helper_functions_for_r_analysis.R')

load("R:\\IMI-PROMPT\\Reetta\\Saara\\LOPULLISET AINEISTOT\\Tunnedata\\esikasitelty_tunnedata_klusteri.RData")


#histograms and distributions
#MAIA
hist(tunnedata_klusterityö_ehp_maia$MAIA_noticing)
hist(tunnedata_klusterityö_ehp_maia$MAIA_not_worrying)
hist(tunnedata_klusterityö_ehp_maia$MAIA_attention_reg)
hist(tunnedata_klusterityö_ehp_maia$MAIA_body_listening)
hist(tunnedata_klusterityö_ehp_maia$MAIA_self_reg)
hist(tunnedata_klusterityö_ehp_maia$MAIA_emotional_awareness)
hist(tunnedata_klusterityö_ehp_maia$MAIA_trusting)

shapiro.test(tunnedata_klusterityö_ehp_maia$MAIA_noticing)
shapiro.test(tunnedata_klusterityö_ehp_maia$MAIA_not_worrying)
shapiro.test(tunnedata_klusterityö_ehp_maia$MAIA_attention_reg) 
shapiro.test(tunnedata_klusterityö_ehp_maia$MAIA_body_listening) 
shapiro.test(tunnedata_klusterityö_ehp_maia$MAIA_self_reg) 
shapiro.test(tunnedata_klusterityö_ehp_maia$MAIA_emotional_awareness)
shapiro.test(tunnedata_klusterityö_ehp_maia$MAIA_trusting)

#EHP
hist(tunnedata_klusterityö_ehp_maia$EHP_pain)
hist(tunnedata_klusterityö_ehp_maia$EHP_control)
hist(tunnedata_klusterityö_ehp_maia$EHP_emotional)
hist(tunnedata_klusterityö_ehp_maia$EHP_social)
hist(tunnedata_klusterityö_ehp_maia$EHP_self)

shapiro.test(tunnedata_klusterityö_ehp_maia$EHP_pain)
shapiro.test(tunnedata_klusterityö_ehp_maia$EHP_control)
shapiro.test(tunnedata_klusterityö_ehp_maia$EHP_emotional)
shapiro.test(tunnedata_klusterityö_ehp_maia$EHP_social)
shapiro.test(tunnedata_klusterityö_ehp_maia$EHP_self)

#PCS
hist(tunnedata_klusterityö_ehp_maia$PCS_rumination)
hist(tunnedata_klusterityö_ehp_maia$PCS_magnification)
hist(tunnedata_klusterityö_ehp_maia$PCS_helplessness)
hist(tunnedata_klusterityö_ehp_maia$PCS_summa)

shapiro.test(tunnedata_klusterityö_ehp_maia$PCS_rumination) 
shapiro.test(tunnedata_klusterityö_ehp_maia$PCS_magnification)
shapiro.test(tunnedata_klusterityö_ehp_maia$PCS_helplessness)
shapiro.test(tunnedata_klusterityö_ehp_maia$PCS_summa)

#HADS
hist(tunnedata_klusterityö_ehp_maia$hads_a_summa)
hist(tunnedata_klusterityö_ehp_maia$hads_d_summa)

shapiro.test(tunnedata_klusterityö_ehp_maia$hads_a_summa)
shapiro.test(tunnedata_klusterityö_ehp_maia$hads_d_summa)

#drop na
tunnedata_klusterit_ehp_maia <- tunnedata_klusterityö_ehp_maia %>% 
  drop_na(c("MAIA_noticing", "MAIA_not_worrying", "MAIA_attention_reg", "MAIA_body_listening", 
            "MAIA_self_reg", "MAIA_emotional_awareness", "MAIA_trusting"))


#recoding childhood adverse events
LK_data <- tunnedata_klusterit_ehp_maia %>% select(LK1:LK11)
LK_data <- LK_data %>% 
  replace(., is.na(.), 0) %>% 
  replace(., .==1, 0) %>% 
  replace(., .==3, 0) %>%
  replace(., .==2, 1) %>% 
  mutate(LKsum = rowSums(select(.,LK1:LK11)))
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK1_uusi = LK_data$LK1)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK2_uusi = LK_data$LK2)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK3_uusi = LK_data$LK3)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK4_uusi = LK_data$LK4)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK5_uusi = LK_data$LK5)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK6_uusi = LK_data$LK6)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK7_uusi = LK_data$LK7)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK8_uusi = LK_data$LK8)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK9_uusi = LK_data$LK9)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK10_uusi = LK_data$LK10)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LK11_uusi = LK_data$LK11)
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% 
  add_column(LKSum = LK_data$LKsum)

describe(tunnedata_klusterit_ehp_maia$LKSum)
freq_table(tunnedata_klusterit_ehp_maia$LKSum)

#reliability
maia_noticing_alpha <- data.frame(tunnedata_klusterit_ehp_maia[,101:104])
alpha(maia_noticing_alpha, check.keys = T)


maia_notd_alpha <- data.frame(tunnedata_klusterit_ehp_maia[,105:107])
alpha(maia_notd_alpha, check.keys = T)

maia_notw_alpha <- data.frame(tunnedata_klusterit_ehp_maia[,108:110])
alpha(maia_notw_alpha, check.keys = T)

maia_attention_alpha <- data.frame(tunnedata_klusterit_ehp_maia[,111:117])
alpha(maia_attention_alpha, check.keys = T)

maia_emotion_alpha <- data.frame(tunnedata_klusterit_ehp_maia[,118:122])
alpha(maia_emotion_alpha, check.keys = T)

maia_self_alpha <- data.frame(tunnedata_klusterit_ehp_maia[,123:126])
alpha(maia_self_alpha, check.keys = T)

maia_body_alpha <- data.frame(tunnedata_klusterit_ehp_maia[,127:129])
alpha(maia_body_alpha, check.keys = T)

maia_trusting_alpha <- data.frame(tunnedata_klusterit_ehp_maia[,130:132])
alpha(maia_trusting_alpha, check.keys = T)

catastrophising_alpha <- data.frame(tunnedata_klusterit_ehp_maia[,77:89])
alpha(catastrophising_alpha, check.keys = T)

resilience_alpha <- data.frame(tunnedata_klusterit_ehp_maia[,141:154])
alpha(resilience_alpha, check.keys = T)


#clustering
MAIA_klusteri <- (tunnedata_klusterit_ehp_maia %>% dplyr::select(c("MAIA_noticing", 
                                                                   "MAIA_not_worrying", "MAIA_attention_reg", "MAIA_body_listening", 
                                                                   "MAIA_self_reg", "MAIA_emotional_awareness", "MAIA_trusting")))
MAIA_scaled <- scale(MAIA_klusteri)

plot(MAIA_klusteri)

#defining linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#function to compute agglomerative coefficient
ac <- function(x) {
  cluster::agnes(MAIA_scaled, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)

#producing dendogram with Ward method
clust <- agnes(MAIA_scaled, method = "ward")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram")

#clusters with ward method
d <- dist(MAIA_scaled)
d2 <- d^2
fitH <- hclust(d2, "ward.D2")

#number of clusters
#Elbow method
fviz_nbclust(MAIA_scaled, hcut_D2, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#Silhouette
fviz_nbclust(MAIA_scaled, hcut_D2, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#KGS
a <- agnes (MAIA_scaled, method="ward")
b <- kgs (a, d2, maxclust=20)
plot (names (b), b, xlab="# clusters", ylab="penalty")

#choosing a four-cluster solution
#tehdään klusterointi neljän mukaan
plot(fitH, hang=-1)
rect.hclust(fitH, k = 4, border = "red")
clusters_4 <- cutree(fitH, 4)
clusters_4

#clusters as factors
tunnedata_klusterit_ehp_maia["clusters_ward_e"] <- clusters_4
tunnedata_klusterit_ehp_maia <- tunnedata_klusterit_ehp_maia %>% mutate(clusters_ward_faktori = factor(clusters_ward_e, 
                                                                                                       levels = c(1, 2, 3, 4),
                                                                                                       labels = c("Attentive", "Hypo-responsive", "Balanced", "Hypervigilant-distrustful")))


#background information
tunnedata_klusterit_ehp_maia$bmi <- tunnedata_klusterit_ehp_maia$weight/(tunnedata_klusterit_ehp_maia$height/100)^2
tunnedata_klusterit_ehp_maia$age <- as.numeric(tunnedata_klusterit_ehp_maia$age)
class(tunnedata_klusterit_ehp_maia$age)
taustatiedot <- tunnedata_klusterit_ehp_maia

#removing 25 missing subjects
taustatiedot_ei_puuttuvia <- is.na(tunnedata_klusterit_ehp_maia$pain_now)
sum(taustatiedot_ei_puuttuvia)


taustatiedot <- subset(tunnedata_klusterit_ehp_maia,
                       subset = !taustatiedot_ei_puuttuvia)


#background information

taustatiedot <- taustatiedot %>% mutate(pain_now_simple=ifelse(pain_now==1,1,0),
                                        Pain_now=factor(pain_now_simple, levels= c(1, 0), labels= c("Yes", "Others")))


taustatiedot <- taustatiedot %>% mutate(hist_migraine_simple=ifelse(hist_migraine==1,1,0),
                                        Migraine=factor(hist_migraine_simple, levels= c(1, 0), labels=c("Yes", "Others")))

taustatiedot <- taustatiedot %>% mutate(hist_headache_simple=ifelse(hist_headache==1,1,0),
                                        Headache=factor(hist_headache_simple, levels= c(1, 0), labels=c("Yes", "Others")))

taustatiedot <- taustatiedot %>% mutate(hist_abdomen_simple=ifelse(hist_abdomen==1,1,0),
                                        Abdomen=factor(hist_abdomen_simple, levels= c(1, 0), labels=c("Yes", "Others")))

taustatiedot <- taustatiedot %>% mutate(hist_menstrual_simple=ifelse(hist_menstrual==1,1,0),
                                        Menstrual=factor(hist_menstrual_simple, levels= c(1, 0), labels=c("Yes", "Others")))

taustatiedot <- taustatiedot %>% mutate(hist_back_shoulder_simple=ifelse(hist_back_shoulder==1,1,0),
                                        Back_shoulder=factor(hist_back_shoulder_simple, levels= c(1, 0), labels=c("Yes", "Others")))

taustatiedot <- taustatiedot %>% mutate(hist_joint_limb_simple=ifelse(hist_joint_limb==1,1,0),
                                        Joint_limb=factor(hist_joint_limb_simple, levels= c(1, 0), labels=c("Yes", "Others")))

taustatiedot <- taustatiedot %>% filter(!is.na(painkillers_overcounter)) %>% 
  mutate(analgesics = factor(painkillers_overcounter,
                             levels = 1:5,
                             labels = c("Daily", "Weekly", "Monthly", "Rarely", "Never")))

taustatiedot <- taustatiedot %>% filter(!is.na(painkillers_prescription)) %>% 
  mutate(prescribed = factor(painkillers_prescription,
                             levels = 1:5,
                             labels = c("Daily", "Weekly", "Monthly", "Rarely", "Never")))

#table for background information

cols_for_table <- c("clusters_ward_faktori",
                    "age", "bmi")

tbl_by_cluster <- tunnedata_klusterit_ehp_maia %>%
  select(all_of(cols_for_table)) %>%     
  tbl_summary(by = clusters_ward_faktori,
              type = all_categorical() ~ "categorical",
              missing_text = "Missing") %>% 
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>% 
  add_q(method = 'holm')
tbl_by_cluster

cols_for_table <- c("clusters_ward_faktori",
                    "Pain_now", "Migraine", "Headache", "Abdomen", "Menstrual", "Back_shoulder", "Joint_limb")

tbl_by_cluster <- taustatiedot %>%
  select(all_of(cols_for_table)) %>%     
  tbl_summary(by = clusters_ward_faktori,
              type = all_categorical() ~ "categorical",
              missing_text = "Missing") %>% 
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>% 
  add_q(method = 'holm')
tbl_by_cluster

kruskal.test(age ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)
kruskal.test(bmi ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)
T1 <- 0.01015
T2 <- 0.9375
fisher.test(tunnedata_klusterit_ehp_maia$education, tunnedata_klusterit_ehp_maia$clusters_ward_faktori)
T3 <- 0.2104
fisher.test(table(taustatiedot$Pain_now, taustatiedot$clusters_ward_faktori))
T4 <- 0.3919
fisher.test(table(taustatiedot$Migraine, taustatiedot$clusters_ward_faktori))
T5 <- 0.01464
fisher.test(table(taustatiedot$Headache, taustatiedot$clusters_ward_faktori))
T6 <- 0.155
fisher.test(table(taustatiedot$Abdomen, taustatiedot$clusters_ward_faktori))
T7 <- 0.3859
fisher.test(table(taustatiedot$Menstrual, taustatiedot$clusters_ward_faktori))
T8 <- 0.7283
fisher.test(table(taustatiedot$Back_shoulder, taustatiedot$clusters_ward_faktori))
T9 <- 0.0878
fisher.test(table(taustatiedot$Joint_limb, taustatiedot$clusters_ward_faktori))
T10 <- 0.2926
fisher.test(table(taustatiedot$analgesics, taustatiedot$clusters_ward_faktori), workspace = 2e9)
T11 <- 0.004368
fisher.test(table(taustatiedot$prescribed, taustatiedot$clusters_ward_faktori), workspace = 2e9)
T12 <- 0.746

#P-corrections for background information
p_taustatiedot <- p.adjust(c(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), method = "holm")
paste(sprintf("%.10f",p_taustatiedot), collapse=",")



#effect sizes for background information
kruskal_effsize(age ~ clusters_ward_e,
                data = tunnedata_klusterit_ehp_maia)
kruskal_effsize(bmi ~ clusters_ward_e,
                data = tunnedata_klusterit_ehp_maia)

education_table <- matrix(c(2, 3, 8, 10, 6, 13, 28, 14, 8, 10, 14, 6), nrow = 3, ncol=4, byrow = T,
                          dimnames = list(c("Secondary", "High", "Uni"),
                                          c("Attentive", "Hypo-responsive", "Balanced", "Hypervigilant-distrustful")))
education_table
cramerV(education_table,
        y = NULL,
        ci = T,
        conf = 0.95)

Pain_efsize <- table(taustatiedot$Pain_now, taustatiedot$clusters_ward_faktori)
cramerV(Pain_efsize,
        y = NULL,
        ci = T,
        conf = 0.95)

Migraine_efsize <- table(taustatiedot$Migraine, taustatiedot$clusters_ward_faktori)
cramerV(Migraine_efsize,
        y = NULL,
        ci = T,
        conf = 0.95)

Headache_efsize <- table(taustatiedot$Headache, taustatiedot$clusters_ward_faktori)
cramerV(Headache_efsize,
        y = NULL,
        ci = T,
        conf = 0.95)

Abdomen_efsize <- table(taustatiedot$Abdomen, taustatiedot$clusters_ward_faktori)
cramerV(Abdomen_efsize,
        y = NULL,
        ci = T,
        conf = 0.95)

Menstrual_efsize <- table(taustatiedot$Menstrual, taustatiedot$clusters_ward_faktori)
cramerV(Menstrual_efsize,
        y = NULL,
        ci = T,
        conf = 0.95)

Back_shoulder_efsize <- table(taustatiedot$Back_shoulder, taustatiedot$clusters_ward_faktori)
cramerV(Back_shoulder_efsize,
        y = NULL,
        ci = T,
        conf = 0.95)

Joint_limb_efsize <- table(taustatiedot$Joint_limb, taustatiedot$clusters_ward_faktori)
cramerV(Joint_limb_efsize,
        y = NULL,
        ci = T,
        conf = 0.95)

Analgesics_efsize <- table(taustatiedot$analgesics, taustatiedot$clusters_ward_faktori)
cramerV(Analgesics_efsize,
        y = NULL,
        ci = T,
        conf = 0.95)

Prescribed_efsize <- table(taustatiedot$prescribed, taustatiedot$clusters_ward_faktori)
cramerV(Prescribed_efsize,
        y = NULL,
        ci = T,
        conf = 0.95)



#childhood adverse events
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LKSum) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LKSum, fill = list(Frequency = 0)) %>%  # ensure all levels present
  pivot_wider(names_from = clusters_ward_faktori, values_from = Frequency)
#Q1
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK1_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK1_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

#Q2
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK2_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK2_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

#Q3
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK3_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK3_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

#Q4
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK4_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK4_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

#Q5
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK5_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK5_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

#Q6
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK6_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK6_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

#Q7
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK7_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK7_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

#Q8
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK8_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK8_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

#Q9
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK9_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK9_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

#Q10
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK10_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK10_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

#Q11
freq_table <- tunnedata_klusterit_ehp_maia %>%
  group_by(clusters_ward_faktori, LK11_uusi) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  complete(clusters_ward_faktori, LK11_uusi, fill = list(Frequency = 0)) %>%
  group_by(clusters_ward_faktori) %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

cols_for_table <- c("clusters_ward_faktori",
                    "LK1_uusi",
                    "LK2_uusi",
                    "LK3_uusi",
                    "LK4_uusi",
                    "LK5_uusi",
                    "LK6_uusi",
                    "LK7_uusi",
                    "LK8_uusi",
                    "LK9_uusi",
                    "LK10_uusi",
                    "LK11_uusi")

tableLK <- tunnedata_klusterit_ehp_maia %>% 
  select(all_of(cols_for_table)) %>% 
  tbl_summary(by = clusters_ward_faktori,
              type = all_categorical() ~ "categorical",
              missing_text = "Missing") %>% 
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% 
  add_n() %>% 
  add_overall() %>% 
  add_q(method = 'holm')
tableLK

#MAIA subscales table
cols_for_table <- c("clusters_ward_e",
                    "MAIA_noticing", "MAIA_not_worrying", "MAIA_attention_reg", 
                    "MAIA_emotional_awareness", "MAIA_self_reg", "MAIA_body_listening", 
                    "MAIA_trusting")

tunnedata_klusterit_ehp_maia %>% 
  select(cols_for_table) %>% 
  tbl_summary(by = clusters_ward_e,
              type= all_categorical() ~ "continuous",
              missing_text = "Missing") %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n()

#MAIA
options(scipen = 999)
leveneTest(MAIA_noticing ~ clusters_ward_faktori, data=tunnedata_klusterit_ehp_maia)
leveneTest(MAIA_not_worrying ~ clusters_ward_faktori, data=tunnedata_klusterit_ehp_maia)
leveneTest(MAIA_attention_reg ~ clusters_ward_faktori, data=tunnedata_klusterit_ehp_maia)
leveneTest(MAIA_emotional_awareness ~ clusters_ward_faktori, data=tunnedata_klusterit_ehp_maia)
leveneTest(MAIA_self_reg ~ clusters_ward_faktori, data=tunnedata_klusterit_ehp_maia)
leveneTest(MAIA_body_listening ~ clusters_ward_faktori, data=tunnedata_klusterit_ehp_maia)
leveneTest(MAIA_trusting ~ clusters_ward_faktori, data=tunnedata_klusterit_ehp_maia)

M1<-kruskal.test(MAIA_noticing ~ clusters_ward_e,
                 data = tunnedata_klusterit_ehp_maia)
M2<-kruskal.test(MAIA_not_worrying ~ clusters_ward_e,
                 data = tunnedata_klusterit_ehp_maia)
M3 <-kruskal.test(MAIA_attention_reg ~ clusters_ward_e,
                  data = tunnedata_klusterit_ehp_maia)
M4<-kruskal.test(MAIA_emotional_awareness ~ clusters_ward_e,
                 data = tunnedata_klusterit_ehp_maia)
M5<-kruskal.test(MAIA_self_reg ~ clusters_ward_e,
                 data = tunnedata_klusterit_ehp_maia)
M6<-kruskal.test(MAIA_body_listening ~ clusters_ward_e,
                 data = tunnedata_klusterit_ehp_maia)
M7<-kruskal.test(MAIA_trusting ~ clusters_ward_e,
                 data = tunnedata_klusterit_ehp_maia)

MP <- p.adjust(c(M1$p.value, M2$p.value, M3$p.value, M4$p.value, M5$p.value, M6$p.value, M7$p.value))
paste(sprintf("%.10f",MP), collapse=",")


#MAIA effect sizes
needed_colnames <- c("MAIA_noticing", "MAIA_not_worrying", "MAIA_attention_reg", "MAIA_emotional_awareness", 
                     "MAIA_self_reg", "MAIA_body_listening", "MAIA_trusting")

for (name in needed_colnames) {
  table <- kruskal_effsize(eval(as.name(name)) ~ clusters_ward_e, data = tunnedata_klusterit_ehp_maia)
  table[1,1] <- name
  print(table)
}

#MAIA pairwise comparisons and effect sizes
options(scipen = 999)
boxplot(MAIA_noticing ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Noticing",
        xlab="Cluster")
kruskal.test(MAIA_noticing ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)

PT = dunnTest(MAIA_noticing ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm") 
PT

boxplot(MAIA_not_worrying ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Not worrying",
        xlab="Cluster")

kruskal.test(MAIA_not_worrying ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)

PT = dunnTest(MAIA_not_worrying ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm") 
PT

boxplot(MAIA_attention_reg ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Attention regulation",
        xlab="Cluster")

kruskal.test(MAIA_attention_reg ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)
options(scipen = 999)
PT = dunnTest(MAIA_attention_reg ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm") 
PT

boxplot(MAIA_emotional_awareness ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Emotional awareness",
        xlab="Cluster")

kruskal.test(MAIA_emotional_awareness ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)
options(scipen = 999)
PT = dunnTest(MAIA_emotional_awareness ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm") 
PT

boxplot(MAIA_self_reg ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Self regulation",
        xlab="Cluster")

kruskal.test(MAIA_self_reg ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)
options(scipen = 999)
PT = dunnTest(MAIA_self_reg ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm") 
PT

boxplot(MAIA_body_listening ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Body listening",
        xlab="Cluster")

kruskal.test(MAIA_body_listening ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)
options(scipen = 999)
PT = dunnTest(MAIA_body_listening ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm") 
PT


boxplot(MAIA_trusting ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Trusting",
        xlab="Cluster")

kruskal.test(MAIA_trusting ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)
options(scipen = 999)
PT = dunnTest(MAIA_trusting ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm") 
PT

#MAIA pairwise effect sizes with CI
a <- combn(4, 2)
a <- data.frame(a)
a

attentive_hyporesponsive <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,1]),]
attentive_balanced <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,2]),]
attentive_hypervigilantdistrustful <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,3]),]
hyporesponsive_balanced <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,4]),]
hyporesponsive_hypervigilantdistrustful <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,5]),]
balanced_hypervigilantdistrustful <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,6]),]
#noticing
rank_biserial(
  attentive_hyporesponsive$MAIA_noticing,
  attentive_hyporesponsive$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$MAIA_noticing,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_hypervigilantdistrustful$MAIA_noticing,
  attentive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_balanced$MAIA_noticing,
  hyporesponsive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_hypervigilantdistrustful$MAIA_noticing,
  hyporesponsive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$MAIA_noticing,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#not worrying
rank_biserial(
  attentive_hyporesponsive$MAIA_not_worrying,
  attentive_hyporesponsive$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$MAIA_not_worrying,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_hypervigilantdistrustful$MAIA_not_worrying,
  attentive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_balanced$MAIA_not_worrying,
  hyporesponsive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_hypervigilantdistrustful$MAIA_not_worrying,
  hyporesponsive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$MAIA_not_worrying,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#attention regulation
rank_biserial(
  attentive_hyporesponsive$MAIA_attention_reg,
  attentive_hyporesponsive$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$MAIA_attention_reg,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_hypervigilantdistrustful$MAIA_attention_reg,
  attentive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_balanced$MAIA_attention_reg,
  hyporesponsive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_hypervigilantdistrustful$MAIA_attention_reg,
  hyporesponsive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$MAIA_attention_reg,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#emotional awareness
rank_biserial(
  attentive_hyporesponsive$MAIA_emotional_awareness,
  attentive_hyporesponsive$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$MAIA_emotional_awareness,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_hypervigilantdistrustful$MAIA_emotional_awareness,
  attentive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_balanced$MAIA_emotional_awareness,
  hyporesponsive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_hypervigilantdistrustful$MAIA_emotional_awareness,
  hyporesponsive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$MAIA_emotional_awareness,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#self-regulation
rank_biserial(
  attentive_hyporesponsive$MAIA_self_reg,
  attentive_hyporesponsive$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$MAIA_self_reg,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_hypervigilantdistrustful$MAIA_self_reg,
  attentive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_balanced$MAIA_self_reg,
  hyporesponsive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_hypervigilantdistrustful$MAIA_self_reg,
  hyporesponsive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$MAIA_self_reg,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#body listening
rank_biserial(
  attentive_hyporesponsive$MAIA_body_listening,
  attentive_hyporesponsive$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$MAIA_body_listening,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_hypervigilantdistrustful$MAIA_body_listening,
  attentive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_balanced$MAIA_body_listening,
  hyporesponsive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_hypervigilantdistrustful$MAIA_body_listening,
  hyporesponsive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$MAIA_body_listening,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#trusting
rank_biserial(
  attentive_hyporesponsive$MAIA_trusting,
  attentive_hyporesponsive$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$MAIA_trusting,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_hypervigilantdistrustful$MAIA_trusting,
  attentive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_balanced$MAIA_trusting,
  hyporesponsive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_hypervigilantdistrustful$MAIA_trusting,
  hyporesponsive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$MAIA_trusting,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)


#MAIA subscales plot
cols_for_plot <- c("MAIA_noticing", "MAIA_not_worrying", "MAIA_attention_reg", 
                   "MAIA_emotional_awareness", "MAIA_self_reg", "MAIA_body_listening", 
                   "MAIA_trusting")

MAIA_long <- tunnedata_klusterit_ehp_maia %>% 
  pivot_longer(all_of(cols_for_plot),
               names_to = 'dimension', values_to='score') %>% 
  select(c(dimension, score, clusters_ward_faktori)) %>% 
  mutate(dimension=factor(dimension, 
                          levels=c("MAIA_noticing", "MAIA_not_worrying", "MAIA_attention_reg", 
                                   "MAIA_emotional_awareness", "MAIA_self_reg", "MAIA_body_listening", 
                                   "MAIA_trusting"),
                          labels=c("Noticing", "Not-worrying", "Attention regulation", "Emotional awareness", "Self-regulation", "Body listening", "Trusting")))

ggplot(data=MAIA_long, aes(x=dimension, y=score, fill=factor(clusters_ward_faktori))) +
  geom_boxplot(position = position_dodge(width = 0.7), notch=TRUE)+
  labs(x="", y = "Subscale score", fill="Group") +
  theme_minimal() +
  scale_fill_discrete(labels = c("Attentive", "Hypo-responsive", "Balanced", "Hypervigilant-distrustful")) +
  theme(text = element_text(size=18),
        axis.text.x = element_text(size = rel(1.0),angle=-30,hjust=0))



#psychological factors
cols_for_table <- c("clusters_ward_e",
                    "EHP_pain", "EHP_control", "EHP_emotional", "EHP_social", "EHP_self",
                    "RS14_Summa", "LKSum", "hads_a_summa", "hads_d_summa",
                    "PCS_rumination", "PCS_magnification", "PCS_helplessness", "PCS_summa")

#table for EHP-30
tbl_EHP_cluster <-
  tunnedata_klusterit_ehp_maia %>%
  tbl_summary(
    include = c(clusters_ward_e, starts_with("EHP")),    
    type = all_categorical() ~ "categorical",
    by= clusters_ward_e,
    missing='no') %>% 
  add_n() %>% 
  add_overall() %>% 
  add_p() %>% 
  add_q(method = 'holm')
tbl_EHP_cluster

#table for other psychological factors
tbl_by_cluster <- tunnedata_klusterit_ehp_maia %>%
  tbl_summary(
    include = c(clusters_ward_e,
                RS14_Summa, LKSum,
                starts_with("hads"),  starts_with("pcs")),    
    type = all_categorical() ~ "categorical",
    by= clusters_ward_e,
    missing='no') %>% 
  add_n() %>% 
  add_overall() %>% 
  add_p() %>% 
  add_q(method = 'holm')
tbl_by_cluster

#corrected p-values for all psychological factors
E1 <- kruskal.test(EHP_pain ~ clusters_ward_e,
                   data = tunnedata_klusterit_ehp_maia)
E2 <- kruskal.test(EHP_control ~ clusters_ward_e,
                   data = tunnedata_klusterit_ehp_maia)
E3 <- kruskal.test(EHP_emotional ~ clusters_ward_e,
                   data = tunnedata_klusterit_ehp_maia)
E4 <- kruskal.test(EHP_social ~ clusters_ward_e,
                   data = tunnedata_klusterit_ehp_maia)
E5 <- kruskal.test(EHP_self ~ clusters_ward_e,
                   data = tunnedata_klusterit_ehp_maia)
R <- kruskal.test(RS14_Summa ~ clusters_ward_e,
                  data = tunnedata_klusterit_ehp_maia)
H1 <- kruskal.test(hads_a_summa ~ clusters_ward_e,
                   data = tunnedata_klusterit_ehp_maia)
H2 <- kruskal.test(hads_d_summa ~ clusters_ward_e,
                   data = tunnedata_klusterit_ehp_maia)
LK <- kruskal.test(LKSum ~ clusters_ward_e,
                   data = tunnedata_klusterit_ehp_maia)
PCS <- kruskal.test(PCS_summa ~ clusters_ward_e,
                    data = tunnedata_klusterit_ehp_maia)
PCS_rum <- kruskal.test(PCS_rumination ~ clusters_ward_e,
                        data = tunnedata_klusterit_ehp_maia)
PCS_mag <- kruskal.test(PCS_magnification ~ clusters_ward_e,
                        data = tunnedata_klusterit_ehp_maia)
PCS_hel <- kruskal.test(PCS_helplessness ~ clusters_ward_e,
                        data = tunnedata_klusterit_ehp_maia)

KW_P <- p.adjust(c(E1$p.value, E2$p.value, E3$p.value, E4$p.value, E5$p.value, R$p.value, H1$p.value, H2$p.value, LK$p.value, PCS$p.value, PCS_rum$p.value, PCS_mag$p.value, PCS_hel$p.value))
paste(sprintf("%.10f",KW_P), collapse=",")

#EHP-30 spider plot
clusters_EHP <- tunnedata_klusterit_ehp_maia %>% dplyr::select("clusters_ward_e", "EHP_pain", "EHP_control", "EHP_emotional", "EHP_social", "EHP_self")

clusters_EHP_medians <- clusters_EHP %>%
  group_by(clusters_ward_e) %>%
  summarise(Median_pain=median(EHP_pain), Median_control=median(EHP_control), 
            Median_emotional=median(EHP_emotional), Median_social=median(EHP_social), 
            Median_self=median(EHP_self)) %>% 
  as_tibble()

glabels <- c("Pain", "Control", "Emotional wellbeing", "Social support", "Self-image")
gcols <- c("#f8766d", "#7dac06", "#0cb9be", "#c77bff")
ggradar(clusters_EHP_medians,
        values.radar = c(0, 40, 80),
        axis.labels = glabels,
        grid.min = 0,
        grid.mid = 40,
        grid.max = 80,
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        group.colours = gcols)

#effect sizes
#EHP effect size
needed_colnames <- c("EHP_pain", "EHP_control", "EHP_emotional", "EHP_social", "EHP_self")

for (name in needed_colnames) {
  table <- kruskal_effsize(eval(as.name(name)) ~ clusters_ward_e, data = tunnedata_klusterit_ehp_maia)
  table[1,1] <- name
  print(table)
}

#EHP pairwise comparisons 
boxplot(EHP_pain ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Pain",
        xlab="Cluster")

kruskal.test(EHP_pain ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)

PT = dunnTest(EHP_pain ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm")    # Can adjust p-values;

PT

boxplot(EHP_control ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Control",
        xlab="Cluster")

kruskal.test(EHP_control ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)

PT = dunnTest(EHP_control ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm")    # Can adjust p-values;

PT

boxplot(EHP_emotional ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Emotional",
        xlab="Cluster")

kruskal.test(EHP_emotional ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)

PT = dunnTest(EHP_emotional ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm")    # Can adjust p-values;

PT

boxplot(EHP_social ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Social",
        xlab="Cluster")

kruskal.test(EHP_social ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)

PT = dunnTest(EHP_social ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm")    # Can adjust p-values;

PT

boxplot(EHP_self ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Self",
        xlab="Cluster")

kruskal.test(EHP_self ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)

PT = dunnTest(EHP_self ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm")    # Can adjust p-values;

PT

#EHP significant pairwise effect sizes with CI
a <- combn(4, 2)
a <- data.frame(a)
a

attentive_hyporesponsive <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,1]),]
attentive_balanced <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,2]),]
attentive_hypervigilantdistrustful <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,3]),]
hyporesponsive_balanced <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,4]),]
hyporesponsive_hypervigilantdistrustful <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,5]),]
balanced_hypervigilantdistrustful <- tunnedata_klusterit_ehp_maia[which(tunnedata_klusterit_ehp_maia$clusters_ward_e %in% a[,6]),]

rank_biserial(
  balanced_hypervigilantdistrustful$EHP_pain,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$EHP_control,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_hypervigilantdistrustful$EHP_control,
  hyporesponsive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$EHP_emotional,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$EHP_emotional,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_hypervigilantdistrustful$EHP_emotional,
  hyporesponsive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$EHP_social,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_balanced$EHP_social,
  hyporesponsive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$EHP_social,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$EHP_self,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$EHP_self,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_hypervigilantdistrustful$EHP_self,
  hyporesponsive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#RESILIENCE
resilienssi_data <- tunnedata_klusterit_ehp_maia %>% drop_na(c("RS14_Summa"))
boxplot(RS14_Summa ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="Resilience",
        xlab="Cluster")
kruskal.test(RS14_Summa ~ clusters_ward_e,
             data = resilienssi_data)
kruskal_effsize(RS14_Summa ~ clusters_ward_e,
                data = resilienssi_data)

#Resilience pairwise comparisons
options(scipen=999)
PT = dunnTest(RS14_Summa ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm")    # Can adjust p-values;
PT

rank_biserial(
  attentive_hyporesponsive$RS14_Summa,
  attentive_hyporesponsive$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_balanced$RS14_Summa,
  hyporesponsive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$RS14_Summa,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_hypervigilantdistrustful$RS14_Summa,
  attentive_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#childhood adversities
boxplot(LKSum ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="LKSum",
        xlab="Cluster")
kruskal.test(LKSum ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)
paste(sprintf("%.10f", 3.915e-05), collapse=",")

kruskal_effsize(LKSum ~ clusters_ward_e,
                data = tunnedata_klusterit_ehp_maia)

#childhood pairwise comparisons
options(scipen=999)
PT = dunnTest(LKSum ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm")    # Can adjust p-values;
PT

#childhood significant pairwise effect sizes with CI
rank_biserial(
  balanced_hypervigilantdistrustful$LKSum,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#Pain catastrophising
boxplot(PCS_summa ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="PCS",
        xlab="Cluster")
kruskal.test(PCS_summa ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)
paste(sprintf("%.10f", 3.915e-05), collapse=",")

kruskal_effsize(PCS_summa ~ clusters_ward_e,
                data = tunnedata_klusterit_ehp_maia)
kruskal_effsize(PCS_rumination ~ clusters_ward_e,
                data = tunnedata_klusterit_ehp_maia)
kruskal_effsize(PCS_magnification ~ clusters_ward_e,
                data = tunnedata_klusterit_ehp_maia)
kruskal_effsize(PCS_helplessness ~ clusters_ward_e,
                data = tunnedata_klusterit_ehp_maia)

#pairwise comparisons
PT = dunnTest(PCS_summa ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm")    # Can adjust p-values;
PT

#significant pairwise effect sizes
rank_biserial(
  balanced_hypervigilantdistrustful$PCS_summa,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  attentive_balanced$PCS_summa,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#anxiety
boxplot(hads_a_summa ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="HADS_a",
        xlab="Cluster")
kruskal.test(hads_a_summa ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)
paste(sprintf("%.10f", 1.835e-06), collapse=",")

kruskal_effsize(hads_a_summa ~ clusters_ward_e,
                data = tunnedata_klusterit_ehp_maia)

#anxiety pairwise comparisons
PT = dunnTest(hads_a_summa ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm")    # Can adjust p-values;
PT

#anxiety significant pairwise effect sizes
rank_biserial(
  attentive_balanced$hads_a_summa,
  attentive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  balanced_hypervigilantdistrustful$hads_a_summa,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

#depression
boxplot(hads_d_summa ~ clusters_ward_e,
        data = tunnedata_klusterit_ehp_maia,
        ylab="HADS_d",
        xlab="Cluster")

kruskal.test(hads_d_summa ~ clusters_ward_e,
             data = tunnedata_klusterit_ehp_maia)

kruskal_effsize(hads_d_summa ~ clusters_ward_e,
                data = tunnedata_klusterit_ehp_maia)

#depression pairwise comparisons
PT = dunnTest(hads_d_summa ~ clusters_ward_faktori,
              data=tunnedata_klusterit_ehp_maia,
              method="holm")    # Can adjust p-values;
PT

#depression significant pairwise effect sizes
rank_biserial(
  balanced_hypervigilantdistrustful$hads_d_summa,
  balanced_hypervigilantdistrustful$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)

rank_biserial(
  hyporesponsive_balanced$hads_d_summa,
  hyporesponsive_balanced$clusters_ward_faktori,
  mu = 0,
  ci = 0.95,
  iterations = 200,
  paired = FALSE,
  verbose = TRUE)


#comparing MAIA subscale means in endometriosis and musculosceletal pain patients
#MAIA keskiarvojen vertailu Oliveira
options(scipen = 999)
m1<-t.test(tunnedata_klusterit_ehp_maia$MAIA_not_distracting,
           mu = 1.41)
m2<-t.test(tunnedata_klusterit_ehp_maia$MAIA_not_worrying,
           mu = 2.93)
m3<-t.test(tunnedata_klusterit_ehp_maia$MAIA_attention_reg,
           mu = 3.06)
m4<-t.test(tunnedata_klusterit_ehp_maia$MAIA_emotional_awareness,
           mu = 4.04)
m5<-t.test(tunnedata_klusterit_ehp_maia$MAIA_self_reg,
           mu = 2.35)
m6<-t.test(tunnedata_klusterit_ehp_maia$MAIA_body_listening,
           mu = 2.97)
m7<-t.test(tunnedata_klusterit_ehp_maia$MAIA_trusting,
           mu = 2.78)
p_maia <- p.adjust(c(m1$p.value, m2$p.value, m3$p.value, m4$p.value, m5$p.value, m6$p.value, m7$p.value))
paste(sprintf("%.10f",p_maia), collapse=",")
