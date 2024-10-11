library(data.table)
library(dplyr)
library(tidyverse)
library(jmvReadWrite)
library(haven)

taula = read_omv('~/idiap/data/BBDD_sentinella/bbdd_final_sentinella_12072024.omv')
setDT(taula)
taula = taula[!is.na(CONSENT)]
taula$`Filter 1`<-NULL
taula$`F1 (2)`<-NULL
taula = taula %>% mutate(Midtgaard14 = ifelse(!is.na(Midtgaard14), 'si', 'no')) %>% 
  mutate(Midtgaard14_F = ifelse(!is.na(Midtgaard14_F), 'si', 'no'))
taula$`AUDIT_total_S (2)`<-NULL
taula$AUDIT_total_2<-NULL
taula = taula %>% mutate(AUDIT_cat = ifelse(SEX=='Femenino' & AUDIT_total>=4 | SEX=='Masculino' & AUDIT_total>=5, 'risc', ifelse(is.na(SEX) | is.na(AUDIT_total), NA, 'no risc'))) %>% 
  mutate(AUDIT_cat_F = ifelse(SEX=='Femenino' & AUDIT_total_F>=4 | SEX=='Masculino' & AUDIT_total_F>=5, 'risc', ifelse(is.na(SEX) | is.na(AUDIT_total_F), NA, 'no risc')))

taula[CODE == "667TOO", SMBM7 := 2]

taula = taula %>% mutate(PHYSICAL_FATIGUE = ifelse(!is.na(SMBM1) & !is.na(SMBM2) & !is.na(SMBM3) & !is.na(SMBM4) & !is.na(SMBM5) & !is.na(SMBM6),
                                                    round((as.numeric(SMBM1) + as.numeric(SMBM2) + as.numeric(SMBM3) + as.numeric(SMBM4) + as.numeric(SMBM5) + as.numeric(SMBM6))/6, 2),
                                                    NA)) %>% 
  mutate(EMOTIONAL_EXAHUSTION = ifelse(!is.na(SMBM12) & !is.na(SMBM13) & !is.na(SMBM14),
                                   round((as.numeric(SMBM12) + as.numeric(SMBM13) + as.numeric(SMBM14))/3, 2),
                                   NA)) %>% 
  mutate(COGNITIVE_FATIGUE = ifelse(!is.na(SMBM7) & !is.na(SMBM8) & !is.na(SMBM9) & !is.na(SMBM10) & !is.na(SMBM11),
                                   round((as.numeric(SMBM7) + as.numeric(SMBM8) + as.numeric(SMBM9) + as.numeric(SMBM10) + as.numeric(SMBM11))/5, 2),
                                   NA)) %>% 
  mutate(PHYSICAL_FATIGUE_F = ifelse(!is.na(SMBM1_F) & !is.na(SMBM2_F) & !is.na(SMBM3_F) & !is.na(SMBM4_F) & !is.na(SMBM5_F) & !is.na(SMBM6_F),
                                   round((as.numeric(SMBM1_F) + as.numeric(SMBM2_F) + as.numeric(SMBM3_F) + as.numeric(SMBM4_F) + as.numeric(SMBM5_F) + as.numeric(SMBM6_F))/6, 2),
                                   NA)) %>% 
  mutate(EMOTIONAL_EXAHUSTION_S = ifelse(!is.na(SMBM12_F) & !is.na(SMBM13_F) & !is.na(SMBM14_F),
                                       round((as.numeric(SMBM12_F) + as.numeric(SMBM13_F) + as.numeric(SMBM14_F))/3, 2),
                                       NA)) %>% 
  mutate(COGNITIVE_FATIGUE_F = ifelse(!is.na(SMBM7_F) & !is.na(SMBM8_F) & !is.na(SMBM9_F) & !is.na(SMBM10_F) & !is.na(SMBM11_F),
                                    round((as.numeric(SMBM7_F) + as.numeric(SMBM8_F) + as.numeric(SMBM9_F) + as.numeric(SMBM10_F) + as.numeric(SMBM11_F))/5, 2),
                                    NA))
taula$`COGNITIVE FATIGUE`<-NULL

taula[, paste0("Midtgaard", 1:14)] <- lapply(taula[, paste0("Midtgaard", 1:14)], function(x) ifelse(x == "Sí", 1, 0))
taula$Midtgaard_total <- rowSums(taula[, paste0("Midtgaard", 1:14)])

taula[, paste0("Midtgaard", 1:14, '_F')] <- lapply(taula[, paste0("Midtgaard", 1:14, '_F')], function(x) ifelse(x == "Sí", 1, 0))
taula$Midtgaard_total_F <- rowSums(taula[, paste0("Midtgaard", 1:14, '_F')])

taula = rename(taula, GAD_score_F = GAd_score_F)

taula = taula %>% mutate(SMBM_mean = ifelse(!is.na(SMBM1) & !is.na(SMBM2) & !is.na(SMBM3) & !is.na(SMBM4) & !is.na(SMBM5) & !is.na(SMBM6) & !is.na(SMBM7_F) & !is.na(SMBM8_F) & 
                                              !is.na(SMBM9_F) & !is.na(SMBM10_F) & !is.na(SMBM11_F) & !is.na(SMBM12) & !is.na(SMBM13) & !is.na(SMBM14),
                                            round((as.numeric(SMBM1) + as.numeric(SMBM2) + as.numeric(SMBM3) + as.numeric(SMBM4) + as.numeric(SMBM5) + as.numeric(SMBM6) +
                                                     as.numeric(SMBM7_F) + as.numeric(SMBM8_F) + as.numeric(SMBM9_F) + as.numeric(SMBM10_F) + as.numeric(SMBM11_F)+
                                                     as.numeric(SMBM12) + as.numeric(SMBM13) + as.numeric(SMBM14))/14,2 ),NA))

save(taula, file = '~/idiap/projects/TFG_sentinella/dades.RData')
