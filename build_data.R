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

taula = taula %>% mutate(SMBM_mean = ifelse(!is.na(SMBM1) & !is.na(SMBM2) & !is.na(SMBM3) & !is.na(SMBM4) & !is.na(SMBM5) & !is.na(SMBM6) & !is.na(SMBM7) & !is.na(SMBM8) & 
                                              !is.na(SMBM9) & !is.na(SMBM10) & !is.na(SMBM11) & !is.na(SMBM12) & !is.na(SMBM13) & !is.na(SMBM14),
                                            round((as.numeric(SMBM1) + as.numeric(SMBM2) + as.numeric(SMBM3) + as.numeric(SMBM4) + as.numeric(SMBM5) + as.numeric(SMBM6) +
                                                     as.numeric(SMBM7) + as.numeric(SMBM8) + as.numeric(SMBM9) + as.numeric(SMBM10) + as.numeric(SMBM11)+
                                                     as.numeric(SMBM12) + as.numeric(SMBM13) + as.numeric(SMBM14))/14,2 ),NA))

taula = taula %>% mutate(SMBM_mean_F = ifelse(!is.na(SMBM1_F) & !is.na(SMBM2_F) & !is.na(SMBM3_F) & !is.na(SMBM4_F) & !is.na(SMBM5_F) & !is.na(SMBM6_F) & !is.na(SMBM7_F) & !is.na(SMBM8_F) & 
                                              !is.na(SMBM9_F) & !is.na(SMBM10_F) & !is.na(SMBM11_F) & !is.na(SMBM12_F) & !is.na(SMBM13_F) & !is.na(SMBM14_F),
                                            round((as.numeric(SMBM1_F) + as.numeric(SMBM2_F) + as.numeric(SMBM3_F) + as.numeric(SMBM4_F) + as.numeric(SMBM5_F) + as.numeric(SMBM6_F) +
                                                     as.numeric(SMBM7_F) + as.numeric(SMBM8_F) + as.numeric(SMBM9_F) + as.numeric(SMBM10_F) + as.numeric(SMBM11_F)+
                                                     as.numeric(SMBM12_F) + as.numeric(SMBM13_F) + as.numeric(SMBM14_F))/14,2 ),NA))


taula = taula %>% mutate(MENTAL_HEALTH_DIAGNOSIS=ifelse(`MENTAL HEALTH DIAGNOSIS`=='Si', 1, ifelse(`MENTAL HEALTH DIAGNOSIS`=='No', 0, NA)))
taula = taula %>% mutate(MENTAL_HEALTH_DIAGNOSIS_F=ifelse(`MENTAL HEALTH DIAGNOSIS_F`=='Si', 1, ifelse(`MENTAL HEALTH DIAGNOSIS_F`=='No', 0, NA)))
taula = taula %>% mutate(MENTAL_HEALTH_TREATMENT=ifelse(`MENTAL HEALTH TREATMENT`=='Sí', 1, ifelse(`MENTAL HEALTH TREATMENT`=='No', 0, NA)))
taula = taula %>% mutate(MENTAL_HEALTH_TREATMENT_F=ifelse(`MENTAL HEALTH TREATMENT_F`=='Sí', 1, ifelse(`MENTAL HEALTH TREATMENT_F`=='No', 0, NA)))
taula$DAST1 = as.numeric(taula$DAST1)
taula$DAST1_F = as.numeric(taula$DAST1_F)
taula = taula %>% mutate(PHQ_recoded = ifelse(PHQ_score>=8, 'Depression', 'No depression'))
taula = taula %>% mutate(PHQ_recoded_s = ifelse(PHQ_score_S>=8, 'Depression', 'No depression'))
taula = taula %>% mutate(GAD_recoded = ifelse(GAD_score>=10, 'Anxiety', 'No Anxiety'))
taula = taula %>% mutate(GAD_recoded_F = ifelse(GAD_score_F>=10, 'Anxiety', 'No Anxiety'))
taula = taula %>% mutate(SMBM_recorded = ifelse(SMBM_mean>=4.4, 'burnout', 'No burnout'))
taula = taula %>% mutate(SMBM_recorded_F = ifelse(SMBM_mean_F>=4.4, 'burnout', 'No burnout'))

# EMPLOYMENT
taula = taula %>% mutate(EMPLOYMENT = ifelse(EMPLOYMENT=='Trabajando a jornada completa' | EMPLOYMENT=='Trabajando a media jornada', 'empleo', 'sin empleo'))
# Midtgaard
taula[, midtgaard_change := fifelse(Midtgaard_total == Midtgaard_total_F, 0, 
                                    fifelse(Midtgaard_total < Midtgaard_total_F, 1, -1))]
taula[, midtgaard_change := factor(midtgaard_change, levels = c(0, -1, 1))]
# AUDIT
taula[, audit_change := fifelse(AUDIT_cat == AUDIT_cat_F, 'igual', 
                                fifelse(AUDIT_cat=='risc' & AUDIT_cat_F=='no risc', 'millor', 'pitjor'))]
taula[, audit_change := factor(audit_change, levels = c('igual', 'pitjor', 'millor'))]
# OSS
taula = taula %>% mutate(oss_change = ifelse(OSS_total_categ == OSS_total_categ_F, 'igual',
                                             ifelse(OSS_total_categ=='Poor social suport' & OSS_total_categ_F=='Moderate social support' | 
                                                      OSS_total_categ=='Poor social suport' & OSS_total_categ_F=='Strong social support' |
                                                      OSS_total_categ=='Moderate social support' & OSS_total_categ_F=='Strong social support' , 'millor', 'pitjor')))
taula[, oss_change := factor(oss_change, levels = c('igual', 'pitjor', 'millor'))]
# DAST1
taula[, DAST1_change := fifelse(DAST1 == DAST1_F, 0, 
                                fifelse(DAST1 < DAST1_F, 1, -1))]
taula[, DAST1_change := factor(DAST1_change, levels = c(0, -1, 1))]
# PHQ
taula[, PHQ_change := fifelse(PHQ_recoded == PHQ_recoded_s, 'igual', 
                              fifelse(PHQ_recoded=='Depression' & PHQ_recoded_s=='No depression', 'millor', 'pitjor'))]
taula[, PHQ_change := factor(PHQ_change, levels = c('igual', 'pitjor', 'millor'))]
# GAD
taula[, GAD_change := fifelse(GAD_recoded == GAD_recoded_F, 'igual', 
                              fifelse(GAD_recoded=='Anxiety' & GAD_recoded_F=='No Anxiety', 'millor', 'pitjor'))]
taula[, GAD_change := factor(GAD_change, levels = c('igual', 'pitjor', 'millor'))]
# SMBM
taula[, SMBM_change := fifelse(SMBM_recorded == SMBM_recorded_F, 'igual', 
                               fifelse(SMBM_recorded=='burnout' & SMBM_recorded_F=='No burnout', 'millor', 'pitjor'))]
taula[, SMBM_change := factor(SMBM_change, levels = c('igual', 'pitjor', 'millor'))]
# Mental health diagnosis
taula[, MENTAL_HEALTH_DIAGNOSIS_change := fifelse(MENTAL_HEALTH_DIAGNOSIS == MENTAL_HEALTH_DIAGNOSIS_F, 'igual', 
                                fifelse(MENTAL_HEALTH_DIAGNOSIS < MENTAL_HEALTH_DIAGNOSIS_F, 'pitjor', 'millor'))]
taula[, MENTAL_HEALTH_DIAGNOSIS_change := factor(MENTAL_HEALTH_DIAGNOSIS_change, levels = c('igual', 'pitjor', 'millor'))]
# Mental health treatment
taula[, MENTAL_HEALTH_TREATMENT_change := fifelse(MENTAL_HEALTH_TREATMENT == MENTAL_HEALTH_TREATMENT_F, 'igual', 
                                                  fifelse(MENTAL_HEALTH_TREATMENT < MENTAL_HEALTH_TREATMENT_F, 'pitjor', 'millor'))]
taula[, MENTAL_HEALTH_TREATMENT_change := factor(MENTAL_HEALTH_TREATMENT_change, levels = c('igual', 'pitjor', 'millor'))]

taula$MENTAL_HEALTH_FAMILY_HISTORY<-taula$`MENTAL HEALTH FAMILY HISTORY`
save(taula, file = '~/idiap/projects/TFG_sentinella/dades.RData')
