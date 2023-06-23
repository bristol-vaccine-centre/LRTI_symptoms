pacman::p_load(readr,
               lubridate,
               janitor,
               zoo,
               gtsummary,
               huxtable,
               gridExtra,
               tidyverse,
               tidylo,
               tidytuesdayR,
               tidytext,
               ggrepel,
               tidymodels,
               embed,
               here,
               rio,
               expss,
               conflicted
              )

tidymodels_prefer()
conflict_prefer("mutate", "dplyr")

## load data, clean it
  # there are field mismatches between the datasets
  # select only the fields we need and check for mismatches afterwards


data1 <- import(here::here("scripts", "AvonCAPLRTD_1.csv")) %>% 
  clean_names%>% 
  mutate(admission_date = make_date(year = year) + weeks(week_number))%>%
  #select(record_number,admission_date, year, week_number)
  select(record_number,admission_date, enrollment_date, gender, age_at_admission,
         covid19_vax, care_home, dementia_2, dementia_3, imd, rockwood,
         cci_total_score, cci_age_score, mi_score, chf_score, pvd_score, 
         neuro_score, dementia_score, copd_score, peptic_ulcer_score, ctd_score,
         liver_disease_score, diabetes_score, neuro_disease_score, ckd_score,
         cancer_score, leukemia_score, lymphoma_score, hiv_score, comorbidities_complete,
         qualifying_symptoms_signs,first_radio_2, 
         # Diagnosis
         starts_with("final_soc_lrtd_diagnosis"), covid_19_diagnosis,
         # Lab/ radiology tests
         starts_with("microtest_done"), starts_with("micro_test"), starts_with("micro_isolates"), starts_with("isolate_identified"),
         starts_with("viral_testing_performed"), starts_with ("test_type"), starts_with("virus_isolated"), starts_with("virus_pathogen"),
         starts_with("radio_exam"),
         # Symptoms
         general_det, confusion, worse_confusion, malaise, myalgia,
         fever2, fever, hypothermia, chills,
         cough2, sput_prod, ausc_find,
         dyspnoea, tachypnoea2,
         pleurtic_cp,
         complications_13, complications_14,
         consented, ppc, include_patient, withdrawal_aspect, withdrawal_aspect, withdrawal, duplicate,
         anosmia, ageusia, dysgeusia, oe_cap, oe_lrtd, headache, wheeze,
         # Severity
         crb_test_mai, curb65_4h)


data2 <- import(here::here("scripts", "AvonCAPLRTD_2.csv")) %>% 
  clean_names%>% 
  mutate(admission_date = make_datetime(year = year) + weeks(week_number))%>%
  #select(record_number,admission_date, year, week_number)
  select(record_number,admission_date, enrollment_date, gender, age_at_admission,
         covid19_vax, care_home, dementia_2, dementia_3, imd, rockwood,
         cci_total_score, cci_age_score, mi_score, chf_score, pvd_score, 
         neuro_score, dementia_score, copd_score, peptic_ulcer_score, ctd_score,
         liver_disease_score, diabetes_score, neuro_disease_score, ckd_score,
         cancer_score, leukemia_score, lymphoma_score, hiv_score, comorbidities_complete,
         qualifying_symptoms_signs,first_radio_2,
         # Diagnosis
         starts_with("final_soc_lrtd_diagnosis"), covid_19_diagnosis,
         # Lab/ radiology tests
         starts_with("microtest_done"), starts_with("micro_test"), starts_with("micro_isolates"), starts_with("isolate_identified"),
         starts_with("viral_testing_performed"), starts_with ("test_type"), starts_with("virus_isolated"), starts_with("virus_pathogen"),
         starts_with("radio_exam"),
         # Symptoms
         general_det, confusion, worse_confusion, malaise, myalgia,
         fever2, fever, hypothermia, chills,
         cough2, sput_prod, ausc_find,
         dyspnoea, tachypnoea2,
         pleurtic_cp,
         complications_13, complications_14,
         consented, ppc, include_patient, withdrawal_aspect, withdrawal_aspect, withdrawal, duplicate,
         anosmia, ageusia, dysgeusia, oe_cap, oe_lrtd, headache, wheeze,
         # Severity
         crb_test_mai, curb65_4h)

#compare_df_cols(data1,data2, return = "mismatch")

data1 <- data1%>%
  mutate(micro_isolates_13 = case_when(micro_isolates_13=="TRUE"~1,
                                       micro_isolates_13=="FALSE"~2,
                                       is.na(micro_isolates_13) ~ as.numeric(NA)))%>%
  mutate(micro_isolates_14 = case_when(micro_isolates_14=="TRUE"~1,
                                       micro_isolates_14=="FALSE"~2,
                                       is.na(micro_isolates_14) ~ as.numeric(NA)))%>%
  mutate(micro_isolates_15 = case_when(micro_isolates_15=="TRUE"~1,
                                       micro_isolates_15=="FALSE"~2,
                                       is.na(micro_isolates_15) ~ as.numeric(NA)))%>%
  mutate(micro_isolates_16 = case_when(micro_isolates_16=="TRUE"~1,
                                       micro_isolates_16=="FALSE"~2,
                                       is.na(micro_isolates_16) ~ as.numeric(NA)))%>%
  mutate(micro_isolates_17 = case_when(micro_isolates_17=="TRUE"~1,
                                       micro_isolates_17=="FALSE"~2,
                                       is.na(micro_isolates_17) ~ as.numeric(NA)))%>%
  mutate(micro_isolates_18 = case_when(micro_isolates_18=="TRUE"~1,
                                       micro_isolates_18=="FALSE"~2,
                                       is.na(micro_isolates_18) ~ as.numeric(NA)))%>%
  mutate(micro_isolates_19 = case_when(micro_isolates_19=="TRUE"~1,
                                       micro_isolates_19=="FALSE"~2,
                                       is.na(micro_isolates_19) ~ as.numeric(NA)))%>%
  mutate(microtest_done_14 = case_when(microtest_done_14=="TRUE"~1,
                                       microtest_done_14=="FALSE"~0,
                                       is.na(microtest_done_14) ~ as.numeric(NA)))%>%
  mutate(microtest_done_15 = case_when(microtest_done_15=="TRUE"~1,
                                       microtest_done_15=="FALSE"~0,
                                       is.na(microtest_done_15) ~ as.numeric(NA)))%>%
  mutate(microtest_done_16 = case_when(microtest_done_16=="TRUE"~1,
                                       microtest_done_16=="FALSE"~0,
                                       is.na(microtest_done_16) ~ as.numeric(NA)))%>%
  mutate(microtest_done_17 = case_when(microtest_done_17=="TRUE"~1,
                                       microtest_done_17=="FALSE"~0,
                                       is.na(microtest_done_17) ~ as.numeric(NA)))%>%
  mutate(microtest_done_18 = case_when(microtest_done_18=="TRUE"~1,
                                       microtest_done_18=="FALSE"~0,
                                       is.na(microtest_done_18) ~ as.numeric(NA)))%>%
  mutate(microtest_done_19 = case_when(microtest_done_19=="TRUE"~1,
                                       microtest_done_19=="FALSE"~0,
                                       is.na(microtest_done_19) ~ as.numeric(NA)))%>%
  mutate(viral_testing_performed_16 = case_when(viral_testing_performed_16=="TRUE"~1,
                                                viral_testing_performed_16=="FALSE"~0,
                                                is.na(viral_testing_performed_16) ~ as.numeric(NA)))%>%
  mutate(virus_isolated_15 = case_when(virus_isolated_15=="TRUE"~1,
                                       virus_isolated_15=="FALSE"~0,
                                       is.na(virus_isolated_15) ~ as.numeric(NA)))%>%
  mutate(withdrawal_aspect = case_when(withdrawal_aspect=="TRUE"~1,
                                       withdrawal_aspect=="FALSE"~0,
                                       is.na(withdrawal_aspect) ~ as.numeric(NA)))

data2 <- data2%>%
  mutate(radio_exam_16 = case_when(radio_exam_16=="TRUE"~1,
                                   radio_exam_16=="FALSE"~0,
                                   is.na(radio_exam_16) ~ as.numeric(NA)))%>%
  mutate(radio_exam_17 = case_when(radio_exam_17=="TRUE"~1,
                                   radio_exam_17=="FALSE"~0,
                                   is.na(radio_exam_17) ~ as.numeric(NA)))%>%
  mutate(radio_exam_18 = case_when(radio_exam_18=="TRUE"~1,
                                   radio_exam_18=="FALSE"~0,
                                   is.na(radio_exam_18) ~ as.numeric(NA)))%>%
  mutate(radio_exam_19 = case_when(radio_exam_19=="TRUE"~1,
                                   radio_exam_19=="FALSE"~0,
                                   is.na(radio_exam_19) ~ as.numeric(NA)))

data <- rbind(data1, data2) %>% 
  # mutate(consent_declined = case_when(consented==2 | ppc==2 | include_patient==0 | 
  #                                     withdrawal==1 ~ "yes",   
  #                                     TRUE ~ "no"))%>%
  mutate(consent_declined = case_when(consented==2 | ppc==2 | 
                                        withdrawal==1 ~ "yes",   
                                      TRUE ~ "no"))%>%
  mutate(duplicate_adm = case_when(duplicate==1~"yes", 
                                   TRUE ~ "no"))%>%
  mutate(age_at_admission = round(age_at_admission, digits = 0))%>%
  filter(age_at_admission < 120)%>%
  mutate(male_sex = as.logical(ifelse(gender==1, 1, 0)))%>%
  mutate(care_home = as.logical(case_when(care_home==1 ~ 1,
                                        care_home==0 | is.na(care_home) ~ 0)))%>%
  mutate(dementia= as.logical(case_when(dementia_2==1 | dementia_3==1 ~ 1,
                            (dementia_2==0 | is.na(dementia_2)) &
                              (dementia_3==0 | is.na(dementia_3)) ~ 0)))%>%
  mutate(imd=factor(imd, levels = c("0","1","2","3","4","5","6","7","8","9","10"))) %>%
  mutate(frail = as.logical(case_when(rockwood <5 ~ 0,
                           rockwood > 4 & rockwood < 10 ~ 1))) %>% 
  mutate(rockwood = case_when(rockwood < 3 ~ "Fit, well",
                                    rockwood ==3 | rockwood ==4 ~ "Managing, vulnerable",
                                    rockwood ==5 | rockwood ==6 ~ "Mild/ moderately frail",
                                    rockwood ==7 | rockwood ==8 ~ "Severely frail",
                                    rockwood ==9 ~ "End of life",
                                    rockwood ==10 ~ NA_character_)) %>% 
  mutate(rockwood = factor(rockwood, levels = c("Fit, well",
                                                "Managing, vulnerable", 
                                                "Mild/ moderately frail",
                                                "Severely frail",
                                                "End of life"))) %>% 
  mutate(cci_no.age = as.double(cci_total_score) - as.double(cci_age_score),
         cci_no.dementia = if_else(dementia_score == 1, 
                                   as.double(cci_no.age)-1, as.double(cci_no.age)),
         cci_no.dementia = case_when(cci_no.dementia==0 ~ "None",
                                     cci_no.dementia==1 | cci_no.dementia==2 ~ "Mild",
                                     cci_no.dementia==3 | cci_no.dementia==4 ~ "Moderate",
                                     cci_no.dementia>4 ~ "Severe") %>% 
           factor(levels = c("None","Mild","Moderate", "Severe")),
         severe_comorbid = as.logical(case_when(cci_no.dementia=="Severe" ~ 1,
                                        cci_no.dementia!="Severe" ~ 0))) %>% 
  mutate(cci_category = case_when(cci_total_score==0 ~ "None",
                                  cci_total_score==1 | cci_total_score==2 ~ "Mild",
                                  cci_total_score==3 | cci_total_score==4 ~ "Moderate",
                                  cci_total_score>4 ~ "Severe") %>% 
           factor(levels = c("None","Mild","Moderate", "Severe"))) %>% 
  mutate(cci_cat2 = case_when(cci_category== "None"| cci_category== "Mild" ~ "Low",
                             cci_category==  "Moderate" | cci_category== "Severe" ~ "High")%>%
           ordered(c("Low","High")))%>%
  mutate(cci_age_score = factor(cci_age_score, levels = c("0", "1", "2", "3", "4")),
         mi_score = factor(mi_score, levels = c("0", "1")),
         chf_score = factor(chf_score, levels = c("0", "1")),
         pvd_score = factor(pvd_score, levels =c("0", "1")),
         neuro_score = factor(neuro_score, levels = c("0", "1")),
         dementia_score = factor(dementia_score, levels = c("0", "1")),
         copd_score = factor(copd_score, levels = c("0", "1")),
         peptic_ulcer_score = factor(peptic_ulcer_score, levels = c("0", "1")),
         ctd_score = factor(ctd_score, levels = c("0", "1")),
         liver_disease_score = factor(liver_disease_score, levels = c("0", "1", "3")),
         diabetes_score = factor(diabetes_score, levels = c("0", "1", "2")),
         neuro_disease_score = factor(neuro_disease_score, levels = c("0", "2")),
         ckd_score = factor(ckd_score, levels = c("0", "2")),
         cancer_score = factor(cancer_score, levels = c("0", "2", "6")),
         leukemia_score = factor(leukemia_score, levels = c("0", "2")),
         lymphoma_score = factor(lymphoma_score, levels = c("0", "2")),
         hiv_score = factor(hiv_score, levels = c("0", "6"))) %>% 
  mutate(over_65 = as.logical(case_when(age_at_admission <65 ~ 0,
                               age_at_admission >= 65 ~ 1)),
         over_75 = as.logical(case_when(age_at_admission <75 ~ 0,
                                       age_at_admission >=75 ~ 1)),
         over_85 = as.logical(case_when(age_at_admission <85 ~ 0,
                                       age_at_admission >=85 ~ 1)),
         age_group = dplyr::case_when(
           age_at_admission > 17 & age_at_admission <= 24 ~ "18-24",
           age_at_admission > 24 & age_at_admission <= 34 ~ "25-34",
           age_at_admission > 34 & age_at_admission <= 44 ~ "35-44",
           age_at_admission > 44 & age_at_admission <= 54 ~ "45-54",
           age_at_admission > 54 & age_at_admission <= 64 ~ "55-64",
           age_at_admission > 64 & age_at_admission <= 74 ~ "65-74",
           age_at_admission > 74 & age_at_admission <= 84 ~ "75-84",
           age_at_admission > 84             ~ ">84"),
         age_group = factor(age_group,
                            level = c("18-24", "25-34", "35-44", "45-54",
                            "55-64", "65-74", "75-84", ">84"))) %>% 

## Symptoms: treat No and Unknown and NA's to category of "no evidence of symptom"
mutate(deterioration=case_when(general_det==1 ~ 1,
                               general_det==2 | general_det==3 | is.na(general_det) ~ 0))%>%
  mutate(confusion=case_when(confusion==1 | worse_confusion==1 ~ 1,
                             confusion==0 | is.na(confusion) |
                               worse_confusion==2 | worse_confusion==3 |
                               is.na(worse_confusion) ~ 0))%>%
  mutate(malaise=case_when(malaise==1 ~ 1,
                           malaise==2 | malaise==3 | is.na(malaise) ~ 0))%>%
  mutate(falls=case_when(complications_13==1 | complications_14==1 ~ 1,
                         (complications_13==0 | is.na(complications_13)) &     
                           (complications_14==0 | is.na(complications_14)) ~ 0))%>%
  mutate(fever=case_when(fever2==1 | fever==1 | hypothermia==1 | chills==1 ~ 1,
                         (fever2==0 |is.na(fever2)) & 
                           (fever==2 | is.na(fever) | fever==3) & 
                           (hypothermia==2 |is.na(hypothermia) | hypothermia==3) & 
                           (chills==2 | is.na(chills) | chills==3) ~ 0))%>%
  mutate(cough=case_when(cough2==1 ~ 1,
                         cough2==0 | is.na(cough2) ~ 0))%>%
  mutate(sputum=case_when(sput_prod==1 ~ 1,
                          sput_prod==0 | is.na(sput_prod) ~ 0))%>%
  mutate(dyspnoea=case_when(dyspnoea==1 ~ 1, dyspnoea==0 | is.na(dyspnoea) ~ 0)) %>% 
  mutate(chest_sign=case_when(ausc_find==1 | tachypnoea2==1 | oe_cap==1 | oe_lrtd==1 ~ 1, 
                              (ausc_find==0 | is.na(ausc_find)) & 
                                (tachypnoea2==0 | is.na(tachypnoea2)) &
                                (oe_cap==2 | oe_cap==3 | is.na(oe_cap)) &
                                (oe_lrtd==2 | oe_lrtd==3 | is.na(oe_lrtd)) ~ 0))%>%
  mutate(pleurisy=case_when(pleurtic_cp==1 ~ 1,
                            pleurtic_cp==0 | is.na(pleurtic_cp) ~ 0))%>%
  mutate(anosmia=case_when(anosmia==1 ~ 1,
                           anosmia==2 | anosmia==3 | is.na(anosmia) ~ 0))%>%
  mutate(ageusia_dysgeusia=case_when((ageusia==1 | dysgeusia==1) ~ 1,
                           (ageusia==2 | ageusia==3 | is.na(ageusia) |
                              dysgeusia==2 | dysgeusia==3 | is.na(dysgeusia)) ~ 0)) %>% 
  mutate(myalgia=case_when(myalgia==1 ~ 1,
                            myalgia==2 | myalgia==3 | is.na(myalgia) ~ 0))%>%
  mutate(headache=case_when(headache==1 ~ 1,
                            headache==2 | headache==3 | is.na(headache) ~ 0))%>%
  mutate(wheeze=case_when(wheeze==1 ~ 1,
                          wheeze==2 | wheeze==3 | is.na(wheeze) ~ 0))%>%
  mutate(symptom_category = case_when(
    deterioration==1 | confusion==1 | worse_confusion==1 | malaise==1 | falls==1 ~ "non-specific",
    fever==1 | cough==1 | sputum==1 | dyspnoea==1 | wheeze==1 | pleurisy==1 ~ "specific",
    anosmia==1 | ageusia_dysgeusia==1 | headache==1 ~ "other")
    %>% ordered(c("specific","non-specific", "other")))%>%
  mutate(n_covid.sympts = select(., c("fever", "cough", "dyspnoea", 
                                      "pleurisy", "sputum",
           "myalgia","headache", "deterioration", "malaise")) %>% rowSums(),
         n_lrti.sympts = select(., c("fever", "cough", "dyspnoea", "wheeze",
                                     "pleurisy", "sputum", "myalgia", "headache",
                                    "deterioration")) %>% rowSums()) %>% 
  mutate(case_def = as.logical(case_when((fever==1 & cough==1) |
                                                 n_lrti.sympts>=3 ~1,
                                               (fever==0 | cough==0) &
                                                 n_lrti.sympts<3 ~ 0)),
         case_def2 = as.logical(case_when(n_lrti.sympts>=1 ~1,
                                         (fever==0 | cough==0) &
                                           n_lrti.sympts==0 ~ 0)),
         case_def_covid = as.logical(case_when((fever==1 & cough==1) |
                                      n_covid.sympts>=3 |
                                      anosmia==1 | ageusia_dysgeusia==1 ~1,
                                    (fever==0 | cough==0) &
                                      n_covid.sympts<3 &
                                      anosmia==0 & ageusia_dysgeusia==0 ~ 0))) %>% 
  mutate(pneumonia_diagnosis = as.logical(ifelse(
    final_soc_lrtd_diagnosis_1==1 |
      final_soc_lrtd_diagnosis_2==1 |
      final_soc_lrtd_diagnosis_3==1 |
      final_soc_lrtd_diagnosis_5==1 |
      (!is.na(first_radio_2) & first_radio_2==1),
    1,0)))%>%
  mutate(NP_LRTI_diagnosis = ifelse(
    pneumonia_diagnosis=="no" &
      final_soc_lrtd_diagnosis_4==1,
    "yes","no") %>% factor(levels = c("no","yes")))%>%
  mutate(exacerbation_ChrRD_diagnosis = ifelse(
    final_soc_lrtd_diagnosis_6==1 |
      final_soc_lrtd_diagnosis_7==1 , 
    "yes","no") %>% factor(levels = c("no","yes")))%>%
  mutate(heart_failure_diagnosis = ifelse(
    final_soc_lrtd_diagnosis_8==1, 
    "yes","no") %>% factor(levels = c("no","yes")))%>%
  mutate(non_infectious_diagnosis= ifelse(
    final_soc_lrtd_diagnosis_9==1, 
    "yes","no") %>% factor(levels = c("no","yes")))%>%
  mutate(non_LRTD_diagnosis= ifelse(
    final_soc_lrtd_diagnosis_10==1, 
    "yes","no") %>% factor(levels = c("no","yes")))%>%
  
  mutate(diagnosis_category = case_when(
    pneumonia_diagnosis=="yes" ~ "Pneumonia",
    NP_LRTI_diagnosis=="yes" ~ "NP_LRTI",
    exacerbation_ChrRD_diagnosis=="yes"| heart_failure_diagnosis=="yes"| 
      non_infectious_diagnosis=="yes"| non_LRTD_diagnosis=="yes" ~ "Non-Infective")
    %>% factor(levels = c("Pneumonia","NP_LRTI","Non-Infective")))%>%
  
  mutate(microbiology = case_when(microtest_done_1==1 | microtest_done_2==1 | microtest_done_3==1 |
                                    microtest_done_4==1 | microtest_done_5==1 |  microtest_done_6==1 |
                                    microtest_done_7==1 | microtest_done_8==1 | microtest_done_9==1 |
                                    microtest_done_10==1 | microtest_done_11==1 | microtest_done_12==1 |
                                    microtest_done_13==1 | microtest_done_14==1 | microtest_done_15==1 |
                                    microtest_done_16==1 | microtest_done_17==1 | microtest_done_18==1 |
                                    microtest_done_19==1 ~ 1,
                                  microtest_done_1==0 & microtest_done_2==0 & microtest_done_3==0 &
                                    microtest_done_4==0 & microtest_done_5==0 &  microtest_done_6==0 &
                                    microtest_done_7==0 & microtest_done_8==0 & microtest_done_9==0 &
                                    microtest_done_10==0 & microtest_done_10==0 & microtest_done_12==0 &
                                    microtest_done_13==0 & microtest_done_14==0 & microtest_done_15==0 &
                                    microtest_done_16==0 & microtest_done_17==0 & microtest_done_18==0 &
                                    microtest_done_19==0 ~ 0),
         microbiology[is.na(microbiology)] <- 0)%>%
  mutate(virology = case_when(viral_testing_performed_1==1 | viral_testing_performed_2==1 | viral_testing_performed_3==1 |
                                viral_testing_performed_4==1 | viral_testing_performed_5==1 |  viral_testing_performed_6==1 |
                                viral_testing_performed_7==1 | viral_testing_performed_8==1 | viral_testing_performed_9==1 |
                                viral_testing_performed_10==1 | viral_testing_performed_11==1 | viral_testing_performed_12==1 |
                                viral_testing_performed_13==1 | viral_testing_performed_14==1 | viral_testing_performed_15==1 |
                                viral_testing_performed_16==1 | viral_testing_performed_17==1 | viral_testing_performed_18==1 |
                                viral_testing_performed_19==1 | viral_testing_performed_20==1 ~ 1,
                              viral_testing_performed_1==0 & viral_testing_performed_2==0 & viral_testing_performed_3==0 &
                                viral_testing_performed_4==0 & viral_testing_performed_5==0 &  viral_testing_performed_6==0 &
                                viral_testing_performed_7==0 & viral_testing_performed_8==0 & viral_testing_performed_9==0 &
                                viral_testing_performed_10==0 & viral_testing_performed_10==0 & viral_testing_performed_12==0 &
                                viral_testing_performed_13==0 & viral_testing_performed_14==0 & viral_testing_performed_15==0 &
                                viral_testing_performed_16==0 & viral_testing_performed_17==0 & viral_testing_performed_18==0 &
                                viral_testing_performed_19==0 & viral_testing_performed_20==0 ~ 0),
         virology[is.na(virology)] <- 0)%>%
  mutate(imaging = case_when(radio_exam_1==1 | radio_exam_2==1 | radio_exam_3==1 |
                                 radio_exam_4==1 | radio_exam_5==1 |  radio_exam_6==1 |
                                 radio_exam_7==1 | radio_exam_8==1 | radio_exam_9==1 |
                                 radio_exam_10==1 | radio_exam_11==1 | radio_exam_12==1 |
                                 radio_exam_13==1 | radio_exam_14==1 | radio_exam_15==1 |
                                 radio_exam_16==1 | radio_exam_17==1 | radio_exam_18==1 |
                                 radio_exam_19==1 | radio_exam_20==1 ~ 1,
                               radio_exam_1==0 & radio_exam_2==0 & radio_exam_3==0 &
                                 radio_exam_4==0 & radio_exam_5==0 &  radio_exam_6==0 &
                                 radio_exam_7==0 & radio_exam_8==0 & radio_exam_9==0 &
                                 radio_exam_10==0 & radio_exam_10==0 & radio_exam_12==0 &
                                 radio_exam_13==0 & radio_exam_14==0 & radio_exam_15==0 &
                                 radio_exam_16==0 & radio_exam_17==0 & radio_exam_18==0 &
                                 radio_exam_19==0 & radio_exam_20==0 ~ 0),
         imaging[is.na(imaging)] <- 0)%>%
  mutate(lab_test = case_when(microbiology == 1 | virology == 1 ~ 1, 
                              microbiology ==0 & virology ==0 ~ 0)) %>%
  mutate(test_category = case_when(microbiology==1 | virology==1 | imaging==1 ~ "lab/radio test done",
                                   microbiology==0 & virology==0 & imaging==0 ~ " no lab/radio test"))%>%
  mutate(microbiology_results = case_when((micro_isolates_1==1 | micro_isolates_2==1 | micro_isolates_3==1 |
                                             micro_isolates_4==1 | micro_isolates_5==1 |  micro_isolates_6==1 |
                                             micro_isolates_7==1 | micro_isolates_8==1 | micro_isolates_9==1 |
                                             micro_isolates_10==1 | micro_isolates_11==1 | micro_isolates_12==1 |
                                             micro_isolates_13==1 | micro_isolates_14==1 | micro_isolates_15==1 |
                                             micro_isolates_16==1 | micro_isolates_17==1 | micro_isolates_18==1 |
                                             micro_isolates_19==1) ~ "Positive",
                                          (micro_isolates_1==2 & micro_isolates_2==2 & micro_isolates_3==2 &
                                             micro_isolates_4==2 & micro_isolates_5==2 &  micro_isolates_6==2 &
                                             micro_isolates_7==2 & micro_isolates_8==2 & micro_isolates_9==2 &
                                             micro_isolates_10==2 & micro_isolates_11==2 & micro_isolates_12==2 &
                                             micro_isolates_13==2 & micro_isolates_14==2 & micro_isolates_15==2 &
                                             micro_isolates_16==2 & micro_isolates_17==2 & micro_isolates_18==2 &
                                             micro_isolates_19==2)  ~ "Negative",
                                          (micro_isolates_1==3 & micro_isolates_2==3 & micro_isolates_3==3 &
                                             micro_isolates_4==3 & micro_isolates_5==3 &  micro_isolates_6==3 &
                                             micro_isolates_7==3 & micro_isolates_8==3 & micro_isolates_9==3 &
                                             micro_isolates_10==3 & micro_isolates_11==3 & micro_isolates_12==3 &
                                             micro_isolates_13==3 & micro_isolates_14==3 & micro_isolates_15==3 &
                                             micro_isolates_16==3 & micro_isolates_17==3 & micro_isolates_18==3 &
                                             micro_isolates_19==3)  ~ "Unknown"))%>%
  mutate(virology_results = case_when(virus_isolated_1==1 | virus_isolated_2==1 | virus_isolated_3==1 |
                                        virus_isolated_4==1 | virus_isolated_5==1 |  virus_isolated_6==1 |
                                        virus_isolated_7==1 | virus_isolated_8==1 | virus_isolated_9==1 |
                                        virus_isolated_10==1 | virus_isolated_11==1 | virus_isolated_12==1 |
                                        virus_isolated_13==1 | virus_isolated_14==1 | virus_isolated_15==1 |
                                        virus_isolated_16==1 | virus_isolated_17==1 | virus_isolated_18==1 |
                                        virus_isolated_19==1 | virus_isolated_20==1 ~ "Positive",
                                      virus_isolated_1==0 & virus_isolated_2==0 & virus_isolated_3==0 &
                                        virus_isolated_4==0 & virus_isolated_5==0 &  virus_isolated_6==0 &
                                        virus_isolated_7==0 & virus_isolated_8==0 & virus_isolated_9==0 &
                                        virus_isolated_10==0 & virus_isolated_11==0 & virus_isolated_12==0 &
                                        virus_isolated_13==0 & virus_isolated_14==0 & virus_isolated_15==0 &
                                        virus_isolated_16==0 & virus_isolated_17==0 & virus_isolated_18==0 &
                                        virus_isolated_19==0 & virus_isolated_20==0 ~ "Negative",
                                      (virus_isolated_1==1 & virus_pathogen_1_11==1) | (virus_isolated_2==1 & virus_pathogen_2_11==1) |
                                        (virus_isolated_3==1 & virus_pathogen_3_11==1) | (virus_isolated_4==1 & virus_pathogen_4_11==1) |
                                        (virus_isolated_5==1 & virus_pathogen_5_11==1) | (virus_isolated_6==1 & virus_pathogen_6_11==1) |
                                        (virus_isolated_7==1 & virus_pathogen_7_11==1) | (virus_isolated_8==1 & virus_pathogen_8_11==1) |
                                        (virus_isolated_9==1 & virus_pathogen_9_11==1) | (virus_isolated_10==1 & virus_pathogen_10_11==1) |
                                        (virus_isolated_11==1 & virus_pathogen_11_11==1) | (virus_isolated_12==1 & virus_pathogen_12_11==1) |
                                        (virus_isolated_13==1 & virus_pathogen_13_11==1) | (virus_isolated_14==1 & virus_pathogen_14_11==1) |
                                        (virus_isolated_15==1 & virus_pathogen_15_11==1) | (virus_isolated_16==1 & virus_pathogen_16_11==1) |
                                        (virus_isolated_17==1 & virus_pathogen_17_11==1) | (virus_isolated_18==1 & virus_pathogen_18_11==1) |
                                        (virus_isolated_19==1 & virus_pathogen_19_11==1) | (virus_isolated_20==1 & virus_pathogen_20_11==1) |
                                        covid_19_diagnosis==1 | covid_19_diagnosis==2 ~ "Covid Positive",
                                      (virus_isolated_1==1 & virus_pathogen_1_11!=1) | (virus_isolated_2==1 & virus_pathogen_2_11!=1) |
                                        (virus_isolated_3==1 & virus_pathogen_3_11!=1) | (virus_isolated_4==1 & virus_pathogen_4_11!=1) |
                                        (virus_isolated_5==1 & virus_pathogen_5_11!=1) | (virus_isolated_6==1 & virus_pathogen_6_11!=1) |
                                        (virus_isolated_7==1 & virus_pathogen_7_11!=1) | (virus_isolated_8==1 & virus_pathogen_8_11!=1) |
                                        (virus_isolated_9==1 & virus_pathogen_9_11!=1) | (virus_isolated_10==1 & virus_pathogen_10_11!=1) |
                                        (virus_isolated_11==1 & virus_pathogen_11_11!=1) | (virus_isolated_12==1 & virus_pathogen_12_11!=1) |
                                        (virus_isolated_13==1 & virus_pathogen_13_11!=1) | (virus_isolated_14==1 & virus_pathogen_14_11!=1) |
                                        (virus_isolated_15==1 & virus_pathogen_15_11!=1) | (virus_isolated_16==1 & virus_pathogen_16_11!=1) |
                                        (virus_isolated_17==1 & virus_pathogen_17_11!=1) | (virus_isolated_18==1 & virus_pathogen_18_11!=1) |
                                        (virus_isolated_19==1 & virus_pathogen_19_11!=1) | (virus_isolated_20==1 & virus_pathogen_20_11!=1) ~ "Non-covid Positive"))%>%
  mutate(radiology_results = case_when(final_soc_lrtd_diagnosis_1==1  ~ "Positive",
                                       final_soc_lrtd_diagnosis_1==0  ~ "Negative"))%>%
  mutate(test_results = case_when((microbiology_results=="Positive" | virology_results=="Positive") & radiology_results=="Positive" ~ "PosLab_PosRadio",
                                  (microbiology_results=="Positive" | virology_results=="Positive") & radiology_results=="Negative" ~ "PosLab_NegRadio",
                                  (microbiology_results=="Negative" & virology_results=="Negative") & radiology_results=="Positive" ~ "NegLab_PosRadio",
                                  (microbiology_results=="Negative" & virology_results=="Negative") & radiology_results=="Negative" ~ "NegLab_NegRadio"))%>%
  mutate(clinical_results = case_when(final_soc_lrtd_diagnosis_4==1 ~ "NP LRTI",
                                      final_soc_lrtd_diagnosis_1==1 ~ "Pneumonia - xray",
                                      final_soc_lrtd_diagnosis_2==1 | final_soc_lrtd_diagnosis_3==1
                                      ~ "Pneumonia - clinical",
                                      (final_soc_lrtd_diagnosis_1==0 | is.na(final_soc_lrtd_diagnosis_1))
                                      & (final_soc_lrtd_diagnosis_2==0 | is.na(final_soc_lrtd_diagnosis_2))
                                      & (final_soc_lrtd_diagnosis_3==0 | is.na(final_soc_lrtd_diagnosis_3))
                                      & (final_soc_lrtd_diagnosis_4==0 | is.na(final_soc_lrtd_diagnosis_4))
                                      ~ "Other"), 
         diagnosis = case_when(clinical_results=="NP LRTI" ~ "NP LRTI",
                               clinical_results=="Pneumonia - clinical" ~ "Pneumonia",
                               clinical_results=="Pneumonia - xray" ~ "Pneumonia",
                               clinical_results=="Other" ~ "Other"),
         covid = as.logical(case_when(covid_19_diagnosis==1 | covid_19_diagnosis==2
                                    | covid_19_diagnosis==3 ~ 1,
                                    covid_19_diagnosis==4 | covid_19_diagnosis==5
                                    | is.na(covid_19_diagnosis) ~ 0))) %>% 
  mutate(crb_score = if_else(over_65 == 1,
                            as.double(crb_test_mai - 1), as.double(crb_test_mai)) %>% 
           factor(levels = c("0", "1", "2", "3"))) %>% 
  mutate(covid19_vax = as.logical(case_when(covid19_vax == 1 ~ 1,
                                            covid19_vax == 2 ~ 0,
                                            covid19_vax == 3 ~ NA_real_)))
###

df<-data%>%
  filter(admission_date <= Sys.Date())%>%
  filter(age_at_admission < 120)%>%
  filter(duplicate_adm!="yes")%>%
  filter(consent_declined!="yes")%>%
  select(record_number, admission_date, pneumonia_diagnosis, age_at_admission,
         age_group, over_65, over_75, over_85,
         male_sex, care_home, dementia, imd, cci_category, cci_cat2, rockwood, frail,
         cci_total_score, cci_no.dementia, severe_comorbid, cci_age_score, mi_score, chf_score, pvd_score, 
         neuro_score, dementia_score, copd_score, peptic_ulcer_score, ctd_score,
         liver_disease_score, diabetes_score, neuro_disease_score, ckd_score,
         cancer_score, leukemia_score, lymphoma_score, hiv_score,
         deterioration, confusion, malaise, falls,
         fever, cough, sputum, dyspnoea, chest_sign, pleurisy,
         anosmia, ageusia_dysgeusia, myalgia, headache, wheeze, 
         symptom_category, case_def, case_def2, case_def_covid, diagnosis_category, diagnosis,
         covid, microbiology, virology, lab_test, imaging,
         microbiology_results, virology_results, test_results,clinical_results,
         starts_with("micro_test"), starts_with("test_type"),
         starts_with("isolate_identified"), starts_with("virus_pathogen"),
         crb_test_mai, curb65_4h, crb_score, covid19_vax) %>% 
  apply_labels(age_at_admission = "Age (yrs) at admission",
               age_group = "Age group",
               over_65 = "Aged >=65y",
               over_75 = "Aged >=75y",
               over_85 = "Aged >=85y",
               male_sex = "Male sex",
               care_home = "Care home resident",
               dementia = "Dementia/ cognitive impairment",
               cci_category = "Comorbidity level",
               cci_no.dementia = "Comorbidity level",
               severe_comorbid = "CCI score >4 (severe)",
               mi_score = "MI score",
               chf_score = "CHF score",
               pvd_score = "PVD score",
               neuro_score = "CVA/ TIA score",
               dementia_score = "Dementia score",
               copd_score = "COPD score",
               peptic_ulcer_score = "Peptic ulcer score",
               ctd_score = "CTD score",
               liver_disease_score = "Liver disease score",
               diabetes_score = "Diabetes score",
               neuro_disease_score = "Hemi/ paraplegia score",
               ckd_score = "CKD score",
               cancer_score = "Cancer score",
               leukemia_score = "Leukaemia score",
               lymphoma_score = "Lymphoma score",
               hiv_score = "HIV score",
               chf_score = "CHF score",
               covid = "SARS-CoV-2 test positive",
               frail = "Clinically frail",
               rockwood = "Frailty score",
               case_def = "Symptoms meeting case definition",
               case_def2 = "Symptoms meeting case definition",
               pneumonia_diagnosis = "Pneumonia",
               crb_score = "CRB score",
               covid19_vax = "Vaccinated against SARS-CoV-2")

## Create separate filtered datasets for COVID-19 positive and negative individuals
# add categorical variable for year & quarter
df_lrti <- df %>% 
  filter(diagnosis != "Other") %>% 
  mutate(yrqtr = as.yearqtr((admission_date))) %>% 
  mutate(yrqtr = factor(yrqtr, levels = c("2020 Q3", "2020 Q4", "2021 Q1",
                                          "2021 Q2", "2021 Q3", "2021 Q4",
                                          "2022 Q1", "2022 Q2", "2022 Q3"),
                        labels = c("2020 Aug 1 - Sept 30", "2020 Q4", "2021 Q1",
                                   "2021 Q2", "2021 Q3", "2021 Q4",
                                   "2022 Q1", "2022 Q2", "2022 July 1 - July 31")))

df_covidneg <- df %>% 
  filter(covid == 0 & (diagnosis != "Other")) %>% 
  mutate(yrqtr = as.yearqtr((admission_date))) %>% 
  mutate(yrqtr = factor(yrqtr, levels = c("2020 Q3", "2020 Q4", "2021 Q1",
                                             "2021 Q2", "2021 Q3", "2021 Q4",
                                             "2022 Q1", "2022 Q2", "2022 Q3"),
                        labels = c("2020 Aug 5 - Sept 30", "2020 Q4", "2021 Q1",
                                   "2021 Q2", "2021 Q3", "2021 Q4",
                                   "2022 Q1", "2022 Q2", "2022 July 1 - Aug 6")))

df_covidpos <- df %>% 
  filter(covid == 1 & (diagnosis != "Other")) %>% 
  mutate(yrqtr = as.yearqtr((admission_date))) %>% 
  mutate(yrqtr = factor(yrqtr, levels = c("2020 Q3", "2020 Q4", "2021 Q1",
                                          "2021 Q2", "2021 Q3", "2021 Q4",
                                          "2022 Q1", "2022 Q2", "2022 Q3"),
                        labels = c("2020 Aug 5 - Sept 30", "2020 Q4", "2021 Q1",
                                   "2021 Q2", "2021 Q3", "2021 Q4",
                                   "2022 Q1", "2022 Q2", "2022 July 1 - Aug 6")))


df_long<-df %>%
  select(admission_date, record_number, age_group, over_65, diagnosis, covid,
         rockwood, dementia, cci_no.dementia, severe_comorbid,
         cough, dyspnoea, sputum, wheeze, pleurisy,
         fever, malaise, myalgia, headache,
         deterioration, confusion, falls)%>% 
  dplyr::mutate(ID = dplyr::row_number())  %>% 
  pivot_longer(cols = c(cough, dyspnoea, sputum, wheeze, pleurisy,
                        fever, malaise, myalgia, headache,
                        deterioration, confusion, falls), names_to = "symptoms") %>%  
  filter(value == 1)
