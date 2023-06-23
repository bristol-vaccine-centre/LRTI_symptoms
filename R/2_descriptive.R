
pacman::p_load(here,
               tidyselect,
               ggpubr,
               rsample,
               ftExtra,
               DescTools,
               dgof, # for ks.test
               gplots,
               ggridges,
               patchwork,
               viridis,
               gt,
               remotes,
               ggpattern,
               corrplot)

# source(here::here("scripts", "1_cleandata.R"))

## CONSORT diagram #############
data<- data %>%
  filter(admission_date <= dmy('28/09/22'))%>%
  filter(age_at_admission < 120)

data %>% 
  group_by(duplicate_adm) %>% 
  tally()

data %>%
  filter(duplicate_adm!="yes")%>%
  group_by(consent_declined) %>% 
  tally()

data%>%
  filter(duplicate_adm!="yes")%>%
  filter(consent_declined!="yes")%>%
  filter(diagnosis != "Other") %>% 
  group_by(case_def) %>% 
  tally()

data%>%
  filter(duplicate_adm!="yes")%>%
  filter(consent_declined!="yes")%>%
  group_by(age_group, symptom_category) %>% 
  tally()

data%>%
  filter(duplicate_adm!="yes")%>%
  filter(consent_declined!="yes")%>%
  group_by(symptom_category) %>% 
  tally()

data%>%
  filter(duplicate_adm!="yes")%>%
  filter(consent_declined!="yes")%>%
  group_by(symptom_category, diagnosis_category) %>% 
  tally()



############# descriptive: all eligible ##############
  # no. admissions
nrow(df)
  # date range
min(df$admission_date)
max(df$admission_date)

  # n (%) over 65
df %>% group_by(over_65) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) %>% 
  adorn_totals("row")

# n (%) in care homes by age band
df %>% 
  group_by(care_home, age_group) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) %>% 
  adorn_totals("row")

  # n (%) in care home
df %>% 
  filter(over_65 == 1) %>% 
  group_by(care_home) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) %>% 
  adorn_totals("row")

  # n (%) with dementia
df %>% 
  filter(over_65 == 1) %>% 
  group_by(dementia) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) %>% 
  adorn_totals("row")

  # n (%) comorbidity levels
df %>% group_by(cci_total_score) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) %>% 
  adorn_totals("row")

# tested for viruses/ bacteria
df_lrti %>% 
  group_by(microbiology, virology) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  adorn_totals("row")

############# descriptive: confirmed LRTI ##############
# N meetng case definition
df_lrti %>% 
  group_by(case_def) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  adorn_totals("row")

df_lrti %>% 
  group_by(clinical_results) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  adorn_totals("row")


temp <- df_lrti %>% 
  select(starts_with("test_type")) %>% 
  dplyr::mutate(ID = dplyr::row_number())  %>% 
  pivot_longer(cols = c(starts_with("test_type")),
               names_to = "test_n") %>% 
  mutate(value = case_when(value==1 ~ "PCR covid",
                           value==2 ~ "PCR resp panel",
                           value==3 ~ "PCR resp biofire",
                           value==4 ~ "viral culture",
                           value==5 ~ "covid LFT",
                           value==6 ~ "POCT abbott"))

unique(temp$value)

temp %>% 
  group_by(value) %>%
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  adorn_totals("row")


# n (%) covid diagnosis
df %>% filter(diagnosis != "Other") %>% 
  group_by(covid) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) %>% 
  adorn_totals("row")

  # n (%) meeting case definition
df %>% filter(diagnosis != "Other" & covid==1) %>% 
  group_by(case_def_covid) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) %>% 
  adorn_totals("row")

  # proportion of cases classed as 'frail' (rockwood score >4) by agegroup
tabyl(df_lrti, age_group, frail) %>% 
  adorn_percentages(denominator = "row") 

  # histogram frail cases
df_lrti %>% 
  ggplot(aes(age_at_admission, group = as.factor(frail), fill = as.factor(frail))) + 
  geom_histogram() +
  facet_wrap(~as.factor(yrqtr), ncol = 1)+
  scale_fill_discrete(labels = c("Not frail", "Frail", "Missing"), name=NULL)+
  xlim(15, 120) +
  scale_x_continuous(name = "Age (years)", breaks = c(20,30,40,50,60,70,80,90, 100))+
  labs(title = "Counts of LRTI cases by age and frailty",
       subtitle = "(Frail = Rockwood score >4)")

  # disease severity
df_lrti %>% 
  group_by(crb_score, over_65) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) %>% 
  adorn_totals("row")

##########

  # disease-specific comorbidity scores by age group
df_lrti %>% select(c(over_65, mi_score, chf_score, pvd_score, neuro_score,
                         dementia_score, copd_score, peptic_ulcer_score,
                         ctd_score, liver_disease_score, diabetes_score,
                         neuro_disease_score, ckd_score, cancer_score,
                         leukemia_score, lymphoma_score, hiv_score)) %>% 
  group_by(over_65, mi_score) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) %>% 
  adorn_totals("row")

tbl_comorbidities <- df_lrti %>% 
  select(case_def,
         mi_score, chf_score, pvd_score, neuro_score,
         dementia_score, copd_score, peptic_ulcer_score,
         ctd_score, liver_disease_score, diabetes_score,
         neuro_disease_score, ckd_score, cancer_score,
         leukemia_score, lymphoma_score, hiv_score) %>%
  tbl_summary(by = case_def,
              type = list(where(is.factor) ~ "categorical"),
              digits = all_continuous() ~ 0) %>% 
  add_p(list(all_continuous() ~ "t.test", 
 #            all_dichotomous() ~ "fisher.test.simulate.p.values",
             all_categorical() ~ "wilcox.test"))

tbl_comorbidities %>% 
  modify_header(stat_1 = "**Not meeting LRTI case definition**  
                N = 12,789") %>% 
  modify_header(stat_2 = "**Meeting LRTI case definition**  
                N = 4,831") %>% 
  modify_table_styling(columns = label, rows = label == "MI score", 
                       footnote = "Myocardial Infarction") %>% 
  modify_table_styling(columns = label, rows = label == "CHF score", 
                       footnote = "Congestive Heart Failure") %>% 
  modify_table_styling(columns = label, rows = label == "PVD score", 
                       footnote = "Peripheral Vascular Disease") %>% 
  modify_table_styling(columns = label, rows = label == "CVA/ TIA score", 
                       footnote = "Cerebrovascular Accident/ Transient Ischaemic Attack") %>% 
  modify_table_styling(columns = label, rows = label == "COPD score", 
                       footnote = "Chronic Obstructive Pulmonary Disease") %>% 
  modify_table_styling(columns = label, rows = label == "CTD score", 
                       footnote = "Connective Tissue Disorder") %>% 
  modify_table_styling(columns = label, rows = label == "CKD score", 
                       footnote = "Chronic Kidney Disease") %>% 
  modify_table_styling(columns = label, rows = label == "HIV score", 
                       footnote = "Human Immunodeficiency Virus")

tbl_comorbidities %>% 
  as_gt() %>% 
  gt::gtsave( # save table as image
    path = here("outputs"), filename = "Comorbidities.png")


#######################################

## plot symptom incidence over time
df_long %>% 
  ggplot(aes(admission_date, group = symptoms, color = symptoms)) +
  geom_density() +
  facet_wrap(~over_65, ncol = 2,
             labeller = labeller(over_65 = c("0" = "age <65", "1" = "age >=65")))


df_lrti %>%
  ggplot(aes(admission_date, group = symptom_category, color = symptom_category)) +
  geom_density() +
  facet_wrap(~over_65, ncol = 2,
             labeller = labeller(over_65 = c("0" = "age <65", "1" = "age >=65")))

 
 ######################
 ## Correlation matrix
 
 covariates <- df_lrti %>%
   mutate(severe_comorbid = as.double(severe_comorbid),
     #cci_no.dementia = 
#            case_when(cci_no.dementia == "None" ~ 0,
#                      cci_no.dementia == "Mild" ~ 1,
#                      cci_no.dementia == "Moderate" ~ 2,
#                      cci_no.dementia == "Severe" ~ 3),
          imd = as.double(imd),
          crb_score = as.double(crb_score)) %>% 
   dplyr::select(over_65, male_sex, covid, covid19_vax, crb_score,
                 dementia, severe_comorbid, frail, care_home, imd
                 ) %>%
   drop_na()
 
 M <- cor(covariates)
 colnames(M) <- c("age >=65y", "male", "SARS-CoV-2", "vaccinated", "CRB score",
                  "dementia", "CCI score >4", "frail", "care home", "IMD")
 rownames(M) <- c("age >=65y", "male", "SARS-CoV-2", "vaccinated", "CRB score",
                  "dementia", "CCI score >4", "frail", "care home", "IMD")
 
 corrplot(M,
          addCoef.col = "black",
          lower = "number",
          upper = "circle",
          tl.col = "black",
          tl.cex = 1,
          col = COL1('OrRd'),
          number.digits = 2)
 