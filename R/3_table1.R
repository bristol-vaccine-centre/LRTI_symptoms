pacman::p_load(here,
               tidyselect,
               ggpubr,
               rsample,
               ftExtra,
               DescTools,
               dgof, # for ks.test
               gt)

# source(here::here("scripts", "1_cleandata.R"))

## prep
fisher.test.simulate.p.values <- function(data, variable, by, ...) {
  result <- list()
  test_results <- stats::fisher.test(data[[variable]], data[[by]],
                                     simulate.p.value = TRUE)
  result$p <- test_results$p.value
  result$test <- test_results$method
  result
}

## Table 1
table1a <- df_lrti %>% 
  dplyr::select(case_def, age_at_admission, age_group, covid, covid19_vax, crb_score, 
                male_sex, care_home, dementia, frail, cci_no.dementia) %>%
  tbl_summary(by = case_def,
              type = list(c(covid, covid19_vax, male_sex, care_home, dementia, frail) ~ "dichotomous",
                          c(age_group, crb_score, cci_no.dementia) ~ "categorical",
                          age_at_admission ~ "continuous"),
              digits = all_continuous() ~ 0) %>% 
  add_p(list(all_continuous() ~ "t.test", 
             all_dichotomous() ~ "fisher.test.simulate.p.values",
             c(age_group, crb_score, cci_no.dementia) ~ "wilcox.test"))

# show_header_names(table1a)
table1a <-  table1a %>% 
  modify_header(stat_1 = "**Not meeting LRTI case definition**  
                N = 9,133") %>% 
  modify_header(stat_2 = "**Meeting LRTI case definition**  
                N = 8,487") %>% 
  modify_table_styling(columns = label, rows = label == "Clinically frail", 
                       footnote = "Rockwood frailty score >4") %>% 
  modify_table_styling(columns = label, rows = label == "CRB score",
                       footnote = "Pneumonia severity score, 1 point assigned for each of:
                       acute confusion, raised respiratory rate, low blood pressure.") %>% 
  modify_table_styling(columns = label, rows = label == "Comorbidity level",
                       footnote = "Charlson Comorbidity Index minus age and dementia scores.
                       Mild: 1-2, Moderate: 3-4, Severe: >4")

table1a %>% 
  as_gt() %>% 
  gt::gtsave( # save table as image
    path = here("outputs"), filename = "Table1.png")

table1a %>% 
  as_hux_xlsx(file = here("outputs", "Table1.xlsx"))

###########

table1b <- df_lrti %>% 
  dplyr::select(over_65, case_def, age_group, covid, covid19_vax, crb_score, 
                male_sex, care_home, dementia, frail, cci_no.dementia) %>%
  tbl_summary(by = over_65,
              type = list(c(case_def, covid, covid19_vax, male_sex, care_home, dementia, frail) ~ "dichotomous",
                          c(crb_score, cci_no.dementia) ~ "categorical"),
              digits = all_continuous() ~ 0) %>% 
  add_p(list(all_continuous() ~ "t.test", 
             all_dichotomous() ~ "fisher.test.simulate.p.values",
             c(crb_score, cci_no.dementia) ~ "wilcox.test"))

#show_header_names(table1b)

table1b <- table1b %>%
  modify_header(stat_1 = "**Aged <65 years**  N = 6,698") %>% 
  modify_header(stat_2 = "**Aged >=65 years** N = 10,922") %>% 
  modify_table_styling(columns = label, rows = label == "Clinically frail", 
                       footnote = "Rockwood frailty score >4") %>%
  modify_table_styling(columns = label, rows = label == "CRB score",
                       footnote = "Pneumonia severity score, 1 point assigned for each of:
                       acute confusion, raised respiratory rate, low blood pressure.") %>% 
  modify_table_styling(columns = label, rows = label == "Comorbidity level",
                       footnote = "Charlson Comorbidity Index minus age and dementia scores.
                       Mild: 1-2, Moderate: 3-4, Severe: >4") 

table1b %>%
  as_gt() %>% 
  gt::gtsave(path = here("outputs"), filename = "Table1b.png")

table1b %>% 
  as_hux_xlsx(file = here("outputs", "Table1b.xlsx"))
