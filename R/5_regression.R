pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics, 
  stringr,      # manipulate text strings 
  purrr,        # loop over objects in a tidy way
  gtsummary,    # summary statistics and tests 
  broom,        # tidy up results from regressions
  lmtest,       # likelihood-ratio tests
  parameters,   # alternative to tidy up results from regressions
  see,          # alternative to visualise forest plots
  gt,
  webshot2,      # for exporting gt objects as images
  flextable
)

conflict_prefer("here", "here")

#### Full model #####

  # Univariable results table
explanatory_uv1 <- c("over_65", "male_sex", "dementia", "severe_comorbid")


uv_lrti <- df_lrti %>% 
  dplyr::select(all_of(explanatory_uv1), case_def) %>% ## select variables of interest
  tbl_uvregression(                         ## produce univariable table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = case_def,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    show_single_row = c(over_65, male_sex, dementia, severe_comorbid),
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

  # Multivariable results table
mvm_lrti <- glm(case_def ~ over_65 + male_sex + dementia + severe_comorbid,
                family = "binomial", data = df_lrti)


mv_lrti <- tbl_regression(mvm_lrti, 
                          show_single_row = c(over_65, male_sex, dementia, severe_comorbid),
                          exponentiate = TRUE)

  # Combined table univariable & multivariable

tbl_lrti <- tbl_merge(tbls = list(uv_lrti, mv_lrti),
                      tab_spanner = c("**Unadjusted odds**", "**Adjusted odds**")) %>% 
  modify_table_styling(columns = label, rows = label == "CCI score >4 (severe)",
                       footnote = "Charlson Comorbidity Index minus age and dementia scores.")


tbl_lrti %>% 
  as_gt() %>% 
  gt::gtsave(path = here("outputs"), filename = "regression_lrti.png")

tbl_lrti %>% 
  as_hux_xlsx(file = here("outputs", "regression_lrti.xlsx"))


###### Check for effect modification ######
with(df_lrti, epiDisplay::mhor(case_def, over_65, severe_comorbid, decimal = 2))
with(df_lrti, epiDisplay::mhor(case_def, over_65, dementia, decimal = 2))
with(df_lrti, epiDisplay::mhor(case_def, over_65, covid, decimal = 2))


###### Sensitivity analyses ########

## 1: Apply broader case definition

explanatory_uv1 <- c("over_65", "male_sex", "dementia", "severe_comorbid")


uv_lrti2 <- df_lrti %>% 
  dplyr::select(all_of(explanatory_uv1), case_def2) %>% ## select variables of interest
  tbl_uvregression(                         ## produce univariable table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = case_def2,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    show_single_row = c(over_65, male_sex, dementia, severe_comorbid),
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

# Multivariable results table
mvm_lrti2 <- glm(case_def2 ~ over_65 + male_sex + dementia + severe_comorbid,
                family = "binomial", data = df_lrti)


mv_lrti2 <- tbl_regression(mvm_lrti2, 
                          show_single_row = c(over_65, male_sex, dementia, severe_comorbid),
                          exponentiate = TRUE)

# Combined table univariable & multivariable

tbl_lrti2 <- tbl_merge(tbls = list(uv_lrti2, mv_lrti2),
                      tab_spanner = c("**Unadjusted odds**", "**Adjusted odds**")) %>% 
  modify_table_styling(columns = label, rows = label == "CCI score >4 (severe)",
                       footnote = "Charlson Comorbidity Index minus age and dementia scores.")


tbl_lrti2 %>% 
  as_gt() %>% 
  gt::gtsave(path = here("outputs"), filename = "regression_lrti_sensitivity.png")

tbl_lrti %>% 
  as_hux_xlsx(file = here("outputs", "regression_lrti_sensitivity.xlsx"))


## 2: stratify by severe comorbidities (CCI score >4)

  # Univariable

explanatory_uv2 <- c("over_65", "male_sex", "dementia")

uv_no.comorbid <- df_lrti %>% 
  filter(severe_comorbid == 0) %>% 
  dplyr::select(all_of(explanatory_uv2), case_def) %>% ## select variables of interest
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = case_def,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    show_single_row = c(over_65, male_sex, dementia),
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )


uv_comorbid <- df_lrti %>% 
  filter(severe_comorbid == 1) %>% 
  dplyr::select(all_of(explanatory_uv2), case_def) %>% ## select variables of interest
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = case_def,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    show_single_row = c(over_65, male_sex, dementia),
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

                     
  # Multivariable

mvm_no.comorbid <- glm(case_def ~ over_65 + male_sex + dementia,
                  family = "binomial", data = filter(df_lrti, severe_comorbid == 0))

mv_no.comorbid <- tbl_regression(mvm_no.comorbid,
                                 show_single_row = c(over_65, male_sex, dementia),
                                 exponentiate = TRUE)

mvm_comorbid <- glm(case_def ~ over_65 + male_sex + dementia,
                family = "binomial", data = filter(df_lrti, severe_comorbid == 1))

mv_comorbid <- tbl_regression(mvm_comorbid,
                          show_single_row = c(over_65, male_sex, dementia),
                          exponentiate = TRUE)

  # Merge

tbl_no.comorbid <- tbl_merge(tbls = list(uv_no.comorbid, mv_no.comorbid),
                        tab_spanner = c("**Unadjusted odds**", "**Adjusted odds**")) %>% 
  as_gt() %>% 
  gt::tab_header(title = md("**CCI score <=4:** Odds of symptoms meeting LRTI case definition")) %>% 
  opt_align_table_header(align = "left")


gtsave(tbl_no.comorbid, here("outputs", "regression_no.comorbid.png"))


tbl_comorbid <- tbl_merge(tbls = list(uv_comorbid, mv_comorbid),
                      tab_spanner = c("**Unadjusted odds**", "**Adjusted odds**")) %>% 
  as_gt() %>% 
  gt::tab_header(title = md("**CCI score >4:** Odds of symptoms meeting LRTI case definition")) %>% 
  opt_align_table_header(align = "left")


gtsave(tbl_comorbid, here("outputs", "regression_comorbid.png"))


## 3: Stratify by SARS-CoV-2 positivity

  # Univariable

explanatory_uv3 <- c("over_65", "male_sex",
                    "dementia", "severe_comorbid", "covid19_vax") # include vaccination status

uv_covidneg <- df_covidneg %>% 
  dplyr::select(all_of(explanatory_uv3), case_def) %>% ## select variables of interest
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = case_def,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    show_single_row = c(over_65, male_sex, dementia, severe_comorbid, covid19_vax),
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

uv_covidpos <- df_covidpos %>% 
  dplyr::select(all_of(explanatory_uv3), case_def) %>% ## select variables of interest
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = case_def,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    show_single_row = c(over_65, male_sex, dementia, severe_comorbid, covid19_vax),
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

  # Multivariable

mvm_covidneg <- glm(case_def ~ over_65 + male_sex + dementia + severe_comorbid + covid19_vax,
                    family = "binomial", data = df_covidneg)

mv_covidneg <- tbl_regression(mvm_covidneg,
                              show_single_row = c(over_65, male_sex, dementia, severe_comorbid, covid19_vax),
                              exponentiate = TRUE)

mvm_covidpos <- glm(case_def ~ over_65 + male_sex + dementia + severe_comorbid + covid19_vax,
                    family = "binomial", data = df_covidpos)

mv_covidpos <- tbl_regression(mvm_covidpos,
                              show_single_row = c(over_65, male_sex, dementia, severe_comorbid, covid19_vax),
                              exponentiate = TRUE)

  # Merge

tbl_covidneg <- tbl_merge(tbls = list(uv_covidneg, mv_covidneg),
                          tab_spanner = c("**Unadjusted odds**", "**Adjusted odds**")) %>% 
  modify_table_styling(columns = label, rows = label == "CCI score >4 (severe)",
                       footnote = "Charlson Comorbidity Index minus age and dementia scores.") %>% 
  as_gt() %>% 
  gt::tab_header(title = md("**SARS-CoV-2 negative LRTI:** Odds of symptoms meeting LRTI case definition")) %>% 
  opt_align_table_header(align = "left")

gtsave(tbl_covidneg, here("outputs", "regression_covidneg.png"))

tbl_covidpos <- tbl_merge(tbls = list(uv_covidpos, mv_covidpos),
                          tab_spanner = c("**Unadjusted odds**", "**Adjusted odds**")) %>% 
  modify_table_styling(columns = label, rows = label == "CCI score >4 (severe)",
                       footnote = "Charlson Comorbidity Index minus age and dementia scores.") %>% 
  as_gt() %>% 
  gt::tab_header(title = md("**SARS-CoV-2 positive LRTI:** Odds of symptoms meeting LRTI case definition")) %>% 
  opt_align_table_header(align = "left")

gtsave(tbl_covidneg, here("outputs", "regression_covidpos.png"))


