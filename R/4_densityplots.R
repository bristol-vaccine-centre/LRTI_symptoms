pacman::p_load(here,
               ggpubr,
               rsample,
               ftExtra,
               DescTools,
               gplots,
               ggridges,
               patchwork,
               viridis,
               gt,
               remotes,
               ggpattern,
               ggsci)

# remotes::install_github("coolbutuseless/ggpattern")
# library(ggpattern)

# source(here::here("scripts", "1_cleandata.R"))

## Density plots - age distribution by outcome category

###########
## DENSITY PLOTS IN COLOUR

  # SARS-CoV-2 negative cases
density_covidneg <- df_covidneg %>% 
  ggplot(aes(x=age_at_admission, fill = case_def, colour = case_def)) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  facet_wrap(~as.factor(yrqtr), ncol = 1) +   # split by year & quarter
  
  scale_fill_jco(labels=c("Not meeting case definition    ",
                                                      "Meeting case definition    "),
                                             name = NULL)+

  scale_colour_manual(values = c("black", "black"), guide = "none") +
  scale_y_continuous(breaks = c(0, 0.02)) +
  labs(x = "Age (years)", y = "Proportion of cases") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "white"),
        #axis.title.x = element_text(hjust = 0),
        legend.position = "bottom")

  # SARS-CoV-2 positive cases
density_covidpos <- df_covidpos %>% 
  ggplot(aes(x=age_at_admission, fill = case_def, colour = case_def)) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  facet_wrap(~as.factor(yrqtr), ncol = 1) +   # split by year & quarter
  
  scale_fill_jco(labels=c("Not meeting case definition    ",
                          "Meeting case definition    "),
                 name = NULL)+
  
  scale_colour_manual(values = c("black", "black"), guide = "none") +
  scale_y_continuous(breaks = c(0, 0.02)) +
  labs(x = "Age (years)", y = "Proportion of cases") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "white"),
        #axis.title.x = element_text(hjust = 0),
        legend.position = "bottom")

  # overall case frequency by year of age for context
freq_poly <- df_lrti %>% 
  ggplot(aes(age_at_admission)) +
  geom_freqpoly(aes(linetype = as.factor(covid)), binwidth = 1) +
  facet_wrap(~as.factor(yrqtr) , ncol = 1, scales = "fixed", strip.position = "top") +
  scale_linetype_discrete(labels=c("Other LRTI",
                                   "SARS-CoV-2 LRTI"),
                          name = NULL) +
  scale_y_continuous(breaks = c(0, 40)) +
  theme_minimal() +
  theme(strip.background = element_blank(),
   #     axis.title.x = element_text(hjust = 0)
   ) +
  labs(x = "Age (years)", y = "Count of cases") 

  # print plots side by side
p <- (freq_poly + rremove("grid")) +
  (density_covidneg + rremove("legend") + rremove("grid")) +
  (density_covidpos #+ rremove("xlab") 
   + rremove("ylab") + rremove("grid")) +
#  (freq_poly_1 + rremove("xlab") + rremove("grid")) +
 # (density_covidneg_1 + rremove("ylab") + rremove("legend") + rremove("grid")) +
 # (density_covidpos_1 + rremove("xlab") + rremove("ylab") + rremove("grid")) +
 plot_layout(ncol = 3, widths = c(1,1,1), guides = "collect") +
  plot_annotation(tag_levels = list(c("A", "B", "C"))) &
  theme(legend.position = "bottom")

set_last_plot(p)
ggsave(here("outputs", "test3.png"), p)

p

###########
## DENSITY PLOTS IN B&W (HATCHING)

density_covidneg <- df_covidneg %>% 
  ggplot(aes(x=age_at_admission, fill = case_def, colour = case_def)) +
  ggpattern::geom_density_pattern(pattern_colour = "black",
                                  pattern_fill = "black",
                                  adjust = 1.5,
                                  alpha = 0.4,
                                  pattern_alpha= 0.4,
                                  pattern_density = 0.1,
                                  aes(pattern = case_def)) +
  facet_wrap(~as.factor(yrqtr), ncol = 1) +   # split by year & quarter
  scale_fill_manual(values = c("grey", "white"), guide = "none")+
  scale_colour_manual(values = c("black", "black"), guide = "none") +
  scale_pattern_manual(values = c("none", "circle"),
                       labels=c("Symptoms not meeting case definition",
                                "Symptoms meeting case definition"),
                       name = NULL) +
  labs(x = "Age (years)", y = "Proportion of cases") +
  scale_y_continuous(breaks = c(0, 0.02)) +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "white"),
        axis.title.x = element_text(hjust = 0),
        legend.position = "bottom")

density_covidpos <- df_covidpos %>% 
  ggplot(aes(x=age_at_admission, fill = case_def, colour = case_def)) +
  ggpattern::geom_density_pattern(pattern_colour = "black",
                                  pattern_fill = "black",
                                  adjust = 1.5,
                                  alpha = 0.4,
                                  pattern_alpha= 0.4,
                                  pattern_density = 0.1,
                                  aes(pattern = case_def)) +
  facet_wrap(~as.factor(yrqtr), ncol = 1) +  # split by year & quarter
  scale_fill_manual(values = c("grey", "white"), guide = "none")+
  scale_colour_manual(values = c("black", "black"), guide = "none") +
  scale_pattern_manual(values = c("none", "circle"),
                       labels=c("Symptoms not meeting case definition",
                                "Symptoms meeting case definition"),
                       name = NULL) +
  labs(x = "Age (years)", y = "Proportion of cases") +
  scale_y_continuous(breaks = c(0, 0.02)) +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "white"),
        axis.title.x = element_text(hjust = 0),
        legend.position = "bottom")






