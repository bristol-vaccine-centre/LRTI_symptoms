pacman::p_load(readr,
               dplyr,
               stats,
               plyr,
               lubridate,
               janitor,
               gtsummary,
               gplots,
               gridExtra,
               tidylo,   # for calculating log odds ratios
               tidytuesdayR,
               tidytext,
               ggrepel,
               tidymodels,
               embed,
               factoextra,
               viridis,
               vegan,
               ggrepel,
               patchwork,
               ggpubr,
               conflicted,
               tidyverse,
               ggsci,
               corrplot)

conflict_prefer("select", "dplyr")
conflict_prefer("count", "dplyr")

# source(here::here("scripts", "1_cleandata.R"))

#################################
## Heatmap symptom v agegroup ##

temp <- (df_lrti %>% 
           dplyr::select(age_group, 
                         cough, dyspnoea, sputum, wheeze, pleurisy,
                         fever, malaise, myalgia, headache,
                         deterioration, confusion, falls) %>% 
           group_by(age_group) %>%
           dplyr::summarise(across(everything(), sum)))

heatmap_df <- as.matrix(temp[,-1])
rownames(heatmap_df) <- temp$age_group


plot_heatmap <- heatmap.2(heatmap_df, 
                     scale = "row",
                     Colv = FALSE,
                     main = " ",
                     density.info="none",  # turns off density plot inside color legend
                     trace="none",         # turns off trace lines inside the heat map
                     margins =c(12,9),     # widens margins around plot
                     dendrogram="row",     #  draw a row and column dendrogram
                     cexRow =  1.5, # row label size
                     cexCol = 1.5,  # column label size
                     col = COL1('YlOrRd'))

#####################################
## Log odds of symptom expression ##

# Prep
groups <- c("TRUE", "FALSE")
panel_labs <- c("TRUE" = "Aged >=65y", "FALSE" = "Aged <65y")

  # For entire cohort
symptom_counts <- df_long %>%
  filter(diagnosis != "Other") %>% 
  group_by(over_65, symptoms)%>%
  tally(sort = TRUE)

symptom_log_odds <- symptom_counts %>%
  bind_log_odds(over_65, symptoms, n)

symptom_log_odds %>%
  filter(n > 5) %>%
  dplyr::arrange(-log_odds_weighted)

logodds_plot <- symptom_log_odds %>%
  #  filter(over_65 == 1 %in% groups) %>%   # repeat for >65s/ <65s
  group_by(over_65) %>%
  top_n(12) %>%
  ungroup() %>%
  mutate(symptoms = reorder_within(symptoms, log_odds_weighted, over_65)) %>%
  ggplot(aes(log_odds_weighted, symptoms, fill = over_65)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~over_65, scales = "free_y", 
             labeller = labeller(over_65 = panel_labs)) +
  scale_y_reordered() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_jco(alpha = 0.7) +  
  theme_classic() +
  labs(
 #   title = "SARS-CoV-2 negative",
    y = NULL, x = "Weighted log-odds ratios (as z scores)") +
  theme(
  strip.text = element_text(size = 13),
  strip.background = element_blank(),
        axis.text = element_text(size = 11.5),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        panel.grid = element_blank(),
        plot.margin = margin(0,0,20,0))

# By covid status
symptom_counts1 <- df_long %>%
  filter(diagnosis != "Other") %>% 
  filter(covid ==0) %>% 
  group_by(over_65, symptoms)%>%
  tally(sort = TRUE)

symptom_log_odds1 <- symptom_counts1 %>%
  bind_log_odds(over_65, symptoms, n)

symptom_log_odds1 %>%
  filter(n > 5) %>%
  dplyr::arrange(-log_odds_weighted)


logodds_plot1 <- symptom_log_odds1 %>%
  #  filter(over_65 == 1 %in% groups) %>%   # repeat for >65s/ <65s
  group_by(over_65) %>%
  top_n(12) %>%
  ungroup() %>%
  mutate(symptoms = reorder_within(symptoms, log_odds_weighted, over_65)) %>%
  ggplot(aes(log_odds_weighted, symptoms, fill = over_65)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~over_65, scales = "free_y", 
             labeller = labeller(over_65 = panel_labs)) +
  scale_y_reordered() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_jco(alpha = 0.7) +  
  theme_classic() +
  labs(
       title = "SARS-CoV-2 negative",
    y = NULL, x = "Weighted log-odds ratios (as z scores)") +
  theme(
    strip.text = element_text(size = 13),
    strip.background = element_blank(),
    axis.text = element_text(size = 11.5),
    axis.title.x = element_text(size = 12, vjust = -0.5),
    panel.grid = element_blank(),
    plot.margin = margin(0,0,20,0))

##
symptom_counts2 <- df_long %>%
  filter(diagnosis != "Other") %>% 
  filter(covid ==1) %>% 
  group_by(over_65, symptoms)%>%
  tally(sort = TRUE)

symptom_log_odds2 <- symptom_counts2 %>%
  bind_log_odds(over_65, symptoms, n)

symptom_log_odds2 %>%
  filter(n > 5) %>%
  dplyr::arrange(-log_odds_weighted)

logodds_plot2 <- symptom_log_odds2 %>%
  #  filter(over_65 == 1 %in% groups) %>%   # repeat for >65s/ <65s
  group_by(over_65) %>%
  top_n(12) %>%
  ungroup() %>%
  mutate(symptoms = reorder_within(symptoms, log_odds_weighted, over_65)) %>%
  ggplot(aes(log_odds_weighted, symptoms, fill = over_65)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~over_65, scales = "free_y", 
             labeller = labeller(over_65 = panel_labs)) +
  scale_y_reordered() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_jco(alpha = 0.7) +  
  theme_classic() +
  labs(
       title = "SARS-CoV-2 positive",
    y = NULL, x = "Weighted log-odds ratios (as z scores)") +
  theme(
    strip.text = element_text(size = 13),
    strip.background = element_blank(),
    axis.text = element_text(size = 11.5),
    axis.title.x = element_text(size = 12, vjust = -0.5),
    panel.grid = element_blank(),
    plot.margin = margin(0,0,20,0))

p1 <- (logodds_plot2 + rremove("xlab")) + (logodds_plot1 + rremove("xlab")) +
  #  plot_layout(ncol = 2, nrow = 1) +
    plot_annotation(tag_levels = 'A',
                    caption = "Weighted log-odds ratios (as z scores)") &
    theme(plot.caption = element_text(size = 11, hjust = 0.5, vjust = 0),
          plot.margin = margin(0.5, 1, 0.5, 1, "cm"))

##########################################
## Hierarchical clustering by age group##

cluster_df <- df_lrti %>%
  #  filter(covid==0) %>% 
  dplyr::select(c("age_group","cough", "dyspnoea", "sputum", "wheeze", "pleurisy",
                  "fever", "malaise", "myalgia", "headache",
                  "deterioration", "confusion", "falls")) %>% 
  group_by(age_group) %>% 
  dplyr::summarise(across(everything(), sum))

cluster_df <- data.frame(cluster_df, row.names = "age_group") %>% 
  na.omit() %>% 
  as.matrix()

  # determine optimal no. clusters using elbow method (https://uc-r.github.io/hc_clustering)
fviz_nbclust(cluster_df, FUNcluster=kmeans, k.max = 7) 

  # ggplot version of cluster plot:
cluster_df %>% 
  scale() -> sympts.scale
sympts.scale %>% 
  get_dist(upper = TRUE, diag = TRUE) -> sympts.dist

km.sympts <- kmeans(sympts.scale, centers = 2, nstart = 25)

temp<-stats::prcomp(sympts.scale, scale = FALSE, center = FALSE)
agegrp_scores<-as.data.frame(scores(temp))
agegrp_scores$cluster <- km.sympts$cluster
agegrp_scores$agegrp <- rownames(agegrp_scores)
head(agegrp_scores)

chull(agegrp_scores %>% filter(cluster ==1) %>% select(PC1, PC2) ) # identify points forming perimeter of each group (hulls)
chull(agegrp_scores %>% filter(cluster ==2) %>% select(PC1, PC2) )
grp.1 <- agegrp_scores[agegrp_scores$cluster == 1, ][chull(agegrp_scores %>% filter(cluster ==1) %>% select(PC1, PC2) ), ]  # hull values for cluster 1
grp.2 <- agegrp_scores[agegrp_scores$cluster == 2, ][chull(agegrp_scores %>% filter(cluster ==2) %>% select(PC1, PC2) ), ]  # hull values for cluster 2

all_hulls <- rbind(grp.1,grp.2)
head(all_hulls)

cluster_plot2 <- ggplot(data = agegrp_scores) + 
  geom_point(aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_polygon(data = all_hulls, aes(x = PC1, y = PC2,
                                     fill = as.factor(cluster),
                                     colour =  as.factor(cluster))) + 
  geom_text_repel(aes(x = PC1, y = PC2, 
                      colour = as.factor(cluster),
                      label = agegrp, size = 14))  +
  scale_fill_manual(values = alpha(c("#EFC00099","#0073C299")), 0.25) +
  scale_colour_manual(values = alpha(c("black", "black"), 1)) +
  theme_classic() +
#  labs(title = "       Cluster plot: age groups clustered by symptom profiles") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.grid = element_blank(),
        legend.position = "none", 
        plot.margin = margin(20,0,20,0))

#########################################
## Put all plots together in a matrix ##

blank <- ggplot() + # blank space to insert heatmap in to
# labs(title = "") +
  theme_void() +
  theme()

p2 <- logodds_plot + blank + cluster_plot2 +
  plot_layout(nrow = 3, heights = c(1,2.3,1.2)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold", color = "grey35"),
        plot.caption = element_text(color = "grey68"),
        plot.caption.position = "plot")
