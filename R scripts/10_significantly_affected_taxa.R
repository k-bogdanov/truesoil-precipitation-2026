# 16S example
# transform fresh
ps_16s_df_fresh <- ps_16S_rare %>% 
  transform_sample_counts(function(x) {x/sum(x)}) %>%
  psmelt() %>%
  mutate(Condition = "Fresh") %>%
  select(Sample, Abundance, Location, MAP..mm., Condition, Kingdom:Species)
# transform dry
ps_16s_df_dry <- ps_Birch_16S_rare %>% 
  transform_sample_counts(function(x) {x/sum(x)}) %>%
  psmelt() %>%
  filter(Water == "Before") %>%
  mutate(Condition = "Dry") %>%
  select(Site, Abundance, Location, MAP..mm., Condition, Kingdom:Species) %>%
  dplyr::rename("Sample" = Site)
#saveRDS(ps_16s_df_dry, "C:/Users/bogda/Documents/PhD/TRUESOIL/rainfall_and_birch/RDS/ps_16s_df_dry.rds")
ps_16s_df_all <- bind_rows(ps_16s_df_fresh, ps_16s_df_dry)

# filter sample names
filter_for_samples <- unique(ps_16s_df_dry$Sample)

# stats
filtered_data <- ps_16s_df_all %>%
  filter(Abundance > 0) %>%
  filter(Sample %in% filter_for_samples) %>%
  group_by(Class, Condition) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  group_by(Class) %>%
  filter(n_distinct(Condition) > 1) %>%
  dplyr::group_by(Sample, Condition, Phylum, Class, MAP..mm.) %>%
  dplyr::summarise(Abundance = sum(Abundance), .groups = "drop")

# Calculate median abundance per class and condition
class_medians <- filtered_data %>%
  group_by(Class, Condition) %>%
  dplyr::summarise(median_abund = median(Abundance), .groups = "drop") %>%
  tidyr::pivot_wider(
    id_cols = Class,
    names_from = Condition,
    values_from = median_abund,
    names_prefix = "median_"
  ) %>%
  mutate(
    direction = case_when(
      median_Dry > median_Fresh ~ "increase",
      median_Dry < median_Fresh ~ "decrease",
      TRUE ~ "no change"
    )
  )

# Run Wilcoxon test and join with direction
wilcox_16S_drying_class <- filtered_data %>%
  group_by(Class) %>%
  filter(n_distinct(Condition) > 1) %>%
  wilcox_test(Abundance ~ Condition, paired = FALSE, alternative = "two.sided") %>%
  adjust_pvalue(method = "fdr") %>% 
  left_join(
    filtered_data %>%
      group_by(Class) %>%
      filter(n_distinct(Condition) > 1) %>%
      wilcox_effsize(Abundance ~ Condition, paired = FALSE, alternative = "two.sided"),
    by = c("Class", ".y.", "group1", "group2")
  ) %>%
  filter(p.adj < 0.05) %>%
  left_join(class_medians, by = "Class") %>%
  select(Class, group1, group2, statistic, p, p.adj,
         effsize, magnitude, direction) %>%
  arrange(desc(effsize))

wilcox_16S_drying_class

unique(wilcox_16S_drying_class$Class)

# filter
only_sig_16S <- ps_16s_df_all %>%
  filter(Abundance > 0) %>%
  filter(Sample %in% filter_for_samples) %>%
  filter(Class %in% wilcox_16S_drying_class$Class) %>%
  filter(Class %in% affected_genera$Class) %>%
  #filter(Class %in% taxa$class) %>% # this is from MAGs dataset
  mutate(Condition = factor(Condition, levels = c("Fresh", "Dry")))

unique(affected_genera$Genus)

# summarize

only_sig_16S_sum <- only_sig_16S %>%
  dplyr::group_by(Sample, Condition, Phylum, Class, MAP..mm.) %>%
  dplyr::summarise(Abundance = sum(Abundance), .groups = "drop") %>%
  mutate(Condition = factor(Condition, levels = c("Fresh", "Dry")),
         Phylum = str_replace_all(Phylum, "Candidatus", "Cand."),
         Class = str_replace_all(Class, "terrestrial", "terr."),
         Phylum = str_replace_all(Phylum, "Unknown Bacteria \\(Kingdom\\)", "Unknown phylum"),
         Class = str_replace_all(Class, "Unknown Bacteria \\(Kingdom\\)", "Unknown class"),
         Class = str_replace_all(Class, "Unknown MBNT15 \\(Phylum\\)", "Unknown class"),
         Class = str_replace_all(Class, "Unknown Latescibacterota \\(Phylum\\)", "Unknown class"))

#unique(only_sig_16S$Class)

# plot
sig_affected_16S_plot <- ggplot(only_sig_16S_sum, aes(x = Condition, y = Abundance * 100, color = MAP..mm.)) +
  geom_point(aes(color = MAP..mm.)) +
  geom_line(aes(group = interaction(Sample, Class))) +
  geom_boxplot(aes(group = Condition), color = "grey50", outlier.size = 0, outlier.alpha = 0, outlier.shape = NA) +
  facet_wrap(~ Phylum + Class, scales = "free_y") +
  revised_theme +
  #scale_color_viridis_c(direction = -1) +
  scale_color_viridis_c(direction = -1, guide = guide_colorbar(title.position = "top", title.hjust = 0.5, label.theme = element_text(angle = 45), direction = "horizontal", label.vjust = 0.8)) +
  theme(strip.text = element_text(face = "italic"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 17),
        strip.text.x = element_text(color = "black", face = "italic", size = 14),
        plot.margin = margin(l = 15, b = 5, t = 5, r = 5),
        legend.position = "top",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.2, "cm")) +
  #legend.position = "right") +
  labs(y = "Abundance of ASVs (%)", color = "MAP (mm)")

sig_affected_16S_plot
