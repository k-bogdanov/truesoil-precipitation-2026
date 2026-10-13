# combine dry and fresh ps objects
dataset1 <- ps_16S_rare %>% tax_fix(unknowns = c("Incertae Sedis"))
dataset2 <- ps_Birch_16S_rare %>% tax_fix(unknowns = c("Incertae Sedis"))

# first make sure the rank_names are the same format
colnames(dataset1@tax_table) <- rank_names(dataset1) %>% tolower()
colnames(dataset2@tax_table) <- rank_names(dataset2) %>% tolower()

# remove ranks that are not shared
dataset1 <- dataset1 %>% tax_mutate(kingdom = NULL)
dataset2 <- dataset2 %>% tax_mutate(kingdom = NULL)

# create a dataset ID variable to distinguish the two datasets after merging
dataset1 <- dataset1 %>% ps_mutate(dataset = "Fresh soil")
dataset2 <- dataset2 %>% ps_mutate(dataset = "Dry soil")

# aggregate and merge
combined <- phyloseq::merge_phyloseq(
  dataset1 %>% tax_agg("class") %>% ps_get(),
  dataset2 %>% tax_agg("class") %>% ps_get()
)

combined@sam_data$dataset <- factor(combined@sam_data$dataset, levels = c("Fresh soil", "Dry soil"))

# Extract as data frame
sample_df <- data.frame(combined_16S@sam_data)

# Modify the data frame
sample_df <- sample_df %>%
  mutate(Location_code = case_when(
    is.na(Location_code) ~ "KR",
    TRUE ~ Location_code
  ))

# Put back into phyloseq object
sample_data(combined_16S) <- sample_df

# trying to add MAP data to labels
combined_16S@sam_data$Location_code <- paste0(combined_16S@sam_data$Location_code, " ", combined_16S@sam_data$MAP..mm.)
unique(combined_16S@sam_data$Location_code)

combined_16S@sam_data$Location_code <- factor(combined_16S@sam_data$Location_code, levels = c("LI 549", "TA 553", "LH 605", "WA 629", "TU 630", "SC 640", "TH 902", "MC 953", "RS 1918", "MA 2478", "KJ 2779", "HA 3158", "KP 3439", "KR 4967"))

combined_16S_merged <- merge_treatments(combined_16S, c("Location_code", "dataset"))

combined__16S_merged_class <- combined_16S_merged %>%
  ps_select(Location_code, Location_code_dataset, dataset) %>% 
  merge_samples2(group = "Location_code_dataset", funs = list()) %>%
  tax_glom(taxrank = "class")

# plot
combined_16S_plot <- combined__16S_merged_class %>%
  tax_fix() %>%
  tax_filter(
    tax_level = "class", min_prevalence = 0.001,
    prev_detection_threshold = 100
  ) %>%
  comp_barplot("class", n_taxa = 9, merge_other = T, bar_width = 1,
               bar_outline_colour = NA, x = "Location_code", label = "Location_code", facet_by = "dataset", palette = taxa_pal_9) +
  revised_theme +
  theme(legend.text = element_text(face = "italic"), legend.position = "right", legend.title = element_blank(), axis.title.y = element_text(face = "bold", size = 16),
        axis.title.x = element_text(face = "bold", size = 16), axis.text.x = element_text(size = 11)) +
  labs(x = "Site & MAP (mm)", y = "Relative abundance") +
  scale_x_discrete(labels = scales::label_wrap(2)) +
  ggtitle("Prokaryotes")

# replot normally
combined_16S_plot$layers[[1]] <- NULL
combined_16S_plot <- combined_16S_plot +
  geom_col(position = ggplot2::position_stack(reverse = TRUE), width = 1, color = "black") +
  theme(panel.border = element_blank(), panel.background = element_blank(), plot.margin = margin(l = 10, t = 5, r = 5, b = 15))
combined_16S_plot

# get a table from this
combined_16S_plot$data %>%
  select(OTU, Abundance, Location_code, dataset) %>%
  group_by(dataset) %>%
  arrange(desc(Abundance), desc(dataset)) %>%
  mutate(Abundance = round(Abundance, 2)*100) %>%
  ungroup() %>%
  group_by(OTU, dataset) %>%
  dplyr::summarise(
    across(
      Abundance,
      list(median = ~median(.x, na.rm = TRUE), 
           IQR = ~IQR(.x, na.rm = TRUE)))) %>%
  dplyr::rename("Class" = OTU, "Median abundance (%)" = Abundance_median, "IQR (%)" = Abundance_IQR)