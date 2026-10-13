cn_data <- read.csv("~/total_cn_birch.csv")
# at site level

all_data_shifts_plot <- cn_data %>%
  filter(Time_Water %in% c("Before", "After"),
         !Sample %in% c("KJ2", "KJ3")) %>%
  mutate(
    C_ug = C_percent/100*1000*1000,
    Time_Water = factor(Time_Water, levels = c("Before", "After"))
  ) %>%
  select(Sample, Time_Water, C_ug) %>%
  group_by(Sample, Time_Water) %>%
  mutate(Sample = paste0(Sample, "_", row_number())) %>%
  ungroup() %>%
  gather(Variable, value, 3, factor_key = F) %>%
  spread(key = Time_Water, value = value) %>%
  filter(!grepl("AF", Sample)) %>%
  mutate(Shift = After - Before) %>%
  mutate(Sample = sub("_[0-9]+$", "", Sample)) %>%
  left_join(metadata_all %>%
              select("MAP (mm)", "Sample"), by = "Sample") %>%
  dplyr::rename("MAP" = "MAP (mm)") %>%
  group_by(MAP) %>%
  dplyr::summarise_at(vars(Shift), funs(median(.))) %>%
  mutate(site_name = case_when(
    MAP == 629 ~ "WA",
    MAP == 640 ~ "SC",
    MAP == 630 ~ "TU",
    MAP == 553 ~ "TA",
    MAP == 605 ~ "LH",
    MAP == 1918 ~ "RS",
    MAP == 4967 ~ "KR",
    MAP == 2478 ~ "MA",
    MAP == 953 ~ "MC",
    MAP == 3439 ~ "KP",
    MAP == 3158 ~ "HA",
    MAP == 2779 ~ "KJ",
    MAP == 902 ~ "TH")) %>%
  ggplot(aes(
    x = MAP,
    y = Shift
  )) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), color = "grey40", linetype = "dashed") +
  geom_point(aes(color = MAP), position = position_jitter(width = 100, seed = 15), size = 3) +
  stat_cor(method = "spearman", cor.coef.name = "rho", p.digits = 1, label.x = 1000, size = 5) +
  scale_color_viridis_c(direction = -1) +
  revised_theme +
  labs(color = "MAP (mm)", x = "MAP (mm)") +
  ylab(expression(bold("Shift in TC (µg C "*g^{-1}*")"))) +
  theme(legend.position = "none") +
  ggtitle("(e)") +
  geom_text_repel(aes(label = site_name, color = MAP), size = 5, box.padding = 0.1, show.legend = FALSE)
all_data_shifts_plot