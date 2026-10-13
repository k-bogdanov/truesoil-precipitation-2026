max_emissions <- readRDS("~/max_emissions.rds")

max_emissions_summ <- max_emissions %>%
  group_by(MAP) %>%
  dplyr::summarise_at(vars(CO2max, CO2_C_ug_g), funs(median(.))) %>%
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
    MAP == 902 ~ "TH"))

# at site level
polynomial_curve <- ggplot(max_emissions_summ, aes(
  x = MAP,
  y = CO2_C_ug_g
)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "black", se = F) +
  geom_point(aes(color = MAP), size = 3) +
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 3000, size = 5) +
  scale_color_viridis_c(direction = -1) +
  revised_theme +
  labs(color = "MAP (mm)", x = "MAP (mm)") +
  ylab(expression(bold(CO[2]~"C (µg C "*g^{-1}*")"))) +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ggtitle("(d)") +
  geom_text_repel(aes(label = site_name, color = MAP), size = 5, box.padding = 0.1, show.legend = FALSE)