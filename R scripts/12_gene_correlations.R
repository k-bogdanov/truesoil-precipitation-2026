hits_and_properties$site <- sub("[0-9]_[0-9]+$", "", hits_and_properties$sample)

hits_and_properties_sum <- hits_and_properties %>%
  group_by(site, database_clean, MAP) %>%
  dplyr::summarise(median_rpm = median(rpm))
hits_and_properties_sum

mm <- hits_and_properties_sum %>%
  mutate(database_clean = str_extract(database_clean, "(?<=\\().+?(?=\\))")) %>%
  filter(database_clean %in% c("coxA", "cydA", "cyoA", "ccoN", "sdhA", "nuoF", "atpA",
                               "narG", "napA", "nirK", "norB",)) %>%
  ggplot(aes(x = MAP, y = median_rpm, color = MAP)) +
  geom_point(aes(color = MAP), size = 3) +
  geom_smooth(method = "lm", formula = y ~ x, color = "grey40", se = F, linetype = "dashed") +
  labs(color = "MAP (mm)", x = "MAP (mm)", y = "RPKM") +
  stat_cor(method = "spearman", cor.coef.name = "rho", p.digits = 1, label.x.npc = 0.3, label.y.npc = 0.1) +
  geom_text_repel(aes(label = site, color = MAP), size = 3, box.padding = 0.1, show.legend = FALSE) +
  facet_wrap(~ database_clean, scales = "free_y") +
  revised_theme +
  guides(shape = guide_legend(override.aes = list(color="black"))) +
  scale_color_viridis_c(direction = -1, guide = guide_colorbar(title.position = "top", title.hjust = 0.5, label.theme = element_text(angle = 45), direction = "horizontal", label.vjust = 0.8)) +
  theme(strip.text = element_text(face = "italic"),
        axis.title.x = element_blank(),
        #axis.text.x = element_text(size = 17),
        strip.text.x = element_text(color = "black", face = "italic", size = 14),
        plot.margin = margin(l = 15, b = 10, t = 10, r = 15),
        legend.position = c(0.89, 0.1),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.2, "cm")) +
  ggtitle("9 of 10 non-zero correlations matched the predicted direction;\none-tailed exact binomial test, p = 0.011")

mm