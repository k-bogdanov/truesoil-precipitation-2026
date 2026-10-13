# same code for all data sets
truesoil_rainfall_16s_NMDS.ord <- ordinate(ps_16S_rare, "NMDS", "bray")
truesoil_rainfall_16s_NMDS.ord

stressplot(truesoil_rainfall_16s_NMDS.ord)

# create label
ado_label_16S_rain = grobTree(textGrob("MAP: ns\nStress: 0.16", x = 0.05,  y = 0.90, hjust = 0, gp = gpar(col = "black", fontsize = fontsize, fontface = "italic")))

# rainfall
Fresh_16s_NMDS <- plot_ordination(ps_16S_rare, truesoil_rainfall_16s_NMDS.ord, color = "MAP..mm.") +
  geom_point(size = 3) + 
  revised_theme + 
  annotation_custom(ado_label_16S_rain) +
  scale_color_viridis_c(direction = -1) +
  geom_text_repel(aes(label = Sample, color = MAP..mm.), size = label_size, box.padding = padding) +
  labs(x = "", y = "Prokaryotes") +
  ggtitle("Fresh soil") +
  xlim(-0.7, 0.7) +
  ylim(-0.7, 0.7) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "none")