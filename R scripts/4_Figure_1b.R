chem_pca <- truesoil_metadata_sum %>%
  dplyr::rename("SOM" = "SOM....",
                "C:N" = "C.N",
                "POM/MAOM" = "POM.MAOM",
                "NH4-N" = "NH4.N..mg.L.", "EC" = "EC..mS.m2.",
                "NO3-N" = "NO3.N..mg.L.", "Clay, %" = "Clay....", "Silt, %" = "Silt....", "Sand, %" = "Sand....",
                "N, %" = "TN....", "C, %" = "TC....")


# calculate 
chem_pca_codes <- prcomp(na.omit(chem_pca[,c(8, 12, 13, 14, 21, 22, 23)]), scale = TRUE)
summary(chem_pca_codes)

var_explained <- chem_pca_codes$sdev^2/sum(chem_pca_codes$sdev^2)
var_explained[1:5]

pca_codes_correlation <- data.frame(chem_pca_codes$rotation[,1:2]) %>%
  rownames_to_column() %>%
  dplyr::rename("Variable" = rowname) %>%
  arrange(desc(PC1))
pca_codes_correlation

# PCA and variance explained
pca <- chem_pca_codes
ve <- 100 * pca$sdev^2 / sum(pca$sdev^2)

# identify the samples used in the PCA
pca_data <- chem_pca %>%
  dplyr::select(all_of(c(8, 12, 13, 14, 21, 22, 23))) %>%
  mutate(.original_row = row_number()) %>%
  filter(complete.cases(across(-.original_row)))

# sample scores
lam <- pca$sdev[1:2] * sqrt(nrow(pca$x))

scores <- data.frame(
  PC1 = pca$x[, 1] / lam[1],
  PC2 = pca$x[, 2] / lam[2],
  Sample = chem_pca$Sample[pca_data$.original_row],
  MAP = chem_pca$MAP..mm.[pca_data$.original_row]
)

# variable loadings
load <- as.data.frame(pca$rotation[, 1:2])
names(load) <- c("PC1", "PC2")
load$var <- rownames(load)

# scale loading arrows to fit the sample score cloud
k <- 0.8 * min(
  max(abs(scores$PC1)) / max(abs(load$PC1)),
  max(abs(scores$PC2)) / max(abs(load$PC2))
)

load <- load %>%
  mutate(
    PC1 = PC1 * k,
    PC2 = PC2 * k
  )

# combine sample and loading labels
labs_df <- bind_rows(
  scores %>%
    transmute(
      x = PC1,
      y = PC2,
      label = Sample,
      MAP = MAP,
      kind = "sample"
    ),
  load %>%
    transmute(
      x = PC1,
      y = PC2,
      label = var,
      MAP = NA_real_,
      kind = "loading"
    )
)

# PCA plot
plot_pca_rain <- ggplot() +
  geom_hline(
    yintercept = 0,
    colour = "grey85",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    colour = "grey85",
    linewidth = 0.4
  ) +
  geom_segment(
    data = load,
    aes(
      x = 0,
      y = 0,
      xend = PC1,
      yend = PC2
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "grey40",
    linewidth = 0.5
  ) +
  geom_point(
    data = scores,
    aes(
      x = PC1,
      y = PC2,
      colour = MAP
    ),
    size = 3
  ) +
  geom_text_repel(
    data = labs_df,
    aes(
      x = x,
      y = y,
      label = label,
      colour = MAP,
      fontface = ifelse(kind == "loading", "bold", "plain")
    ),
    size = 4,
    box.padding = 0.2,
    point.padding = 0.2,
    force = 4,
    force_pull = 0.5,
    max.iter = 200000,
    max.time = 30,
    max.overlaps = Inf,
    min.segment.length = 0,
    seed = 1,
    show.legend = FALSE
  ) +
  scale_color_viridis_c(
    direction = -1,
    na.value = "grey40",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      label.theme = element_text(angle = 45),
      label.vjust = 0.8
    )
  ) +
  labs(
    x = sprintf("PC1 (%.2f%%)", ve[1]),
    y = sprintf("PC2 (%.2f%%)", ve[2]),
    colour = "MAP (mm)"
  ) +
  revised_theme +
  theme(
    legend.position = "top",
    plot.margin = margin(l = 15, b = 5, r = 5, t = 5),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.2, "cm")
  ) +
  ggtitle("(b)")