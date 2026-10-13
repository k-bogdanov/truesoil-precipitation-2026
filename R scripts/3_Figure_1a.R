# import NZ map
nz$geom <- st_transform(nz$geom, "OGC:CRS84")

# transform metadata to plot the map
truesoil_metadata_sum_map <- truesoil_metadata_sum %>%
  mutate(site_code = case_when(
    MAP..mm. == 629 ~ "WA",
    MAP..mm. == 640 ~ "SC",
    MAP..mm. == 630 ~ "TU",
    MAP..mm. == 553 ~ "TA",
    MAP..mm. == 605 ~ "LH",
    MAP..mm. == 1918 ~ "RS",
    MAP..mm. == 4967 ~ "KR",
    MAP..mm. == 2478 ~ "MA",
    MAP..mm. == 953 ~ "MC",
    MAP..mm. == 3439 ~ "KP",
    MAP..mm. == 3158 ~ "HA",
    MAP..mm. == 2779 ~ "KJ",
    MAP..mm. == 902 ~ "TH",
    MAP..mm. == 549 ~ "LI")) %>%
  group_by(site_code) %>%
  dplyr::summarise_at(vars(Longitude, Latitude, MAP..mm.), funs(mean(.)))

# plot
nz_map <- ggplot(nz) +
  geom_sf(fill = "grey92", color = "grey80") +
  geom_point(data = truesoil_metadata_sum_map, aes(x = Longitude, y = Latitude, color = MAP..mm.), size = 3, shape = 4) +
  coord_sf(xlim = c(163.5, 177.5), ylim = c(-40.4, -46.7)) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_text_repel(data = truesoil_metadata_sum_map,
                  aes(x = Longitude, y = Latitude, label = site_code, color = MAP..mm.),
                  size = 5, box.padding = 0.7, max.overlaps = 40, show.legend = FALSE) +
  labs(color = "MAP (mm)", x = "", y = "") +
  revised_theme + 
  theme(legend.position = "none") +
  scale_color_viridis_c(direction = -1) +
  ggtitle("(a)") +
  scale_x_continuous(breaks = c(164, 166, 168, 170, 172, 174, 176),
                     labels = c("164°E", "166°E", "168°E", "170°E", "172°E", "174°E", "176°E"))

nz_map