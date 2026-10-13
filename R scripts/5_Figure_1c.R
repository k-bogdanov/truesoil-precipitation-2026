list_csv_files <- list.files(path = "~/truesoil_birch_effect_2025/", pattern = "*.csv")

# CO2 data

licor_data <- readr::read_delim(list_csv_files, id = "file_name", delim = " ")

# pattern and replacement

licor_data_f <- licor_data %>%
  dplyr::rename(Time = "Time(H:M:S)", CO2 = "CO2(ppm)", Ring = "file_name") %>%
  mutate(Ring = str_replace_all(Ring, replacement_string)) %>%
  group_by(Ring) %>%
  mutate(CO2 = case_when(CO2 < 0 ~ CO2 == 0, TRUE ~ CO2)) %>%
  mutate(Mins = as.numeric(difftime(Time, dplyr::first(Time), units = "mins")),
         MAP = case_when(
           Ring == "C" ~ NA,
           Ring %in% c("WA1", "WA2") ~ 629,
           Ring %in% c("SC1", "SC2") ~ 640,
           Ring %in% c("TU1", "TU2") ~ 630,
           Ring %in% c("TA1", "TA2") ~ 553,
           Ring %in% c("LH1", "LH2") ~ 605,
           Ring %in% c("RS1", "RS2") ~ 1918,
           Ring %in% c("KR1", "KR2") ~ 4967,
           Ring %in% c("MA1", "MA2") ~ 2478,
           Ring %in% c("MC1", "MC2") ~ 953,
           Ring %in% c("KP1", "KP2") ~ 3439,
           Ring %in% c("HA1", "HA2") ~ 3158,
           Ring %in% c("KJ1") ~ 2779,
           Ring %in% c("TH1", "TH2", "TH3") ~ 902
         ),
         Texture = case_when(
           Ring %in% c("WA1") ~ "Silt",
           Ring %in% c("WA2") ~ "Silt Loam",
           Ring %in% c("SC1", "SC2") ~ "Silt Loam",
           Ring %in% c("TU1", "TU2") ~ "Silt",
           Ring %in% c("TA1", "TA2") ~ "Silt Loam",
           Ring %in% c("LH1", "LH2") ~ "Silt Loam",
           Ring %in% c("RS1", "RS2") ~ "Silt Loam",
           Ring %in% c("KR1", "KR2") ~ "Sandy Loam",
           Ring %in% c("MA1", "MA2") ~ "Silt Loam",
           Ring %in% c("MC1", "MC2") ~ "Silt Loam",
           Ring %in% c("KP1", "KP2") ~ "Sandy Loam",
           Ring %in% c("HA1") ~ "Silt",
           Ring %in% c("HA2") ~ "Silt",
           Ring %in% c("KJ1") ~ "Sandy Loam",
           Ring %in% c("TH1") ~ "Silt Loam",
           Ring %in% c("TH2") ~ "Silt"
         ),
         Weight = case_when(
           Ring == "WA1" ~ 125,
           Ring == "WA2" ~ 124.87,
           Ring == "SC1" ~ 125.16,
           Ring == "SC2" ~ 125.23,
           
           Ring == "TU1" ~ 125,
           Ring == "TU2" ~ 125.3,
           
           Ring == "TA1" ~ 125.16,
           Ring == "TA2" ~ 125.73,
           
           Ring == "LH1" ~ 125.07,
           Ring == "LH2" ~ 125.14,
           
           Ring == "RS1" ~ 125.09,
           Ring == "RS2" ~ 125.53,
           
           Ring == "KR1" ~ 125.25,
           Ring == "KR2" ~ 122.58,
           
           Ring == "MA1" ~ 125,
           Ring == "MA2" ~ 125,
           
           Ring == "MC1" ~ 124.99,
           Ring == "MC2" ~ 125.26,
           
           Ring == "KP1" ~ 125.3,
           Ring == "KP2" ~ 125.29,
           
           Ring == "HA1" ~ 112.97,
           Ring == "HA2" ~ 125.16,
           
           Ring == "KJ1" ~ 125.47,
           
           Ring == "TH1" ~ 125.22,
           Ring == "TH2" ~ 125.2,
           
           Ring == "C" ~ 1)) %>%
  filter(Mins %in% c(1:60))

licor_data_f_ug <- read.csv("~/licor_data_f.csv", sep = ",", dec = ".")

emissions <- ggplot(licor_data_f %>% filter(Ring != "TH3"), aes(x = Mins, y = CO2, color = MAP, shape = Ring)) +
  geom_vline(xintercept = 5, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = 530, linetype = "dashed", colour = "grey40") +
  geom_line() +
  scale_color_viridis_c(direction = -1) +
  geom_label(aes(label = Ring),
             data = licor_data_f %>% filter(Mins == max(Mins), Ring != "TH3"),
             nudge_x = 0.2,
             size = 4) +
  revised_theme +
  labs(color = "MAP (mm)", x = "Time (min)", y = "") +
  ylab(expression(bold(CO[2]~"concentration (ppm)"))) +
  scale_x_continuous(breaks = c(5, 15, 30, 45, 60)) +
  scale_y_continuous(breaks = c(530, 1000, 1500, 2000, 2500, 3000)) +
  theme(legend.position = "none") +
  ggtitle("(c)")

emissions