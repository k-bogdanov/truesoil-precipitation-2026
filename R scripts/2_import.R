# set seed for future
set.seed(1509)

# import rarefied ps objects from fresh soils
ps_16S_rare <- readRDS("~/truesoil_rain_16s_phyloseq_rare.rds")
ps_ITS_rare <- readRDS("~/truesoil_rain_ITS_phyloseq_rare.rds")

sample_names(ps_16S_rare) <- sample_data(ps_16S_rare)$Sample
sample_names(ps_ITS_rare) <- sample_data(ps_ITS_rare)$Sample

# assign MAP
ps_16S_rare <- ps_16S_rare %>%
  ps_mutate(MAP..mm. = case_when(
    Sample  %in% c("WA1", "WA2") ~ 629,
    Sample  %in% c("SC1", "SC2") ~ 640,
    Sample  %in% c("TU1", "TU2") ~ 630,
    Sample  %in% c("TA1", "TA2") ~ 553,
    Sample  %in% c("LH1", "LH2") ~ 605,
    Sample  %in% c("RS1", "RS2") ~ 1918,
    Sample  %in% c("KR1", "KR2") ~ 4967,
    Sample  %in% c("MA1", "MA2") ~ 2478,
    Sample  %in% c("MC1", "MC2") ~ 953,
    Sample  %in% c("KP1", "KP2") ~ 3439,
    Sample  %in% c("HA1", "HA2") ~ 3158,
    Sample  %in% c("KJ1", "KJ2", "KJ3") ~ 2779,
    Sample  %in% c("TH1", "TH2") ~ 902,
    Sample %in% c("LI1") ~ 549))

ps_ITS_rare <- ps_ITS_rare %>%
  ps_mutate(MAP..mm. = case_when(
    Sample  %in% c("WA1", "WA2") ~ 629,
    Sample  %in% c("SC1", "SC2") ~ 640,
    Sample  %in% c("TU1", "TU2") ~ 630,
    Sample  %in% c("TA1", "TA2") ~ 553,
    Sample  %in% c("LH1", "LH2") ~ 605,
    Sample  %in% c("RS1", "RS2") ~ 1918,
    Sample  %in% c("KR1", "KR2") ~ 4967,
    Sample  %in% c("MA1", "MA2") ~ 2478,
    Sample  %in% c("MC1", "MC2") ~ 953,
    Sample  %in% c("KP1", "KP2") ~ 3439,
    Sample  %in% c("HA1", "HA2") ~ 3158,
    Sample  %in% c("KJ1", "KJ2", "KJ3") ~ 2779,
    Sample  %in% c("TH1", "TH2") ~ 902,
    Sample %in% c("LI1") ~ 549))

# extract metadata from the ps (summarised already)
truesoil_metadata_sum <- data.frame(sample_data(ps_16S_rare))

# order for easy plotting
ordered_by_rain <- c(truesoil_metadata_sum %>%
                       arrange(MAP..mm.) %>%
                       dplyr::select(Location) %>%
                       unique(.))

# establish MAP gradient order
ps_16S_rare@sam_data$Location_code <- substr(ps_16S_rare@sam_data$Sample, 1, 2)
ps_16S_rare@sam_data$Location_code <- factor(ps_16S_rare@sam_data$Location_code, levels = c("LI", "TA", "LH", "WA", "TU", "SC", "TH", "MC", "RS", "MA", "KJ", "HA", "KP", "KR"))

ps_ITS_rare@sam_data$Location_code <- substr(ps_ITS_rare@sam_data$Sample, 1, 2)
ps_ITS_rare@sam_data$Location_code <- factor(ps_ITS_rare@sam_data$Location_code, levels = c("LI", "TA", "LH", "WA", "TU", "SC", "TH", "MC", "RS", "MA", "KJ", "HA", "KP", "KR"))


# now, import ps objects obtained from dry soils
ps_Birch_16S <- readRDS("~/ps_Birch_16S.rds") %>%
  name_na_taxa(na_label = "Unknown <tax> (<rank>)")

ps_Birch_ITS <- readRDS("~/ps_Birch_ITS.rds") %>%
  name_na_taxa(na_label = "Unknown <tax> (<rank>)")

# get rid of ugly "_" before taxa names
tax_table(ps_Birch_ITS)[, colnames(tax_table(ps_Birch_ITS))] <- gsub(tax_table(ps_Birch_ITS)[, colnames(tax_table(ps_Birch_ITS))], pattern = "[a-z]__", replacement = "")

# assign MAP
ps_Birch_16S <- ps_Birch_16S %>%
  ps_mutate(MAP..mm. = case_when(
    Site  %in% c("WA1", "WA2") ~ 629,
    Site  %in% c("SC1", "SC2") ~ 640,
    Site  %in% c("TU1", "TU2") ~ 630,
    Site  %in% c("TA1", "TA2") ~ 553,
    Site  %in% c("LH1", "LH2") ~ 605,
    Site  %in% c("RS1", "RS2") ~ 1918,
    Site  %in% c("KR1", "KR2") ~ 4967,
    Site  %in% c("MA1", "MA2") ~ 2478,
    Site  %in% c("MC1", "MC2") ~ 953,
    Site  %in% c("KP1", "KP2") ~ 3439,
    Site  %in% c("HA1", "HA2") ~ 3158,
    Site  %in% c("KJ1", "KJ2", "KJ3") ~ 2779,
    Site  %in% c("TH1", "TH2") ~ 902))

ps_Birch_ITS <- ps_Birch_ITS %>%
  ps_mutate(MAP..mm. = case_when(
    Site  %in% c("WA1", "WA2") ~ 629,
    Site  %in% c("SC1", "SC2") ~ 640,
    Site  %in% c("TU1", "TU2") ~ 630,
    Site  %in% c("TA1", "TA2") ~ 553,
    Site  %in% c("LH1", "LH2") ~ 605,
    Site  %in% c("RS1", "RS2") ~ 1918,
    Site  %in% c("KR1", "KR2") ~ 4967,
    Site  %in% c("MA1", "MA2") ~ 2478,
    Site  %in% c("MC1", "MC2") ~ 953,
    Site  %in% c("KP1", "KP2") ~ 3439,
    Site  %in% c("HA1", "HA2") ~ 3158,
    Site  %in% c("KJ1", "KJ2", "KJ3") ~ 2779,
    Site  %in% c("TH1", "TH2") ~ 902))

# rarefy
ps_Birch_16S_rare <- rarefy_even_depth(ps_Birch_16S, 5000, rngseed = TRUE)
ps_Birch_ITS_rare <- rarefy_even_depth(ps_Birch_ITS, 5000, rngseed = TRUE)

# establish MAP gradient order
ps_Birch_16S_rare@sam_data$Location_code <- substr(ps_Birch_16S_rare@sam_data$Site, 1, 2)
ps_Birch_16S_rare@sam_data$Location_code <- factor(ps_Birch_16S_rare@sam_data$Location_code, levels = c("TA", "LH", "WA", "TU", "SC", "TH", "MC", "RS", "MA", "KJ", "HA", "KP"))

ps_Birch_ITS_rare@sam_data$Location_code <- substr(ps_Birch_ITS_rare@sam_data$Site, 1, 2)
ps_Birch_ITS_rare@sam_data$Location_code <- factor(ps_Birch_ITS_rare@sam_data$Location_code, levels = c("TA", "LH", "WA", "TU", "SC", "TH", "MC", "RS", "MA", "KJ", "HA", "KP"))