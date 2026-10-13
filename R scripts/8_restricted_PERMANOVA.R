# restricted PERMANOVA
df_ado = as(sample_data(ps_16S_rare), "data.frame")

# calculate Bray distance matrix
ado = phyloseq::distance(ps_16S_rare, "bray")

# observed statistic
obs_R2 <- adonis2(ado ~ MAP..mm., data = df_ado)$R2[1]

# one row per Location
loc_map <- unique(df_ado[, c("Location", "MAP..mm.")])

perm_R2 <- numeric(n_perm)
for (i in seq_len(n_perm)) {
  shuffled_MAP <- sample(loc_map$MAP..mm.)        
  names(shuffled_MAP) <- loc_map$Location
  df_perm <- df_ado
  df_perm$MAP_perm <- shuffled_MAP[df_perm$Location]
  perm_R2[i] <- adonis2(ado ~ MAP_perm, data = df_perm)$R2[1]
}

p_val <- (sum(perm_R2 >= obs_R2) + 1) / (n_perm + 1)