# set up the theme
truesoil_theme <- theme(
  axis.title.y = element_text(size = 12, face = "bold", vjust = 3),
  axis.title.x = element_text(size = 12, face = "bold", vjust = -1),
  panel.background = element_rect(fill = "white", colour = "grey90"),
  strip.text.x = element_text(size = 12, colour = 'black'),
  legend.key.width = unit(0.5, "cm"),
  legend.key.height = unit(0.5, "cm"),
  axis.text = element_text(color = "grey40"),
  axis.ticks = element_blank(),
  strip.text = element_text(colour = "black", size = 12),
  panel.grid = element_line(color = "grey90"),
  panel.grid.major = element_blank(),
  plot.margin = margin(l = 5, b = 5, r = 5, t = 5),
  panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold"),
  strip.background = element_rect(colour = "grey90", fill ="white"),
  legend.position = "right", legend.title = element_text(size = 12, colour = "black"),
  legend.text = element_text(size = 12, color = "black"),
  legend.key = element_rect(fill = NA))

# and some properties
point_size = 3
label_size = 3
padding = 0.4
fontsize = 10

# set up the colour coding
col1 <- "#f94144"
col2 <- "#f3722c" 
col3 <- "#f8961e"
col4 <- "#f9844a" 
col5 <- "#f9c74f"
col6 <- "#90be6d" 
col7 <- "#43aa8b"
col8 <- "#4d908e"
col9 <- "#577590"
col10 <- "#277da1"
col11 <- "#001219"

# establosh the pallette
color_code_MAP <- c(col1, col2, col3, col4, col5, col6, col7, col8, col9, col10)