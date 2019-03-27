library(extrafont)
library(hexSticker)

# run README.Rmd first

p <- bullet_scores$data[[2]]$aligned[[9]]$bullets %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = sig1), size=.75, linetype=2, colour = "grey90") +
  geom_line(aes(y = sig2), size=.75, linetype=3, colour = "grey80") +
  theme_void() +
  theme_transparent()

p <- ggplot() + theme_void() + theme_transparent()

sticker(p, s_x = 1, s_y = 1.25,
        s_width = 1.75, s_height = 1,
        package = "", filename="x3ptools-noname.png",
        p_x = 1, p_y = .65, p_color = "grey20",
        h_color = "#941A1D", h_fill = "grey90")

######################

hexagon(size = 1.2, fill = "#FFFFFF", color = "#b30000") +
 geom_pkgname("csafe", x = 1, y = .8, color = "#b30000",
             family = "Wingdings", size = 36) +
  geom_pkgname("Center for Statistics Application and Forensic Evidence",
               x = 1, y = .8, color = "#b30000",
               family = "Sand", size = 8)

