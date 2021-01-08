# Packages
require(hexSticker)
require(emojifont)
require(tidyverse)

# Get fonts
load.emojifont('OpenSansEmoji.ttf')

# Padding
pad <- 10

# Set up plot
df <- tibble( x=c(-pad, 0, pad),
              y=c(0, -pad * 1.3, 0),
              label=c(emoji('fried_egg'), emoji('fish'), "\u03B8") )

# Plot
hImage <- ggplot( data=df, aes(x = x, y = y, label = label)) +
  geom_text(family = "OpenSansEmoji", size = pad*2) +
  expand_limits(x = c(min(df$x) - pad, max(df$x) + pad),
                y = c(min(df$y) - pad, max(df$y) + pad)) +
  annotate(geom = "segment", x = 0, xend = 0, y = 0, yend = pad) +
  annotate(geom = "segment", x = 0, xend = -pad, y = 0, yend = -pad) +
  annotate(geom = "segment", x = 0, xend = pad, y = 0, yend = -pad) +
  coord_fixed() +
  theme_void()

# Make the sticker
stickerSI <- sticker(subplot = hImage,
                     s_x = 1, s_y = 0.65, s_width = 1.5, s_height = 1.5,
                     package = "SpawnIndex", p_size = 10, p_color = "red",
                     spotlight = TRUE, l_alpha = 0.35,
                     l_x = 1, l_y = 0.7, l_width = 5, l_height = 5,
                     h_fill = "blue", h_size = 2, h_color = "red",
                     dpi = 150, filename = file.path("sticker", "sticker.png"))
