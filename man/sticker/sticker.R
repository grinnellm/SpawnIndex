# Packages
require(hexSticker)
require(emojifont)
require(sysfonts)
require(tidyverse)
require(png)
require(cowplot)

# Set the seed (for highlight)
set.seed(11)

# Get fonts: emojis
load.emojifont("OpenSansEmoji.ttf")

# Font name: Orbitron, Righteous, Limelight
fName <- "Righteous"

# Get google font
font_add_google(name = fName)

# URL
link <- "github.com/grinnellm/SpawnIndex"

# Egg image (not sure why the egg emoji doesn't work)
eggs <- readPNG(source = file.path("man", "sticker", "egg.png"))

# Padding
pad <- 10

# # Point cloud
# dat <- tibble(x = rnorm(n = pad * 500, mean = 0, sd = pad / 2),
#               y = rnorm(n = pad * 500, mean = 0, sd = pad / 2),
#               label = 0)

# Angles
a1 <- c(60, 180, 300) * pi / 180
a2 <- c(0, 120, 240) * pi / 180

# Set up plot
df <- tibble(
  x = c(pad * sin(a1[1]), pad * sin(a1[2]), pad * sin(a1[3])),
  y = c(pad * cos(a1[1]), pad * cos(a1[2]), pad * cos(a1[3])),
  label = c("", emoji("fish"), "\u03B8")
)

# Plot
hImage <- ggplot(data = df, mapping = aes(x = x, y = y, label = label)) +
  # geom_point(data = dat, colour = "transparent", fill = "white", alpha = 0.01) +
  geom_text(data = df[1:2, ], family = "OpenSansEmoji", size = pad * c(7, 8)) +
  geom_text(data = df[3, ], fontface = "italic", size = pad * 8) +
  draw_image(eggs, width = 10, height = 10, x = df$x[1]/3, y = df$y[1]/5) +
  expand_limits(
    x = c(min(df$x) - pad, max(df$x) + pad),
    y = c(min(df$y) - pad, max(df$y) + pad)
  ) +
  annotate(
    geom = "segment", colour = "red",
    x = 0, xend = pad * 1.25 * sin(a2[1]), y = 0, yend = pad * 1.25 * cos(a2[1])
  ) +
  annotate(
    geom = "segment", colour = "red",
    x = 0, xend = pad * 1.25 * sin(a2[2]), y = 0, yend = pad * 1.25 * cos(a2[2])
  ) +
  annotate(
    geom = "segment", colour = "red",
    x = 0, xend = pad * 1.25 * sin(a2[3]), y = 0, yend = pad * 1.25 * cos(a2[3])
  ) +
  coord_fixed() +
  theme_void()

# Make the sticker
stickerSI <- sticker(
  subplot = hImage, s_x = 1, s_y = 0.65, s_width = 1.5, s_height = 1.5,
  package = "SpawnIndex", p_size = pad * 4.25, p_color = c("darkblue", "red"),
  p_x=c(1.02, 1), p_y = c(1.38, 1.4), p_family = fName,
  spotlight = FALSE, l_alpha = 0.4, l_x = 1.06, l_y = 0.735, l_width = 5,
  l_height = 5,
  h_fill = "blue", h_size = 1, h_color = "red",
  url = link, u_y = 0.06, u_size = pad * 0.75, u_family = "mono",
  dpi = 600, filename = file.path("man", "sticker", "sticker.png")
)

# # Reorder the layers to put highlight on bottom
# stickerSI$layers <- stickerSI$layers[c(1, 4, 2, 3, 5, 6)]

# Re-save the sticker
# save_sticker(file.path("man", "sticker", "sticker.png"), stickerSI, dpi = 600)
