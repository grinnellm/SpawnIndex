# Packages
require(hexSticker)
require(emojifont)
require(sysfonts)
require(tidyverse)

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

# Padding
pad <- 10

# Angles
a1 <- c(60, 180, 300) * pi / 180
a2 <- c(0, 120, 240) * pi / 180

# Set up plot
df <- tibble(
  x = c(pad * sin(a1[1]), pad * sin(a1[2]), pad * sin(a1[3])),
  y = c(pad * cos(a1[1]), pad * cos(a1[2]), pad * cos(a1[3])),
  label = c(emoji("fried_egg"), emoji("fish"), "\u03B8")
)

# Plot
hImage <- ggplot(data = df, mapping = aes(x = x, y = y, label = label)) +
  geom_text(data = df[1:2, ], family = "OpenSansEmoji", size = pad * c(7, 8)) +
  geom_text(data = df[3, ], fontface = "italic", size = pad * 8) +
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
  subplot = hImage,
  s_x = 1, s_y = 0.65, s_width = 1.5, s_height = 1.5,
  package = "SpawnIndex", p_size = pad * 4.25, p_color = c("darkblue", "red"),
  p_x=c(1.02, 1), p_y = c(1.38, 1.4),
  p_family = fName, spotlight = TRUE, l_alpha = 0.35,
  l_x = 1, l_y = 0.7, l_width = 5, l_height = 5,
  h_fill = "blue", h_size = 1, h_color = "red",
  url = link, u_y = 0.06, u_size = pad * 0.75,
  dpi = 600, filename = file.path("man", "sticker", "sticker.png")
)

# Reorder the layers to put highlight on bottom
stickerSI$layers <- stickerSI$layers[c(1, 4, 2, 3, 5, 6)]

# Re-save the sticker
save_sticker(file.path("man", "sticker", "sticker.png"), stickerSI, dpi = 600)
