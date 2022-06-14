# Packages
require(hexSticker)
require(emojifont)
require(sysfonts)
require(ggimage)
require(tidyverse)
require(here)

# Set the seed (for highlight)
set.seed(11)

# Get fonts: emojis
load.emojifont("OpenSansEmoji.ttf")

# Font name: Orbitron, Righteous, Limelight
font_name <- "Righteous"

# Get google font
font_add_google(name = font_name)

# URL
link <- "github.com/grinnellm/SpawnIndex"

# Egg image (not sure why the egg emoji doesn't work) from
# https://www.clipartmax.com/middle/m2H7G6K9H7H7m2b1_chicken-eggs-egg-outline/
egg_image <- here("man", "sticker", "egg.png")

# Padding
pad <- 12.5

# Angles
a1 <- c(60, 180, 300) * pi / 180
a2 <- c(0, 120, 240) * pi / 180

# Set up plot
df <- tibble(
  x = c(pad * sin(a1[1]), pad * sin(a1[2]), pad * sin(a1[3])),
  y = c(pad * cos(a1[1]), pad * cos(a1[2]), pad * cos(a1[3])),
  thing = c(egg_image, emoji("fish"), "\u03B8")
)

# Plot
sticker_image <- ggplot(data = df, mapping = aes(x = x, y = y)) +
  geom_image(data = df[1, ], mapping = aes(image = thing), size = 0.35) +
  geom_text(
    data = df[2, ], mapping = aes(label = thing), family = "OpenSansEmoji",
    fontface = "italic", size = pad * 8
  ) +
  # "Monotype Modern" would be a nice font for theta
  geom_text(
    data = df[3, ], mapping = aes(label = thing), fontface = "italic",
    size = pad * 8
  ) +
  expand_limits(
    x = c(min(df$x) - pad, max(df$x) + pad),
    y = c(min(df$y) - pad, max(df$y) + pad)
  ) +
  annotate(
    geom = "segment", colour = "red", size = 0.5,
    x = 0, xend = pad * 1.25 * sin(a2[1]), y = 0, yend = pad * 1.25 * cos(a2[1])
  ) +
  annotate(
    geom = "segment", colour = "red", size = 0.5,
    x = 0, xend = pad * 1.25 * sin(a2[2]), y = 0, yend = pad * 1.25 * cos(a2[2])
  ) +
  annotate(
    geom = "segment", colour = "red", size = 0.5,
    x = 0, xend = pad * 1.25 * sin(a2[3]), y = 0, yend = pad * 1.25 * cos(a2[3])
  ) +
  coord_fixed() +
  theme_void()

# Make the sticker
spawn_index_sticker <- sticker(
  subplot = sticker_image, s_x = 1, s_y = 0.65, s_width = 1.5, s_height = 1.5,
  package = "SpawnIndex", p_size = pad * 4.25, p_color = c("darkblue", "red"),
  p_x = c(1.02, 1), p_y = c(1.38, 1.4), p_family = font_name,
  h_fill = "blue", h_size = 1, h_color = "red",
  url = link, u_y = 0.06, u_size = pad * 0.75, u_family = "mono",
  filename = here("man", "sticker", "sticker.png"), dpi = 750
)
