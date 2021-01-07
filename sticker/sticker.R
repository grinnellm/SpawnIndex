# Packages
require(hexSticker)

# Image
hImage <- file.path("sticker", "cube.jpg")

# URL
siURL <- "https://github.com/grinnellm/SpawnIndex"

# Make the sticker
stickerSI <- sticker(subplot = hImage, package = "SpawnIndex", p_size = 12,
                     p_y = 1.6, s_x = 1, s_y = 0.9, s_width = 0.5,
                     h_fill = "white", p_color = "red", h_color = "red",
                     h_size = 1, dpi = 300, u_size = nchar(siURL)/10.5,
                     url = siURL,
                     filename = file.path("sticker", "sticker.png"))

# Display the sticker
print(stickerSI)
