hImage <- file.path("sticker", "HerringColourDFO.jpg")
siURL <- "https://github.com/grinnellm/SpawnIndex"
stickerSI <- sticker(subplot = hImage, package = "SpawnIndex", p_size = 20,
                     s_x = 1, s_y = 0.8, s_width = 0.9, h_fill = "white",
                     p_color = "red", h_color = "black", h_size = 6,
                     spotlight = TRUE, dpi = 600, u_size = nchar(siURL)/11,
                     url = siURL, white_around_sticker = TRUE,
                     filename = file.path("sticker", "SpawnIndex-sticker.png"))
print(stickerSI)
