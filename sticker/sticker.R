herring <- file.path("sticker", "HerringColourDFO.jpg")
stickerSI <- sticker(subplot = herring, package = "SpawnIndex", p_size = 20,
                     s_x = 1, s_y = 0.8, s_width = 0.9, h_fill = "white",
                     p_color = "red", h_color = "black", h_size = 2,
                     spotlight = TRUE, dpi = 300, u_size = 3.5,
                     url = "https://github.com/grinnellm/SpawnIndex",
                     filename = file.path("sticker", "SpawnIndexLogo.png"))
print(stickerSI)
