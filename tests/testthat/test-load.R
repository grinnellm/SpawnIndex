# test_that("Load area data", {
#   expect_type(
#     {
#       load_area_data(reg = "WCVI", where = list(
#         loc = system.file("extdata", package = "SpawnIndex"),
#         db = "HerringSpawn.mdb",
#         fns = list(sections = "Sections", locations = "Location")
#       ), quiet = TRUE)
#     },
#     "list"
#   )
#   expect_named(
#     {
#       load_area_data(reg = "WCVI", where = list(
#         loc = system.file("extdata", package = "SpawnIndex"),
#         db = "HerringSpawn.mdb",
#         fns = list(sections = "Sections", locations = "Location")
#       ), quiet = TRUE)
#     },
#     c(
#       "SAR", "Region", "RegionName", "StatArea", "Group", "Section",
#       "LocationCode", "LocationName", "Pool", "Eastings", "Northings",
#       "Longitude", "Latitude"
#     )
#   )
# })
