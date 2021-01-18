context("load.R")

# test_that("Load area data", {
#   expect_type(
#     {
#       dbLoc <- system.file("extdata", package = "SpawnIndex")
#       areaLoc <- list(
#         loc = dbLoc, db = "HerringSpawn.mdb",
#         fns = list(sections = "Sections", locations = "Location")
#       )
#       areas <- LoadAreaData(reg = "WCVI", where = areaLoc, quiet = TRUE)
#       areas
#     },
#     "list"
#   )
#   expect_equal(
#     {
#       dbLoc <- system.file("extdata", package = "SpawnIndex")
#       areaLoc <- list(
#         loc = dbLoc, db = "HerringSpawn.mdb",
#         fns = list(sections = "Sections", locations = "Location")
#       )
#       areas <- LoadAreaData(reg = "WCVI", where = areaLoc, quiet = TRUE)
#       ncol(areas)
#     },
#     13
#   )
# })

# test_that("Load allSpawn", {
#   expect_is(
#     {
#       dbLoc <- system.file("extdata", package = "SpawnIndex")
#       areaLoc <- list(
#         loc = dbLoc, db = "HerringSpawn.mdb",
#         fns = list(sections = "Sections", locations = "Location")
#       )
#       areas <- LoadAreaData(reg = "WCVI", where = areaLoc, quiet = TRUE)
#       allSpawnLoc <- list(
#         loc = dbLoc, db = "HerringSpawn.mdb",
#         fns = list(allSpawn = "tSSAllspawn", stations = "tSSStations")
#       )
#       allSpawn <- LoadAllSpawn(where = allSpawnLoc, a = areas, yrs = 2010:2015)
#       allSpawn
#     },
#     "data.frame"
#   )
# })
