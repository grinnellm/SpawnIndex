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

test_that("Load sections", {
  library(sf)
  db_loc <- system.file("extdata", package = "SpawnIndex")
  area_loc <- list(
    loc = db_loc, db = "HerringSpawn.mdb",
    fns = list(sections = "Sections", locations = "Location")
  )
  areas <- load_area_data(reg = "WCVI", where = area_loc)
  sections_files <- system.file("extdata", "Sections", package = "SpawnIndex")
  sections_loc <- list(
    loc_sec = sections_files,
    fns = list(sections = "HerringSections")
  )
  polys <- load_sections(where = sections_loc, areas = areas, quiet = TRUE)
  expect_type(polys, "list")
  expect_named(polys, c("sections", "groups", "stat_areas", "regions"))
  # expect_equal(st_geometry_type(polys$sections, by_geometry = FALSE), "POLYGON")
  # expect_equal(st_geometry_type(polys$groups, by_geometry = FALSE), "POLYGON")
  # expect_equal(st_geometry_type(polys$stat_areas, by_geometry = FALSE), "POLYGON")
  # expect_equal(st_geometry_type(polys$regions, by_geometry = FALSE), "POLYGON")
})
