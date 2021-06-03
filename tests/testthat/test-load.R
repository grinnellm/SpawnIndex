test_that("Load sections", {
  library(sf)
  db_loc <- system.file("extdata", package = "SpawnIndex")
  area_loc <- list(
    loc = db_loc, db = "HerringSpawn.mdb",
    fns = list(sections = "Sections", locations = "Location")
  )
  areas <- load_area_data(reg = "WCVI", where = area_loc)
  sections_loc <- list(
    loc = file.path(db_loc, "Sections"),
    fns = list(sections = "HerringSections")
  )
  polys <- load_sections(where = sections_loc, areas = areas)
  expect_type(polys, "list")
  expect_named(
    polys, c("sections", "groups", "stat_areas", "regions", "xy_ratio")
  )
  expect_true("sf" %in% class(polys$sections))
  expect_true("sf" %in% class(polys$groups))
  expect_true("sf" %in% class(polys$stat_areas))
  expect_true("sf" %in% class(polys$regions))
  expect_true(
    "POLYGON" %in%
      as.character(st_geometry_type(polys$sections, by_geometry = FALSE))
  )
  expect_type(polys$xy_ratio, "double")
  expect_silent(load_sections(where = sections_loc, areas = areas))
  expect_message(
    load_sections(where = sections_loc, areas = areas, buffer = -1)
  )
})
