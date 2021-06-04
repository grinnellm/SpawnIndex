test_that("Load sections", {
  db_loc <- system.file("extdata", package = "SpawnIndex")
  area_loc <- list(
    loc = db_loc, db = "HerringSpawn.mdb",
    fns = list(sections = "Sections", locations = "Location")
  )
  areas <- load_area_data(reg = "WCVI", where = area_loc)
  polys <- load_sections(sections = sections, areas = areas)
  expect_type(polys, "list")
  expect_named(
    polys, c("sections", "groups", "stat_areas", "regions", "xy_ratio")
  )
  expect_true(all(st_is_valid(polys$sections)))
  expect_true(all(st_is_valid(polys$groups)))
  expect_true(all(st_is_valid(polys$stat_areas)))
  expect_true(all(st_is_valid(polys$regions)))
  expect_type(polys$xy_ratio, "double")
  expect_silent(load_sections(sections = sections, areas = areas))
  expect_message(
    load_sections(sections = sections, areas = areas, buffer = -1)
  )
  expect_error(
    load_sections(sections = sections, areas = areas, buffer = "5000")
  )
  expect_error(
    load_sections(sections = sections, areas = areas, out_crs = "4326")
  )
})
