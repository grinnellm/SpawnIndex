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
    polys, c("sections", "stat_areas", "groups", "regions")
  )
  expect_true(all(st_is_valid(polys$sections)))
  expect_true(all(st_is_valid(polys$groups)))
  expect_true(all(st_is_valid(polys$stat_areas)))
  expect_true(all(st_is_valid(polys$regions)))
  expect_silent(load_sections(sections = sections, areas = areas))
})
