test_that("Sections", {
  library(sf)
  sections_loc <- system.file("extdata", "Sections", package = "SpawnIndex")
  sections <- st_read(
    dsn = sections_loc, layer = "HerringSections", quiet = TRUE
  )
  expect_type(sections, "list")
  expect_true("sf" %in% class(sections))
  expect_equal(
    names(sections),
    c("OBJECTID", "Section", "SAR", "Assessment", "StatArea", "geometry")
  )
  expect_equal(dim(sections), c(108, 6))
  expect_equal(st_crs(sections)$input, "NAD83 / BC Albers")
  expect_true(
    "POLYGON" %in% as.character(st_geometry_type(sections, by_geometry = FALSE))
  )
})
