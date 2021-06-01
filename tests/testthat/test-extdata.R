test_that("Sections", {
  sections_loc <- system.file("extdata", "Sections", package = "SpawnIndex")
  library(sf)
  sections <- st_read(
    dsn = sections_loc, layer = "HerringSections", quiet = TRUE
  )
  expect_type(sections, "list")
  expect_equal(class(sections), c('sf', 'data.frame'))
  expect_equal(
    names(sections),
    c("OBJECTID", "Section", "SAR", "Assessment", "StatArea", "geometry")
  )
  expect_equal(dim(sections), c(108, 6))
  expect_equal(st_crs(sections)$input, "NAD83 / BC Albers")
})
