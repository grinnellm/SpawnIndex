test_that("Herring spawn database", {
  library(odbc)
  library(DBI)
  db_loc <- system.file("extdata", "HerringSpawn.mdb", package = "SpawnIndex")
  access_db <- dbConnect(
    drv = odbc(),
    .connection_string = paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db_loc,
      sep = ""
    )
  )
  db_tables <- dbListTables(conn = access_db)
  expect_type(db_tables, "character")
  expect_true(
    all(c(
      "Location", "PoolStd", "RegionStd", "Sections", "SectionStd",
      "tSSAllspawn", "tSSMacPlant", "tSSMacTrans", "tSSStations", "tSSSurface",
      "tSSVegetation", "tSSVegTrans"
    ) %in% db_tables)
  )
  dbDisconnect(conn = access_db)
})
