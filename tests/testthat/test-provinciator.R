# devtools::load_all()

x <- "Madrid"
division <- "provincia"

test_that("Provinciator 'x' throws errors.", {
  not_x <- TRUE

  expect_error(provinciator())
  expect_error(provinciator(not_x))
  })


test_that("Provinciator 'division' throws errors.", {
  not_division <- "not_division"

  expect_error(provinciator(x, not_division))
  })


test_that("Provinciator 'scope' throws errors.", {
  not_bias <- list("provincia", 17, NULL)

  expect_error(provinciator(x, division, not_bias))
  })


test_that("Provinciate '.data' throws errors.", {
  expect_error(provinciate())
  expect_error(provinciate(x))
  expect_error(provinciate(1))
  expect_error(provinciate(as.matrix(edcn), town_of_birth))
  })


test_that("Provinciator is right.", {
  expect_equal(provinciator("Redondela - Chapela", "municipio")[[1]],
               "{Chapela (San Fausto)} Redondela; Pontevedra; Galicia")
  expect_equal(provinciator("Seca, La", "municipio")[[1]],
               "La Seca; Valladolid; Castilla y León")
  expect_equal(provinciator("Vila-seca", "municipio")[[1]],
               "Vila-seca; Tarragona; Cataluña")
})
