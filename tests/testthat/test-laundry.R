# devtools::load_all()

test_that("Strip_place is right.", {
  expect_equal(strip_place("Madrid"), "madrid")
  expect_equal(strip_place("ÁÀÄÂÃáàäâã"), "aaaaaaaaaa")
  expect_equal(strip_place("Oria - Madrid"), "madrid")
  expect_equal(strip_place("Madrid (Oria)"), "madrid")
  expect_equal(strip_place("València/Valencia"), "valencia")
  expect_equal(strip_place("València / Valencia"), "valencia")
  expect_equal(strip_place("Sto. Domingo"), "santo domingo")
  expect_equal(strip_place("Sta. Monica"), "santa monica")
  expect_equal(strip_place("Ojos-Albos"), "ojos albos")
  expect_equal(strip_place("El Encinar de los Reyes"), "encinar reyes")
  expect_equal(strip_place("San Vicente/Sant Vicent - Ras-Pas"), "ras pas")
  expect_equal(strip_place("San Vicente / Sant Vicent - Ras-Pas"), "ras pas")
  expect_equal(strip_place("San-Vicente/Sant-Vicent - Ras-Pas"), "ras pas")
  expect_equal(strip_place("!\"·$%&/()"), "")
  })

test_that("Strip_name is right.", {
  expect_equal(strip_name("Mª"), "maria")
  expect_equal(strip_name("M.ª"), "maria")
  expect_equal(strip_name("Mº"), "mº")
  expect_equal(strip_name("Ja7ime"), "jaime")
  expect_equal(strip_name("José M."), "josé")
})

test_that("Clean_name is right.", {
  expect_equal(clean_name("  ALEJANDRO  "), "Alejandro")
  expect_equal(clean_name("Ana·$%&/()="), "Ana")
  expect_equal(clean_name("Mª  Antonia"), "María Antonia")
})
