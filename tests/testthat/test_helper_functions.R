
test_that("test init data frame", {
  skip_on_cran()
  df <- init_dates_dataframe(
    2018,
    2019,
    noleap = TRUE,
    timescale = "d"
  )
  
  expect_type(df, "list")
})

test_that("test outlier removal", {
  skip_on_cran()
  
  vector <- c(rnorm(1000, sd = 2))
  df <- remove_outliers(vector, coef = 1.5)
  
  expect_type(df, "double")
})

test_that("test vp", {
  skip_on_cran()
  
  df <- calc_vp_inst(
    1,
    100000
  )
  
  expect_type(df, "double")
})

test_that("test vp from rh", {
  skip_on_cran()
  
  expect_warning(
    calc_vpd(
    elv = NA,
    patm = NA
  ))
  
  df <- calc_vpd(
      elv = NA,
      patm = 100000,
      tmin = 10,
      tmax = 25,
      tc = 18,
      qair = 0.01
    )
  
  expect_type(df, "double")
  
  
  df <- calc_vpd(
    elv = 0,
    patm = NA,
    tmin = 10,
    tmax = 25,
    tc = 18,
    qair = 0.01
  )
  
  expect_type(df, "double")
  
})


test_that("test vpd from rh", {
  skip_on_cran()
  
  expect_warning(
    calc_vpd(
      elv = NA,
      patm = NA
    ))
  
  df <- calc_vpd(
    elv = NA,
    patm = 100000,
    tmin = 10,
    tmax = 25,
    tc = 18,
    qair = 0.01
  )
  
  expect_type(df, "double")
  
  
  df <- calc_vpd(
    elv = 0,
    patm = NA,
    tmin = 10,
    tmax = 25,
    tc = 18,
    qair = 0.01
  )
  
  expect_type(df, "double")
  
})

test_that("test patm from elevation", {
  skip_on_cran()
  
  df <- calc_patm(
    elv = 0
  )
  
  expect_type(df, "double")
  
})



