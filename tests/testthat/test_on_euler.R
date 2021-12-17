# is the server reachable
euler <- grepl('eu-', Sys.info()['nodename'])

test_that("test MODIS LST download", {
  skip_on_cran()
  skip_if_not(euler)
  
  
  
  
  })
