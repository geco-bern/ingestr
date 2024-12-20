
test_that("test HH data", {
  skip_on_cran()
  
  siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
    dplyr::filter(sitename == "FR-Pue")
  
  settings_fluxnet <- list(
    getswc       = FALSE,
    filter_ntdt  = TRUE,
    threshold_GPP= 0.8,
    remove_neg   = FALSE,
    dir_hh = paste0(path.package("ingestr"), "/extdata/")
  )

  df <-testthat::expect_warning(ingestr::ingest(
    siteinfo,
    source    = "fluxnet",
    getvars   = list(
      gpp = "GPP_NT_VUT_REF"
    ),
    dir       = paste0(path.package("ingestr"), "/extdata/"),
    settings  = settings_fluxnet,
    timescale = "hh",
    verbose = TRUE
  ))

  testthat::expect_type(df, "list")
  testthat::expect_equal(c("sitename","date","gpp"),
                         df |> tidyr::unnest(data) |> colnames())
})

test_that("test Daily data", {
  skip_on_cran()
  
  siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
    filter(sitename == "FR-Pue")
  
  settings_fluxnet <- list(
    getswc       = FALSE,
    filter_ntdt  = TRUE,
    threshold_GPP= 0.8,
    remove_neg   = FALSE,
    dir_hh = paste0(path.package("ingestr"), "/extdata/")
  )
  
  df <- testthat::expect_warning(ingestr::ingest(
    siteinfo,
    source    = "fluxnet",
    getvars   = list(
        gpp = "GPP_NT_VUT_REF",
        gpp_unc = "GPP_NT_VUT_SE"
      ),
    dir       = paste0(path.package("ingestr"), "/extdata/"), 
    settings  = settings_fluxnet,
    timescale = "d",
    verbose = TRUE
  ))
  
  testthat::expect_type(df, "list")
  testthat::expect_equal(c("sitename","date","gpp", "gpp_unc"),
                         df |> tidyr::unnest(data) |> colnames())
  
})

