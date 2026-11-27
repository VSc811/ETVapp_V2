testthat::test_that(
  desc = "Test that get_peakdata works for manual peak picking",
  code = {
    raw_data <- ETVapp::ETVapp_testdata[["IDMS"]][["Samples"]][[1]]
    pro_data <- process_data(raw_data, c1 = "113Cd", fl = 151)
    peak_data <- get_peakdata(pro_data, int_col = "113Cd", peak_start = 74, peak_end = 132,
                              PPmethod = c("Peak (manual)"), BLmethod = c("modpolyfit"),
                              deg = 1, cf = 50)

    testthat::expect_equal(ncol(peak_data), 5)
    testthat::expect_equal(colnames(peak_data), c("Isotope","Start [s]","End [s]","Area [cts]","BLmethod"))
    testthat::expect_equal(peak_data[,1], "113Cd")
    testthat::expect_equal(peak_data[,2], 74)
    testthat::expect_equal(peak_data[,3], 132)
    testthat::expect_true(is.numeric(peak_data[,4]))
    testthat::expect_equal(peak_data[,5], "modpolyfit")
  }
)

testthat::test_that(
  desc = "Test that get_peakdata works for automated peak picking",
  code = {
    raw_data <- ETVapp::ETVapp_testdata[["IDMS"]][["Samples"]][[1]]
    pro_data <- process_data(raw_data, c1 = "113Cd", fl = 151)
    peak_data <- get_peakdata(pro_data, int_col = "113Cd", minpeakheight = 3300,
                              PPmethod = c("Peak (height)"), BLmethod = c("modpolyfit"),
                              deg = 1, cf = 50)

    testthat::expect_equal(ncol(peak_data), 5)
    testthat::expect_equal(colnames(peak_data), c("Isotope","Start [s]","End [s]","Area [cts]","BLmethod"))
    testthat::expect_equal(peak_data[,1], "113Cd")
    testthat::expect_equal(peak_data[,2], 74, tolerance = 1)
    testthat::expect_equal(peak_data[,3], 98, tolerance = 1)
    testthat::expect_true(is.numeric(peak_data[,4]))
    testthat::expect_equal(peak_data[,5], "modpolyfit")
  }
)

testthat::test_that(
  desc = "Test that get_peakdata works for computing the mean signal",
  code = {
    raw_data <- ETVapp::ETVapp_testdata[["IDMS"]][["Massbias"]]
    pro_data <- process_data(raw_data, c1 = "80Se", fl = 151)
    peak_data <- get_peakdata(pro_data, int_col = "80Se", peak_start = 10, peak_end = 60,
                              PPmethod = c("mean signal"))

    testthat::expect_true(is.data.frame(peak_data))
    testthat::expect_equal(nrow(peak_data), 3)
    testthat::expect_equal(ncol(peak_data), 4)
    testthat::expect_equal(colnames(peak_data), c("Isotope","Start [s]","End [s]","Mean Signal [cps]"))
    testthat::expect_equal(peak_data[,1], rep("80Se", nrow(peak_data)))
    testthat::expect_equal(peak_data[,2], rep(10, nrow(peak_data)))
    testthat::expect_equal(peak_data[,3], rep(60, nrow(peak_data)))
    testthat::expect_true(is.numeric(peak_data[,4]))
  }
)

testthat::test_that(
  desc = "Test that get_peakdata error messages work",
  code = {
    raw_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
    pro_data <- process_data(raw_data, c1 = "117Sn", c2 = "80Se", fl = 151)

    testthat::expect_error(get_peakdata(pro_data, int_col = "80Se", PPmethod = "mean signal"))
    testthat::expect_error(get_peakdata(pro_data, int_col = "80Se", peak_start = 10, PPmethod = "mean signal"))
    testthat::expect_error(get_peakdata(pro_data, int_col = "113Cd", peak_start = 10, peak_end = 60, PPmethod = "mean signal"))
  }
)
