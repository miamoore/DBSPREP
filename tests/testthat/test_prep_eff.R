context("test_prep_eff.R")

test_that("output length matches quantiles",{
  DBS.measurements = runif(500, 300, 1000)
  default.length = length(get.control.list(list())$quantiles.to.calc)

  expect_equal(length(prep.dbs.efficacy(DBS.measurements,
                                        gender = "msmtgw",
                                        control = list(quantiles.to.calc = c(.25, .75)))),
               2)
  expect_equal(length(prep.dbs.efficacy(DBS.measurements,
                                        gender = "msmtgw")),
               default.length)
  expect_equal(length(prep.dbs.efficacy(DBS.measurements,
                                        gender = "ciswomen",
                                        control = list(quantiles.to.calc = c(.25, .75)))),
               2)
  expect_equal(length(prep.dbs.efficacy(DBS.measurements,
                                        gender = "ciswomen")),
               default.length)
})


test_that("output is less than or equal to 1",{
  DBS.measurements = runif(500, 300, 1000)

  expect_equal(sum(prep.dbs.efficacy(DBS.measurements,
                                        gender = "msmtgw")>1),
               0)
  expect_equal(sum(prep.dbs.efficacy(DBS.measurements,
                                     gender = "ciswomen")>1),
               0)

})
