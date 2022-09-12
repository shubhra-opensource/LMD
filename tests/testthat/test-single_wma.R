test_that("single_wma works", {
  x=1:3
  weights=c(1,2,1)

  expect_equal(round(single_wma(x,weights),3), 2.667)
})
