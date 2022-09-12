test_that("Extrema_Points works", {
  x=1:5
  y = sin(x)
  aa=extreme_points(y)
  expect_equal(aa[["maxindex"]][1], 2)
  expect_equal(aa[["nextreme"]], 1)
  expect_equal(aa[["cross"]][1], 3)
  expect_equal(aa[["ncross"]], 1)

})
