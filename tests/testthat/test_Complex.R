context("String length")
library(TestPackage2)

test_that("Create complex numbers", {
  myCmplx <- myComplex(a = 1, b = 2)
  myCmplx2 <- myComplex2(S4Vectors::SimpleList(a = 1, b = 2))

  expect_equal(real(myCmplx), 1)
})
