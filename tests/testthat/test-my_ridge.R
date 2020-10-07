library(testthat)

context("Test the function my_ridge()")

test_that("You my_ridge() function works when lambda = 0", {

  data(iris)

  my_fit <- my_ridge(Sepal.Length~.,iris,lambda=0,tol=1e-8)
  lm_fit <- lm(Sepal.Length~.,iris)

  expect_equivalent(lm_fit$coefficients, my_fit$coefficients,tolerance=1e-05
  )
})


