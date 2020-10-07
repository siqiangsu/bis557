library(testthat)

context("Test the function of problem 2.")

test_that("You my_lm_gd_out_of_sample() function works.", {

  data(iris)

  my_mse <- my_lm_gd_out_of_sample(Sepal.Length~.,iris,eta=0.05,iters=100000,seed=1234)$out_of_sample_MSE

  lm_mse <- mean((lm(Sepal.Length  ~ ., iris)$fitted.values - iris$Sepal.Length)^2)

  expect_lt(lm_mse, my_mse
                    )
})

