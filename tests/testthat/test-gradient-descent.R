library(testthat)

context("Test the gradient descent function.")

test_that("You my_gd() function works in an easy case.", {
  
  data(iris)
  
  fit_gd <- my_gd(Sepal.Length ~ ., iris)
  
  fit_lm <- lm(Sepal.Length  ~ ., iris)
  
  expect_equivalent(fit_lm$coefficients, fit_gd,
                    tolerance = 1e-5)
})

test_that("You my_gd() function works with contrasts.", {
  
  data(iris)
  
  fit_gd <- my_gd(Sepal.Length ~ ., iris, 
                                   contrasts = list(Species = "contr.sum"))
  
  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))
  
  expect_equivalent(fit_lm$coefficients, fit_gd,
                    tolerance = 1e-5)
})

test_that("Your my_gd() function works in a tougher case.", {
  
  data(lm_patho)
  
  fit_gd <- my_gd(y ~., lm_patho)
  
  fit_lm <- lm(y ~., lm_patho)
  
  expect_equivalent(fit_lm$coefficients, fit_gd,
                    tolerance = 1e-4)
})

