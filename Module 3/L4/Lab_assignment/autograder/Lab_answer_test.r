library(testthat)

# each call to test_that() produces one test
# each test represents one point value
# you can have multiple tests for each question


test_that(" question 1", {
  
  expect_vector(population, ptype=integer() , size = 100000)
})


test_that(" question 2.1", {
  
  expect_equal( mean_pop, 0.09987, tolerance=1e-3) 
  
})

test_that(" question 2.2", {
  
  expect_equal( sd_pop, 0.2998281, tolerance=1e-3) 
  
})

test_that(" question 3.1", {
  
  expect_equal( mean_sample, 2.872222, tolerance=1e-3) 
  
})

test_that(" question 3.2", {
  
  expect_equal( sd_sample, 0.1550281, tolerance=1e-3) 
  
})

test_that(" question 4.1", {
  
  expect_equal( mean_sample2, 2.865067, tolerance=1e-3) 
  
})

test_that(" question 4.2", {
  
  expect_equal( sd_sample2, 0.07170299, tolerance=1e-3) 
  
})



