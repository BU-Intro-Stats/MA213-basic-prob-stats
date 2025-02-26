library(testthat)

# each call to test_that() produces one test
# each test represents one point value
# you can have multiple tests for each question


test_that(" question 1.1", {
  
  expect_equal( myobj1, 152.2, tolerance=1e-3) 

})


test_that(" question 1.2", {
  
  expect_equal( myobj2, 0.0006297376, tolerance=1e-3) 
  
})

test_that(" question 1.3", {
  
  expect_equal( myobj3, 0.0001595569, tolerance=1e-3) 
  
})


test_that(" question 2", {
  
  expect_equal( Mydata1, 1:10) 
  
})


test_that("question 3", {

  expect_true(is.character(Mydata2))
  expect_equal(Mydata2, c(rep("A",3),rep("B",2),rep("C",4)))

})

test_that(" question 4", {
  
  expect_equal( Mydata3, seq(1,99,2)) 
  
})



test_that("question 5", {

  expect_equal(Mymean, 2.457143, tolerance=1e-3)

})
