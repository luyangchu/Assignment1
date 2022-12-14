library("Assignment")
library("checkmate")

# Test the function returns the correct results
test_that("kaya_identity_equation returns the correct results",{
  expect_equal(kaya_identity_equation(20,30,6,0.02), 72)
  expect_equal(kaya_identity_equation(300,50,7,0.08), 8400)
  expect_equal(kaya_identity_equation(3,5,3,0), 0)

})

# Test the function with negative inputs.
test_that("kaya_identity_equation with negative inputs", {
  expect_error(kaya_identity_equation(20,-30,6,0.02),
               "Assertion on 'gdp' failed: Element 1 is not >= 0.")
  expect_error(kaya_identity_equation(300,50,7,-3),
               "Assertion on 'carbInt' failed: Element 1 is not >= 0.")
  expect_error(kaya_identity_equation(-3,5,3,0),
               "Assertion on 'pop' failed: Element 1 is not >= 0.")

})

# Test the function  with two output types.
test_that("kaya_identity_equation returns the correct output type",{
  expect_equal(kaya_identity_equation(20,30,6,0.02,"CO2"), 72)
  expect_equal(kaya_identity_equation(367,50,7,1,"C"), 35000)
  expect_equal(kaya_identity_equation(3,5,3,0.367,"C"), 4.5)

})
