context("Testing bit helpers")

test_that("int_to_bits produces big-endian, fixed-width logical vectors", {
  expect_equal(rdiversity:::int_to_bits(5, 4), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(rdiversity:::int_to_bits(0, 3), c(FALSE, FALSE, FALSE))
  expect_equal(rdiversity:::int_to_bits(255, 8), rep(TRUE, 8))
  expect_length(rdiversity:::int_to_bits(1, 6), 6)
  expect_type(rdiversity:::int_to_bits(3, 4), "logical")
})

test_that("bits_to_int inverts int_to_bits", {
  for (x in 0:64) {
    expect_equal(rdiversity:::bits_to_int(rdiversity:::int_to_bits(x, 8)), x)
  }
})

test_that("bits_to_int reads big-endian order", {
  expect_equal(rdiversity:::bits_to_int(c(1, 0, 0)), 4)
  expect_equal(rdiversity:::bits_to_int(c(0, 0, 1)), 1)
  expect_equal(rdiversity:::bits_to_int(c(TRUE, TRUE, TRUE, TRUE)), 15)
})
