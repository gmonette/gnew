test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("tonum works on factor of character", {
  expect_equal(tonum(factor('2')), 2)
})
test_that("tonum works on factor of numeric", {
  expect_equal(tonum(factor(2)), 2)
})
test_that("combine small categories", {
  expect_equal(
    gr(c('a','a','b','b','c','c','d','d')), 
    c("a", "a", "b", "b", "Other", "Other", "Other", "Other"))
})
