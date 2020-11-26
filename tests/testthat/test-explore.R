test_that("non-muscldfs are detected", {
  data <- iris[1:4]
  expect_error(
    explore(muscldf = data),
    "Specified object is not of type `muscldf`"
    )
})
