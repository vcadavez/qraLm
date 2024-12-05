test_that("batches contamination in a list", {
  Cinit <- LotGen(
    nLots = 10,
    sizeLot = 10,
    P = 0.14,
    C0MeanLog = 1.5,
    C0SdLog = 0.2,
    unitSize = 500
  )
  expect_true(is.list(Cinit) == TRUE)
})
