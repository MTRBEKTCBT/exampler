test_that("2022年現在の祝日の日数", {
  expect_equal(zipangu::jholiday(2022) |> length(), 16)
})

test_that("2024年はうるう年になる" , {
  expect_equal(seq_days_in_months(2024, 1, 12) |> length(), 366)
})
