# recompose_flags() --------------------------------------------------------

test_that("recompose_flags returns NA for all-NA input", {
  expect_identical(recompose_flags(c(NA, NA)), NA)
})

test_that("recompose_flags returns NA for zero-length input", {
  expect_identical(recompose_flags(character(0)), NA)
})

test_that("recompose_flags returns a single flag unchanged", {
  expect_identical(recompose_flags("686"), "686")
})

test_that("recompose_flags drops NAs and returns single remaining value", {
  expect_identical(recompose_flags(c(NA, "686")), "686")
})

test_that("recompose_flags sorts and collapses multiple flags", {
  expect_identical(recompose_flags(c("686", "660")), "660:686")
})

test_that("recompose_flags splits colon-separated inputs and deduplicates", {
  expect_identical(recompose_flags(c("686:660", "640")), "640:660:686")
})

test_that("recompose_flags deduplicates flags that appear more than once", {
  expect_identical(recompose_flags(c("686:660", "660")), "660:686")
})

test_that("recompose_flags handles a single colon-separated string", {
  expect_identical(recompose_flags("640:660:686"), "640:660:686")
})


# coalesce_flags() ---------------------------------------------------------

make_flags <- function(flag, comment, qc_outcome, manual_flag, manual_comment, manual_qc_outcome) {
  tibble(
    flag               = flag,
    comment            = comment,
    qc_outcome         = qc_outcome,
    manual_flag        = manual_flag,
    manual_comment     = manual_comment,
    manual_qc_outcome  = manual_qc_outcome
  )
}

test_that("coalesce_flags passes through auto values when manual columns are NA", {
  result <- make_flags("660", "flow issue", 3L, NA, NA, NA) |>
    coalesce_flags()

  expect_identical(result$flag,       "660")
  expect_identical(result$comment,    "flow issue")
  expect_identical(result$qc_outcome, 3L)
})

test_that("coalesce_flags passes through manual values when auto columns are NA", {
  result <- make_flags(NA, NA, NA, "686", "missing data", 4L) |>
    coalesce_flags()

  expect_identical(result$flag,    "686")
  expect_identical(result$comment, "missing data")
  expect_identical(result$qc_outcome, 4L)
})

test_that("coalesce_flags combines auto and manual flags with colon separator", {
  result <- make_flags("660", "flow issue", 3L, "686", "manual note", 4L) |>
    coalesce_flags()

  expect_identical(result$flag, "686:660")
})

test_that("coalesce_flags combines comments with ' : ' separator", {
  result <- make_flags("660", "auto note", 3L, "686", "manual note", 4L) |>
    coalesce_flags()

  expect_identical(result$comment, "manual note : auto note")
})

test_that("coalesce_flags qc_outcome takes the maximum of auto and manual", {
  result <- make_flags("660", "flow issue", 3L, "686", "manual note", 4L) |>
    coalesce_flags()

  expect_identical(result$qc_outcome, 4L)
})

test_that("coalesce_flags qc_outcome uses auto value when manual_qc_outcome is NA", {
  result <- make_flags("660", "flow issue", 2L, NA, NA, NA) |>
    coalesce_flags()

  expect_identical(result$qc_outcome, 2L)
})

test_that("coalesce_flags drops source columns and keeps correct names", {
  result <- make_flags("660", "auto", 2L, "686", "manual", 3L) |>
    coalesce_flags()

  expect_named(result, c("flag", "qc_outcome", "comment"))
})

test_that("coalesce_flags handles multiple rows with mixed scenarios", {
  df <- tibble(
    flag              = c("660",  NA,     "660"),
    comment           = c("auto", NA,     "auto"),
    qc_outcome        = c(3L,     NA,     2L),
    manual_flag       = c(NA,     "686",  "683"),
    manual_comment    = c(NA,     "note", "qc sample"),
    manual_qc_outcome = c(NA,     4L,     3L)
  )

  result <- coalesce_flags(df)

  expect_identical(result$flag,       c("660",  "686",       "683:660"))
  expect_identical(result$comment,    c("auto",  "note",      "qc sample : auto"))
  expect_identical(result$qc_outcome, c(3L,      4L, 3L))
})
