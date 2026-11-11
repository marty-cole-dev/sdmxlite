answers <- function(...) {
  replies <- c(...)
  calls <- 0
  function(prompt) {
    calls <<- calls + 1
    replies[calls]
  }
}

quiet_pick <- function(...) {
  res <- NULL
  utils::capture.output(suppressMessages(res <- pick(...)))
  res
}

keys <- c("A", "M", "Q")
labels <- c("A  Annual", "M  Monthly", "Q  Quarterly")

test_that("pick accepts numbers or keys, case-insensitive", {
  expect_equal(quiet_pick(keys, labels, "FREQ", input = answers("2")), "M")
  expect_equal(quiet_pick(keys, labels, "FREQ", input = answers("q")), "Q")
  expect_equal(
    quiet_pick(keys, labels, "FREQ", input = answers("")), character(0)
  )
})

test_that("multiple choice mixes numbers and keys with any separator", {
  expect_equal(
    quiet_pick(keys, labels, "FREQ", multiple = TRUE, input = answers("1 m,Q")),
    c("A", "M", "Q")
  )

  expect_equal(
    quiet_pick(keys, labels, "FREQ", multiple = TRUE, input = answers("A+M")),
    c("A", "M")
  )
})

test_that("unknown answers are asked again", {
  expect_equal(
    quiet_pick(keys, labels, "FREQ", input = answers("X", "7", "M")), "M"
  )
})

test_that("a numeric code typed exactly wins over the position", {
  expect_equal(
    quiet_pick(
      c("10", "15", "1"), c("a", "b", "c"), "AGE", input = answers("1")
    ),
    "1"
  )
  expect_equal(
    quiet_pick(
      c("10", "15", "1"), c("a", "b", "c"), "AGE", input = answers("2")
    ),
    "15"
  )
})

test_that("aliases: a dataflow can be chosen by id or full key", {
  expect_equal(
    quiet_pick(
      c("ECB,EXR,1.0", "ECB,BSI,1.0"), c("l1", "l2"), "Flow",
      aliases = c("EXR", "BSI"), input = answers("bsi")
    ), "ECB,BSI,1.0"
  )
})

testthat::test_that("ask_yes_no: only y/yes is yes, the default is no", {
  ask <- function(reply) ask_yes_no("Download?", input = function(prompt) reply)
  testthat::expect_true(ask("y"))
  testthat::expect_true(ask(" YES "))
  testthat::expect_false(ask(""))
  testthat::expect_false(ask("n"))
  testthat::expect_false(ask("sure"))
})
