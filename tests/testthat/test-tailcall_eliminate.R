test_that("a simple example works", {
  sum_n <- function(n, accumulator = 0) {
    if (n == 0) {
      return(accumulator)
    }
    Recall(n - 1, accumulator + n)
  }
  sum_n_2 <- tailcall_eliminate(sum_n)
  expect_equal(sum_n_2(1000), sum(1:1000))
})

test_that("it does not optimize if no recall in tail position", {
  sum_n <- function(n) {
    n + Recall(n - 1)
  }
  sum_n_2 <- tailcall_eliminate(sum_n)
  expect_equal(sum_n_2, sum_n)
})

test_that("it works with no arguments", {
  x <- 0
  fun <- function() {
    if (x == 2){
      return(x)
    }
    x <<- x + 1
    Recall()
  }
  fun_2 <- tailcall_eliminate(fun)
  res <- fun_2()
  expect_equal(res, 2)
  expect_equal(x, 2)
})
