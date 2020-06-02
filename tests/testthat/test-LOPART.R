library(testthat)
context("LOPART")

x <- c(1.1, 2.2, 5.5, 6.6)
no.labels <- data.frame(
  start=integer(),
  end=integer(),
  changes=integer())

test_that("LOPART with no labels and penalty=0", {
  out_list <- LOPART::LOPART(x, no.labels, 0)
  expect_equal(out_list$loss$n.changes, 3)
  expect_equal(out_list$loss$total.loss + sum(x*x), 0)
})

test_that("LOPART with no labels and big penalty", {
  out_list <- LOPART::LOPART(x, no.labels, 10000)
  expect_equal(out_list$loss$n.changes, 0)
})

test_that("LOPART with one positive label on [1,4] and penalty=0", {
  pos14.label <- data.frame(
    start=1,
    end=length(x),
    changes=1)
  out_list <- LOPART::LOPART(x, pos14.label, 0)
  expect_equal(out_list$loss$n.changes, 1)
})

pos13.label <- data.frame(
  start=1,
  end=3,
  changes=1)
test_that("LOPART with one positive label on [1,3] and penalty=0", {
  out_list <- LOPART::LOPART(x, pos13.label, 0)
  expect_equal(out_list$loss$n.changes, 2)
  expect_equal(out_list$segments$end, 2:4)
})
test_that("LOPART with one positive label on [1,3] and big penalty", {
  out_list <- LOPART::LOPART(x, pos13.label, 100000)
  expect_equal(out_list$segments$end, c(2, 4))
})

two.labels <- data.frame(
  start=1:2,
  end=2:3,
  changes=1)
test_that("LOPART with two positive labels on [1,3] and big penalty", {
  out_list <- LOPART::LOPART(x, two.labels, 100000)
  expect_equal(out_list$segments$end, c(1, 2, 4))
})
test_that("LOPART with two positive labels on [1,3] and penalty=0", {
  out_list <- LOPART::LOPART(x, two.labels, 0)
  expect_equal(out_list$segments$end, 1:4)
})

three.labels <- data.frame(
  start=1:3,
  end=2:4,
  changes=c(1, 0, 1))
test_that("LOPART with three labels and penalty=0", {
  out_list <- LOPART::LOPART(x, three.labels, 0)
  expect_equal(out_list$segments$end, c(1, 3, 4))
})
