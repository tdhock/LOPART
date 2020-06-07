##' Compute an optimal segmentation (change in Gaussian mean model,
##' square loss), which is consistent with the given labels, and with
##' a penalty for each changepoint outside of labeled regions.
##'
##' Provides a high-level interface to LOPART_interface R function and
##' LOPART C code.
##' @title Labeled Optimal PARTitioning
##' @return list with named elements, all of which are data
##'   tables. loss has one row with loss/cost values. cost is the
##'   output from LOPART_interface. changes has one row for each
##'   predicted changepoint (e.g. change=1.5 means a change between
##'   data points 1 and 2). segments has one row for each segment.
##' @author Toby Dylan Hocking
##' @param x numeric vector of data to fit a Gaussian mean model.
##' @param labels data frame with at least three columns: start, end,
##'   changes. start/end should be indices of x, from 1 to
##'   length(x). changes should be either 0 or 1. The prediced
##'   changepoints are guaranteed to be consistent with these labels.
##' @param penalty non-negative penalty constant (larger for fewer
##'   changes, smaller for more changes). penalty=0 means a change in
##'   every unlabeled region, penalty=Inf means no changes in
##'   unlabeled regions.
##' @example inst/examples/LOPART.R
LOPART <- function
(x, labels, penalty_unlabeled, n_updates=length(x), penalty_labeled=0){
  last_change <- cost_optimal <- . <- NULL
  ## above to avoid CRAN NOTE.
  out_df <- LOPART_interface(
    x,
    labels$start-1L,
    labels$end-1L,
    labels$changes,
    n_updates,
    penalty_unlabeled,
    penalty_labeled)
  out_dt <- data.table(out_df)
  change.vec <- out_dt[0 <= last_change, last_change+1L]
  penalized_cost <- out_dt[.N, cost_optimal]
  changes_total <- length(change.vec)
  changes_labeled <- sum(labels$changes==1)
  changes_unlabeled <- changes_total-changes_labeled
  term_unlabeled <- if(changes_unlabeled==0){
    0 # special case needed when penalty=Inf.
  }else{
    changes_unlabeled*penalty_unlabeled
  }
  term_labeled <- changes_labeled*penalty_labeled
  total_loss <- penalized_cost-term_labeled-term_unlabeled
  list(
    loss=data.table(
      changes_total,
      changes_labeled,
      changes_unlabeled,
      penalty_labeled,
      penalty_unlabeled,
      penalized_cost,
      total_loss),
    cost=out_dt,
    changes=data.table(
      change=change.vec+0.5),
    segments=out_dt[last_change != -2, .(
      start=c(1L, change.vec+1L),
      end=c(change.vec, nrow(out_dt)),
      mean)])
}
