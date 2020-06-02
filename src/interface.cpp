#include "LOPART.h"
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List LOPART_interface
(Rcpp::NumericVector input_data,
 Rcpp::IntegerVector input_label_start,
 Rcpp::IntegerVector input_label_end,
 Rcpp::IntegerVector input_label_changes,
 double penalty
 ) {
  int n_data = input_data.size();
  int n_labels = input_label_changes.size();
  if(input_label_start.size() != n_labels){
    Rcpp::stop("input_label_start and input_label_changes sizes must match");
  }
  if(input_label_end.size() != n_labels){
    Rcpp::stop("input_label_end and input_label_changes sizes must match");
  }
  Rcpp::NumericVector out_cumsum(n_data);
  Rcpp::IntegerVector out_change_candidates(n_data);
  Rcpp::NumericVector out_cost_candidates(n_data);
  Rcpp::NumericVector out_cost(n_data);
  Rcpp::NumericVector out_mean(n_data);
  Rcpp::IntegerVector out_last_change(n_data);
  int status = LOPART
    (&input_data[0],
     n_data,
     &input_label_start[0],
     &input_label_end[0],
     &input_label_changes[0],
     n_labels,
     penalty,
     //inputs above, outputs below.
     &out_cumsum[0],
     &out_change_candidates[0],
     &out_cost_candidates[0],
     &out_cost[0],
     &out_mean[0],
     &out_last_change[0]);
  if(status != 0){
    Rcpp::stop("non-zero status"); 
  }
  return Rcpp::DataFrame::create
    (
     Rcpp::Named("cost_candidates", out_cost_candidates),
     Rcpp::Named("cost_optimal", out_cost),
     Rcpp::Named("mean", out_mean),
     Rcpp::Named("last_change", out_last_change)
     ) ;
}

