#include "LOPART.h"
#include <math.h>
#include <stdio.h>

double sum_from_to(double *out_cumsum, int first, int last){
  double total = out_cumsum[last];
  if(0 < first){
    total -= out_cumsum[first-1];
  }
  return total;
}

int LOPART
(double *input_data,
 int n_data, //N in paper
 int *input_label_start,
 int *input_label_end,
 int *input_label_changes,
 int n_labels,//M in paper
 double penalty,//lambda.
 double *out_cumsum,//for computing optimal cost of a segment.
 int *out_change_candidates,//T_t
 double *out_cost_candidates,// for visualization.
 double *out_cost, //out_cost[t-1] = W_t in paper, for t=1 to N.
 double *out_mean,
 int *out_last_change //out_last_change[t-1] tau*_t in paper.
 ){
  //error checking.
  if(penalty<0){
    return ERROR_PENALTY_NEGATIVE;
  }
  for(int j=0; j<n_labels; j++){
    if(input_label_end[j] <= input_label_start[j]){
      return ERROR_LABEL_END_NOT_GREATER_THAN_START;
    }
    if(input_label_changes[j] != 0 && input_label_changes[j] != 1){
      return ERROR_LABEL_NOT_ZERO_ONE;
    }
    if(0<j){
      if(input_label_start[j] < input_label_end[j-1]){
	return ERROR_LABEL_START_LESS_THAN_NEXT_PREV_END;
      }
    }
    if(input_label_start[j] < 0){
      return ERROR_LABEL_START_LESS_THAN_ZERO;
    }
    if(n_data <= input_label_end[j]){
      return ERROR_LABEL_END_NOT_LESS_THAN_N_DATA;
    }
  }
  // initialize cumsum vector.
  double total = 0.0;
  for(int t=0; t<n_data; t++){
    total += input_data[t];
    out_cumsum[t] = total;
  }
  int n_change_candidates = 0;//DP initialization.
  int current_label_j = 0;
  int current_label_changes = UNLABELED;
  for(int t=0; t<n_data; t++){
    if(current_label_changes == UNLABELED){
      // if we are in an unlabeled region then add this changepoint.
      out_change_candidates[n_change_candidates] = t-1;
      n_change_candidates++;
    }else if(current_label_changes == 1 && t == input_label_end[current_label_j]){
      // if we are at the last point of a positive label then reset
      // the set of change candidates to all changes in this region.
      n_change_candidates=0;
      for(int change_candidate=input_label_start[current_label_j];
	  change_candidate<t; change_candidate++){
	out_change_candidates[n_change_candidates] = change_candidate;
	n_change_candidates++;
      }
    }
    out_cost[t] = out_cost_candidates[t] = INFINITY;
    double cost_up_to_candidate;
    for(int candidate_i=0; candidate_i<n_change_candidates; candidate_i++){
      int change_candidate = out_change_candidates[candidate_i];
      if(change_candidate == -1){
	cost_up_to_candidate = -penalty;
      }else{
	cost_up_to_candidate = out_cost[change_candidate];
      }
      // sum_i (x_i - m)^2 =
      // [sum_i x_i^2] - [2 m sum_i x_i] + [sum_i m^2]
      double total = sum_from_to(out_cumsum, change_candidate+1, t);
      int seg_size = t-change_candidate;
      double seg_mean = total/seg_size;
      double seg_cost = seg_size*seg_mean*seg_mean - 2*seg_mean*total;
      double cost_up_to_t = cost_up_to_candidate + penalty + seg_cost;
      if(t == n_data-1){
	// store cost of each candidate at the end for visualization.
	out_cost_candidates[change_candidate+1] = cost_up_to_t;
      }
      if(cost_up_to_t < out_cost[t]){
	out_cost[t] = cost_up_to_t;
	out_mean[t] = seg_mean;
	out_last_change[t] = change_candidate;
      }
    }
    if(current_label_j < n_labels && t == input_label_end[current_label_j]){
      // if we are at the end of any label then we are going into an
      // UNLABELED region.
      current_label_j++;
      current_label_changes = UNLABELED;
    }
    if(current_label_j < n_labels && t == input_label_start[current_label_j]){
      current_label_changes = input_label_changes[current_label_j];
    }
  }//for(t
  //decoding.
  int seg_end = n_data-1;
  while(0 <= seg_end){
    int prev_end = out_last_change[seg_end];
    for(int t=prev_end+1; t<seg_end; t++){
      out_last_change[t] = -2;
    }
    seg_end = prev_end;
  }
  return 0;
}
