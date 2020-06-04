#define ERROR_LABEL_END_NOT_GREATER_THAN_START 1
#define ERROR_LABEL_NOT_ZERO_ONE 2
#define ERROR_LABEL_START_LESS_THAN_NEXT_PREV_END 3
#define ERROR_LABEL_START_LESS_THAN_ZERO 4
#define ERROR_LABEL_END_NOT_LESS_THAN_N_DATA 5
#define ERROR_PENALTY_MUST_BE_NON_NEGATIVE 6
#define UNLABELED -1

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
 );
