LOPART <- structure(function # Labeled Optimal PARTitioning
### Compute an optimal segmentation (change in Gaussian mean model,
### square loss), with a penalty for each changepoint, which is
### consistent with the given labels.
(x,
### numeric vector of data to fit a Gaussian mean model.
  labels,
### data frame with at least three columns: start, end,
### changes. start/end should be indices of x, from 1 to
### length(x). changes should be either 0 or 1. The prediced
### changepoints are guaranteed to be consistent with these labels.
  penalty
### non-negative penalty constant.
){
  last_change <- cost_optimal <- . <- NULL
  out_df <- LOPART_interface(
    x,
    labels$start-1L,
    labels$end-1L,
    labels$changes,
    penalty)
  out_dt <- data.table(out_df)
  change.vec <- out_dt[0 <= last_change, last_change+1L]
  penalized.cost <- out_dt[.N, cost_optimal]
  n.changes <- length(change.vec)
  labeled.changes <- sum(labels$changes==1)
  unlabeled.changes <- n.changes-labeled.changes
  complexity.term <- if(unlabeled.changes==0){
    0 # special case needed when penalty=Inf.
  }else{
    unlabeled.changes*penalty
  }
  total.loss <- penalized.cost-complexity.term
  ##value<< list with named elements, all of which are data tables
  list(
    loss=data.table( ##<< one row with loss/cost values
      n.changes,
      labeled.changes,
      unlabeled.changes,
      penalty,
      penalized.cost,
      total.loss),
    cost=out_dt, ##<< output from LOPART_interface
    changes=data.table( ##<< one row for each predicted changepoint
      change=change.vec+0.5),
    segments=out_dt[last_change != -2, .( ##<< one row for each segment
      start=c(1L, change.vec+1L),
      end=c(change.vec, nrow(out_dt)),
      mean)])
  ##end<<
}, ex=function(){

  set.seed(2)
  library(data.table)
  signal <- c(
    rnorm(25, mean = 10),
    rnorm(25, mean = 7),
    rnorm(25, mean = 8),
    rnorm(25, mean = 5))
  #outliers
  signal[86] <- 10
  labels.dt <- data.table(
    start = c(20, 45, 80),
    end = c(30, 55, 90),
    changes = c(1, 1, 0))
  signal.dt <- data.table(
    signal,
    position=seq_along(signal))
  label.colors <- c(
    "1"="#ff7d7d",
    "0"="#f6c48f")
  sig.color <- "grey50"
  if(require(ggplot2)){
    gg.data <- ggplot()+
      geom_rect(aes(
        xmin=start, xmax=end,
        fill=paste(changes),
        ymin=-Inf, ymax=Inf),
        alpha=0.5,
        data=labels.dt)+
      geom_point(aes(
        position, signal),
        color=sig.color,
        data=signal.dt)+
      scale_x_continuous(
        "position",
        breaks=seq(0, 100, by=10))+
      scale_fill_manual("label", values=label.colors)+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))
    print(gg.data)
  }
  
  label.list <- list(
    OPART=labels.dt[0],
    LOPART=labels.dt)
  seg.dt.list <- list()
  change.dt.list <- list()
  cost.dt.list <- list()
  for(model.name in names(label.list)){
    label.dt <- data.table(label.list[[model.name]])
    fit <- LOPART::LOPART(signal, label.dt, 10)
    Algorithm <- factor(model.name, names(label.list))
    tau.dt <- fit$cost[, .(
      cost_candidates,
      tau=0:(.N-1),
      change=seq_along(cost_candidates)-0.5
    )]
    cost.dt.list[[model.name]] <- data.table(Algorithm, tau.dt)
    seg.dt.list[[model.name]] <- data.table(Algorithm, fit$segments)
    change.dt.list[[model.name]] <- data.table(Algorithm, fit$changes)
  }
  seg.dt <- do.call(rbind, seg.dt.list)
  change.dt <- do.call(rbind, change.dt.list)
  cost.dt <- do.call(rbind, cost.dt.list)

  algo.sizes <- c(
    OPART=1,
    LOPART=0.5)
  algo.colors <- c(
    OPART="deepskyblue",
    LOPART="black")
  algo.shapes <- c(
    OPART=1,
    LOPART=2)
  if(require(ggplot2)){
    gg.data+
      scale_size_manual(values=algo.sizes)+
      scale_color_manual(values=algo.colors)+      
      geom_vline(aes(
        xintercept=change,
        size=Algorithm,
        color=Algorithm),
        data=change.dt)+
      geom_segment(aes(
        start-0.5, mean,
        size=Algorithm,
        color=Algorithm,
        xend=end+0.5, yend=mean),
        data=seg.dt)
  }

  if(require(ggplot2)){
    ggplot()+
      geom_rect(aes(
        xmin=start, xmax=end,
        fill=paste(changes),
        ymin=-Inf, ymax=Inf),
        alpha=0.5,
        data=labels.dt)+
      scale_fill_manual("label", values=label.colors)+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      scale_x_continuous(
        "position",
        breaks=seq(0, 100, by=10))+
      geom_point(aes(
        change, cost_candidates,
        color=Algorithm, shape=Algorithm),
        data=cost.dt)+
      scale_color_manual(values=algo.colors)+      
      scale_shape_manual(values=algo.shapes)
  }

  abbrev.vec <- c(
    data="data and models",
    cost="cost of last change")
  yfac <- function(l){
    factor(abbrev.vec[[l]], abbrev.vec)
  }
  COST <- function(dt){
    data.table(y.var=yfac("cost"), dt)
  }
  DATA <- function(dt){
    data.table(y.var=yfac("data"), dt)
  }
  if(require(ggplot2)){
    ggplot()+
      geom_rect(aes(
        xmin=start, xmax=end,
        fill=paste(changes),
        ymin=-Inf, ymax=Inf),
        alpha=0.5,
        data=labels.dt)+
      scale_fill_manual("label", values=label.colors)+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      facet_grid(y.var ~ ., scales="free")+
      geom_vline(aes(
        xintercept=change,
        size=Algorithm,
        color=Algorithm),
        data=change.dt)+
      geom_segment(aes(
        start-0.5, mean,
        size=Algorithm,
        color=Algorithm,
        xend=end+0.5, yend=mean),
        data=DATA(seg.dt))+
      geom_point(aes(
        position, signal),
        color=sig.color,
        shape=1,
        data=DATA(signal.dt))+
      scale_size_manual(values=algo.sizes)+
      scale_color_manual(values=algo.colors)+
      scale_shape_manual(values=algo.shapes)+
      ylab("")+
      scale_x_continuous(
        "position",
        breaks=seq(0, 100, by=10))+
      geom_point(aes(
        change, cost_candidates,
        color=Algorithm, shape=Algorithm),
        data=COST(cost.dt))
    }
  
})
