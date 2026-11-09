###############################################################
# Real-data benchmark helpers
###############################################################

truncate_at <- function(data, time, status, horizon) {
  status[time > horizon] <- 0L
  data$time <- pmin(time, horizon)
  data$event <- status
  data
}

cif_at <- function(time, status, cause, horizon) {
  fit <- cmprsk::cuminc(ftime = time, fstatus = status)
  obj <- fit[[paste("1", cause)]]
  stats::approx( obj$time, obj$est, xout = horizon, method = "constant", 
                 ties = "ordered", rule = 2 )$y
}

benchmark_metrics <- function(data, event_interest, competing_event,
                              horizon, dataset) {
  
  stopifnot(all(c("time", "event") %in% names(data)))
  
  fit <- kmaudit::tod(data$time, data$event, censor = 0, times = horizon)
  km_fit <- survival::survfit( 
    survival::Surv(data$time, data$event == event_interest) ~ 1 )
  
  km_risk <- 1 - summary(km_fit, times = horizon, extend = TRUE)$surv
  cif <- cif_at(data$time, data$event, event_interest, horizon)
  
  n_interest <- sum(data$event == event_interest)
  n_competing <- sum(data$event == competing_event)
  
  data.frame(
    dataset = dataset,
    n = nrow(data),
    competing_frequency = n_competing / (n_interest + n_competing),
    censoring_frequency = mean(data$event == 0),
    oi = kmaudit::oi( data$time, data$event, interest = event_interest,
                      competing = competing_event, ties = "average" ),
    km_risk = km_risk,
    cif = cif,
    cause_specific_overestimation = km_risk - cif,
    tod = fit$estimates$TOD[[1]]
  )
}

prepare_pbc <- function() {
  d <- survival::pbc
  horizon <- 10
  d <- truncate_at(d, d$time / 365.25, d$status, horizon)
  list(data = d, interest = 2, competing = 1, horizon = horizon, name = "PBC")
}

prepare_melanoma <- function() {
  d <- MASS::Melanoma
  horizon <- 10
  event <- ifelse(d$status == 1, 1L, ifelse(d$status == 3, 2L, 0L))
  d <- truncate_at(d, d$time / 365.25, event, horizon)
  list(data = d, interest = 1, competing = 2, horizon = horizon, name = "Melanoma")
}

prepare_mgus <- function() {
  d <- survival::mgus
  horizon <- 25
  
  time <- ifelse( is.na(d$pctime), d$futime, pmin(d$pctime, d$futime) ) / 365.25
  
  event <- ifelse( !is.na(d$pctime) & d$pctime <= d$futime, 1L, 
                   ifelse(d$death == 1, 2L, 0L) )
  
  d <- truncate_at(d, time, event, horizon)
  list(data = d, interest = 1, competing = 2, horizon = horizon, name = "MGUS")
}

prepare_transplant <- function() {
  d <- survival::transplant
  horizon <- 2
  
  event <- ifelse( d$event == "death", 1L, 
                   ifelse(d$event == "ltx", 2L, 0L) )
  
  d <- truncate_at(d, d$futime / 365.25, event, horizon)
  list(data = d, interest = 1, competing = 2, horizon = horizon, name = "TRANSPLANT")
}

prepare_myeloid <- function() {
  d <- survival::myeloid
  horizon <- 5
  
  relapse <- ifelse(is.na(d$rltime), Inf, d$rltime)
  death <- ifelse(d$death == 1, d$futime, Inf)
  time <- pmin(relapse, death, d$futime)
  
  event <- ifelse( relapse <= death & relapse <= d$futime, 1L,
                   ifelse(death < relapse & death <= d$futime, 2L, 0L) )
  
  d <- truncate_at(d, time / 365.25, event, horizon)
  list(data = d, interest = 1, competing = 2, horizon = horizon, name = "MYELOID")
}

prepare_udca <- function() {
  d <- survival::udca
  horizon <- 5
  
  death <- as.numeric(d$death.dt - d$entry.dt) / 365.25
  transplant <- as.numeric(d$tx.dt - d$entry.dt) / 365.25
  censor <- as.numeric(d$last.dt - d$entry.dt) / 365.25
  
  death[is.na(death)] <- Inf
  transplant[is.na(transplant)] <- Inf
  
  time <- pmin(death, transplant, censor)
  
  event <- ifelse( death <= transplant & death <= censor, 1L,
                   ifelse(transplant <= death & transplant <= censor, 2L, 0L) )
  
  d <- truncate_at(d, time, event, horizon)
  list(data = d, interest = 1, competing = 2, horizon = horizon, name = "UDCA")
}