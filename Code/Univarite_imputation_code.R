rm(list = ls())
library(imputeTS)
library(zoo)
library(TSA)
library(tidyverse)
library(missMethods)
library(assertr)
data("sunspots")
data("beersales")
data("LakeHuron")
data("lynx")
set.seed(123)

seeds <- sample(1:1000,15, replace = F)

N_times = 3
# Generate the missingness in the datasets
data_frames <- list(data.frame(), data.frame(), data.frame())
for(i in 1:N_times){
  set.seed(seeds[i])
# sunspots
sunspots_missing_10 <-  delete_MCAR(as.matrix(sunspots), p = 0.1) |> ts(frequency = frequency(sunspots), start = start(sunspots))
sunspots_missing_20 <-  delete_MCAR(as.matrix(sunspots), p = 0.2) |> ts(frequency = frequency(sunspots), start = start(sunspots))
sunspots_missing_30 <-  delete_MCAR(as.matrix(sunspots), p = 0.3) |> ts(frequency = frequency(sunspots), start = start(sunspots))
sunspots_missing_40 <-  delete_MCAR(as.matrix(sunspots), p = 0.4) |> ts(frequency = frequency(sunspots), start = start(sunspots))
sunspots_missing_50 <-  delete_MCAR(as.matrix(sunspots), p = 0.5) |> ts(frequency = frequency(sunspots), start = start(sunspots))
sunspots_missing_60 <-  delete_MCAR(as.matrix(sunspots), p = 0.6) |> ts(frequency = frequency(sunspots), start = start(sunspots))
sunspots_missing_70 <-  delete_MCAR(as.matrix(sunspots), p = 0.7) |> ts(frequency = frequency(sunspots), start = start(sunspots))

#Lake Huron

lake_missing_10 <-  delete_MCAR(as.matrix(LakeHuron), p = 0.1) |> ts(frequency = frequency(LakeHuron), start = start(LakeHuron))
lake_missing_20 <-  delete_MCAR(as.matrix(LakeHuron), p = 0.2) |> ts(frequency = frequency(LakeHuron), start = start(LakeHuron))
lake_missing_30 <-  delete_MCAR(as.matrix(LakeHuron), p = 0.3) |> ts(frequency = frequency(LakeHuron), start = start(LakeHuron))
lake_missing_40 <-  delete_MCAR(as.matrix(LakeHuron), p = 0.4) |> ts(frequency = frequency(LakeHuron), start = start(LakeHuron))
lake_missing_50 <-  delete_MCAR(as.matrix(LakeHuron), p = 0.5) |> ts(frequency = frequency(LakeHuron), start = start(LakeHuron))
lake_missing_60 <-  delete_MCAR(as.matrix(LakeHuron), p = 0.6) |> ts(frequency = frequency(LakeHuron), start = start(LakeHuron))
lake_missing_70 <-  delete_MCAR(as.matrix(LakeHuron), p = 0.7) |> ts(frequency = frequency(LakeHuron), start = start(LakeHuron))

# Beer

beer_missing_10 <-  delete_MCAR(as.matrix(beersales), p = 0.1) |> ts(frequency = frequency(beersales), start = start(beersales))
beer_missing_20 <-  delete_MCAR(as.matrix(beersales), p = 0.2) |> ts(frequency = frequency(beersales), start = start(beersales))
beer_missing_30 <-  delete_MCAR(as.matrix(beersales), p = 0.3) |> ts(frequency = frequency(beersales), start = start(beersales))
beer_missing_40 <-  delete_MCAR(as.matrix(beersales), p = 0.4) |> ts(frequency = frequency(beersales), start = start(beersales))
beer_missing_50 <-  delete_MCAR(as.matrix(beersales), p = 0.5) |> ts(frequency = frequency(beersales), start = start(beersales))
beer_missing_60 <-  delete_MCAR(as.matrix(beersales), p = 0.6) |> ts(frequency = frequency(beersales), start = start(beersales))
beer_missing_70 <-  delete_MCAR(as.matrix(beersales), p = 0.7) |> ts(frequency = frequency(beersales), start = start(beersales))

# Lynx

lynx_missing_10 <-  delete_MCAR(as.matrix(lynx), p = 0.1) |> ts(frequency = frequency(lynx), start = start(lynx))
lynx_missing_20 <-  delete_MCAR(as.matrix(lynx), p = 0.2) |> ts(frequency = frequency(lynx), start = start(lynx))
lynx_missing_30 <-  delete_MCAR(as.matrix(lynx), p = 0.3) |> ts(frequency = frequency(lynx), start = start(lynx))
lynx_missing_40 <-  delete_MCAR(as.matrix(lynx), p = 0.4) |> ts(frequency = frequency(lynx), start = start(lynx))
lynx_missing_50 <-  delete_MCAR(as.matrix(lynx), p = 0.5) |> ts(frequency = frequency(lynx), start = start(lynx))
lynx_missing_60 <-  delete_MCAR(as.matrix(lynx), p = 0.6) |> ts(frequency = frequency(lynx), start = start(lynx))
lynx_missing_70 <-  delete_MCAR(as.matrix(lynx), p = 0.7) |> ts(frequency = frequency(lynx), start = start(lynx))

sunspots_names <- c("sunspots_missing_10", "sunspots_missing_20", "sunspots_missing_30", "sunspots_missing_40", "sunspots_missing_50", "sunspots_missing_60", "sunspots_missing_70")

lake_names <- c("lake_missing_10", "lake_missing_20", "lake_missing_30", "lake_missing_40", "lake_missing_50", "lake_missing_60", "lake_missing_70")

beer_names <- c("beer_missing_10", "beer_missing_20", "beer_missing_30", "beer_missing_40", "beer_missing_50", "beer_missing_60", "beer_missing_70")

lynx_names <- c("lynx_missing_10", "lynx_missing_20", "lynx_missing_30", "lynx_missing_40", "lynx_missing_50", "lynx_missing_60", "lynx_missing_70")


# Fit the univariate methods and gather results
sunspots_locf_list <- list(
  sunspots_missing_10.locf <- sunspots_missing_10 |> zoo::na.locf(na.rm = F),
  sunspots_missing_20.locf <- sunspots_missing_20 |> zoo::na.locf(na.rm = F),
  sunspots_missing_30.locf <- sunspots_missing_30 |> zoo::na.locf(na.rm = F),
  sunspots_missing_40.locf <- sunspots_missing_40 |> zoo::na.locf(na.rm = F),
  sunspots_missing_50.locf <- sunspots_missing_50 |> zoo::na.locf(na.rm = F),
  sunspots_missing_60.locf <- sunspots_missing_60 |> zoo::na.locf(na.rm = F),
  sunspots_missing_70.locf <- sunspots_missing_70 |> zoo::na.locf(na.rm = F)
)

lake_locf_list <- list(
  lake_missing_10.locf <- lake_missing_10 |> zoo::na.locf(na.rm = F),
  lake_missing_20.locf <- lake_missing_20 |> zoo::na.locf(na.rm = F),
  lake_missing_30.locf <- lake_missing_30 |> zoo::na.locf(na.rm = F),
  lake_missing_40.locf <- lake_missing_40 |> zoo::na.locf(na.rm = F),
  lake_missing_50.locf <- lake_missing_50 |> zoo::na.locf(na.rm = F),
  lake_missing_60.locf <- lake_missing_60 |> zoo::na.locf(na.rm = F),
  lake_missing_70.locf <- lake_missing_70 |> zoo::na.locf(na.rm = F)
)

beer_locf_list <- list(
  beer_missing_10.locf <- beer_missing_10 |> zoo::na.locf(na.rm = F),
  beer_missing_20.locf <- beer_missing_20 |> zoo::na.locf(na.rm = F),
  beer_missing_30.locf <- beer_missing_30 |> zoo::na.locf(na.rm = F),
  beer_missing_40.locf <- beer_missing_40 |> zoo::na.locf(na.rm = F),
  beer_missing_50.locf <- beer_missing_50 |> zoo::na.locf(na.rm = F),
  beer_missing_60.locf <- beer_missing_60 |> zoo::na.locf(na.rm = F),
  beer_missing_70.locf <- beer_missing_70 |> zoo::na.locf(na.rm = F)
)
lynx_locf_list <- list(
  lynx_missing_10.locf <- lynx_missing_10 |> zoo::na.locf(na.rm = F),
  lynx_missing_20.locf <- lynx_missing_20 |> zoo::na.locf(na.rm = F),
  lynx_missing_30.locf <- lynx_missing_30 |> zoo::na.locf(na.rm = F),
  lynx_missing_40.locf <- lynx_missing_40 |> zoo::na.locf(na.rm = F),
  lynx_missing_50.locf <- lynx_missing_50 |> zoo::na.locf(na.rm = F),
  lynx_missing_60.locf <- lynx_missing_60 |> zoo::na.locf(na.rm = F),
  lynx_missing_70.locf <- lynx_missing_70 |> zoo::na.locf(na.rm = F)
)

## mean Impute
sunspots_mean_list <- list(
  sunspots_missing_10.mean <- sunspots_missing_10 |> zoo::na.aggregate(FUN = mean),
  sunspots_missing_20.mean  <- sunspots_missing_20 |> zoo::na.aggregate(FUN = mean),
  sunspots_missing_30.mean  <- sunspots_missing_30 |> zoo::na.aggregate(FUN = mean),
  sunspots_missing_40.mean <- sunspots_missing_40 |> zoo::na.aggregate(FUN = mean),
  sunspots_missing_50.mean <- sunspots_missing_50 |> zoo::na.aggregate(FUN = mean),
  sunspots_missing_60.mean <- sunspots_missing_60 |> zoo::na.aggregate(FUN = mean),
  sunspots_missing_70.mean <- sunspots_missing_70 |> zoo::na.aggregate(FUN = mean))

lake_mean_list <- list(
  lake_missing_10.mean <- lake_missing_10 |> zoo::na.aggregate(FUN = mean),
  lake_missing_20.mean <- lake_missing_20 |> zoo::na.aggregate(FUN = mean),
  lake_missing_30.mean <- lake_missing_30 |> zoo::na.aggregate(FUN = mean),
  lake_missing_40.mean <- lake_missing_40 |> zoo::na.aggregate(FUN = mean),
  lake_missing_50.mean <- lake_missing_50 |> zoo::na.aggregate(FUN = mean),
  lake_missing_60.mean <- lake_missing_60 |> zoo::na.aggregate(FUN = mean),
  lake_missing_70.mean <- lake_missing_70 |> zoo::na.aggregate(FUN = mean))

beer_mean_list <- list(
  beer_missing_10.mean <- beer_missing_10 |> zoo::na.aggregate(FUN = mean),
  beer_missing_20.mean <- beer_missing_20 |> zoo::na.aggregate(FUN = mean),
  beer_missing_30.mean <- beer_missing_30 |> zoo::na.aggregate(FUN = mean),
  beer_missing_40.mean <- beer_missing_40 |> zoo::na.aggregate(FUN = mean),
  beer_missing_50.mean <- beer_missing_50 |> zoo::na.aggregate(FUN = mean),
  beer_missing_60.mean <- beer_missing_60 |> zoo::na.aggregate(FUN = mean),
  beer_missing_70.mean <- beer_missing_70 |> zoo::na.aggregate(FUN = mean))

lynx_mean_list <- list(
  lynx_missing_10.mean <- lynx_missing_10 |> zoo::na.aggregate(FUN = mean),
  lynx_missing_20.mean <- lynx_missing_20 |> zoo::na.aggregate(FUN = mean),
  lynx_missing_30.mean <- lynx_missing_30 |> zoo::na.aggregate(FUN = mean),
  lynx_missing_40.mean <- lynx_missing_40 |> zoo::na.aggregate(FUN = mean),
  lynx_missing_50.mean <- lynx_missing_50 |> zoo::na.aggregate(FUN = mean),
  lynx_missing_60.mean <- lynx_missing_60 |> zoo::na.aggregate(FUN = mean),
  lynx_missing_70.mean <- lynx_missing_70 |> zoo::na.aggregate(FUN = mean))


# Linear approximaton

sunspots_linear_list <- list(
  sunspots_missing_10.linear <- sunspots_missing_10 |> zoo::na.approx(na.rm = F),
  sunspots_missing_20.linear <- sunspots_missing_20 |> zoo::na.approx(na.rm = F),
  sunspots_missing_30.linear <- sunspots_missing_30 |> zoo::na.approx(na.rm = F),
  sunspots_missing_40.linear <- sunspots_missing_40 |> zoo::na.approx(na.rm = F),
  sunspots_missing_50.linear <- sunspots_missing_50 |> zoo::na.approx(na.rm = F),
  sunspots_missing_60.linear <- sunspots_missing_60 |> zoo::na.approx(na.rm = F),
  sunspots_missing_70.linear <- sunspots_missing_70 |> zoo::na.approx(na.rm = F))

lake_linear_list <- list(
  lake_missing_10.linear <- lake_missing_10 |> zoo::na.approx(na.rm = F),
  lake_missing_20.linear <- lake_missing_20 |> zoo::na.approx(na.rm = F),
  lake_missing_30.linear <- lake_missing_30 |> zoo::na.approx(na.rm = F),
  lake_missing_40.linear <- lake_missing_40 |> zoo::na.approx(na.rm = F),
  lake_missing_50.linear <- lake_missing_50 |> zoo::na.approx(na.rm = F),
  lake_missing_60.linear <- lake_missing_60 |> zoo::na.approx(na.rm = F),
  lake_missing_70.linear <- lake_missing_70 |> zoo::na.approx(na.rm = F))

beer_linear_list <- list(
  beer_missing_10.linear <- beer_missing_10 |> zoo::na.approx(na.rm = F),
  beer_missing_20.linear <- beer_missing_20 |> zoo::na.approx(na.rm = F),
  beer_missing_30.linear <- beer_missing_30 |> zoo::na.approx(na.rm = F),
  beer_missing_40.linear <- beer_missing_40 |> zoo::na.approx(na.rm = F),
  beer_missing_50.linear <- beer_missing_50 |> zoo::na.approx(na.rm = F),
  beer_missing_60.linear <- beer_missing_60 |> zoo::na.approx(na.rm = F),
  beer_missing_70.linear <- beer_missing_70 |> zoo::na.approx(na.rm = F))

lynx_linear_list <- list(
  lynx_missing_10.linear <- lynx_missing_10 |> zoo::na.approx(na.rm = F),
  lynx_missing_20.linear <- lynx_missing_20 |> zoo::na.approx(na.rm = F),
  lynx_missing_30.linear <- lynx_missing_30 |> zoo::na.approx(na.rm = F),
  lynx_missing_40.linear <- lynx_missing_40 |> zoo::na.approx(na.rm = F),
  lynx_missing_50.linear <- lynx_missing_50 |> zoo::na.approx(na.rm = F),
  lynx_missing_60.linear <- lynx_missing_60 |> zoo::na.approx(na.rm = F),
  lynx_missing_70.linear <- lynx_missing_70 |> zoo::na.approx(na.rm = F))


# Kalman filter

sunspots_kalman_list <- list(
  sunspots_missing_10.kalman <- sunspots_missing_10 |> na_kalman(),
  sunspots_missing_20.kalman <- sunspots_missing_20 |> na_kalman(),
  sunspots_missing_30.kalman <- sunspots_missing_30 |> na_kalman(),
  sunspots_missing_40.kalman <- sunspots_missing_40 |> na_kalman(),
  sunspots_missing_50.kalman <- sunspots_missing_50 |> na_kalman(),
  sunspots_missing_60.kalman <- sunspots_missing_60 |> na_kalman(),
  sunspots_missing_70.kalman <- sunspots_missing_70 |> na_kalman())

lake_kalman_list <- list(
  lake_missing_10.kalman <- lake_missing_10 |> na_kalman(),
  lake_missing_20.kalman <- lake_missing_20 |> na_kalman(),
  lake_missing_30.kalman <- lake_missing_30 |> na_kalman(),
  lake_missing_40.kalman <- lake_missing_40 |> na_kalman(),
  lake_missing_50.kalman <- lake_missing_50 |> na_kalman(),
  lake_missing_60.kalman <- lake_missing_60 |> na_kalman(),
  lake_missing_70.kalman <- lake_missing_70 |> na_kalman())

beer_kalman_list <- list(
  beer_missing_10.kalman <- beer_missing_10 |> na_kalman(),
  beer_missing_20.kalman <- beer_missing_20 |> na_kalman(),
  beer_missing_30.kalman <- beer_missing_30 |> na_kalman(),
  beer_missing_40.kalman <- beer_missing_40 |> na_kalman(),
  beer_missing_50.kalman <- beer_missing_50 |> na_kalman(),
  beer_missing_60.kalman <- beer_missing_60 |> na_kalman(),
  beer_missing_70.kalman <- beer_missing_70 |> na_kalman())

lynx_kalman_list <- list(
  lynx_missing_10.kalman <- lynx_missing_10 |> na_kalman(),
  lynx_missing_20.kalman <- lynx_missing_20 |> na_kalman(),
  lynx_missing_30.kalman <- lynx_missing_30 |> na_kalman(),
  lynx_missing_40.kalman <- lynx_missing_40 |> na_kalman(),
  lynx_missing_50.kalman <- lynx_missing_50 |> na_kalman(),
  lynx_missing_60.kalman <- lynx_missing_60 |> na_kalman(),
  lynx_missing_70.kalman <- lynx_missing_70 |> na_kalman())





#Calculate error metrics for each time series

RMSE2 <- function(new_series, og_series) {
 
  new_series[is.na(new_series)] <- 0
  og_series[is.na(og_series)] <- 0
  rmse_value <- MLmetrics::RMSE(new_series, og_series)
  return(rmse_value)
}


#Sunspots
sunspots.locf.rmse <- sapply(sunspots_locf_list, function(imputed) RMSE2(imputed, sunspots))
sunspots.mean.rmse <- sapply(sunspots_mean_list, function(imputed) RMSE2(imputed, sunspots))
sunspots.linear.rmse <- sapply(sunspots_linear_list, function(imputed) RMSE2(imputed, sunspots))
sunspots.kalman.rmse <- sapply(sunspots_kalman_list, function(imputed) RMSE2(imputed, sunspots))

RMSE_sunspots <- cbind(sunspots.locf.rmse, sunspots.mean.rmse,sunspots.linear.rmse, sunspots.kalman.rmse)

#Lake
lake.locf.rmse <- sapply(lake_locf_list, function(imputed) RMSE2(imputed, LakeHuron))
lake.mean.rmse <- sapply(lake_mean_list, function(imputed) RMSE2(imputed, LakeHuron))
lake.linear.rmse <- sapply(lake_linear_list, function(imputed) RMSE2(imputed, LakeHuron))
lake.kalman.rmse <- sapply(lake_kalman_list, function(imputed) RMSE2(imputed, LakeHuron))

RMSE_lake <- cbind(lake.locf.rmse, lake.mean.rmse,lake.linear.rmse, lake.kalman.rmse)

#Beer

beer.locf.rmse <- sapply(beer_locf_list, function(imputed) RMSE2(imputed, beersales))
beer.mean.rmse <- sapply(beer_mean_list, function(imputed) RMSE2(imputed, beersales))
beer.linear.rmse <- sapply(beer_linear_list, function(imputed) RMSE2(imputed, beersales))
beer.kalman.rmse <- sapply(beer_kalman_list, function(imputed) RMSE2(imputed, beersales))

RMSE_beer <- cbind(beer.locf.rmse, beer.mean.rmse,beer.linear.rmse, beer.kalman.rmse)

lynx.locf.rmse <- sapply(lynx_locf_list, function(imputed) RMSE2(imputed, lynx))
lynx.mean.rmse <- sapply(lynx_mean_list, function(imputed) RMSE2(imputed, lynx))
lynx.linear.rmse <- sapply(lynx_linear_list, function(imputed) RMSE2(imputed, lynx))
lynx.kalman.rmse <- sapply(lynx_kalman_list, function(imputed) RMSE2(imputed, lynx))

RMSE_lynx <- cbind(lynx.locf.rmse, lynx.mean.rmse,lynx.linear.rmse, lynx.kalman.rmse)

data_frames[[i]] <- cbind(RMSE_sunspots, RMSE_lake, RMSE_beer, RMSE_lynx)

}


average_rmse_frame <- Reduce('+', data_frames) / 3

