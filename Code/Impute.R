library(imputeTS)
library(zoo)
library(tidyverse)

x <- tsNH4
#Impute missing data using mean
na_interpolation(x)

#kalman <- na_kalman(x)


set.seed(123)
weatherAUS <- read.csv("C:/Users/Jacob/OneDrive - University of Canterbury/UC Stuff/4th year/Stat456/Project/Time-Series-Project/Code/weatherAUS.csv")


rainfall <- weatherAUS |> select(Date, Rainfall)
rainfall$Date <- rainfall$Date |> as.Date(format = "%Y-%m-%d")

original.seres <- zoo(rainfall$Rainfall, order.by = rainfall$Date)


data(Nile)


data("AirPassengers")
missing_index <- sample(seq_along(AirPassengers), size = floor(length(AirPassengers)*0.05))

AirPassengers_missing <- AirPassengers
AirPassengers_missing[missing_index] <- NA


#############################

#Last Observed Carried Forward (LOCF) Next Observation Carried Backward (NOCB)

Air.locf <- AirPassengers_missing |> na.locf()
plot(AirPassengers)
lines(Air.locf, type = "p", col = 'blue', lwd =2)

Air.nocb <- AirPassengers_missing |> na.locf(option = "nocb")
plot(AirPassengers)
lines(Air.locf, type = "p", col = 'blue', lwd =2)



#Mean/median

plot(AirPassengers)
Air.mean <- AirPassengers_missing |> na_mean()

lines(Air.mean, type = "p", col = 'blue', lwd =2)

#Air.median <- AirPassengers_missing |> na_median()
#lines(Air.median, type = "p", col = "green", lwd = 2)


Air.interp <- AirPassengers_missing |> na_interpolation(option = "linear")
plot(AirPassengers)
lines(Air.interp, type = "p", col = 'blue', lwd = 2)

Air.interp <- AirPassengers_missing |> na_kalman()
plot(AirPassengers)
lines(Air.interp, type = "p", col = 'blue', lwd = 2)


# Now do it with more missingness

data("AirPassengers")
missing_index <- sample(seq_along(AirPassengers), size = floor(length(AirPassengers)*0.20))

AirPassengers__more_missing <- AirPassengers
AirPassengers__more_missing[missing_index] <- NA







