library(imputeTS)
summary(tsNH4)
class(tsNH4)

x <- tsNH4
#Impute missing data using mean
na_interpolation(x)

kalman <- na_kalman(x)

seadec <- na_seadec(x)

ggplot_na_imputations(tsNH4, kalman, tsNH4Complete)

