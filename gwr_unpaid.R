
setwd("~/Desktop/Data Science/p8105_maternity_leave_nyc/map")


library(spdep)
library(maptools)
library(rgdal)
library(spatialreg)
library(sf)



unpaid_poly <- readOGR(dsn = "wksunpaidand%_postgeoda.shp", layer = "wksunpaidand%_postgeoda")
names(unpaid_poly)


###Create a queen's neighborhood weight matrix using the poly2nb command.
unpaid_nbq <- poly2nb(unpaid_poly)


###extract coordinates to plot the connectivity matrix for visualization.
coords <- coordinates(unpaid_poly)
plot(unpaid_poly)
plot(unpaid_nbq, coords, add=T)


###convert the neighborhood matrix into a list so that the connections between counties can be used in
###Moran's I test.
summary(unpaid_nbq)
unpaid_nbq_w <- nb2listw(unpaid_nbq, zero.policy=TRUE)


###Convert Exposure variable to z-form and then create the lag of that variable.
unpaid_poly$swksunpaid <- scale(unpaid_poly$wksunpaid)
unpaid_poly$lag_sWU <- lag.listw(unpaid_nbq_w, unpaid_poly$swksunpaid, zero.policy=TRUE, NAOK=TRUE)
summary(unpaid_poly$swksunpaid)
summary(unpaid_poly$lag_sWU)

names(unpaid_poly)
head(unpaid_poly)
unpaid_data <- as.data.frame(unpaid_poly)
head(unpaid_data)

###Run the morans I test.
 
#******SPATIAL REGRESSION ***********************

###Test baseline linear model.
unpaid.lm <- lm(unpaid_poly$swksunpaid~jobtypefix + leavetype + parenttype + leaveweeks + edtype + race, data=unpaid_poly)
summary(unpaid.lm)

