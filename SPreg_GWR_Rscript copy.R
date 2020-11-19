###Set your working directory.  It's easy in Rstudio to use the "Session" menu to do this.
setwd("G:/GIS_Materials/Session8")


###Load the following packages (install if necessary).
install.packages("spdep")
install.packages("maptools")
install.packages("rgdal")
install.packages("spatialreg")
library(spdep)
library(maptools)
library(rgdal)
library(spatialreg)


###Confirm your working directory and read in the "zonal_stats_TRI.shp" file 
###to create the TRI_poly object.
working <- getwd()
TRI_poly <- readOGR(dsn = working, layer ="zonal_stats_TRI")
names(TRI_poly)


###Create a queen's neighborhood weight matrix using the poly2nb command.
TRI_nbq <- poly2nb(TRI_poly)


###extract coordinates to plot the connectivity matrix for visualization.
coords <- coordinates(TRI_poly)
plot(TRI_poly)
plot(TRI_nbq, coords, add=T)


###convert the neighborhood matrix into a list so that the connections between counties can be used in
###Moran's I test.
summary(TRI_nbq)
TRI_nbq_w <- nb2listw(TRI_nbq)


###Convert Exposure variable to z-form and then create the lag of that variable.
TRI_poly$sQuali_Life <- scale(TRI_poly$Quali_Life)
TRI_poly$lag_sQL <- lag.listw(TRI_nbq_w,TRI_poly$sQuali_Life)
summary(TRI_poly$sQuali_Life)
summary(TRI_poly$lag_sQL)

names(TRI_poly)
head(TRI_poly)
TRI_data <- as.data.frame(TRI_poly)
head(TRI_data)

###Run the morans I test and plot the results.
moran.test(TRI_poly$sQuali_Life, listw=TRI_nbq_w)
moran.plot(as.vector(TRI_poly$sQuali_Life), listw=TRI_nbq_w,labels=as.character(TRI_poly$NAME), 
           xlim=c(-3,3),ylim=c(-2,2),
                 main="Moran's I = 0.4299, p-value < 0.001", 
           xlab="Quality of Life",ylab="Spatial Lag Quality of Life",pch=19)


###Set the county ID to the county FIPS ("COUNTY") and extract the LISA p-values.
head(TRI_poly)
fips <- (TRI_poly$COUNTY)
head(fips)
MSlocI <- localmoran(TRI_poly$Quali_Life, TRI_nbq_w)
fips <- as.data.frame(fips)
MSlocI_combine <- cbind(fips,MSlocI)
View(MSlocI_combine)

###Identify signficant clusters based on the data values in z-form and LISA p-values.
TRI_poly$lisa_cl <- NULL
TRI_poly$lisa_cl <- 5
TRI_poly@data[(TRI_poly$sQuali_Life>=0 & TRI_poly$lag_sQL >=0) & MSlocI_combine[,6]<=0.05, "lisa_cl"] <- 1
TRI_poly@data[(TRI_poly$sQuali_Life<=0 & TRI_poly$lag_sQL <=0) & MSlocI_combine[,6]<=0.05, "lisa_cl"] <- 2
TRI_poly@data[MSlocI_combine[,6]>=0.05, "lisa_cl"] <- 5


###Write the LISA cluster information to a new .csv file for use in QGIS.
names(TRI_poly)
TRI_data <-as.data.frame(TRI_poly)
table(TRI_data$lisa_cl)
names(TRI_data)
write.csv(TRI_data[,c(7,32:34)],"TRI_LISA.csv")

#******SPATIAL REGRESSION ***********************

###Test baseline linear model.
TRI.lm <- lm(Quali_Life~TRImean + POV + PBLK, data=TRI_poly)
summary(TRI.lm)


###Run Langrane Multiplier tests to identify the type of spatial regression model to run.
TRI.lagrange <- lm.LMtests(TRI.lm,TRI_nbq_w, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(TRI.lagrange)


###Specify Spatial Lag Model.
TRI.lag <- spatialreg::lagsarlm(Quali_Life~TRImean + POV + PBLK, data=TRI_poly, TRI_nbq_w, tol.solve = 1.0e-15)
TRI.err <- spatialreg::errorsarlm(Quali_Life~TRImean + POV + PBLK, data=TRI_poly, TRI_nbq_w, tol.solve = 1.0e-15)
TRI.sar <- spatialreg::lagsarlm(Quali_Life~TRImean + POV + PBLK, data=TRI_poly, TRI_nbq_w, type = "mixed", tol.solve = 1.0e-15)

summary(TRI.lag)
summary(TRI.err)
summary(TRI.sar)


##### GWR #################################
install.packages("spgwr")
library(spgwr)

bwG <- gwr.sel(Quali_Life~TRImean + POV + PBLK, data=TRI_poly, gweight=gwr.Gauss, verbose=TRUE)

gwrG <- gwr(Quali_Life~TRImean + POV + PBLK, data=TRI_poly, bandwidth=bwG, gweight=gwr.Gauss)

gwrG

names(gwrG)

names (gwrG$SDF)

spplot(gwrG$SDF, "localR2")

spplot(gwrG$SDF, "TRImean")

spplot(gwrG$SDF, "POV")

getwd()
writeOGR(gwrG$SDF, 
         dsn = working, 
         layer= "GWR_Results_for_QGIS",
         driver="ESRI Shapefile")
