
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
moran.test(unpaid_poly$swksunpaid, listw=unpaid_nbq_w, na.action = na.omit, zero.policy = TRUE)

#******SPATIAL REGRESSION ***********************

###Test baseline linear model.
linear_df = read.csv("data/merged_wfls.csv")
View(linear_df)

unpaid.lm <- lm(unpaid_leave_weeks~job_type + partner + leave_weeks + education + race + family_income, data=linear_df)
summary(unpaid.lm)
###how to make the reference category different?

unpaid.lm %>% 
  broom::glance()

unpaid.lm %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value) %>% 
  mutate(
    term = str_replace(term, "^job_type", "Job type: "),
    term = str_replace(term, "^partner",  "Partner: "),
    term = str_replace(term, "leave_weeks",  "Number of weeks on leave"),
    term = str_replace(term, "^education", "Education: "),
    term = str_replace(term, "^race", "Race: "),
    term = str_replace(term, "^family_income", "Family Income: ")) %>% 
  knitr::kable(digits = 3)

modelr::add_residuals(linear_df, unpaid.lm)
modelr::add_predictions(linear_df, unpaid.lm)

linear_df %>% 
  modelr::add_residuals(unpaid.lm) %>% 
  ggplot(aes(x = job_type, y = resid)) + geom_violin()

linear_df %>% 
  modelr::add_residuals(unpaid.lm) %>% 
  ggplot(aes(x = partner, y = resid)) + geom_violin()

linear_df %>% 
  modelr::add_residuals(unpaid.lm) %>% 
  ggplot(aes(x = leave_weeks, y = resid)) + geom_violin()

linear_df %>% 
  modelr::add_residuals(unpaid.lm) %>% 
  ggplot(aes(x = education, y = resid)) + geom_violin()

linear_df %>% 
  modelr::add_residuals(unpaid.lm) %>% 
  ggplot(aes(x = race, y = resid)) + geom_violin()

linear_df %>% 
  modelr::add_residuals(unpaid.lm) %>% 
  ggplot(aes(x = family_income, y = resid)) + geom_violin()

###we probably need to do null/alt testing for the cat variables with more than 2 levels...how should we do this? f test? anova?
