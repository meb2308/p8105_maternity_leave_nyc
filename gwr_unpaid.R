
setwd("~/Desktop/Data Science/p8105_maternity_leave_nyc/map")


library(spdep)
library(maptools)
library(rgdal)
library(spatialreg)
library(sf)


unpaid_poly <- readOGR(dsn = "nyc_only_zips.shp", layer = "nyc_only_zips")
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
unpaid_poly$swksunpaid <- scale(as.numeric(unpaid_poly$wksunpaid))
unpaid_poly$lag_sWU <- lag.listw(unpaid_nbq_w, unpaid_poly$swksunpaid, zero.policy=TRUE, NAOK=TRUE)
summary(unpaid_poly$swksunpaid)
summary(unpaid_poly$lag_sWU)

names(unpaid_poly)
head(unpaid_poly)
unpaid_data <- as.data.frame(unpaid_poly)
head(unpaid_data)

###Run the morans I test.
moran.test(unpaid_poly$swksunpaid, listw=unpaid_nbq_w, na.action = na.omit, zero.policy = TRUE)
###moran's I statistic: 0.055, p-value = 0.2131
#******REGRESSION ***********************

###Test baseline linear model.
linear_df = read.csv("data/merged_wfls.csv")
View(linear_df)

unpaid.lm <- lm(wksunpaid~jobtypefix + parenttype + as.numeric(leaveweeks) + edtype + race + X.family_in + X.borough, data=unpaid_poly)
summary(unpaid.lm)
###how to make the reference category different?

unpaid.lm %>% 
  broom::glance()

unpaid.lm %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value) %>% 
  mutate(
    term = str_replace(term, "^jobtypefix", "Job type: "),
    term = str_replace(term, "^parenttype",  "Partner: "),
    term = str_replace(term, "^as.numeric(leaveweeks)",  "Number of weeks on leave"),
    term = str_replace(term, "^edtype", "Education: "),
    term = str_replace(term, "^race", "Race: "),
    term = str_replace(term, "^X.family_in", "Family Income: "),
    term = str_replace(term, "^X.borough", "Borough: "),
    term = str_replace(term, "^Job type: 2", "Job type: Private"),
    term = str_replace(term, "^Job type: 3", "Job type: Non-profit"),
    term = str_replace(term, "^Job type: 4", "Job type: Self-employed"),
    term = str_replace(term, "^Partner: 2", "Partner: Single parent"),
    term = str_replace(term, "^Education: 3", "Education: No high school degree"),
    term = str_replace(term, "^Education: 4", "Education: High school degree/GED"),
    term = str_replace(term, "^Education: 5", "Education: Some college or technical school"),
    term = str_replace(term, "^Education: 6", "Education: Four-year college or higher"),
    term = str_replace(term, "^Race: 2", "Race: Black/African American"),
    term = str_replace(term, "^Race: 3", "Race: Asian"),
    term = str_replace(term, "^Race: 8", "Race: Other")) %>% 
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

