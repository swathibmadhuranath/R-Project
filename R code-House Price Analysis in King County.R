library(stargazer)

# rename dataset
house<-kc_house_data

# add Age variable
house$Age <- 2015 - house$yr_built

# Table 1 - summary statistics for selected variables used in our models
stargazer(house[c('sqft_living','Age','grade')], median = TRUE, 
          type = 'text', digits = 1,
          title = 'Table 1: Summary Statistics for Selected Variables')

# Table 2 
stargazer(m1,m2,m3, type = 'text', dep.var.labels = 'House Price Analysis',
          title = 'Table 2: MLR Results', digits = 3)

#Appendix
#summary statistics for all variables attached in appendix
stargazer(house, type = 'text', title = 'Summary Statistics', median = TRUE, digits = 1)

#regression models
m1<-lm(log(house$price)~house$sqft_living)
m2<-lm(log(house$price)~house$sqft_living+house$Age)
m3<-lm(log(house$price)~house$sqft_living+house$Age+house$grade)

# regression results
summary(m1)
summary(m2)
summary(m3)

# plot all models
plot(m1)
plot(m2)
plot(m3)

house$yr_bORr <- apply(cbind(yr_built,yr_renovated),1,max)
variables("price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","waterfront","view","condition","grade",
          "sqft_above","sqft_basement","yr_bORr","zipcode","sqft_living15","sqft_lot15")

prelim_fit <- lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+sqft_above+sqft_basement+
                   floors+waterfront+view+condition+grade+yr_bORr+
                   as.factor(zipcode)+sqft_living15+sqft_lot15, data = house)
par(mfrow=c(2,2))
plot(prelim_fit)
# QQplot: residual is not normal
# fitted vs res: variance is no equal

# attempt: log transformation
house$logPrice <- log(price)
prelim_fit2 <- lm(logPrice ~ bedrooms+bathrooms+sqft_living+sqft_lot+sqft_above+sqft_basement+
                    floors+waterfront+view+condition+grade+yr_bORr+
                    as.factor(zipcode)+sqft_living15+sqft_lot15, data = house)
par(mfrow=c(2,2))
plot(prelim_fit2)

