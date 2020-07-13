data <- read.csv("replaced.csv")

data$bg <- ifelse(data$ownership=="SFF", 0, 1)


data_roa <- data[,c( 'sa_company_name', 'ownership', 'tobin_q', 'roa', 'size_assets', 'size_total_sales', 'ln_age', 'loans', 'marketing', 'research', 'investment', 'time_clock', 'ind', 'fy')]

data_ros <- data[,c('ros', 'size_assets', 'size_total_sales', 'ln_age', 'loans', 'marketing', 'research', 'investment', 'time_clock', 'ind', 'fy')]

data_tobin <- data[,c('tobin_q', 'size_assets', 'size_total_sales', 'ln_age', 'loans', 'marketing', 'research', 'investment', 'time_clock', 'ind', 'fy')]

library(VIM)
list_na <- colnames(data_roa)[ apply(data_roa, 2, anyNA) ]
list_na
mice_plot <- aggr(data_roa, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, labels=names(data_roa), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

library(dplyr)
# Exclude the missing observations
data_roa_drop <-data_roa %>%
  na.omit()		
dim(data_roa_drop)

model1 <- lm(roa~ size_assets + size_total_sales + ln_age + loans + marketing + research + investment + time_clock, data= data_roa_drop)
summary(model1)
library(car)
vif(model1)
cook_dist=cooks.distance(model1)
barplot(cook_dist)
length(cook_dist)

cookdist2=cook_dist[cook_dist < 0.005]
barplot(cookdist2)
length(cookdist2)

data_roa_removeOutlier=data_roa_drop[cook_dist < 0.005,]

model2 <- lm(roa~ size_assets + size_total_sales + ln_age + loans + marketing + research + investment + time_clock, data= data_roa_removeOutlier)
summary(model2)


library(lme4)
model3 <- lmer(roa~ size_assets + size_total_sales + ln_age + loans + marketing + research + investment + time_clock + (1|ind) + (1|fy), data= data_roa_removeOutlier)
summary(model3)

data_roa_removeOutlier$year_factor <- as.factor(data_roa_removeOutlier$fy)
data_roa_removeOutlier$ind_factor <- as.factor(data_roa_removeOutlier$ind)


model4 <- lm(roa~ size_assets + size_total_sales + ln_age + loans + marketing + research + investment + time_clock + ind_factor + year_factor, data= data_roa_removeOutlier)
summary(model4)


model5 <- lmer(roa~ size_assets + size_total_sales + ln_age + loans + marketing + research + investment + year_factor + (1|ind), data= data_roa_removeOutlier)
summary(model5)


resids = model4$resid
fits = model4$fitted
plot(fits, resids, xlab="Fitted Values",ylab="Residuals")
abline(0,0,col="red")

hist(residuals(model4),xlab="Residuals",main="Histogram of residuals",col="blue", breaks = 100)
qqnorm(residuals(model4))
qqline(residuals(model4),col="blue")
summary(model4)


