library(readxl)
covidpredict <- read_excel("C:/Users/TomKu/OneDrive/Dokumente/PRediction/covidpredict-1.xlsx")
View(covidpredict)

dev <- covidpredict[covidpredict$set=="dev", ]

summary(dev)

model <- glm(data=recodedDev, 
                  formula = mortality~rr+oxygen_sat+urea+crp+gcs+age+female+comorbidity, family=binomial)
model
summary(model)

Tom <- data.frame(rr=22, oxygen_sat=94, gcs=15, urea=7, 
                  crp=84.9, age=24, female=0, comorbidity=0)
predict(model, newdata=Tom, type="response")

install.packages("rms")
library(rms)


dev <- dplyr::mutate(dev,
                            mortality = as.factor(mortality))

model_rms <- lrm(data=dev, mortality~rr+oxygen_sat+urea+crp+
                   gcs+age+female+comorbidity, x=TRUE, y=TRUE)
model_rms


#assignment 9
model_rms2 <-lrm(data=dev, 
                 mortality ~ age + female + comorbidity + gcs +
                   rcs(rr,3) + rcs(oxygen_sat,3) + rcs(urea,3) +rcs(crp,3) + age*comorbidity, 
                 x=TRUE, y=TRUE)

model_rms2


p<-Predict(model_rms, age=seq(18:100), female=0, comorbidity = c(0,1,2), 
           rr=20, oxygen_sat=94, gcs=15, urea=7, crp=84.9)
ggplot(p)


## Practical 2 Assignment 1 

val <- covidpredict[covidpredict$set=="val",]
val <- dplyr::mutate(val,
                     mortality = as.factor(mortality))


summary(val$mortality)

## 1 - 1362
## 0 - 2184

valid <- validate(model_rms, method="boot", B=200)
valid[1,]

cstatapp <- 0.5*(valid[1,]+1)
cstatapp

cstatcorr <- 0.5*(0.5423+1)
cstatcorr

cal <- calibrate(model_rms, B=200)
plot(cal)
summary(cal)

valid2 <- validate(model_rms2, method = "boot", B=200)
valid2

cstatapp2 <- 0.5*(valid2[1,]+1)
cstatapp2
cal2 <- calibrate(model_rms2, B=200)
plot(cal2)


val <- covidpredict[covidpredict$set=="val",]
### val <- dplyr::mutate(val, mortality = as.factor(mortality)) --> when doing this it does not work 
val <- covidpredict[covidpredict$set=="val",]
p_pred <- predict(model_rms, val, type="fitted")
pre
val.prob(p_pred, val$mortality, g=10)


val$pred <- p_pred
summary(val$pred)

install.packages("rmda")
library("rmda")

val <- cbind(val, p_pred)

dca <- decision_curve(mortality~p_pred, data= val, fitted.risk=TRUE)
dca <- decision_curve(mortality~pred, data= val, fitted.risk=TRUE)
?decision_curve
plot_decision_curve(dca)

### why is the standardized net benefit 

