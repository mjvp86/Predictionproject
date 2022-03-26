#assignment 1

library(readxl)
covidpredict_1 <- read_excel("covidpredict-1.xlsx")
View(covidpredict_1)

library(readxl)
covidpredict <- read_excel("C:/Users/TomKu/OneDrive/Dokumente/PRediction/covidpredict-1.xlsx")
View(covidpredict)

library(dplyr)
dev <- covidpredict_1[covidpredict_1$set=="dev", ]
summary(dev)

recodedDev <- dplyr::mutate(dev,
                            mortality = as.factor(mortality),
                            date = as.Date(date))

summary(recodedDev)

modelTotal <- glm(data=recodedDev, 
                  formula = mortality~rr+oxygen_sat+urea+crp+
                    gcs+age+female+comorbidity, 
                  family=binomial)
modelTotal
summary(modelTotal)

#assignment 2

#Sanne
Sanne <- data.frame(rr=22, oxygen_sat=94, gcs=15, urea=7, 
                    crp=84.9, age=23, female=1, comorbidity=0)
predict(modelTotal, newdata=Sanne, type="response")

#Tom
Tom <- data.frame(rr=22, oxygen_sat=94, gcs=15, urea=7, 
                  crp=84.9, age=24, female=0, comorbidity=0)
predict(modelTotal, newdata=Tom, type="response")

#Renate
Renate <- data.frame(rr=22, oxygen_sat=94, gcs=15, urea=7, 
                    crp=84.9, age=27, female=1, comorbidity=0)
predict(modelTotal, newdata=Renate, type="response")

#assignment 3
Fict <- data.frame(rr=22, oxygen_sat=94, gcs=15, urea=7, 
                     crp=84.9, age=47, female=1, comorbidity=0)
predict(modelTotal, newdata=Fict, type="response")

#assignment 4
Fict2 <- data.frame(rr=22, oxygen_sat=94, gcs=15, urea=7, 
                   crp=84.9, age=70, female=1, comorbidity=0)
predict(modelTotal, newdata=Fict2, type="response")

#assignment 5
Fict3 <- data.frame(rr=35, oxygen_sat=76, gcs=15, urea=34, 
                   crp=200, age=76, female=0, comorbidity=2)
predict(modelTotal, newdata=Fict3, type="response")

#assignment 6
## Fict3 to ICU
## We will collaborate with professionals to determine which level is 
## too high of a risk. Furthermore, an ROC-curve can be used to support 
## the decision making.

#assignment 7
## No, because the dominant covid-variant has a lower mortality-rate. 
## However, if this tool will be adapted to the new variant, and it is valid 
## and reliable, it is a sufficient tool to use in healthcare. 

#assignment 8
install.packages("rms")
library(rms)

model_rms <- lrm(data=recodedDev, mortality~rr+oxygen_sat+urea+crp+
                   gcs+age+female+comorbidity, x=TRUE, y=TRUE)
model_rms

#assignment 9
model_rms2 <- lrm(data=dev, mortality~rr+oxygen_sat+urea+crp+
                   +age+comorbidity+age*comorbidity, x=TRUE, y=TRUE)
model_rms2

p<-Predict(model_rms, age=seq(18:100), female=0, comorbidity = c(0,1,2), 
           rr=20, oxygen_sat=94, gcs=15, urea=7, crp=84.9)
ggplot(p)

