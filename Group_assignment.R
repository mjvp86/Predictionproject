
## Install several packages
source("Functions.R")
list.of.packages <- c("haven", "dplyr", "reshape","reshape2", "magrittr", "httr","kableExtra", "ggplot2", "GGally", "epiDisplay", "rms", "rmda","glmnet") 
installRequiredPackages(list.of.packages)

## Reproducible way to import dataset 
pathToData <- '.' #this means "the same folder as where my markdown script is"
pancreatitis <- read.csv( 'pancreatitis.csv')
summary(pancreatitis)
#recoding for despcriptive 
recoded_pancreatitis <- dplyr::mutate(pancreatitis,
                                      site = as.factor(site),
                                      age = as.numeric(age),
                                      risk = as.numeric(risk),
                                      gender = as.factor(gender),
                                      outcome = as.factor(outcome),
                                      sod = as.factor(sod),
                                      pep = as.factor(pep),
                                      recpanc = as.factor(recpanc) ,
                                      psphinc = as.factor(psphinc),
                                      precut = as.factor(precut),
                                      difcan = as.factor(difcan), 
                                      pneudil = as.factor(pneudil),
                                      amp = as.factor(amp),
                                      paninj = as.factor(paninj),
                                      acinar = as.factor(acinar),
                                      brush = as.factor(brush),
                                      asa81 = as.factor(asa81),
                                      asa325 = as.factor(asa325),
                                      asa = as.factor(asa),
                                      prophystent = as.factor(prophystent), 
                                      therastent = as.factor(therastent),  
                                      pdstent = as.factor(pdstent),       
                                      sodsom = as.factor(sodsom),
                                      bsphinc = as.factor(bsphinc),
                                      bstent = as.factor(bstent), 
                                      chole = as.factor(chole),
                                      pbmal = as.factor(pbmal),
                                      train = as.factor(train), 
                                      status = as.factor(status),
                                      type = as.factor(type),
                                      rx = as.factor(rx),
                                      bleed = as.factor(bleed))
summary (recoded_pancreatitis)
sd(recoded_pancreatitis$age)

# Model 1: with all chosen variables

model_rms_p1 <- lrm(data = pancreatitis, outcome ~ rx + age + acinar + amp +
                      pep + train+ gender + difcan + recpanc + sod+ therastent,
                    x = TRUE, y = TRUE)

model_rms_p1

# Model 2 recpanc = excluded

model_rms_p2 <- lrm(data = pancreatitis, outcome ~ rx + age + acinar + amp +
                      pep + train+ gender + difcan + sod+ therastent,
                    x = TRUE, y = TRUE)

model_rms_p2

# Model 3 gender = excluded
model_rms_p3 <- lrm(data = pancreatitis, outcome ~ rx + age + acinar + amp +
                      pep + train+ difcan + sod+ therastent,
                    x = TRUE, y = TRUE)

model_rms_p3

# Model 4 therastent = excluded

model_rms_p4 <- lrm(data = pancreatitis, outcome ~ rx + age + acinar + amp +
                      pep + train+ difcan + sod,
                    x = TRUE, y = TRUE)

model_rms_p4

# Final model, gender = excluded

model_rms_pf <- lrm(data = pancreatitis, outcome ~ rx + age + acinar + amp +
                      pep + train+ difcan + sod,
                    x = TRUE, y = TRUE)
                  
model_rms_pf


## ROC curve
library(pROC) # library for ROC curve
p <- predict(model_rms_pf, type = "fitted") # prediction factor

ROC <- roc(pancreatitis$outcome, p, ci = TRUE)
ROC # for AUC value
plot(ROC)

# Validation with bootstrapping = 200
validation_rms <- validate(model_rms_pf, method= "boot", B=200)
validation_rms

plot(validation_rms, B=200)

library(rms)

# Calibration of the model
cal <- calibrate(model_rms_pf, B = 200)
plot(cal)



### using restricted cubic spline for age (a continuous variable) ####

library(plyr)
library(dplyr)
library(ggplot2)
library(janitor)
library(rms)
library(rmda)
library(arsenal)
library(pROC)

# changing outcome to integer to get an average later
class(pancreatitis$outcome) = "integer"
class(pancreatitis$age) = "integer"

# creating age x outcome frequency table
table.age.pancreatitis <- tabyl(pancreatitis, age, outcome, digits = 0)
table.age.pancreatitis

#
# adding columns for total and incidence rate
df.total <- age.pancreatitis.df$`0`+age.pancreatitis.df$`1`
df.percentage <- age.pancreatitis.df$`1` / df.total
age.pancreatitis.df$total <- df.total
age.pancreatitis.df$incidence <- df.percentage

# plotting incidence x age, with n for frequency  transforming that table into a dataframe
age.pancreatitis.df <- as.data.frame(table.age.pancreatitis)
View(age.pancreatitis.df)
of age
age.outcome.plot <- ggplot(age.pancreatitis.df, 
                           aes(x = age, 
                               y = incidence,
                               size = total)) + geom_count()
age.outcome.plot # we see a nonlinear relationship with several 'knots'

# let's try three rcs models for age, with knots = 3, 4, 5

kx <- 5 ## easier to calculate, fill knots here (min. 3, max 16)

model.kx <- lrm(data = pancreatitis, outcome~rcs(age,kx) + rx + acinar + amp +
                  pep + train+ difcan + sod,
                x = TRUE, y = TRUE)
model.kx

# ROC curves and AUC values for each model

p.kx <- predict(model.kx, type = "fitted")

ROC.kx <- roc(pancreatitis$outcome, p.kx, ci = TRUE)

ROC.kx 
## k=3 -> AUC = 0.7301 95% CI: 0.6944-0.7658 , equal to non-rcs model
## k=4 -> AUC = 0.732595% CI: 0.6969-0.7681
## k=5 -> 0.7331 95% CI: 0.6969-0.7693 use this. 

# AUC keeps getting better with more knots, but!
# literature/'rule of thumb' is to use max 5 knots, depending on sample size
# large sample sizes should use 5; more knots can lead to overfitting
# k=10 AUC 0.7467
# k=16 AUC 0.7526 (highest possible knot value)

# altering the final model, adding 5 knots for age


model_rms.rcs_pf <- lrm(data = pancreatitis, outcome ~ rx + rcs(age,5) +  amp +
                      pep + train+ acinar + difcan  + sod,
                    x = TRUE, y = TRUE) 
model_rms.rcs_pf
                                
## ROC curve
library(pROC) 
px <- predict(model_rms.rcs_pf, type = "fitted")

ROCx <- roc(pancreatitis$outcome, px, ci = TRUE)
ROCx
plot(ROCx)

# Validation with bootstrapping = 200

validation_rms.rcs <- validate(model_rms.rcs_pf, method= "boot", B=200)
validation_rms.rcs
0.5 * (validation_rms.rcs[1, ] + 1)


plot(validation_rms.rcs, B=200)

library(rms)

# Calibration of the model
calx <- calibrate(model_rms.rcs_pf, B = 200)
plot(calx)

# Frequency table divided by pep and nopep
## Split data based on pep/nopep
pancreatitis_nopep <- pancreatitis[pancreatitis$pep == "0_no", ] # Selecting the no pep individuals
pancreatitis_pep <- pancreatitis[pancreatitis$pep == "1_yes", ] # Selecting the pep individuals

## Adding means percentages to the variables 
Age_nopep <- round(mean(pancreatitis_nopep$age), 2)
Age_pep <- round(mean(pancreatitis_pep$age), 2)
gender_pep <- round(sum(pancreatitis_pep$gender == "1_female")/length(pancreatitis_pep$gender) *100 ,2)
gender_nopep <- round(sum(pancreatitis_nopep$gender == "1_female")/length(pancreatitis_nopep$gender) *100 ,2)
rx_pep <- round(sum(pancreatitis_pep$rx == "1_indomethacin")/length(pancreatitis_pep$rx) *100 ,2)
rx_nopep <- round(sum(pancreatitis_nopep$rx == "1_indomethacin")/length(pancreatitis_nopep$rx) *100 ,2)
amp_pep <- round(sum(pancreatitis_pep$amp == "1_yes")/length(pancreatitis_pep$amp) *100 ,2)
amp_nopep <- round(sum(pancreatitis_nopep$amp == "1_yes")/length(pancreatitis_nopep$amp) *100 ,2)
train_pep <- round(sum(pancreatitis_pep$train == "1_yes")/length(pancreatitis_pep$train) *100 ,2)
train_nopep <- round(sum(pancreatitis_nopep$train == "1_yes")/length(pancreatitis_nopep$train) *100 ,2)
acinar_pep <- round(sum(pancreatitis_pep$acinar == "1_yes")/length(pancreatitis_pep$acinar) *100 ,2)
acinar_nopep <- round(sum(pancreatitis_nopep$acinar == "1_yes")/length(pancreatitis_nopep$acinar) *100 ,2)
difcan_pep <- round(sum(pancreatitis_pep$difcan == "1_yes")/length(pancreatitis_pep$difcan) *100 ,2)
difcan_nopep <- round(sum(pancreatitis_nopep$difcan == "1_yes")/length(pancreatitis_nopep$difcan) *100 ,2)
sod_pep <- round(sum(pancreatitis_pep$sod == "1_yes")/length(pancreatitis_pep$sod) *100 ,2)
sod_nopep <- round(sum(pancreatitis_nopep$sod == "1_yes")/length(pancreatitis_nopep$sod) *100 ,2)


# Creates table template and defining names
table = matrix(NA, nrow = 8, ncol = 2)
colnames(table) = c("Pep (mean/%)", "noPep (mean/%)")
rownames(table) = c("age", "female", "Indomethacin", "amp", "train", "acinar", "difcan", "sod")

# Assign the variables to the table
table[1,1] = Age_pep
table[1,2] = Age_nopep
table[2,1] = gender_pep
table[2,2] = gender_nopep
table[3,1] = rx_pep
table[3,2] = rx_nopep
table[4,1] = amp_pep
table[4,2] = amp_nopep
table[5,1] = train_pep
table[5,2] = train_nopep
table[6,1] = acinar_pep
table[6,2] = acinar_nopep
table[7,1] = difcan_pep
table[7,2] = difcan_nopep
table[8,1] = sod_pep
table[8,2] = sod_nopep

table

library(knitr) # to make the table more aesthetic and add a caption
kable(table, "pipe", caption = "Table 1: Characteristics of the population") 

install.packages ("sjlabelled")
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(model_rms.rcs_pf)

tab_model(model_rms.rcs_pf, transform = NULL, auto.label = FALSE)

