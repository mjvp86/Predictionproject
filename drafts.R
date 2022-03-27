## recoding by JC Nava

pathToData <- '.' #this means "the same folder as where my markdown script is"
pancreatitis.data <- read.csv(file.path(pathToData,'pancreatitis.csv'))

pct <- pancreatitis.data #I like shorter names, easier to work with, but we can change this later
View(pct)               

pct[pct=="0_no"] <- 0
pct[pct=="1_yes"] <- 1
pct$gender[pct$gender=="1_female"] <- 1
pct$gender[pct$gender=="2_male"] <- 2
pct$status[pct$status=="0_inpatient"] <- 0
pct$status[pct$status=="1_outpatient"] <- 1
pct$type[pct$type=="0_no SOD"] <- 0
pct$type[pct$type=="1_type 1"] <- 1
pct$type[pct$type=="2_type 2"] <- 2
pct$type[pct$type=="3_type 3"] <- 3
pct$rx[pct$rx=="0_placebo"] <- 0
pct$rx[pct$rx=="1_indomethacin"] <- 1
pct$site[pct$site=="1_UM"] <- 1 
pct$site[pct$site=="2_IU"] <- 2
pct$site[pct$site=="3_UK"] <- 3
pct$site[pct$site=="4_Case"] <- 4

View(pct)

pancreatitis.recoded <- pct

## some descriptives

library(arsenal) 
require(knitr)
require(survival)

t1 <- tableby( ~ ., data = pct)
summary(t1)
summary(t1, text = TRUE, title = "Descriptive Statistics on Pancreatitis")

t2 <- tableby( outcome ~ ., data = pct) ## data per outcome
summary(t2)
summary(t2, text = TRUE, title = "Descriptive Statistics on Pancreatitis")

## data 'decoded' for nicer descriptives

pan <- pancreatitis.data

pan[pan=="0_no"] <- "No"
pan[pan=="1_yes"] <- "Yes"
pan$gender[pan$gender=="1_female"] <- "Female"
pan$gender[pan$gender=="2_male"] <- "Male"
pan$status[pan$status=="0_inpatient"] <- "Inpatient"
pan$status[pan$status=="1_outpatient"] <- "Outpatient"
pan$type[pan$type=="0_no SOD"] <- "No SOD"
pan$type[pan$type=="1_type 1"] <- "Type 1"
pan$type[pan$type=="2_type 2"] <- "Type 2"
pan$type[pan$type=="3_type 3"] <- "Type 3" 
pan$rx[pan$rx=="0_placebo"] <- "Placebo"
pan$rx[pan$rx=="1_indomethacin"] <- "Indomethacin"
pan$site[pan$site=="1_UM"] <- "UM"
pan$site[pan$site=="2_IU"] <- "IU"
pan$site[pan$site=="3_UK"] <- "UK"
pan$site[pan$site=="4_Case"] <- "Case"
pan$bleed[pan$bleed==1] <- "No"
pan$bleed[pan$bleed==2] <- "Yes"
pan$outcome[pan$outcome==0] <- "No"
pan$outcome[pan$outcome==1] <- "Yes"

View(pan)

pancreatitis.decoded <- pan

t3 <- tableby( ~ ., data = pan)
summary(t3, text = TRUE, title = "Descriptive Statistics on Pancreatitis")

t4 <- tableby( outcome ~ ., data = pan) ## data per outcome
summary(t4, text = TRUE, title = "Descriptive Statistics on Pancreatitis")

labels(t4) <- c(site = "Study Site", risk = "Risk Score", sod = "Sphincter of Oddi Dysfunction", pep = "Previous post-ERCP Pancreatitis", recpanc = "Recurring Pancreatitis", psphinc = "Pancreatic Sphincterotomy", precut = "Sphincter pre-cut to enter Papilla", difcan = "Cannulation of Papilla", pneudil = "Pneumatic Dilation of Papilla", amp = "Ampullectomy for Dysplasia or Cancer", paninj = "Contrast Injected into Pancreas during Procedure", acinar = "Acinarisation of Pancreas on Imaging", brush = "Brushing taken from Pancreatic Duct", asa81 = "Aspirin dose 81mg/d", asa325 = "Aspirin dose 325mg/d", asa = "Asparin at any dose", prophystent = "Pancreatic duct stent placed preventatively", therastent = "Pancreatic duct stent therapeutically", pdstent = "Pancreatid duct stent placed for any reason", sodsom = "Sphincter of Oddi Manometry", bsphinc = "Biliary Sphincterotomy", bstent = "Biliary stent", chole = "Choledocholithiasis (gallstones)", pbmal = "Malignancy of biliary duct or pancreas", train = "Participation of Trainee in ERCP", outcome = "Post-ERCP Pancreatitis", status = "Patient Status", type = "Sphincter of Oddi dysfunction type", rx = "Treatment", bleed = "Gastrointestinal bleeding")

## recoding by Tommy

## Renaming of data 
recoded_pancreatitis <- dplyr::rename(pancreatitis,
                                      fac_study_site = site,
                                      num_age_years = age,
                                      num_risk = risk,
                                      bin_gender_is_female = gender,
                                      bin_outcome = outcome,
                                      bin_sod_dysfunction = sod,
                                      bin_pep_previous = pep,
                                      bin_rec_panc = recpanc,
                                      bin_panc_sphinc = psphinc,
                                      bin_pre_cut = precut,
                                      bin_dif_can = difcan, 
                                      bin_pneu_dil = pneudil,
                                      bin_amp = amp,
                                      bin_pan_inj = paninj,
                                      bin_acinar = acinar,
                                      bin_brush = brush,
                                      bin_aspirin_81 = asa81,
                                      bin_aspirin_325 = asa325,
                                      bin_aspirin_any = asa,
                                      bin_prophystent = prophystent, ## protective factor, maybe recode so that "1" is higher risk
                                      bin_therastent = therastent,  ## same as previous
                                      bin_pdstent = pdstent,       ## also protective
                                      bin_sodsom = sodsom,
                                      bin_bsphinc = bsphinc,
                                      bin_bstent = bstent,        ## not sure about this one
                                      bin_chole = chole,
                                      bin_pbmal = pbmal,
                                      bin_train = train, 
                                      bin_status_outpatient = status,
                                      fac_type = type,
                                      bin_rx_indometh = rx,
                                      bin_bleed = bleed          ## only one coded with 1 and 2
)






recoded_pancreatitis <- dplyr::mutate(recoded_pancreatitis,
                                      fac_study_site = as.factor(fac_study_site),
                                      #           
                                      fac_study_site=dplyr::recode_factor(fac_study_site, ## Not sure if this is necessary
                                                                          '1_UM' = '1',
                                                                          '2_IU' = '2',
                                                                          '3_UK' = '3',
                                                                          '4_Case' = '4'),
                                      #
                                      num_age_years = as.numeric(num_age_years),
                                      # 
                                      num_risk = as.numeric(num_risk),
                                      #
                                      bin_gender_is_female = as.factor(bin_gender_is_female),
                                      #
                                      bin_gender_is_female=dplyr::recode_factor(bin_gender_is_female, ## Not sure if this is necessary
                                                                                '1_female' = '1',
                                                                                '2_male' = '0'),
                                      bin_outcome = as.factor(bin_outcome),
                                      #
                                      bin_sod_dysfunction = as.factor(bin_sod_dysfunction),
                                      #
                                      bin_pep_previous = as.factor(bin_pep_previous),
                                      #
                                      bin_rec_panc = as.factor(bin_rec_panc),
                                      #
                                      bin_panc_sphinc = as.factor(bin_panc_sphinc),
                                      #
                                      bin_pre_cut = as.factor(bin_pre_cut),
                                      #
                                      bin_dif_can = as.factor(bin_dif_can), 
                                      #
                                      bin_pneu_dil = as.factor(bin_pneu_dil),
                                      #
                                      bin_amp = as.factor(bin_amp),
                                      #
                                      bin_pan_inj = as.factor(bin_pan_inj),
                                      #
                                      bin_acinar = as.factor(bin_acinar),
                                      #
                                      bin_brush = as.factor(bin_brush),
                                      #
                                      bin_aspirin_81 = as.factor(bin_aspirin_81),
                                      #
                                      bin_aspirin_325 = as.factor(bin_aspirin_325),
                                      #
                                      bin_aspirin_any = as.factor(bin_aspirin_any),
                                      #
                                      bin_prophystent = as.factor(bin_prophystent), ## protective factor, maybe recode so that "1" is higher risk
                                      #
                                      bin_therastent = as.factor(bin_therastent),  ## same as previous
                                      #
                                      bin_pdstent = as.factor(bin_pdstent),       ## also protective
                                      #
                                      bin_sodsom = as.factor(bin_sodsom),
                                      #
                                      bin_bsphinc = as.factor(bin_bsphinc),
                                      #
                                      bin_bstent = as.factor(bin_bstent),        ## not sure about this one
                                      #
                                      bin_chole = as.factor(bin_chole),
                                      #
                                      bin_pbmal = as.factor(bin_pbmal),
                                      #
                                      bin_train = as.factor(bin_train),
                                      #
                                      bin_status_outpatient = as.factor(bin_status_outpatient),
                                      #
                                      fac_type = as.factor(fac_type),
                                      #
                                      bin_rx_indometh = as.factor(bin_rx_indometh),
                                      #
                                      bin_bleed = as.factor(bin_bleed), ##coded with 1 and 2
                                      #
                                      bin_bleed=dplyr::recode_factor(bin_bleed, ## Not sure if this is necessary
                                                                     '2' = '1',
                                                                     '1' = '0'))
                                      
#### plotting age with outcome 
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

# transforming that table into a dataframe
age.pancreatitis.df <- as.data.frame(table.age.pancreatitis)
View(age.pancreatitis.df)

# adding columns for total and incidence rate
df.total <- age.pancreatitis.df$`0`+age.pancreatitis.df$`1`
df.percentage <- age.pancreatitis.df$`1` / df.total
age.pancreatitis.df$total <- df.total
age.pancreatitis.df$incidence <- df.percentage

# plotting incidence x age, with n for frequency of age
age.outcome.plot <- ggplot(age.pancreatitis.df, 
       aes(x = age, 
           y = incidence,
           size = total)) + geom_count()
age.outcome.plot # we see a nonlinear relationship with several 'knots'

# let's try three rcs models for age, with knots = 3, 4, 5

kx <- 5 ## easier to calculate, fill knots here (min. 3, max 16)

model.kx <- lrm(data = pancreatitis, outcome~rcs(age,kx) + rx + amp +
                pep + train + chole + difcan + sod,
              x = TRUE, y = TRUE)
model.kx

# ROC curves and AUC values for each model

p.kx <- predict(model.kx, type = "fitted")

ROC.kx <- roc(pancreatitis$outcome, p.kx, ci = TRUE)

ROC.kx 
## k=3 -> AUC = 0.7278 CI 0.6925 - 0.7631
## k=4 -> AUC = 0.7307 CI 0.6953 - 0.7661
## k=5 -> AUC = 0.7321 CI 0.6962 - 0.7680

# AUC keeps getting better with more knots, but!
# literature/'rule of thumb' is to use max 5 knots, depending on sample size
# large sample sizes should use 5; more knots can lead to overfitting, eg k=10
  # k=10 AUC 0.7467
  # k=12 AUC 0.7500
  # k=16 AUC 0.7526 (highest possible knot value)