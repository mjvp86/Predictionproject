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

t3 <- tableby( ~ ., data = pct)
summary(t3)
summary(t3, text = TRUE, title = "Descriptive Statistics on Pancreatitis")

t4 <- tableby( outcome ~ ., data = pct) ## data per outcome
summary(t4)
summary(t4, text = TRUE, title = "Descriptive Statistics on Pancreatitis")