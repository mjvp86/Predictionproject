#importing the data set
dat<- read.csv("pancreatitis.csv")

## Install several packages
source("Functions.R")
list.of.packages <- c("haven", "dplyr", "reshape","reshape2", "magrittr", "httr","kableExtra", "ggplot2", "GGally", "epiDisplay") 
installRequiredPackages(list.of.packages)

## Reproducible way to import dataset 
pathToData <- '.' #this means "the same folder as where my markdown script is"
pancreatitis <- read.csv( file.path(pathToData,'pancreatitis.csv') )
summary(pancreatitis)

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
                                                                         '1' = '0')
                                  )
                                                                   