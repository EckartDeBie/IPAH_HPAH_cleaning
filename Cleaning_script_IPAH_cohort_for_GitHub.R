#NOT HARD CODED CLEANING SCRIPTS EdB
#EdB started here on 05-03-2024

#1) set WD and set data directory
data_dir="~/myfilepath/data_dumps/2024-02-10/cleaned" 
#NOTE: change the folder (date) to the last dump to apply the cleaning
getwd()
setwd("~/myfilepath/2024-02-10/cleaned")
getwd()


#load relevant packages
library(rlang)
library(tidyverse)
library(readxl)
library(ggplot2)
library(naniar)
library(stringr)
library(Hmisc)

#import the data
#a)import the clinical data
load("data_checked.RData")
load("data_clean.RData")

#=============================================================================================
#now cleaning can be applied
#=============================================================================================
#Clean the data of the relatives and controls
relatives <- data.clean.relative
ctrl <- data.clean.unrelctrls

#make subsets of the data to investigate
num_df <- relatives %>% select(where(is.numeric))
f_df <- relatives %>% select(where(is.character))
f_df <- as.data.frame(lapply(f_df, as.factor))
f_df2 <- relatives %>% select(where(is.factor))

lapply(f_df, summary)
#note: all patients are UK patients --> helpful with conversions! 
lapply(f_df2, summary)

#change UNK etc. into NAs
relatives <- relatives %>% replace_with_na_at(.vars = c('lf_sleep_study', 'lf_va_done', 'lf_kco_done', 'lf_done', 'hv_study', 'hb_study', 'ep_2_done', 'ep_1_done', 'el_done', 'ec_done', 'cfe_hr_done', 'cfe_bp_done', 'cfe_rest_spo2_done', 'cbt_thyr_sample', 'cbt_sero_hep_b_sero', 'cbt_sero_sample', 'cbt_renal_sample', 'cbt_liver_sample', 'cbt_iron_transferrin_unit', 'cbt_iron_ferritin_unit', 'cbt_card_trop_unit', 'cbt_haem_haematocrit_unit', 'cbt_lipids_sample', 'cbt_iron_sample', 'cbt_card_ntprobnp_unit', 'cbt_card_bnp_unit', 'cbt_inflammation_sample', 'cbt_haem_serum_protein', 'cbt_haem_sample', 'cbt_card_sample', 'cbt_aab_anca', 'cbt_aab_antiena_ab', 'cbt_aab_antirho_ab', 'cbt_aab_anticentromere_ab', 'cbt_aab_antiscl70_ab', 'cbt_aab_antidsdna_ab', 'cbt_aab_anticardiolipin_ab', 'cbt_aab_ana', 'cbt_aab_sample'), condition = ~.x == 'not done')
relatives <- relatives %>% replace_with_na_at(.vars = c('el_dominant_r_wave', 'el_rbbb', 'cfe_hf_ascites', 'cfe_hf_ankle_swelling', 'cfe_spider_naevi', 'cfe_digital_clubbing', 'ep_1_supplemental_oxygen', 'cfe_supplemental_oxygen', '$ep_2_supplemental_oxygen', ''), condition = ~.x == 'UNK')
relatives <- relatives %>% replace_with_na_at(.vars = c('cfe_jugular_venous_pressure_select'), condition = ~.x == 'not-recorded')
relatives <- relatives %>% replace_with_na_at(.vars = c('cfe_jugular_venous_pressure_select'), condition = ~.x == 'not-seen')

#now investigate the numeric data
num_df <- num_df[,colSums(is.na(num_df))<nrow(num_df)]

num_df %>% gather() %>% head()
ggplot(gather(num_df[,c(91:116)]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
#distributions make sense!

#check extra
lapply(num_df, summary)

#probably left atrial size has errors in it again! 
#QRS cannot be <0!
relatives <- relatives %>% replace_with_na_at(.vars = c('ep_2_peak_spo2', 'el_qrs', 'ep_2_anaer_vo2_at_lmin'), condition = ~.x <= 0)

#one unit conversion issue (change mmHg to KpA)
relatives$ep_2_peak_petco2_kpa <- ifelse(relatives$ep_2_peak_petco2_kpa > 20, relatives$ep_2_peak_petco2_kpa/7.50061683, relatives$ep_2_peak_petco2_kpa)
#one value seems impossible
relatives <- relatives %>% replace_with_na_at(.vars = c('ep_2_anaer_eqco2_at'), condition = ~.x > 900)

#all the other data makes sense
#take the variables where there's at least 1 recorded in each col
relatives <- relatives[,colSums(is.na(relatives))<nrow(relatives)]

write_rds(relatives, file='relatives_data_cleaned_vx_date_y.rds')

#continue with the HVs
#there is no clinical data recorded
write_rds(ctrl, file='controls_data_cleaned_vx_date_y.rds')


#=============================================================
#clean pregnancy and other risk factor data
preg <- pregnancy
#in this dataset, having UNK makes sense --> keep it like it is.

#merge the risk factor data
names(rf)[[11]] <- 'drugname'
rf <- rbind(rf, rf.other)
rf <- rf[,-c(5,6)]
summary(as.factor(rf$drugname))

#remove capital letters and double spaces first
library(stringr)
rf$drugname <- tolower(rf$drugname)
rf$drugname <- str_squish(rf$drugname)
#also remove full stops
rf$drugname <- gsub('[.]','',rf$drugname)

a <- as.data.frame(unique(rf$drugname))

#rename to combine
rf$drugname <- ifelse(grepl('^adalat',rf$drugname), 'Ca_blocker', rf$drugname)
rf$drugname <- ifelse(grepl('^adios',rf$drugname), 'herbal_slimming_tablet', rf$drugname)
rf$drugname <- ifelse(grepl('^alcohol',rf$drugname), 'alcohol_abuse', rf$drugname)
rf$drugname <- ifelse(grepl('^amitriptyline',rf$drugname), 'TCA', rf$drugname)
rf$drugname <- ifelse(grepl('^asbestolux',rf$drugname), 'asbestos', rf$drugname)
rf$drugname <- ifelse(grepl('^anti tnf therapy',rf$drugname), 'TNF_biologic', rf$drugname)
rf$drugname <- ifelse(grepl('^bleaches and cleaning fluids',rf$drugname), 'bleach_cleaning_fluid', rf$drugname)
rf$drugname <- ifelse(grepl('^capiri',rf$drugname), 'Irinotecan and capecitabine', rf$drugname)
rf$drugname <- ifelse(grepl('^champix',rf$drugname), 'varenicline', rf$drugname)
rf$drugname <- ifelse(grepl('^cimetidine',rf$drugname), 'h2_blocker', rf$drugname)
rf$drugname <- ifelse(grepl('^folfiri',rf$drugname), ' folinic acid, fluorouracil and irinotecan', rf$drugname)
rf$drugname <- ifelse(grepl('^herbal slimming tablets',rf$drugname), 'herbal_slimming_tablet', rf$drugname)
rf$drugname <- ifelse(grepl('^industrial metal lodged in right eye',rf$drugname), 'metal_in_eye', rf$drugname)
rf$drugname <- ifelse(grepl('^losartan',rf$drugname), 'ACEi_ARBs', rf$drugname)
rf$drugname <- ifelse(grepl('^mdf and other environmental/industrial materials in the joinery',rf$drugname), 'mdf_industrial_metals', rf$drugname)
rf$drugname <- ifelse(grepl('^mirtazipine',rf$drugname), 'TCA', rf$drugname)
rf$drugname <- ifelse(grepl('^morphine sulphate',rf$drugname), 'opioid', rf$drugname)
rf$drugname <- ifelse(grepl('^nitrox',rf$drugname), 'laugh_gas', rf$drugname)
rf$drugname <- ifelse(grepl('^oestrogen therapy',rf$drugname), 'hrt', rf$drugname)
rf$drugname <- ifelse(grepl('^opiates',rf$drugname), 'opioid', rf$drugname)
rf$drugname <- ifelse(grepl('^oromorph',rf$drugname), 'opioid', rf$drugname)
rf$drugname <- ifelse(grepl('^oral contraceptive pill',rf$drugname), 'contraceptive', rf$drugname)
rf$drugname <- ifelse(grepl('^radical radiotherapy',rf$drugname), 'radiotherapy', rf$drugname)
rf$drugname <- ifelse(grepl('^refib',rf$drugname), 'interferon_therapy', rf$drugname)
rf$drugname <- ifelse(grepl('^interferon',rf$drugname), 'interferon_therapy', rf$drugname)
rf$drugname <- ifelse(grepl('^ritalin',rf$drugname), 'ritalin', rf$drugname)
rf$drugname <- ifelse(grepl('^serotonin_specific_reuptake_inhibitors',rf$drugname), 'SSRI', rf$drugname)
rf$drugname <- ifelse(grepl('^sibutramine',rf$drugname), 'herbal_slimming_tablet', rf$drugname)
rf$drugname <- ifelse(grepl('^temazepam',rf$drugname), 'benzo', rf$drugname)
rf$drugname <- ifelse(grepl('^temoxifen',rf$drugname), 'tamoxifen', rf$drugname)
rf$drugname <- ifelse(grepl('^tenuate',rf$drugname), 'amfepramone', rf$drugname)
rf$drugname <- ifelse(grepl('^teronac',rf$drugname), 'mazindol', rf$drugname)
rf$drugname <- ifelse(grepl('^xenical',rf$drugname), 'orlistat', rf$drugname)
rf$drugname <- ifelse(grepl('^zyban',rf$drugname), 'bupropion', rf$drugname)
rf$drugname <- ifelse(grepl('^warfarin',rf$drugname), 'anticoagulant', rf$drugname)
rf$drugname <- ifelse(grepl('^vicarious exposure to nuclear testing',rf$drugname), 'exposed_to_nuclear_tests', rf$drugname)

a <- as.data.frame(unique(rf$drugname))
#from 67 obs to 52 obs

summary(as.factor(rf$drugname))

write_rds(rf, file = 'risk_factors_cleaned_vx_datey.rds')

#high altitude df is fairly clean
summary(as.factor(hight_altitude$cfh_high_altitude))

#also look into FHx
fhx <- family_history
a <- unique(fhx$id)
summary(as.factor(fhx$family_pah))
summary(as.factor(fhx$family_possible_pah))
#FHx data already is clean!

#==========================================================================================
#clean the medication data
lapply(drug_history_other, class)

#format the data so that cleaning is easier
drug_history_other[,c(1:6, 10:14)] <- lapply(drug_history_other[,c(1:6, 10:14)], as.factor)
drug_history_other[,c(7,8)] <- lapply(drug_history_other[,c(7,8)], as.Date)
abc <- as.data.frame(drug_history_other$dt_cur_name)
abc <- unique(abc)
abc <- na.omit(abc)
#the data isn't formatted properly! 
#double names present (incl typos)
#no separate col for route of administration of drugs

#try to format it normally
dh_o <- drug_history_other
#remove capital letters and double spaces first
dh_o$dt_cur_name <- tolower(dh_o$dt_cur_name)
dh_o$dt_cur_name <- str_squish(dh_o$dt_cur_name)
#also remove full stops
dh_o$dt_cur_name <- gsub('[.]','',dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lorat',dh_o$dt_cur_name), 'loratidine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ferrous fum',dh_o$dt_cur_name), 'ferrous fumerate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mirena',dh_o$dt_cur_name), 'mirena coil', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^co-dy',dh_o$dt_cur_name), 'co_dydramol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lorat',dh_o$dt_cur_name), 'loratidine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^quinine sulfate',dh_o$dt_cur_name), 'quinine sulphate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fexofenedine',dh_o$dt_cur_name), 'fexofenadine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^bisop',dh_o$dt_cur_name), 'bisoprolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^gtn subl',dh_o$dt_cur_name), 'gtn spray', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glyceryl tri',dh_o$dt_cur_name), 'gtn spray', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^clopi',dh_o$dt_cur_name), 'clopidrogel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^colch',dh_o$dt_cur_name), 'colchicine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^quinine sulfate',dh_o$dt_cur_name), 'quinine sulphate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^isosorbide mononitrate modified release',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^isosorbide mononitrate mr',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^isosorbide mono',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^isosorbid mono',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mebever',dh_o$dt_cur_name), 'mebeverine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^codeine phosp',dh_o$dt_cur_name), 'codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^paracetemol',dh_o$dt_cur_name), 'paracetamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^paracetomol',dh_o$dt_cur_name), 'paracetamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^omepr',dh_o$dt_cur_name), 'omeprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^simvast',dh_o$dt_cur_name), 'simvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lans',dh_o$dt_cur_name), 'lansoprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^levo',dh_o$dt_cur_name), 'levothyroxine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seretide 125',dh_o$dt_cur_name), 'seretide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seretide 250',dh_o$dt_cur_name), 'seretide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seretide 500',dh_o$dt_cur_name), 'seretide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seretide evohalere',dh_o$dt_cur_name), 'seretide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seretide inhaler 125',dh_o$dt_cur_name), 'seretide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seretide inhaler',dh_o$dt_cur_name), 'seretide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^asp',dh_o$dt_cur_name), 'aspirin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ramip',dh_o$dt_cur_name), 'ramipril', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lerca',dh_o$dt_cur_name), 'lercanidipine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dulox',dh_o$dt_cur_name), 'duloxetine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^bumet',dh_o$dt_cur_name), 'bumetanide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cholec',dh_o$dt_cur_name), 'colecalciferol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^citalo',dh_o$dt_cur_name), 'citalopram', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^novomi',dh_o$dt_cur_name), 'novomix', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('insulin (novomix)',dh_o$dt_cur_name), 'novomix', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^pregab',dh_o$dt_cur_name), 'pregabalin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^salbutamol nebuliser',dh_o$dt_cur_name), 'nebuliser_salbutamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^salbut',dh_o$dt_cur_name), 'salbutamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^easyhaler salbutamol',dh_o$dt_cur_name), 'salbutamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^allo',dh_o$dt_cur_name), 'allopurinol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('latan',dh_o$dt_cur_name), 'latanoprolt', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fluticasone/salmeterol 125/25',dh_o$dt_cur_name), 'fluticasone/salmeterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fluticasone inhaler',dh_o$dt_cur_name), 'fluticasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^oxycodone',dh_o$dt_cur_name), 'oxycodone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^doxa',dh_o$dt_cur_name), 'doxazosin', dh_o$dt_cur_name)
#making the assumption thyroxine = L-thyrox
dh_o$dt_cur_name <- ifelse(grepl('^thyrox',dh_o$dt_cur_name), 'levothyroxine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amiod',dh_o$dt_cur_name), 'amiodarone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sertra',dh_o$dt_cur_name), 'sertraline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ondan',dh_o$dt_cur_name), 'ondansetron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glicl',dh_o$dt_cur_name), 'gliclazide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^spiron',dh_o$dt_cur_name), 'spironolactone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^metformin mr',dh_o$dt_cur_name), 'metformin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^metformin sr',dh_o$dt_cur_name), 'metformin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^metfor',dh_o$dt_cur_name), 'metformin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glicaxide',dh_o$dt_cur_name), 'gliclazide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('tramdol',dh_o$dt_cur_name), 'tramadol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('spirolactone',dh_o$dt_cur_name), 'spironolactone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^atorvas',dh_o$dt_cur_name), 'atorvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^spiriv',dh_o$dt_cur_name), 'spiriva', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('tiotropium (spiriva)',dh_o$dt_cur_name), 'spiriva', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^spiriva',dh_o$dt_cur_name), 'tiotropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tiotropium bromide 18microgram inhalation powder capsul',dh_o$dt_cur_name), 'tiotropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tiotropium 18 micrograms',dh_o$dt_cur_name), 'tiotropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tiotr',dh_o$dt_cur_name), 'tiotropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^novorap',dh_o$dt_cur_name), 'novorapid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fostair 100/6',dh_o$dt_cur_name), 'fostair', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fostair 100mcg + 6 mcg',dh_o$dt_cur_name), 'fostair', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fostair inhaler 200/6',dh_o$dt_cur_name), 'fostair', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fostair 200/6',dh_o$dt_cur_name), 'fostair', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('formoterol (fostair)',dh_o$dt_cur_name), 'fostair', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('beclomethasone (fostair)',dh_o$dt_cur_name), 'fostair', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^beclometasone dipropionate / formoterol',dh_o$dt_cur_name), 'fostair', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^beclomethasone 100ug',dh_o$dt_cur_name), 'beclomethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^beclomethasone 200 mdi',dh_o$dt_cur_name), 'beclomethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^beclo',dh_o$dt_cur_name), 'beclomethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^frusdemide',dh_o$dt_cur_name), 'furosomide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glargine',dh_o$dt_cur_name), 'glargine_insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glargine insulin',dh_o$dt_cur_name), 'glargine_insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^insulin glargine',dh_o$dt_cur_name), 'glargine_insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^zopicline',dh_o$dt_cur_name), 'zopiclone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^zoplicone',dh_o$dt_cur_name), 'zopiclone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amitryptilline',dh_o$dt_cur_name), 'amitryptyline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amitryptiline',dh_o$dt_cur_name), 'amitryptyline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amitryptaline',dh_o$dt_cur_name), 'amitryptyline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^alendronic sodium',dh_o$dt_cur_name), 'alendronic_acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sodium alendronate',dh_o$dt_cur_name), 'alendronic_acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^alendronate',dh_o$dt_cur_name), 'alendronic_acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^alendronic acid',dh_o$dt_cur_name), 'alendronic_acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mometasone furcate nasal spray',dh_o$dt_cur_name), 'nasal_spray_mometasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mometasone furoate nasal spray',dh_o$dt_cur_name), 'nasal_spray_mometasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mometasone nasal spray',dh_o$dt_cur_name), 'nasal_spray_mometasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^momet',dh_o$dt_cur_name), 'mometasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^prednisalone',dh_o$dt_cur_name), 'prednisolone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^prednizone',dh_o$dt_cur_name), 'prednisone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^folic',dh_o$dt_cur_name), 'folic acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^morphine sulphate continuous release',dh_o$dt_cur_name), 'morphine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^morphine sulphate mr',dh_o$dt_cur_name), 'morphine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('morphine (pump)',dh_o$dt_cur_name), 'morphine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^morphine sulphate mr',dh_o$dt_cur_name), 'morphine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^morphine sulphate continuous release',dh_o$dt_cur_name), 'morphine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^morphi',dh_o$dt_cur_name), 'morphine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lisinpril',dh_o$dt_cur_name), 'lisinopril', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^atenalol',dh_o$dt_cur_name), 'atenolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^atenelol',dh_o$dt_cur_name), 'atenolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^atenolog',dh_o$dt_cur_name), 'atenolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^accr',dh_o$dt_cur_name), 'accrete_d3', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^atrove',dh_o$dt_cur_name), 'atrovent', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^atrovent',dh_o$dt_cur_name), 'ipratropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ipratr',dh_o$dt_cur_name), 'ipratropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^imatineb',dh_o$dt_cur_name), 'imatinib', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('imatinib (glivanc)',dh_o$dt_cur_name), 'imatinib', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^allipurinol',dh_o$dt_cur_name), 'allopurinol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^allupurinol',dh_o$dt_cur_name), 'allopurinol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^alluprinol',dh_o$dt_cur_name), 'allopurinol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^allpurinol',dh_o$dt_cur_name), 'allopurinol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^carbom',dh_o$dt_cur_name), 'carbomer', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mini pill',dh_o$dt_cur_name), 'micronor', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fol',dh_o$dt_cur_name), 'folate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^iron infusions',dh_o$dt_cur_name), 'iv_iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^iron',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^strong iron formula',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sytrom iron solution',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('iron (sytron)',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^clenil',dh_o$dt_cur_name), 'beclometasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sumariptan',dh_o$dt_cur_name), 'sumatriptan', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^co-amoxiclav',dh_o$dt_cur_name), 'co_amoxiclav', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^coamoxiclav',dh_o$dt_cur_name), 'co_amoxiclav', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^hydroxocobalamin',dh_o$dt_cur_name), 'hydroxycobalamin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fluticasone 250',dh_o$dt_cur_name), 'fluticasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fluticasone 125',dh_o$dt_cur_name), 'fluticasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fluticasone 250/salmeterol 25',dh_o$dt_cur_name), 'fluticasone/salmeterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fluticasone 125 and salmeterol 25',dh_o$dt_cur_name), 'fluticasone/salmeterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^flutiform 125/5',dh_o$dt_cur_name), 'fluticasone/formoterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^flutiform',dh_o$dt_cur_name), 'flucatisone/formoterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^flut',dh_o$dt_cur_name), 'fluticasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cetirazine',dh_o$dt_cur_name), 'cetirizine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^avamy',dh_o$dt_cur_name), 'fluticasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sitaglipin',dh_o$dt_cur_name), 'sitagliptin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^esopmeprazole',dh_o$dt_cur_name), 'esomeprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^esopemprazole',dh_o$dt_cur_name), 'esomeprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^norethasterone',dh_o$dt_cur_name), 'norethisterone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^vento',dh_o$dt_cur_name), 'salbutamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^medroxy',dh_o$dt_cur_name), 'medroxyprogesterone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^paractemol',dh_o$dt_cur_name), 'paracetamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^oxybutin',dh_o$dt_cur_name), 'oxybutinin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tinzaparine',dh_o$dt_cur_name), 'tinzaparin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^propranol',dh_o$dt_cur_name), 'propranolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^propanolol',dh_o$dt_cur_name), 'propranolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^propanalol',dh_o$dt_cur_name), 'propranolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dihydracodeine',dh_o$dt_cur_name), 'codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dihydrocodine',dh_o$dt_cur_name), 'codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dihydrocodeine tartrate',dh_o$dt_cur_name), 'codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^temazepan',dh_o$dt_cur_name), 'temazepam', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^prograf',dh_o$dt_cur_name), 'tracolimus', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^colcichine',dh_o$dt_cur_name), 'colchicine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ferrous',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('pregaday ferrous',dh_o$dt_cur_name), 'iron & folate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fexo',dh_o$dt_cur_name), 'fexofenadine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^isos',dh_o$dt_cur_name), 'isosorbide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^codein',dh_o$dt_cur_name), 'codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^felo',dh_o$dt_cur_name), 'felodipine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seret',dh_o$dt_cur_name), 'seretide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^serevent',dh_o$dt_cur_name), 'salmeterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lithium',dh_o$dt_cur_name), 'lithium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^duloglutide',dh_o$dt_cur_name), 'dulaglutide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nov',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^insulin',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^salbu',dh_o$dt_cur_name), 'salbutamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^clex',dh_o$dt_cur_name), 'LMWH', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sertaline',dh_o$dt_cur_name), 'sertraline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^potas',dh_o$dt_cur_name), 'potassium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^chloride',dh_o$dt_cur_name), 'chloride', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tram',dh_o$dt_cur_name), 'tramadol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glicoside',dh_o$dt_cur_name), 'glycloside', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glic',dh_o$dt_cur_name), 'gliclazide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^atorvatatin',dh_o$dt_cur_name), 'atorvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fost',dh_o$dt_cur_name), 'formoterol/beclometason', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('formoterol (fostair)',dh_o$dt_cur_name), 'formoterol/beclometason', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^formo',dh_o$dt_cur_name), 'formoterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('frusemide',dh_o$dt_cur_name), 'furosemide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amit',dh_o$dt_cur_name), 'amitriptyline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amil',dh_o$dt_cur_name), 'amiloride', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('morph',dh_o$dt_cur_name), 'morphine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^diclofenac 1% gel',dh_o$dt_cur_name), 'gel_diclofenac', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^diclo',dh_o$dt_cur_name), 'diclofenac', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cody',dh_o$dt_cur_name), 'paracetamol/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^enoxpa',dh_o$dt_cur_name), 'LMWH', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^therapeutic enxoparin',dh_o$dt_cur_name), 'LMWH', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lecarnidipine',dh_o$dt_cur_name), 'lercanidipine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^abidec',dh_o$dt_cur_name), 'multivitamin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^acetazolomide',dh_o$dt_cur_name), 'acetazolamide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^aspirin',dh_o$dt_cur_name), 'acetylsalicyclic acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^adcal',dh_o$dt_cur_name), 'adcal', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ad cal',dh_o$dt_cur_name), 'adcal', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^adalat',dh_o$dt_cur_name), 'adalat', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^adizem',dh_o$dt_cur_name), 'diltiazem', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^adren',dh_o$dt_cur_name), 'adrenaline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^airomir',dh_o$dt_cur_name), 'salbutamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^aledronic acid',dh_o$dt_cur_name), 'alendronic_acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^alfacacidol',dh_o$dt_cur_name), 'alfacalcidol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^aloprazolam',dh_o$dt_cur_name), 'alprazolam', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^alveine',dh_o$dt_cur_name), 'alverine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amiadarone',dh_o$dt_cur_name), 'amiodarone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amidarone',dh_o$dt_cur_name), 'amiodarone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amlo',dh_o$dt_cur_name), 'amlodipine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amoxycillin',dh_o$dt_cur_name), 'amoxicillin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^anastrazole',dh_o$dt_cur_name), 'anastrozole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^anusol',dh_o$dt_cur_name), 'anusol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^apraclonidine',dh_o$dt_cur_name), 'apraclonidine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^apria iinsulin',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^arcoxia',dh_o$dt_cur_name), 'etoricoxib', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^arorvastatin',dh_o$dt_cur_name), 'atorvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^artorvastatin',dh_o$dt_cur_name), 'atorvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^artovastatin',dh_o$dt_cur_name), 'atorvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^atorvatatin',dh_o$dt_cur_name), 'atorvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^atrovastatin',dh_o$dt_cur_name), 'atorvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^atrovastatine',dh_o$dt_cur_name), 'atorvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^augmentin',dh_o$dt_cur_name), 'co_amoxiclav', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('co- amoxiclav',dh_o$dt_cur_name), 'co_amoxiclav', dh_o$dt_cur_name)
#assuming accuhaler contains seretide
dh_o$dt_cur_name <- ifelse(grepl('^accuhaler',dh_o$dt_cur_name), 'seretide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^b12',dh_o$dt_cur_name), 'folate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^vitamin b12',dh_o$dt_cur_name), 'folate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^bactroban',dh_o$dt_cur_name), 'mupirocine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^becl',dh_o$dt_cur_name), 'beclomethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^belcomethasone',dh_o$dt_cur_name), 'beclomethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^becot',dh_o$dt_cur_name), 'beclomethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^becon',dh_o$dt_cur_name), 'nasal_beclomethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^bendro',dh_o$dt_cur_name), 'bendroflumethiazide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^betah',dh_o$dt_cur_name), 'betahistine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^betametasone im injection',dh_o$dt_cur_name), 'im_betamethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^betamethasone cream',dh_o$dt_cur_name), 'topical_betamethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^bisprolol',dh_o$dt_cur_name), 'bisoprolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^bisphosphonate',dh_o$dt_cur_name), 'bisphosphonate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^braltus',dh_o$dt_cur_name), 'tiotrupium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^bricanyl',dh_o$dt_cur_name), 'terbutaline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^brinzolamide',dh_o$dt_cur_name), 'brinzolamide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^budesonide (symbicort)',dh_o$dt_cur_name), 'budesonide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^budensonide',dh_o$dt_cur_name), 'budesonide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^budesonide inhaler',dh_o$dt_cur_name), 'budesonide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^budesonide with formoterol',dh_o$dt_cur_name), 'budesonide/formoterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^budesonide and formoterol',dh_o$dt_cur_name), 'budesonide/formoterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sym',dh_o$dt_cur_name), 'budesonide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^bupre',dh_o$dt_cur_name), 'buprenorphine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^burinex',dh_o$dt_cur_name), 'bumetanide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^butrans',dh_o$dt_cur_name), 'buprenorphine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^bydurureon',dh_o$dt_cur_name), 'exenatide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcichew',dh_o$dt_cur_name), 'calcium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcios',dh_o$dt_cur_name), 'calcium_d', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calceos',dh_o$dt_cur_name), 'calcium_d', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcichew-d3',dh_o$dt_cur_name), 'calcium_d', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcichew d3',dh_o$dt_cur_name), 'calcium_d', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calciferol',dh_o$dt_cur_name), 'vit_D', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcihew d3',dh_o$dt_cur_name), 'calcium_d', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcium and vitamin d supplement',dh_o$dt_cur_name), 'calcium_d', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcium carbonate',dh_o$dt_cur_name), 'calcium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcium and vit d and k',dh_o$dt_cur_name), 'calcium_d_K', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcium carbonate and cholecalciferol',dh_o$dt_cur_name), 'calcium_d', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcium d3',dh_o$dt_cur_name), 'calcium_d', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^calcium tablet',dh_o$dt_cur_name), 'calcium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cand',dh_o$dt_cur_name), 'candesartan', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^canocobalamin',dh_o$dt_cur_name), 'B12', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^carbamazipine',dh_o$dt_cur_name), 'carbamazepine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^carboc',dh_o$dt_cur_name), 'carbocisteine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^carbos',dh_o$dt_cur_name), 'carbocisteine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cdeine phosphate',dh_o$dt_cur_name), 'codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cerazette',dh_o$dt_cur_name), 'desogestrel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cerelle',dh_o$dt_cur_name), 'cerelle', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cetrizine',dh_o$dt_cur_name), 'cetirizine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cert',dh_o$dt_cur_name), 'cetirizine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^chlorph',dh_o$dt_cur_name), 'chlorphenamine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^chlop',dh_o$dt_cur_name), 'chlorphenamine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cholcicoliferol',dh_o$dt_cur_name), 'vit_D', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cholestagel',dh_o$dt_cur_name), 'colesevelam', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^qcipramil',dh_o$dt_cur_name), 'citalopram', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ciproslaxacin',dh_o$dt_cur_name), 'ciprofloxacin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^citirazine',dh_o$dt_cur_name), 'cetirizine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^citirizine',dh_o$dt_cur_name), 'cetirizine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^clar',dh_o$dt_cur_name), 'clarithromycin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^clencile',dh_o$dt_cur_name), 'beclomethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^clobetasol',dh_o$dt_cur_name), 'clobetasol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^clobetasone',dh_o$dt_cur_name), 'clobetasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^clobetesone',dh_o$dt_cur_name), 'clobetasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^co-amilofruse',dh_o$dt_cur_name), 'furosemide/amiloride', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^co-codamol',dh_o$dt_cur_name), 'paracetamol/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cocodamol',dh_o$dt_cur_name), 'paracetamol/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^coed',dh_o$dt_cur_name), 'codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^colecalciferol',dh_o$dt_cur_name), 'vit_D', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^coltrazine',dh_o$dt_cur_name), 'colchicine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^combivent',dh_o$dt_cur_name), 'ipratropium/albuterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('contrace',dh_o$dt_cur_name), 'contraception', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cotrimoxazole',dh_o$dt_cur_name), 'co-trimoxazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cyanocobalamin',dh_o$dt_cur_name), 'B12', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^cyclazine',dh_o$dt_cur_name), 'cyclizine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^d3 2500 units',dh_o$dt_cur_name), 'vit_D', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^daktocort',dh_o$dt_cur_name), 'miconazol/hydrocortison', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dalteparin',dh_o$dt_cur_name), 'LMWH', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dalteparin (fragmin)',dh_o$dt_cur_name), 'LMWH', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dapa',dh_o$dt_cur_name), 'dapagliflozin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^delteparin',dh_o$dt_cur_name), 'LMWH', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^darbepoetin',dh_o$dt_cur_name), 'darbepoetin alpha', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^deo-',dh_o$dt_cur_name), 'medroxyprogesterone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^depo',dh_o$dt_cur_name), 'medroxyprogesterone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dermovate',dh_o$dt_cur_name), 'clobetasol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^deso',dh_o$dt_cur_name), 'desogestrel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^detrusitol',dh_o$dt_cur_name), 'tolterodine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dexasosin',dh_o$dt_cur_name), 'doxazosin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^diamox',dh_o$dt_cur_name), 'acetazolamide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^diaz',dh_o$dt_cur_name), 'diazepam', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dicycloverine',dh_o$dt_cur_name), 'dicycloverine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^digitoxin',dh_o$dt_cur_name), 'digoxin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dihydrocodeine',dh_o$dt_cur_name), 'codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dil',dh_o$dt_cur_name), 'diltiazem', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dipro',dh_o$dt_cur_name), 'diprobase', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dipy',dh_o$dt_cur_name), 'dipyridamole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^disp',dh_o$dt_cur_name), 'acetylsalicyclic acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^domperidone',dh_o$dt_cur_name), 'domperidone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dorzolamide 2%',dh_o$dt_cur_name), 'dorzolamide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dorzolamide/ timolol eye drops',dh_o$dt_cur_name), 'dorzolamide/timolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^doth',dh_o$dt_cur_name), 'dosulepin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^duoresp',dh_o$dt_cur_name), 'budesonide/formoterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^duragesic',dh_o$dt_cur_name), 'fentanyl_transdermal', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^dymista',dh_o$dt_cur_name), 'azelastine/fluticasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^eklira',dh_o$dt_cur_name), 'aclidinium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^eletriptan',dh_o$dt_cur_name), 'eletriptan', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^eltrombopaeg',dh_o$dt_cur_name), 'eltrombopag', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^enox',dh_o$dt_cur_name), 'LMWH', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^enxo',dh_o$dt_cur_name), 'LMWH', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^epaderm',dh_o$dt_cur_name), 'epaderm', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^epilim',dh_o$dt_cur_name), 'sodium valproate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^epler',dh_o$dt_cur_name), 'eplerenone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^escemestane',dh_o$dt_cur_name), 'exemestane', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('estradiol',dh_o$dt_cur_name), 'estradiol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^etonogestrel',dh_o$dt_cur_name), 'etonogestrel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^eumovate',dh_o$dt_cur_name), 'clobetason', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^evra',dh_o$dt_cur_name), 'ethinylestradiol/norelgestromin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ezetrol',dh_o$dt_cur_name), 'ezetimib', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^femodine',dh_o$dt_cur_name), 'ethinylestradiol/gestodene', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^femulen',dh_o$dt_cur_name), 'contraception', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fentanyl pat',dh_o$dt_cur_name), 'fentanyl_transdermal', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ferinj',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ferrus',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ferosemide',dh_o$dt_cur_name), 'furosemide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fex',dh_o$dt_cur_name), 'fexofenadine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^flixonase',dh_o$dt_cur_name), 'flixonase', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^flucatisone and formoterol',dh_o$dt_cur_name), 'flucatisone/formoterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fluclo',dh_o$dt_cur_name), 'flucloxacillin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fluticason',dh_o$dt_cur_name), 'fluticasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^folate',dh_o$dt_cur_name), 'B12', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fortisip',dh_o$dt_cur_name), 'fortisip', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fosamax',dh_o$dt_cur_name), 'alendronic_acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fragmin',dh_o$dt_cur_name), 'LMWH', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^frumeside',dh_o$dt_cur_name), 'furosemide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^frumil',dh_o$dt_cur_name), 'furosemide/amiloride', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fultium',dh_o$dt_cur_name), 'vit_D', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^furosomide',dh_o$dt_cur_name), 'furosemide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fursemide',dh_o$dt_cur_name), 'furosemide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fucibet',dh_o$dt_cur_name), 'fusidic acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fusidic',dh_o$dt_cur_name), 'fusidic acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^fybogel',dh_o$dt_cur_name), 'fybogel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^garbapentin',dh_o$dt_cur_name), 'gabapentin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^gastrocote',dh_o$dt_cur_name), 'otc_reflux', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^gasviscon',dh_o$dt_cur_name), 'otc_reflux', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glandosane',dh_o$dt_cur_name), 'glandosane', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glargine_insulin',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glucazide',dh_o$dt_cur_name), 'gliclazide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glimepride',dh_o$dt_cur_name), 'glimepiride', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glucophage',dh_o$dt_cur_name), 'metformin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glucosaminsulfat',dh_o$dt_cur_name), 'glucosamine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glyceril trinitrate spray',dh_o$dt_cur_name), 'gtn spray', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glycer',dh_o$dt_cur_name), 'gtn', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^glycopyrronium',dh_o$dt_cur_name), 'glycopyrronium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^gylceryl trinitrate',dh_o$dt_cur_name), 'gtn', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^hepsal',dh_o$dt_cur_name), 'heparin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^hrt',dh_o$dt_cur_name), 'hormone_replacement_therapy', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^humalog',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^human mixtard',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^humulin',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^hux d3',dh_o$dt_cur_name), 'vit_D', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^hydroch',dh_o$dt_cur_name), 'hydrochlorothiazide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^hydroxacine',dh_o$dt_cur_name), 'hydroxyzine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^hydroxycobalamin',dh_o$dt_cur_name), 'B12', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^hylo-forte',dh_o$dt_cur_name), 'hylo-forte', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^hyo',dh_o$dt_cur_name), 'butylscopolamine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^hyp',dh_o$dt_cur_name), 'hypromellose', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^iatropium nebuliser',dh_o$dt_cur_name), 'ipratropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ibesartan',dh_o$dt_cur_name), 'irbesartan', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ibruprofen gel',dh_o$dt_cur_name), 'topical_ibuprofen', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ibuprofen gel',dh_o$dt_cur_name), 'topical_ibuprofen', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('imatinib (glivanc)',dh_o$dt_cur_name), 'imatinib', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^imdur',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^immodium',dh_o$dt_cur_name), 'imodium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^implanon',dh_o$dt_cur_name), 'implanon', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^indapapmide',dh_o$dt_cur_name), 'indapamide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^insulatard',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^insulated innolet prefilled disposable device insulin isophane',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ipramine',dh_o$dt_cur_name), 'imipramine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ismn',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^isosorbide',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^isotardxl',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ivabridine',dh_o$dt_cur_name), 'ivabradine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^kenalog',dh_o$dt_cur_name), 'paracetamol/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^kennalog',dh_o$dt_cur_name), 'paracetamol/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^keppra',dh_o$dt_cur_name), 'levetiracetam', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^kleen prep',dh_o$dt_cur_name), 'macrogol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lantus',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lanus',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lanzoprazole',dh_o$dt_cur_name), 'lansoprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lapsoprazole',dh_o$dt_cur_name), 'lansoprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^latanoprolt',dh_o$dt_cur_name), 'lantoprost', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^laxido',dh_o$dt_cur_name), 'macrogol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^leflunamide',dh_o$dt_cur_name), 'leflunomide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^letrezole',dh_o$dt_cur_name), 'letrozole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^levemir',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^levest',dh_o$dt_cur_name), 'ethinylestradiol/levonorgestrel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^l-thyroxine',dh_o$dt_cur_name), 'levothyroxine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lidocaine',dh_o$dt_cur_name), 'lidocaine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^linagliptine',dh_o$dt_cur_name), 'linagliptin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^linsopril',dh_o$dt_cur_name), 'linsinopril', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^liothyronine',dh_o$dt_cur_name), 'levothyroxine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lipitor',dh_o$dt_cur_name), 'atorvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^liragutide',dh_o$dt_cur_name), 'liraglutide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^loestrin',dh_o$dt_cur_name), 'ethinylestradiol/norethisterone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^loper',dh_o$dt_cur_name), 'loperamide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^losec',dh_o$dt_cur_name), 'omeprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lotemax',dh_o$dt_cur_name), 'loteprednol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lucentis',dh_o$dt_cur_name), 'ranibizumab', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lumigan',dh_o$dt_cur_name), 'lumigan', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^lyrica',dh_o$dt_cur_name), 'pregabalin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^macrogel',dh_o$dt_cur_name), 'macrogol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^macrogol',dh_o$dt_cur_name), 'macrogol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^madopar',dh_o$dt_cur_name), 'levodopa/benserazide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^magnesium',dh_o$dt_cur_name), 'magnesium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^maxalon',dh_o$dt_cur_name), 'metoclopramide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^maxidex',dh_o$dt_cur_name), 'maxidex', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^meberverine',dh_o$dt_cur_name), 'mebeverine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^medrone',dh_o$dt_cur_name), 'methylprednisolone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mefenemic acid',dh_o$dt_cur_name), 'mefenamic acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mesasalasine',dh_o$dt_cur_name), 'mesalazine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^metaclopramide',dh_o$dt_cur_name), 'metoclopramide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^metaclopromide',dh_o$dt_cur_name), 'metoclopramide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^metalazone',dh_o$dt_cur_name), 'metolazone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^metildogoxin',dh_o$dt_cur_name), 'metildigoxin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^metoc',dh_o$dt_cur_name), 'metoclopramide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^maxalon metoclopramide',dh_o$dt_cur_name), 'metoclopramide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^metoprolol',dh_o$dt_cur_name), 'metoprolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^metroprolol',dh_o$dt_cur_name), 'metoprolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^miconazole',dh_o$dt_cur_name), 'miconazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^microgynon',dh_o$dt_cur_name), 'ethinylestradiol/levonorgestrel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^micronor',dh_o$dt_cur_name), 'norethisterone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^migraleve',dh_o$dt_cur_name), 'paracetamol/buclizine/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^millinet',dh_o$dt_cur_name), 'ethinylestradiol/gestodene', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mirtazapine',dh_o$dt_cur_name), 'mirtazapine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mirtazepine',dh_o$dt_cur_name), 'mirtazapine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mirtazipine',dh_o$dt_cur_name), 'mirtazapine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mitazapine',dh_o$dt_cur_name), 'mirtazapine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mitirzapin',dh_o$dt_cur_name), 'mirtazapine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mitrazapine',dh_o$dt_cur_name), 'mirtazapine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^monomax',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^monomil',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^movicol',dh_o$dt_cur_name), 'macrogol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mucodyne',dh_o$dt_cur_name), 'carbocisteine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mucolast',dh_o$dt_cur_name), 'levocetirizine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^multivit',dh_o$dt_cur_name), 'mucolast', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^mycoph',dh_o$dt_cur_name), 'mycophenolate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^naftidrofuryl',dh_o$dt_cur_name), 'naftidrofuryl', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^naseptin',dh_o$dt_cur_name), 'naseptin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nasonex',dh_o$dt_cur_name), 'nasal_spray_mometasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nebivol',dh_o$dt_cur_name), 'nebivolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^neurofen',dh_o$dt_cur_name), 'ibuprofen', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nexium',dh_o$dt_cur_name), 'esomeprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nexplanon',dh_o$dt_cur_name), 'etonogestrel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nicroandil',dh_o$dt_cur_name), 'nicorandil', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nicot',dh_o$dt_cur_name), 'nicotine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nifedipine',dh_o$dt_cur_name), 'nifedipine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nifidepine',dh_o$dt_cur_name), 'nifedipine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nilstat',dh_o$dt_cur_name), 'nystatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nystan',dh_o$dt_cur_name), 'nystatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^olanzapin',dh_o$dt_cur_name), 'olanzapine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^omerazole',dh_o$dt_cur_name), 'omeprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^omezprazole',dh_o$dt_cur_name), 'omeprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ompeprazole',dh_o$dt_cur_name), 'omeprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^omprazole',dh_o$dt_cur_name), 'omeprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ostieocaps',dh_o$dt_cur_name), 'vit_D', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^otrivin',dh_o$dt_cur_name), 'otrivin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^oxybut',dh_o$dt_cur_name), 'oxybutynin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^oxycontin',dh_o$dt_cur_name), 'oxycodone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^oxynorm',dh_o$dt_cur_name), 'oxycodone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^oxytetracyclone',dh_o$dt_cur_name), 'oxytetracycline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^palexia',dh_o$dt_cur_name), 'tapentadol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^panto',dh_o$dt_cur_name), 'pantoprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^paracetamol 500 and dihydrocodiene 20mg tablets',dh_o$dt_cur_name), 'paracetamol/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^parecetamol',dh_o$dt_cur_name), 'paracetamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^paramax',dh_o$dt_cur_name), 'paracetamol/metoclopramide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^paroxicam gel',dh_o$dt_cur_name), 'piroxicam_gel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^penicillin',dh_o$dt_cur_name), 'penicillin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^peptac',dh_o$dt_cur_name), 'otc_reflux', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^peptal',dh_o$dt_cur_name), 'otc_reflux', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^persantine',dh_o$dt_cur_name), 'dipyridamole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^phenergan',dh_o$dt_cur_name), 'promethazine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^phyllocontin',dh_o$dt_cur_name), 'aminophylline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^piriton',dh_o$dt_cur_name), 'chlorphenamine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^pivira respimat',dh_o$dt_cur_name), 'tiotropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^pizotifen',dh_o$dt_cur_name), 'pizotifen', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^plenachol d3',dh_o$dt_cur_name), 'vit_D', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^pred forte',dh_o$dt_cur_name), 'prednisolone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^premarin',dh_o$dt_cur_name), 'hormone_replacement_therapy', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^prenisolone',dh_o$dt_cur_name), 'prednisolone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^prevastatin',dh_o$dt_cur_name), 'pravastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^prempak c',dh_o$dt_cur_name), 'hormone_replacement_therapy', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^priadel',dh_o$dt_cur_name), 'lithium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^pridnisolone',dh_o$dt_cur_name), 'prednisolone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^primidon',dh_o$dt_cur_name), 'primidone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^progesterone',dh_o$dt_cur_name), 'progesterone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^prolia',dh_o$dt_cur_name), 'denosumab', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^propylthioruacil',dh_o$dt_cur_name), 'propylthiouracil', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^proranolol',dh_o$dt_cur_name), 'propranolol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^prozac',dh_o$dt_cur_name), 'fluoxetine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^provera',dh_o$dt_cur_name), 'medroxyprogesteron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^pulmicort',dh_o$dt_cur_name), 'budesonide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^pyridoxine',dh_o$dt_cur_name), 'B6', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^qvar',dh_o$dt_cur_name), 'beclometasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^quar',dh_o$dt_cur_name), 'beclometasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^quinine',dh_o$dt_cur_name), 'quinine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^rabeprazol',dh_o$dt_cur_name), 'rabeprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^rami',dh_o$dt_cur_name), 'ramipril', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^rama',dh_o$dt_cur_name), 'ramipril', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ranitidine',dh_o$dt_cur_name), 'rantidine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^rantitidine',dh_o$dt_cur_name), 'rantidine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^regevidan',dh_o$dt_cur_name), 'ethinylestradiol/levonorgestrel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^rennie',dh_o$dt_cur_name), 'otc_reflux', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^relvar',dh_o$dt_cur_name), 'fluticasone/vilanterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^renovit multivitamins',dh_o$dt_cur_name), 'multivitamin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^residronate',dh_o$dt_cur_name), 'risedronic acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^rigevidon',dh_o$dt_cur_name), 'ethinylestradiol/levonorgestrel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^risedronate',dh_o$dt_cur_name), 'risedronic acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^rivaroxiban',dh_o$dt_cur_name), 'rivaroxaban', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^rouvastatin',dh_o$dt_cur_name), 'rosuvastatin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sabutamol',dh_o$dt_cur_name), 'salbutamol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sagaliptin',dh_o$dt_cur_name), 'saxagliptin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sando',dh_o$dt_cur_name), 'potassium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seebri',dh_o$dt_cur_name), 'glycopyrronium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seratide',dh_o$dt_cur_name), 'fluticasone/salmeterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seretide',dh_o$dt_cur_name), 'fluticasone/salmeterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^serotide',dh_o$dt_cur_name), 'fluticasone/salmeterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^serazet',dh_o$dt_cur_name), 'desogestrel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^seroxat',dh_o$dt_cur_name), 'paroxetine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^setraline',dh_o$dt_cur_name), 'sertraline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sev',dh_o$dt_cur_name), 'morphine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^simple linctus',dh_o$dt_cur_name), 'cough_syrup', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sinthrom',dh_o$dt_cur_name), 'acenocoumarol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sirdupla',dh_o$dt_cur_name), 'fluticasone/salmeterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sitalgliptin',dh_o$dt_cur_name), 'sitagliptin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^slidenafil',dh_o$dt_cur_name), 'sildenafil', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^slow',dh_o$dt_cur_name), 'potassium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sloxem',dh_o$dt_cur_name), 'diltiazem', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^slozem',dh_o$dt_cur_name), 'diltiazem', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('sodium alginate with magnesium alginate (gaviscon infant)',dh_o$dt_cur_name), 'otc_reflux', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sodium cromoglycate',dh_o$dt_cur_name), 'sodium cromoglicate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sodium docysate',dh_o$dt_cur_name), 'sodium docusate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sodium feredate',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sodium feredetate',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sodium nedocromil',dh_o$dt_cur_name), 'nedocromil', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sodium valproate',dh_o$dt_cur_name), 'sodium valproate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^solfenacin',dh_o$dt_cur_name), 'solifenacin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^solpadol',dh_o$dt_cur_name), 'paracetamol/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^spiolto',dh_o$dt_cur_name), 'tiotropium/olodaterol', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^spirinolactone',dh_o$dt_cur_name), 'spironolactone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sprivia',dh_o$dt_cur_name), 'tiotropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sulphasalazine',dh_o$dt_cur_name), 'sulfasalazine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sulpramide',dh_o$dt_cur_name), 'sulpiride', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^suphasalazine',dh_o$dt_cur_name), 'sulfasalazine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^supride',dh_o$dt_cur_name), 'glimepiride', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^syron',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^sytron',dh_o$dt_cur_name), 'iron', dh_o$dt_cur_name)
#assuming _l-thyroxine is t4
dh_o$dt_cur_name <- ifelse(grepl('^t4',dh_o$dt_cur_name), 'levothyroxine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tamulozin',dh_o$dt_cur_name), 'tamsulosin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tazocin',dh_o$dt_cur_name), 'piperacillin/tazobactam', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tegretol',dh_o$dt_cur_name), 'carbamazepine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^terbutaline',dh_o$dt_cur_name), 'terbutaline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^teveten',dh_o$dt_cur_name), 'eprosartan', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^theo',dh_o$dt_cur_name), 'theophylline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^throxine',dh_o$dt_cur_name), 'levothyroxine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tildiem',dh_o$dt_cur_name), 'diltiazem', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^timodine',dh_o$dt_cur_name), 'topical_hydrocortisone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tinzaparin',dh_o$dt_cur_name), 'LMWH', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tiotrupium',dh_o$dt_cur_name), 'tiotropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tiptropium',dh_o$dt_cur_name), 'tiotropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tolterodin',dh_o$dt_cur_name), 'tolterodine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^topiromate',dh_o$dt_cur_name), 'topiramate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^torsemide',dh_o$dt_cur_name), 'torasemide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tracolimus',dh_o$dt_cur_name), 'tacrolimus', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^transonamic acid',dh_o$dt_cur_name), 'tranexamic acid', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^travoprost',dh_o$dt_cur_name), 'travoprost', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^travatan',dh_o$dt_cur_name), 'travoprost', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^trelegy',dh_o$dt_cur_name), 'fluticasone/umeclidinium/vilanterol)', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tresiba',dh_o$dt_cur_name), 'insulin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^triamcinolone',dh_o$dt_cur_name), 'triamcinolone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^trijodthyronin',dh_o$dt_cur_name), 'liothyronine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tritropium',dh_o$dt_cur_name), 'tiotropium', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^trusopt',dh_o$dt_cur_name), 'dorzolamide_ocular', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^tylex',dh_o$dt_cur_name), 'paracetamol/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^uniphyllin',dh_o$dt_cur_name), 'theophylline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ubidecarenone',dh_o$dt_cur_name), 'co_enzyme_q10', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^uniquinone',dh_o$dt_cur_name), 'co_enzyme_q10', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^upostelle',dh_o$dt_cur_name), 'levonorgestrel', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^utrogestan',dh_o$dt_cur_name), 'hormone_replacement_therapy', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^valerian',dh_o$dt_cur_name), 'valerian', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^valgancyclovir',dh_o$dt_cur_name), 'valganciclovir', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^venl',dh_o$dt_cur_name), 'venlafaxine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^venflafaxine',dh_o$dt_cur_name), 'venlafaxine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^ventavis',dh_o$dt_cur_name), 'iloprost', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^versatis',dh_o$dt_cur_name), 'lidocaine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^victoza',dh_o$dt_cur_name), 'liraglutide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^vit b thiamine',dh_o$dt_cur_name), 'thiamine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^vit d capsules',dh_o$dt_cur_name), 'vit_D', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^vit c',dh_o$dt_cur_name), 'vit_C', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^vitamin a & d supplment',dh_o$dt_cur_name), 'vit_D_A', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('vitamin b and thiamine',dh_o$dt_cur_name), 'vit_B_thiamine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^vitamin d',dh_o$dt_cur_name), 'vit_D', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^vitamin b',dh_o$dt_cur_name), 'vit_B', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^vitamin tablets',dh_o$dt_cur_name), 'multivitamin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^xipamid',dh_o$dt_cur_name), 'xipamide', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^xylometazoline',dh_o$dt_cur_name), 'xylometazoline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^xismox',dh_o$dt_cur_name), 'isosorbide mononitrate', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^zapain',dh_o$dt_cur_name), 'paracetamol/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^zimovane',dh_o$dt_cur_name), 'zopiclone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^zoladex',dh_o$dt_cur_name), 'goserelin', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^zoton',dh_o$dt_cur_name), 'lansoprazole', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amip',dh_o$dt_cur_name), 'amitryptyline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amy',dh_o$dt_cur_name), 'amitryptyline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^amt',dh_o$dt_cur_name), 'amitryptyline', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^beclometasone',dh_o$dt_cur_name), 'beclomethasone', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^co_dydramol',dh_o$dt_cur_name), 'paracetamol/codeine', dh_o$dt_cur_name)
dh_o$dt_cur_name <- ifelse(grepl('^nebeverine',dh_o$dt_cur_name), 'mebeverine', dh_o$dt_cur_name)

abc <- unique(as.data.frame(dh_o$dt_cur_name, na.rm=TRUE))
#so 1824 to 608 obs, reduces number of drugs without losing data!


#investigate drugs in other dfs to group drugs from the unclean df
summary(as.factor(drug_pah_ac$dh_ac_drug))
#these are just drug names, no classes added
summary(as.factor(drug_pah_at$dh_at_drug))
#Names: Diuretics, Ca Channel Blocker, Digoxin, Longterm Oxygen
summary(as.factor(drug_pah_other$dh_other_drug))
#Imatinib, Racecadotril, Tocilizumab
summary(as.factor(drug_pah_pa$dh_pa_drug))
#makes distinction between IV, inhaled, oral and sc
summary(as.factor(drug_pah_pde5$dh_pde5_drug))
#Sildenafil & Tadalafil
summary(as.factor(drug_pah_pra$dh_pra_drug))
#Selexipag
summary(as.factor(drug_pah_sgc$dh_sgc_drug))
#Riociguat

#name groups to combine drugs 
Imatinib <- c('imatinib', 'imatinib (glivanc)')
K_sparing_diuretic <- c('amiloride', 'eplerenone', 'spironolactone')
loop_diuretic <- c('bumetanide', 'furosemide', 'furosemide/amiloride', 'iv furosemide', 'torasemide')
#sometimes combination drugs in one group...
#xipamide classed as thiazide for now
thiazide_diuretic <- c('bendroflumethiazide', 'hydrochlorothiazide', 'indapamide', 'metolazone', 'valsartan/hydrochlorothiazide', 'xipamide')
CCB <- c('amlodipine', 'diltiazem', 'felodipine', 'lercanidipine', 'verapamil')
SGLT2i <- c('canagliflozin', 'dapagliflozin', 'empaglifloxin')
Digoxin <- c('digoxin', 'glycloside', 'metildigoxin')
LT_oxygen <- c('ltot', 'oxygen')
PPI <- c('esomeprazole', 'lansoprazole', 'omeprazole', 'pantoprazole', 'paroxetine', 'rabeprazole')
SU_derrivate <- c('glibenclamide', 'gliclazide', 'glimepiride')
dm2_drug_other <- c('acarbose', 'linagliptin', 'pioglitazone', 'repaglidine', 'saxagliptin', 'vildaglipitn')
anticoagulant <- c('acenocoumarol','apixaban', 'dabigatran', 'edoxaban', 'phenprocoumon', 'rivaroxaban', 'warfarin')
vasodilator_other <- c('apresoline', 'doxazosin', 'hydralazine hydrochloride', 'minoxidil', 'naftidrofuryl', 'tolazoline')
anti_platelet <- c('acetylsalicyclic acid', 'anagrelide', 'cilostazol', 'clopidrogel', 'dipyridamole', 'ticagrelor')
h2_blocker <- c('cimetidine', 'nizatidine', 'rantidine')
antacid <- c('gaviscon', 'gaviscon advance', 'otc_reflux')
ssri_snri <- c('cipramil', 'citalopram', 'duloxetine', 'escitalopram', 'fluoxetine', 'reboxetine', 'sertraline', 'trazodone', 'venlafaxine')
glp1_agonist <- c('dulaglutide', 'exenatide', 'liraglutide', 'lixisenatide', 'sitagliptin')
muscarine_antagonist <- c('aclidnium bromide', 'aclinidinium', 'aclidinium', 'umeclidinium')
antihistamine <- c('acravistine', 'antihistamine', 'avomine', 'cetirizine', 'chlorphenamine', 'cyclizine', 'desloratadine', 'fexofenadine', 'hydroxyzine', 'levocetirizine', 'loratidine', 'promethazine')
antiarrhythmic <- c('adenosine','amiodarone', 'dronedarone', 'ivabradine')
ACEi_ARBs <- c('aliskiren', 'candesartan', 'captopril', 'enalapril', 'eprosartan', 'irbesartan', 'linsinopril', 'lisinopril', 'losartan', 'nicardipine', 'nifedipine', 'olmesartan medoxomil', 'perindopril', 'quinapril', 'ramipril', 'telmisartan', 'trandolapril', 'valsartan')
gout_drug <- c('allopurinol', 'colchicine')
benzo <- c('alprazolam', 'clonazepam', 'diazepam', 'lorazepam', 'midazolam', 'nitrazepam', 'temazepam', 'triazolam', 'zolpidem', 'zopiclone')
ERA <- c('ambrisentan','bosentan','macitentan')
PA <- c('epoprostenol', 'iloprost', 'intravenous iloprost', 'nebulised lloprost', 'trepostanil')
PRA <- c('selexipag')
TCA <- c('amitryptyline', 'clomipramine', 'dosulepin', 'imipramine', 'mirtazapine', 'nortriptyline', 'trimipramine')
beta_agonist <- c('formoterol', 'indacterol', 'nebuliser_salbutamol', 'salbutamol', 'salmeterol', 'terbutaline', 'vilanterol')
hyperthyroid_drugs <- c('carbimazole', 'propylthiouracil')
hypothyroid_drugs <- c('levothyroxine','liothyronine', 'naturally dessicated thyroxine supplement')
beta_blocker <- c('atenolol','bisoprolol','carvedilol', 'labetalol', 'metoprolol', 'nebivolol', 'propranolol', 'sotalol')
statins <- c('atorvastatin', 'cerivastatin', 'rosuvastatin', 'simvastatin')
lipid_drug_other <- c('ezetimib', 'ezetimibe', 'fenofibrate', 'orlistat')
PDE5i <- c('sildenafil', 'tadalafil')
anticholingerics <- c('ipratropium', 'tiotropium')
anticholingerics_beta_agonists <- c('ipratropium/albuterol', 'tiotropium/olodaterol')
inhaled_corticosteroid <- c('beclomethasone', 'budesonide', 'budesonide (symbicort)', 'clencil', 'easihaler', 'fluticasone')
topical_corticosteroid <- c('betnovate rd', 'bettamouse', 'clobetasol', 'clobetason', 'clobetasone', 'miconazol/hydrocortison', 'mometasone', 'synalar cream', 'topical_betamethasone','topical_hydrocortisone')
mix_beta_agonist_corticosteroid_inhaled <- c('budesonide/formoterol', 'flucatisone/formoterol', 'fluticasone/salmeterol', 'fluticasone/umeclidinium/vilanterol)', 'fluticasone/vilanterol', 'salmeterol/fluticasone propionate')
systemic_corticosteroid <- c('fludrocortisone', 'hydrocortisone', 'im_betamethasone', 'iv methyprednisolone', 'methylprednisolone', 'prednisolone', 'prednisone')
immunosuppresant_other <- c('azathioprine', 'benepali', 'cyclosporin', 'etanercept', 'leflunomide', 'mesalazine', 'methotrexate', 'rituximab', 'sulfasalazine', 'tacrolimus', 'tocilizumab')
NSAID <- c('celecoxib', 'diclofenac', 'etoricoxib', 'gel_diclofenac', 'ibuprofen', 'meloxicam', 'nabumetone', 'naproxen', 'piroxicam_gel', 'topical_ibuprofen')
contraceptive <- c('cerelle', 'contraception', 'desogestrel', 'ethinylestradiol/gestodene', 'ethinylestradiol/levonorgestrel', 'ethinylestradiol/norelgestromin', 'ethinylestradiol/norethisterone', 'etonogestrel', 'femodene', 'implanon', 'levonorgestrel', 'medroxyprogesteron', 'medroxyprogesterone', 'merina coil', 'mirena coil', 'noriday', 'norgeston', 'norethisterone', 'progesterone', 'ulipristal')
opioids <- c('paracetamol/codeine', 'coproxamol', 'codeine', 'fentanyl', 'fentanyl_transdermal', 'kapake', 'methadone', 'morphine', 'oxycodone', 'paracetamol/buclizine/codeine', 'paracetamol/codeine', 'pholcodine', 'tapentadol', 'tramadol')
HRT <- c('estradiol', 'hormone_replacement_therapy', 'oestrogen', 'vagifem pessaries')
nitrates <- c('gtn', 'gtn spray', 'gtn tablet', 'isosorbide mononitrate', 'nicorandil')
heparin_LMWH <- c('heparin', 'LMWH')
Metformin <- c('metformin')
Insulin <- c('insulin')
iron_suppletion <- c('iron', 'iron & folate', 'iv_iron')
pain_other <- c('paracetamol', 'paracetamol/metoclopramid', 'nefopam')
xanthine_derrivate <- c('theophylline')
ap_other <- c('ranolazine')
#assuming lantanoprost is only for eyes when not indicated somewhere else
#given most antibiotics are one-off treatments --> not included these for now (can do later)

#now add the columns to the drugs
dh_o$drug_class <- NA
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% Imatinib, 'Imatinib', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% K_sparing_diuretic, 'K_sparing_diuretic', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% loop_diuretic, 'loop_diuretic', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% thiazide_diuretic, 'thiazide_diuretic', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% CCB, 'CCB', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% SGLT2i, 'SGLT2i', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% LT_oxygen, 'Longterm Oxygen', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% Digoxin, 'Digoxin', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% SU_derrivate, 'SU_derrivate', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% dm2_drug_other, 'dm2_drug_other', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% anticoagulant, 'anticoagulant', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% vasodilator_other, 'vasodilator_other', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% anti_platelet, 'anti_platelet', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% h2_blocker, 'h2_blocker', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% antacid, 'antacid', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% ssri_snri, 'ssri_snri', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% glp1_agonist, 'glp1_agonist', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% muscarine_antagonist, 'muscarine_antagonist', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% antihistamine, 'antihistamine', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% antiarrhythmic, 'antiarrhythmic', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% beta_agonist, 'beta_agonist', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% anticholingerics, 'anticholingerics', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% anti_platelet, 'anti_platelet', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% anticholingerics_beta_agonists, 'anticholingerics_beta_agonists', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% ACEi_ARBs, 'ACEi_ARBs', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% gout_drug, 'gout_drug', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% benzo, 'benzo', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% ERA, 'ERA', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% PRA, 'PRA', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% PDE5i, 'PDE5i', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% TCA, 'TCA', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% hyperthyroid_drugs, 'hyperthyroid_drugs', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% hypothyroid_drugs, 'hypothyroid_drugs', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% beta_agonist, 'beta_agonist', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% beta_blocker, 'beta_blocker', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% immunosuppresant_other, 'immunosuppresant_other', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% Metformin, 'Metformin', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% lipid_drug_other, 'PDE5i', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% statins, 'statins', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% NSAID, 'NSAID', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% inhaled_corticosteroid, 'inhaled_corticosteroid', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% topical_corticosteroid, 'topical_corticosteroid', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% systemic_corticosteroid, 'systemic_corticosteroid', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% contraceptive, 'contraceptive', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% opioids, 'opioids', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% HRT, 'HRT', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% heparin_LMWH, 'heparin_LMWH', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% Insulin, 'Insulin', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% iron_suppletion, 'iron_suppletion', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% PPI, 'PPI', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% xanthine_derrivate, 'xanthine_derrivate', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% pain_other, 'pain_other', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% ap_other, 'ap_other', dh_o$drug_class)
dh_o$drug_class <- ifelse(dh_o$dt_cur_name %in% nitrates, 'nitrates', dh_o$drug_class)


write_rds(dh_o, file='V3_granular_other_drug_data_cohort_vx_datey.rds')

#merge the tables so that one overview of drugs can be achieved
d1 <- dh_o[,c(1,6:12,15)]
d1$superclass <- d1$drug_class
super_diur <- c('K_sparing_diuretic', 'loop_diuretic', 'thiazide_diuretic')
super_ccb <- c('CCB')
d1$superclass <- ifelse(d1$drug_class %in% super_diur, 'Diuretics', d1$superclass)
d1$superclass <- ifelse(d1$drug_class %in% super_ccb, 'Ca Channel Blocker', d1$superclass)
d1 <- d1[,-c(9)]
stand_colnames <- c('id', 'cur', 'end_date', 'start_date', 'dose', 'current', 'drug_name', 'regular', 'class')
names(d1) <- stand_colnames
d1 <- d1[,-c(8)]

d2 <- drug_pah_at[,c(1,6:12)]
d2$class <- d2$dh_at_drug
d2 <- d2[,-c(6)]
stand_colnames2 <- c('id', 'cur', 'end_date', 'start_date', 'dose', 'drug_name', 'current', 'class')
names(d2) <- stand_colnames2

d3 <- drug_pah_ac[,c(1,6:12)]
d3 <- d3[,-c(7)]
d3$class <- 'anticoagulant'
names(d3) <- stand_colnames2

d4 <- drug_pah_era[,c(1,6:12)]
d4 <- d4[,-c(7,8)]
d4$current <- NA
d4$class <- 'ERA'
names(d4) <- stand_colnames2

d5 <-drug_pah_pa[,c(1,6:12)]
d5 <- d5[,-c(7,8)]
d5$current <- NA
d5$class <- 'PA'
names(d5) <- stand_colnames2

d6 <-drug_pah_pra[,c(1,6:12)]
d6 <- d6[,-c(7,8)]
d6$current <- NA
d6$class <- 'PRA'
names(d6) <- stand_colnames2

d7 <- drug_pah_pde5[,c(1,6:12)]
d7 <- d7[,-c(7,8)]
d7$current <- NA
d7$class <- 'PDE5i'
names(d7) <- stand_colnames2

d8 <- drug_pah_sgc[,c(1,6:12)]
d8 <- d8[,-c(7,8)]
d8$current <- NA
d8$class <- 'SGC'
names(d8) <- stand_colnames2

d9 <- drug_pah_other[,c(1,6:12)]
d9 <- d9[,-c(3,4)]
d9$current <- NA
d9$class <- 'Imantinib'
names(d9) <- stand_colnames2
d9$class <- ifelse(d9$drug_name == 'Tocilizumab', 'immunosuppresant_other', d9$class)
d9$class <- ifelse(d9$drug_name == 'Racecadotril', 'Neprilysin_inhib', d9$class)

#now bind the different dfs

tot_df <- rbind(d1, d2, d3, d4,d5, d6, d7, d8, d9)
#filter out the NAs/drugs that are recorded as 'no'
tot_df <- tot_df %>% filter(!is.na(drug_name))
write_rds(tot_df, file='V2_granular_PAH_specific_therapy_drugs_in_cohort_vx_datey.rds')

#remove some cols to make uniqe
tdf2 <- tot_df[,c(1,3:5,8)]
tdf2 <- unique(tdf2)
#some variables clearly had overlap --> removed these variables
tdf2 <- tdf2 %>% filter(!is.na(class))
tdf2 <- tdf2[,c(1,5)]
tdf2 <- unique(tdf2)
unique(tdf2$id)
#in total drugs recorded for 1552 patients! 
write_rds(tdf2, file='V2_cleaned_drugs_PAH_cohort_per_group_vx_datey.rds')

#==========================================================================================
#Now also clean the comorbidities
#also clean all data and not just for the clusters
ad <- associated_diagnoses %>% filter(!is.na(ad_other_diagnosis))
comorb <- comorbid_disease

#merge the 2 dfs containing comorbidity data
comorb <- comorb %>% select('id', 'cfh_comorbid_disease_diagnosis', 'cfh_comorbid_disease_date')
ad <- ad %>% select('id', 'ad_other_diagnosis')
ad$cfh_comorbid_disease_date <- NA
names(ad) <- c('id', 'cfh_comorbid_disease_diagnosis', 'cfh_comorbid_disease_date')

comorb <- rbind(comorb, ad)

#remove NAs as that means there's no comorbidity
comorb <- comorb %>% filter(!is.na(cfh_comorbid_disease_diagnosis))
#problem --> upper & lower case cause factors to be classified differently
comorb$cfh_comorbid_disease_diagnosis <- tolower(comorb$cfh_comorbid_disease_diagnosis)
#still a lot of double vars due to double classification 
#remove double spaces
comorb$cfh_comorbid_disease_diagnosis <- str_squish(comorb$cfh_comorbid_disease_diagnosis)

#also remove full stops
comorb$cfh_comorbid_disease_diagnosis <- gsub('[.]','',comorb$cfh_comorbid_disease_diagnosis)

abc <- as.data.frame(unique(comorb$cfh_comorbid_disease_diagnosis))
#so 2192 different diagnoses are recorded

#reuse code used for cleaning of subset of patients
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^append',comorb$cfh_comorbid_disease_diagnosis), 'appendicitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^asd',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^asth',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^atrial fib',comorb$cfh_comorbid_disease_diagnosis), 'afib', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^atrial flut',comorb$cfh_comorbid_disease_diagnosis), 'atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^atrial sept',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^back pain',comorb$cfh_comorbid_disease_diagnosis), 'back pain', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^barrett',comorb$cfh_comorbid_disease_diagnosis), 'barrett_oesophagus', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bipolar',comorb$cfh_comorbid_disease_diagnosis), 'bipolar', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bmpr2',comorb$cfh_comorbid_disease_diagnosis), 'bmpr2_mutation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^breast cancer',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bronchiec',comorb$cfh_comorbid_disease_diagnosis), 'bronchiectasis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^carcinoma of breast',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^carcinoma of the breast',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^catarac',comorb$cfh_comorbid_disease_diagnosis), 'cataract', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic kidney',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^impaired renal function',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic obstructive airwa',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic obstructive pulm',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic renal',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic tension',comorb$cfh_comorbid_disease_diagnosis), 'chronic tension headache', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cabg',comorb$cfh_comorbid_disease_diagnosis), 'CABG', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^coronary artery bypa',comorb$cfh_comorbid_disease_diagnosis), 'CABG', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^coronary artery disease with st',comorb$cfh_comorbid_disease_diagnosis), 'PCI', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^coronary artery sten',comorb$cfh_comorbid_disease_diagnosis), 'PCI', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes mellitis type 2',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes mellitus 2',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes type 2',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes mellitus type 2',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetic (type ii)',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes mellitus type 1',comorb$cfh_comorbid_disease_diagnosis), 'DM1', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes type i',comorb$cfh_comorbid_disease_diagnosis), 'DM1', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diverticular',comorb$cfh_comorbid_disease_diagnosis), 'diverticulosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diverticulitis',comorb$cfh_comorbid_disease_diagnosis), 'diverticulitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^deep vein',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^depression',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dry senile macular',comorb$cfh_comorbid_disease_diagnosis), 'dry macular degeneration', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dupuytren',comorb$cfh_comorbid_disease_diagnosis), 'dupuytren', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dyslip',comorb$cfh_comorbid_disease_diagnosis), 'dyslipidaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^epistax',comorb$cfh_comorbid_disease_diagnosis), 'epistaxis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^essential',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^family history',comorb$cfh_comorbid_disease_diagnosis), 'FHx_cardiovasc', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gall',comorb$cfh_comorbid_disease_diagnosis), 'gallstone', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gastrooe',comorb$cfh_comorbid_disease_diagnosis), 'GERD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gastro-oe',comorb$cfh_comorbid_disease_diagnosis), 'GERD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gastroinestinal bleed',comorb$cfh_comorbid_disease_diagnosis), 'gi bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gilbert',comorb$cfh_comorbid_disease_diagnosis), 'gilbert_syndrome', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^grave',comorb$cfh_comorbid_disease_diagnosis), 'graves_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^haemangio',comorb$cfh_comorbid_disease_diagnosis), 'haemangioma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^haemochromatosis',comorb$cfh_comorbid_disease_diagnosis), 'haemochromatosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^haemopt',comorb$cfh_comorbid_disease_diagnosis), 'haemoptysis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hay fever',comorb$cfh_comorbid_disease_diagnosis), 'hayfever', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hearling lo',comorb$cfh_comorbid_disease_diagnosis), 'hearing_loss', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hearing lo',comorb$cfh_comorbid_disease_diagnosis), 'hearing_loss', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hereditary haemorrhagic',comorb$cfh_comorbid_disease_diagnosis), 'hereditary_haemorrhagic_telangiectasia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hht',comorb$cfh_comorbid_disease_diagnosis), 'hereditary_haemorrhagic_telangiectasia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^high choles',comorb$cfh_comorbid_disease_diagnosis), 'dyslipidaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypercholester',comorb$cfh_comorbid_disease_diagnosis), 'dyslipidaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hyperlipidaemia',comorb$cfh_comorbid_disease_diagnosis), 'dyslipidaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^high grade ductal carcinoma right breast',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hpothyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hyperthyroid',comorb$cfh_comorbid_disease_diagnosis), 'hyperthyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hyperthyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hyperthyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypothyroid',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypothyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^appendicitis',comorb$cfh_comorbid_disease_diagnosis), 'appendicitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^interstitial lung disease',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ild',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^insulin-dependent diabetes',comorb$cfh_comorbid_disease_diagnosis), 'insulin_dependent_dm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^iron deficiency',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^iron deficiency(not anaemic)',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^iron defeciency anaemia',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^iron defficiency anaemia',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^iron deficiency anaemia',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^irritable bowel syndrome',comorb$cfh_comorbid_disease_diagnosis), 'ibs', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ischaemic heart disease with pci',comorb$cfh_comorbid_disease_diagnosis), 'PCI', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ischaemic heart disease(minor bridging of lad)',comorb$cfh_comorbid_disease_diagnosis), 'PCI', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ischaemic heart disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ischaemic heart disease with stenosis',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ischaemic heart disesae',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ischemic heart disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^itu admission with septic',comorb$cfh_comorbid_disease_diagnosis), 'sepsis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^known left ventricular failure',comorb$cfh_comorbid_disease_diagnosis), 'hfref', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^laparoscopic appendectomy',comorb$cfh_comorbid_disease_diagnosis), 'appendicitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('breast cancer',comorb$cfh_comorbid_disease_diagnosis), 'breast cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left bundle branch',comorb$cfh_comorbid_disease_diagnosis), 'lbbb', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left cerebral infarct',comorb$cfh_comorbid_disease_diagnosis), 'cva', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('mastectomy',comorb$cfh_comorbid_disease_diagnosis), 'mastectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('left occipital stroke',comorb$cfh_comorbid_disease_diagnosis), 'cva', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('low transferrin saturations',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('low vitamin d',comorb$cfh_comorbid_disease_diagnosis), 'vit_d_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^lower respiratory',comorb$cfh_comorbid_disease_diagnosis), 'pneumonia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^malignant neoplasm of breast',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^malignant neoplasm of the right breast',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mennhorhagia',comorb$cfh_comorbid_disease_diagnosis), 'menorrhagia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^migrain',comorb$cfh_comorbid_disease_diagnosis), 'migraine', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^migrain',comorb$cfh_comorbid_disease_diagnosis), 'migraine', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^obstructive sleep',comorb$cfh_comorbid_disease_diagnosis), 'osas', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^osa',comorb$cfh_comorbid_disease_diagnosis), 'osas', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^oseoarthritis',comorb$cfh_comorbid_disease_diagnosis), 'osteoarthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^osteoarthritis',comorb$cfh_comorbid_disease_diagnosis), 'osteoarthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^osteoarthritus',comorb$cfh_comorbid_disease_diagnosis), 'osteoarthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^other forms of systemic lupus erythematosus',comorb$cfh_comorbid_disease_diagnosis), 'sle', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^out of hospital arrest',comorb$cfh_comorbid_disease_diagnosis), 'cardiac_arrest', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^paroxysmal af',comorb$cfh_comorbid_disease_diagnosis), 'afib', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^paroxysmal atrial fibrillation',comorb$cfh_comorbid_disease_diagnosis), 'afib', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^paroxysmal atrial flutter',comorb$cfh_comorbid_disease_diagnosis), 'atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^patent',comorb$cfh_comorbid_disease_diagnosis), 'pfo', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^peripheral vasc',comorb$cfh_comorbid_disease_diagnosis), 'peripheral_vascular_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^peritonitis with appendix abscess',comorb$cfh_comorbid_disease_diagnosis), 'appendicitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('malignant neoplasm of breast',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^plaque in left coronary artery',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('pneumonia',comorb$cfh_comorbid_disease_diagnosis), 'pneumonia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('deep vein thromobosis',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('gout',comorb$cfh_comorbid_disease_diagnosis), 'gout', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('graves',comorb$cfh_comorbid_disease_diagnosis), 'graves_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^prior hyperthyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hyperthyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^psoraisis',comorb$cfh_comorbid_disease_diagnosis), 'psoriasis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^prior hyperthyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hyperthyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^psoriasis arthopathy',comorb$cfh_comorbid_disease_diagnosis), 'psoriatic arthropathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^psoriatic arthritis',comorb$cfh_comorbid_disease_diagnosis), 'psoriatic arthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary embolisim',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary emboli',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary emboli',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^raynaud',comorb$cfh_comorbid_disease_diagnosis), 'raynauds_phenomenon', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cardiac arrest',comorb$cfh_comorbid_disease_diagnosis), 'cardiac_arrest', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic ischaemic heart disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic obstructive sleep apnoea',comorb$cfh_comorbid_disease_diagnosis), 'osas', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^coronary artery disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cough-variant asthma',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cough variant asthma',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dcis right breast',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetic (type ii)',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^enlarged pulmonary artery',comorb$cfh_comorbid_disease_diagnosis), 'enlarged_pulmonary_artery', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gord',comorb$cfh_comorbid_disease_diagnosis), 'GERD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^generalised osteoarthritis',comorb$cfh_comorbid_disease_diagnosis), 'osteoarthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^intermittent claudication',comorb$cfh_comorbid_disease_diagnosis), 'peripheral_vascular_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left invasive breast carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('hickman',comorb$cfh_comorbid_disease_diagnosis), 'line_infection_hickman', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^myositis',comorb$cfh_comorbid_disease_diagnosis), 'myositis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^myocarditis',comorb$cfh_comorbid_disease_diagnosis), 'myocarditis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non insulin dependent diabetes type 2',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non-hodgkins lymphoma',comorb$cfh_comorbid_disease_diagnosis), 'non_hodgkins_lymphoma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^obesity-related hypoventilation',comorb$cfh_comorbid_disease_diagnosis), 'obesity hypoventilation syndrome', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ovarian mycinous cyst',comorb$cfh_comorbid_disease_diagnosis), 'ovarian cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^palpitation',comorb$cfh_comorbid_disease_diagnosis), 'palpitations', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('supraventricular tachycardia',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('svt',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous alcohol excess',comorb$cfh_comorbid_disease_diagnosis), 'previous alcohol history', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous ashthma diagnosis',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous coronary artery bypass graft x3',comorb$cfh_comorbid_disease_diagnosis), 'CABG', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous deep vein thrombosis nos',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous dvt',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous gastrointestinal bleed on warfarin',comorb$cfh_comorbid_disease_diagnosis), 'gi bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('previous haemoptysis',comorb$cfh_comorbid_disease_diagnosis), 'haemoptysis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('previous hyperthyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hyperthyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('previous iron deficiency anaemia',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('previous iron-deficiency anaemia',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('previous mi',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('previous pericarditis',comorb$cfh_comorbid_disease_diagnosis), 'pericarditis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('previous pericarditis',comorb$cfh_comorbid_disease_diagnosis), 'pericarditis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('previous pulmonary embolism',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('previous svts',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('transient ischaemic attack',comorb$cfh_comorbid_disease_diagnosis), 'tia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('primary hypertension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('prior depression',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('raised body mass index (509)',comorb$cfh_comorbid_disease_diagnosis), 'raised body mass index', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('recurrent palpitations',comorb$cfh_comorbid_disease_diagnosis), 'palpitations', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('recurrent syncope',comorb$cfh_comorbid_disease_diagnosis), 'syncope', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('renal failure, stage 3',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('ckd',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('renal impairment',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('rheumatoid arthritis - on prednisolone',comorb$cfh_comorbid_disease_diagnosis), 'rheumatoid arthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('right sided pe',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('sciatic',comorb$cfh_comorbid_disease_diagnosis), 'sciatica', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('secundum atrial septal defect',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^seizure',comorb$cfh_comorbid_disease_diagnosis), 'seizures', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^septicaemia ',comorb$cfh_comorbid_disease_diagnosis), 'sepsis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^seizure',comorb$cfh_comorbid_disease_diagnosis), 'seizures', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('rheumatoid arthritis',comorb$cfh_comorbid_disease_diagnosis), 'rheumatoid arthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('inflammatory arthritis',comorb$cfh_comorbid_disease_diagnosis), 'inflammatory arthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('severe emphysema',comorb$cfh_comorbid_disease_diagnosis), 'emphysema', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^severe osteoarthritis',comorb$cfh_comorbid_disease_diagnosis), 'osteoarthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^severe reflux',comorb$cfh_comorbid_disease_diagnosis), 'GERD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^severe vitamin d deficiency leading to hypoca',comorb$cfh_comorbid_disease_diagnosis), 'vit_d_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sicca syndrome',comorb$cfh_comorbid_disease_diagnosis), 'dry eyes', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dry eye syndrome',comorb$cfh_comorbid_disease_diagnosis), 'dry eyes', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^small asd',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^small to moderate asd',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^systemic hypertention',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^systemic hypertension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^symtomatic gall stones',comorb$cfh_comorbid_disease_diagnosis), 'gallstone', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^t2dm',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^atherosclerotic heart disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^athritis',comorb$cfh_comorbid_disease_diagnosis), 'arthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^autoimmune hypothyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^av nodal re-entry tachycardia',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bicornate uterus',comorb$cfh_comorbid_disease_diagnosis), 'bicornuate uterus', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral breast prosthesis',comorb$cfh_comorbid_disease_diagnosis), 'breast_prosthesis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral augmentation mammoplasty',comorb$cfh_comorbid_disease_diagnosis), 'breast_prosthesis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral cataract',comorb$cfh_comorbid_disease_diagnosis), 'cataract', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral sub capsular cataracts',comorb$cfh_comorbid_disease_diagnosis), 'cataract', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral pulmonary embolism',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^breast cancer',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bronchial asthma',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cerebral bleed april 2011',comorb$cfh_comorbid_disease_diagnosis), 'brain_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^colectomy for ulcerative colitis',comorb$cfh_comorbid_disease_diagnosis), 'ulcerative_colitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^community acquired pnuemonia',comorb$cfh_comorbid_disease_diagnosis), 'pneumonia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^concomitant bilateral basal pulmonary fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary_fibrosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('diabetic (type ii)',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^e coli neutropenic sepsis',comorb$cfh_comorbid_disease_diagnosis), 'sepsis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ecg shows first degree heart block and right bundle branch',comorb$cfh_comorbid_disease_diagnosis), '1st_degree_heart_block & rbbb', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^exertional syncope',comorb$cfh_comorbid_disease_diagnosis), 'syncope', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^facet joint arthritis',comorb$cfh_comorbid_disease_diagnosis), 'arthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^first degree heart block',comorb$cfh_comorbid_disease_diagnosis), '1st_degree_heart_block', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gastric erosions',comorb$cfh_comorbid_disease_diagnosis), 'gastric ulcer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hemorrhoids',comorb$cfh_comorbid_disease_diagnosis), 'haemorrhoids', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hyperlipidemia',comorb$cfh_comorbid_disease_diagnosis), 'dyslipidaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypochronic microcyctic anaemia',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hysterectomy',comorb$cfh_comorbid_disease_diagnosis), 'hysterectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^interstitial pulmonary fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary_fibrosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^iritable bowel disease',comorb$cfh_comorbid_disease_diagnosis), 'ibs', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left knee',comorb$cfh_comorbid_disease_diagnosis), 'knee_surgery', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^native vessel pci to the right coronary artery and circumflex',comorb$cfh_comorbid_disease_diagnosis), 'PCI', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non dependent alcohol abuse',comorb$cfh_comorbid_disease_diagnosis), 'previous alcohol history', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pacemaker insertion for sinus pauses',comorb$cfh_comorbid_disease_diagnosis), 'pacemaker', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pacemaker insertion may 2013 for symptomatic pauses',comorb$cfh_comorbid_disease_diagnosis), 'pacemaker', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^permanent pacemaker in 2011 for complete heart block',comorb$cfh_comorbid_disease_diagnosis), 'pacemaker', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^polycystic ovary syndrome',comorb$cfh_comorbid_disease_diagnosis), 'PCOS', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^past polycystic ovarian disease',comorb$cfh_comorbid_disease_diagnosis), 'PCOS', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^post operative pulmonary embolism (cabg x3)',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous acute pulmonary embolism',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous ablation for atrial flutter',comorb$cfh_comorbid_disease_diagnosis), 'atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous heavy alcohol intake with liver disease',comorb$cfh_comorbid_disease_diagnosis), 'alcoholic liver disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent cardiac arrhythmias (atrial fibrillation and flutter)',comorb$cfh_comorbid_disease_diagnosis), 'afib & atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent cystitis',comorb$cfh_comorbid_disease_diagnosis), 'uti', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent epistaxis',comorb$cfh_comorbid_disease_diagnosis), 'epistaxis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent urinary tract infection',comorb$cfh_comorbid_disease_diagnosis), 'uti', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^renal cyst',comorb$cfh_comorbid_disease_diagnosis), 'renal_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^complex bosniak 2f cyst left kidney',comorb$cfh_comorbid_disease_diagnosis), 'renal_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^1st degree heart block',comorb$cfh_comorbid_disease_diagnosis), '1st_degree_heart_block', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vitamin d deficiency',comorb$cfh_comorbid_disease_diagnosis), 'vit_d_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^b12 deficiency',comorb$cfh_comorbid_disease_diagnosis), 'vit_b12_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vitamin b12 deficiency',comorb$cfh_comorbid_disease_diagnosis), 'vit_b12_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('fe deficiency anaemia',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pernicious anaemia',comorb$cfh_comorbid_disease_diagnosis), 'b12_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^folate deficiency anaemia',comorb$cfh_comorbid_disease_diagnosis), 'b12_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vitamin b12 deficiency anaemia',comorb$cfh_comorbid_disease_diagnosis), 'b12_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^microcytic hypochromoic anaemia',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^persistent microcytic anaemia',comorb$cfh_comorbid_disease_diagnosis), 'microcytic anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ubarachnoid haemorrhage and anterior communicating',comorb$cfh_comorbid_disease_diagnosis), 'brain_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sub clinical hypothyroidism',comorb$cfh_comorbid_disease_diagnosis), 'subclinical hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acquired hypothyroid disease',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sero negative arthritis',comorb$cfh_comorbid_disease_diagnosis), 'arthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('raised body mass index',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^morbide obesity',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^morbid obesity',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^obesity with prior gastric stapling',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^raised body mass index',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^body mass index - 42',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^raised body mass index (509)',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild pulmonary fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary_fibrosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild hay fever',comorb$cfh_comorbid_disease_diagnosis), 'hayfever', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild aortic stenosis',comorb$cfh_comorbid_disease_diagnosis), 'aortic_stenosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^lymphoedema',comorb$cfh_comorbid_disease_diagnosis), 'lymphoedema', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^irritible bowel syndrome',comorb$cfh_comorbid_disease_diagnosis), 'ibs', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hyperthyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hyperthyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^high alcohol intake',comorb$cfh_comorbid_disease_diagnosis), 'previous alcohol history', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hfe haemochromatosis',comorb$cfh_comorbid_disease_diagnosis), 'haemochromatosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hepatitis b infection as a child',comorb$cfh_comorbid_disease_diagnosis), 'hepatitis b', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gastroesophageal reflux disease',comorb$cfh_comorbid_disease_diagnosis), 'GERD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^peptic ulcer disease',comorb$cfh_comorbid_disease_diagnosis), 'gastric ulcer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild emphysema',comorb$cfh_comorbid_disease_diagnosis), 'emphysema', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^first degree heart attack',comorb$cfh_comorbid_disease_diagnosis), '1st_degree_heart_block', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dvt',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^deep venous thrombosis',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cholecystectomy 1991',comorb$cfh_comorbid_disease_diagnosis), 'cholecystectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^breast carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vitamin d deficience',comorb$cfh_comorbid_disease_diagnosis), 'vit_d_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vitamin d defficiency',comorb$cfh_comorbid_disease_diagnosis), 'vit_d_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vit d deficiency',comorb$cfh_comorbid_disease_diagnosis), 'vit_d_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vitamin b12 deficiency',comorb$cfh_comorbid_disease_diagnosis), 'vit_b12_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vasovagal syncope',comorb$cfh_comorbid_disease_diagnosis), 'syncope', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^uterine fibroid',comorb$cfh_comorbid_disease_diagnosis), 'uterine fibroids', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^urinary tract infection',comorb$cfh_comorbid_disease_diagnosis), 'uti', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ulcerative colitis',comorb$cfh_comorbid_disease_diagnosis), 'ulcerative_colitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type 3 ehlers danlos',comorb$cfh_comorbid_disease_diagnosis), 'ehlers danlos', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type 2 dm',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type 1 diabetes',comorb$cfh_comorbid_disease_diagnosis), 'DM1', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^triple vessel coronary disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^transient cerebral ischaemic attack',comorb$cfh_comorbid_disease_diagnosis), 'tia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^transient ischemic attack',comorb$cfh_comorbid_disease_diagnosis), 'tia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('hip replacement',comorb$cfh_comorbid_disease_diagnosis), 'hip_replacement', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('tonsillectomy',comorb$cfh_comorbid_disease_diagnosis), 'tonsillectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thymic enlargement',comorb$cfh_comorbid_disease_diagnosis), 'thymic_enlargement', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thrombocytopoenia',comorb$cfh_comorbid_disease_diagnosis), 'thrombocytopenia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^respiratory failure type i',comorb$cfh_comorbid_disease_diagnosis), 'type 1 respiratory failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous wenkebach phenomenon',comorb$cfh_comorbid_disease_diagnosis), '2nd_degree_heart_block', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^moderate obstructive sleep apnoea',comorb$cfh_comorbid_disease_diagnosis), 'osas', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^moderate aortic stenosis',comorb$cfh_comorbid_disease_diagnosis), 'aortic_stenosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild to moderate copd',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild to moderate depressions',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^minor coronary artery disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild coronary artery disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^single vessel coronary heart disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild intermittent asthma',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild asthma',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild bronchiectasis',comorb$cfh_comorbid_disease_diagnosis), 'bronchiectasis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild centrilobular emphysema',comorb$cfh_comorbid_disease_diagnosis), 'emphysema', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^major haemoptesis',comorb$cfh_comorbid_disease_diagnosis), 'haemoptysis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^macular degeneration',comorb$cfh_comorbid_disease_diagnosis), 'macular degeneration', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('lung nodul',comorb$cfh_comorbid_disease_diagnosis), 'lung nodule', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^lupus syndrome',comorb$cfh_comorbid_disease_diagnosis), 'sle', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('diastolic dysfunction',comorb$cfh_comorbid_disease_diagnosis), 'diastolic dysfunction_LV', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('systolic',comorb$cfh_comorbid_disease_diagnosis), 'systolic dysfunction_LV', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('goitre',comorb$cfh_comorbid_disease_diagnosis), 'goiter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('colonic polyp',comorb$cfh_comorbid_disease_diagnosis), 'colonic polyp', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('anti-cardiolipin antibody positive',comorb$cfh_comorbid_disease_diagnosis), 'anticardiolipin antibody positive', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('aortic stenosis',comorb$cfh_comorbid_disease_diagnosis), 'aortic_stenosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('aortic valve stenosis',comorb$cfh_comorbid_disease_diagnosis), 'aortic_stenosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('inguinal hernia',comorb$cfh_comorbid_disease_diagnosis), 'inguinal hernia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^borderline folate deficiency',comorb$cfh_comorbid_disease_diagnosis), 'vit_b12_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('back pain',comorb$cfh_comorbid_disease_diagnosis), 'back pain', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('complete af block',comorb$cfh_comorbid_disease_diagnosis), 'complete_heart_block', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('focal fatty infiltration of liver',comorb$cfh_comorbid_disease_diagnosis), 'fatty liver', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('non-alcoholic steatohepatitis',comorb$cfh_comorbid_disease_diagnosis), 'fatty liver', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('henoch(-schönlein) purpura',comorb$cfh_comorbid_disease_diagnosis), 'IgA_vasculitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('massive pulmonary emboli',comorb$cfh_comorbid_disease_diagnosis), 'massive pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('mild copd',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('mild dystolic dysfunction',comorb$cfh_comorbid_disease_diagnosis), 'diastolic dysfunction_LV', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('mild diastolic left ventricular dysfunction type 1',comorb$cfh_comorbid_disease_diagnosis), 'diastolic dysfunction_LV', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('mild hypercholesterolaemia',comorb$cfh_comorbid_disease_diagnosis), 'dyslipidaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('mild obstructive sleep apnoea',comorb$cfh_comorbid_disease_diagnosis), 'osas', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('mild thrombocytopenia',comorb$cfh_comorbid_disease_diagnosis), 'thrombocytopenia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('nocturnal epilepsy',comorb$cfh_comorbid_disease_diagnosis), 'epilepsy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non-hodgkinson',comorb$cfh_comorbid_disease_diagnosis), 'non_hodgkins_lymphoma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^post operative pulmonary embolism',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^massive pulmonary embolism',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous alcohol history',comorb$cfh_comorbid_disease_diagnosis), 'alcohol abuse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^angina',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous alcohol history',comorb$cfh_comorbid_disease_diagnosis), 'alcohol abuse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous alcohol history',comorb$cfh_comorbid_disease_diagnosis), 'alcohol abuse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous alcohol history',comorb$cfh_comorbid_disease_diagnosis), 'alcohol abuse', comorb$cfh_comorbid_disease_diagnosis)
#for some reason diabetic typeii isn't registered properly!
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dual chamber pacemaker inserted',comorb$cfh_comorbid_disease_diagnosis), 'pacemaker', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^henoch',comorb$cfh_comorbid_disease_diagnosis), 'IgA_vasculitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non insulin dependent diabetic',comorb$cfh_comorbid_disease_diagnosis), 'diabetes mellitus', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type 2 diabetes',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type 2 diabetes mellitus',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type ii diabetes',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type ii diabetes mellitus',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type 2 diabetes',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type 2 diabetes mellitus',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type ii diabetes',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type i diabetes mellitus',comorb$cfh_comorbid_disease_diagnosis), 'DM1', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ostium secundum',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute-on-chronic bilateral subdural haemorrhage',comorb$cfh_comorbid_disease_diagnosis), 'brain_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral chronic subdural haematoma',comorb$cfh_comorbid_disease_diagnosis), 'brain_bleed', comorb$cfh_comorbid_disease_diagnosis)

#now go back to the original dataframe & split double diagnoses in column to new row
#first remove any double diagnoses
comorb <- unique(comorb)
#some double diagnoses were removed
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('recurrent cardiac arrhythmias (atrial fibrillation and flutter)',comorb$cfh_comorbid_disease_diagnosis), 'afib & atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent ocular hypertension and cataracts right eye)',comorb$cfh_comorbid_disease_diagnosis), 'cataract & occular hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^subarachnoid haemorrhage and anterior communicating arter)',comorb$cfh_comorbid_disease_diagnosis), 'brain_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pfo/small asd',comorb$cfh_comorbid_disease_diagnosis), 'pfo & asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^atrial tachycardia',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('pfo',comorb$cfh_comorbid_disease_diagnosis), 'pfo', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^me since aged fourteen',comorb$cfh_comorbid_disease_diagnosis), 'myalgic encephalomyelitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic lung disease: emphysema and fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary_fibrosis & emphysema', comorb$cfh_comorbid_disease_diagnosis)

#also change emphysema to COPD and fibrosis to ILD (based on lab meeting 19-10-23 discussion)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^emphysema',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary_fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb <- unique(comorb)

abc <- as.data.frame(unique(comorb$cfh_comorbid_disease_diagnosis))

#continue per variable to rename!
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('narrow complex tachycardia',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('a flutter ablatted)',comorb$cfh_comorbid_disease_diagnosis), 'atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^abdominal aneurysm',comorb$cfh_comorbid_disease_diagnosis), 'abdominal_aortic_aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^abdominal aortic aneurysm',comorb$cfh_comorbid_disease_diagnosis), 'abdominal_aortic_aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute and old myocardial infarction',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute coronary sydrome',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute coronary syndrome',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute hepatitis in 20',comorb$cfh_comorbid_disease_diagnosis), 'hepatitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute kidney injury',comorb$cfh_comorbid_disease_diagnosis), 'AKI', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('acute liver failure (secondary to hypoxic injury)',comorb$cfh_comorbid_disease_diagnosis), 'acute liver failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute myocardial infarction',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute pancreatitis',comorb$cfh_comorbid_disease_diagnosis), 'pancreatitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute pyelonephritis',comorb$cfh_comorbid_disease_diagnosis), 'pyelonephritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute renal failure',comorb$cfh_comorbid_disease_diagnosis), 'AKI', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute respiratory distress syndrome',comorb$cfh_comorbid_disease_diagnosis), 'ards', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acvrl1-hht',comorb$cfh_comorbid_disease_diagnosis), 'hereditary_haemorrhagic_telangiectasia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute myeloid leukaemia',comorb$cfh_comorbid_disease_diagnosis), 'AML', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^all',comorb$cfh_comorbid_disease_diagnosis), 'ALL', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^adenocarcinoma of prostate',comorb$cfh_comorbid_disease_diagnosis), 'prostateCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^adenocarcinoma of the bowel',comorb$cfh_comorbid_disease_diagnosis), 'bowelCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^alcohol use',comorb$cfh_comorbid_disease_diagnosis), 'alcohol abuse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^alcoholic cirrhosis of liver',comorb$cfh_comorbid_disease_diagnosis), 'alcoholic liver disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^algodystrophy of left knee',comorb$cfh_comorbid_disease_diagnosis), 'algodystrophy of left knee', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('allergic rhinitis (pollens)',comorb$cfh_comorbid_disease_diagnosis), 'allergic rhinitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^alpha-1-antitrypsin deficiency',comorb$cfh_comorbid_disease_diagnosis), 'alpha 1 antitrypsin deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^amaurosis fugax',comorb$cfh_comorbid_disease_diagnosis), 'tia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^amiodarone induced thyrotoxicosis',comorb$cfh_comorbid_disease_diagnosis), 'thyrotoxicosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^amiodarone induced hypothyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ana positive',comorb$cfh_comorbid_disease_diagnosis), 'ANA_positivity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^aneurism of ascending aorta and aortic root',comorb$cfh_comorbid_disease_diagnosis), 'type_A_aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^aneurysmal dilatation of proximal pulmonary arteries with compression of left main bronchus',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary_artery_aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^angiodysplasia',comorb$cfh_comorbid_disease_diagnosis), 'angiodysplasia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ankle inury ? stress fracture',comorb$cfh_comorbid_disease_diagnosis), 'ankle_fracture', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ankle fracture',comorb$cfh_comorbid_disease_diagnosis), 'ankle_fracture', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^anal carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'analCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^anterior resection for rectal carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'rectalCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^anterior sub endocardial myocardial infarction',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^anti-ro positive, ana weakly positive',comorb$cfh_comorbid_disease_diagnosis), 'anti_Rho_positivity & ANA_positivity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^anxiety and depression',comorb$cfh_comorbid_disease_diagnosis), 'anxiety & depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^anxiety disorder, unspecified',comorb$cfh_comorbid_disease_diagnosis), 'anxiety', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^anxiety states',comorb$cfh_comorbid_disease_diagnosis), 'anxiety', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^anxiety with depression',comorb$cfh_comorbid_disease_diagnosis), 'anxiety & depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^aortic insufficiency > stage b',comorb$cfh_comorbid_disease_diagnosis), 'aortic regurgitation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^apendectomy',comorb$cfh_comorbid_disease_diagnosis), 'appendicitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^arterial hypertension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^asbestos related pleural parenchymal lung disease',comorb$cfh_comorbid_disease_diagnosis), 'asbestos related pleural parenchymal lung disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^asperger',comorb$cfh_comorbid_disease_diagnosis), 'autism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ast closure 2002 with subsequent episode of cerebral hypoxia',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^astma',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^asymmetrical inflammatory polyarthritis',comorb$cfh_comorbid_disease_diagnosis), 'inflammatory arthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^atopic dermatitis',comorb$cfh_comorbid_disease_diagnosis), 'atopic dermatitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^autistic disorder',comorb$cfh_comorbid_disease_diagnosis), 'autism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^auto-immune thyroid failure',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^autoimmune thyroiditis',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^autoimmune haemolytic anaemia',comorb$cfh_comorbid_disease_diagnosis), 'autoimmune haemolytic anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^av nodal reenterant tachycardia',comorb$cfh_comorbid_disease_diagnosis), 'avnrt', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^av reentrant tachycardia',comorb$cfh_comorbid_disease_diagnosis), 'avrt', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^av block',comorb$cfh_comorbid_disease_diagnosis), 'heart_block', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^auxilliary vein thrombosis',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bell',comorb$cfh_comorbid_disease_diagnosis), 'bells_palsy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^benign hypertrophy of prostate',comorb$cfh_comorbid_disease_diagnosis), 'BPH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^benign prostatic enlargment',comorb$cfh_comorbid_disease_diagnosis), 'BPH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^benign prostatic hyperplasia',comorb$cfh_comorbid_disease_diagnosis), 'BPH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^benign prostatic hypertrophy',comorb$cfh_comorbid_disease_diagnosis), 'BPH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^beta thalassemia trait',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral branch occlusion',comorb$cfh_comorbid_disease_diagnosis), 'BRVO', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral diabetic retinopathy',comorb$cfh_comorbid_disease_diagnosis), 'diabetic_retinopathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral kidney stones complicated by urea sepsis and decr',comorb$cfh_comorbid_disease_diagnosis), 'AKI', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('knee replacement',comorb$cfh_comorbid_disease_diagnosis), 'knee_surgery', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^knee surgery',comorb$cfh_comorbid_disease_diagnosis), 'knee_surgery', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral leg ulcers',comorb$cfh_comorbid_disease_diagnosis), 'leg ulcers', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral lipodermatosclerosis',comorb$cfh_comorbid_disease_diagnosis), 'lipodermatosclerosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral lung lung transplant',comorb$cfh_comorbid_disease_diagnosis), 'LTx', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral lower limb cellulitis- treated with iv antibiotics',comorb$cfh_comorbid_disease_diagnosis), 'cellulitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral pleural effusions',comorb$cfh_comorbid_disease_diagnosis), 'pleural_effusion', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral plueral effusions',comorb$cfh_comorbid_disease_diagnosis), 'pleural_effusion', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral pulmonary nodules',comorb$cfh_comorbid_disease_diagnosis), 'lung nodule', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral renal cysts and anuric renal failure as neonate',comorb$cfh_comorbid_disease_diagnosis), 'renal_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral renal artery stenosis',comorb$cfh_comorbid_disease_diagnosis), 'renal_artery_stenosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bilateral svcs with left svc to dilated coronary sinus',comorb$cfh_comorbid_disease_diagnosis), 'superior_vena_cava_syndrome', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bio-prostatic mitral valve replacement',comorb$cfh_comorbid_disease_diagnosis), 'mitral valve replacement', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bladder spasm requiring botox injections',comorb$cfh_comorbid_disease_diagnosis), 'bladder dysfunction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bliateral chronic leg ulcers',comorb$cfh_comorbid_disease_diagnosis), 'leg ulcers', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^blind in left eye',comorb$cfh_comorbid_disease_diagnosis), 'partial_blindness', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^blind in right eye',comorb$cfh_comorbid_disease_diagnosis), 'partial_blindness', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^blindness in right eye',comorb$cfh_comorbid_disease_diagnosis), 'partial_blindness', comorb$cfh_comorbid_disease_diagnosis)
#subclinical hypothyroidism defined as elevated TSH, but not too low fT4
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^borderline elevated tsh',comorb$cfh_comorbid_disease_diagnosis), 'subclinical hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^borderline lad and circumflex disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^borderline thrombocytopenia',comorb$cfh_comorbid_disease_diagnosis), 'thrombocytopenia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^borderline splenomegaly',comorb$cfh_comorbid_disease_diagnosis), 'splenomegaly', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^brief paroxysmal atrial tachyarrythmia',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^bosniak cyst, right upper pole renal lesion',comorb$cfh_comorbid_disease_diagnosis), 'renal_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('bowel polp (removed)',comorb$cfh_comorbid_disease_diagnosis), 'bowel polyp', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ca oesphagus - for radiotherapy',comorb$cfh_comorbid_disease_diagnosis), 'oesophagusCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^caesarean section',comorb$cfh_comorbid_disease_diagnosis), 'caesarean section', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^carcinoma of bowel',comorb$cfh_comorbid_disease_diagnosis), 'bowelCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^carcinoma of prostate',comorb$cfh_comorbid_disease_diagnosis), 'prostateCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^carcinoma of the colon',comorb$cfh_comorbid_disease_diagnosis), 'bowelCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^carcinoma of bladder',comorb$cfh_comorbid_disease_diagnosis), 'bladderCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('carpal tunnel',comorb$cfh_comorbid_disease_diagnosis), 'CTS', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cavotricuspid isthmus ablation',comorb$cfh_comorbid_disease_diagnosis), 'atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cellulitis',comorb$cfh_comorbid_disease_diagnosis), 'cellulitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^central hypertension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^central obesity',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^central retinal vein occlusion 2011',comorb$cfh_comorbid_disease_diagnosis), 'CRVO', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('central serious retinopathy (secondary to pph)',comorb$cfh_comorbid_disease_diagnosis), 'retinopathy_secondary_to_PPH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^centrilobular emphysema',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^centrilobular nodularity',comorb$cfh_comorbid_disease_diagnosis), 'lung nodule', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary_fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cerebral haemorrhage',comorb$cfh_comorbid_disease_diagnosis), 'brain_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cerebral vascular accident',comorb$cfh_comorbid_disease_diagnosis), 'cva', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cerebrovascular accident',comorb$cfh_comorbid_disease_diagnosis), 'cva', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cerebral small vessel disease',comorb$cfh_comorbid_disease_diagnosis), 'cerebrovascular disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cervical ca',comorb$cfh_comorbid_disease_diagnosis), 'cervicalCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cervical cancer',comorb$cfh_comorbid_disease_diagnosis), 'cervicalCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cervical carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'cervicalCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cervical spon',comorb$cfh_comorbid_disease_diagnosis), 'cervical spondylosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^childhood asthma',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chirrhosis and chronic liver disease',comorb$cfh_comorbid_disease_diagnosis), 'liver_chirrhosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cholecystecomy',comorb$cfh_comorbid_disease_diagnosis), 'cholecystectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic active hepatitis',comorb$cfh_comorbid_disease_diagnosis), 'hepatitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic af',comorb$cfh_comorbid_disease_diagnosis), 'afib', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic anaemia',comorb$cfh_comorbid_disease_diagnosis), 'anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic asthma',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic back and hip pain',comorb$cfh_comorbid_disease_diagnosis), 'back pain & hip pain', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic bilateral cellulitis with marked peripheral oedema',comorb$cfh_comorbid_disease_diagnosis), 'cellulitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic cholecystitis',comorb$cfh_comorbid_disease_diagnosis), 'cholecystitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic haemoptysis',comorb$cfh_comorbid_disease_diagnosis), 'haemoptysis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic heart disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic hyperbilirubinaemia',comorb$cfh_comorbid_disease_diagnosis), 'hyperbilirubinaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic hypoxia with secondary polycythaemia',comorb$cfh_comorbid_disease_diagnosis), 'chronic hypoxia & secondary_polycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic liver disease',comorb$cfh_comorbid_disease_diagnosis), 'liver disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic lymphocytic leukemia',comorb$cfh_comorbid_disease_diagnosis), 'CLL', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic lymphoid lukaemia',comorb$cfh_comorbid_disease_diagnosis), 'CLL', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic obstructive plumonary disease',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic osteoarthritis',comorb$cfh_comorbid_disease_diagnosis), 'osteoarthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic pancreatitis',comorb$cfh_comorbid_disease_diagnosis), 'pancreatitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic pericardial effusion',comorb$cfh_comorbid_disease_diagnosis), 'pericardial_effusion', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic rhinitis',comorb$cfh_comorbid_disease_diagnosis), 'rhinitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic tension headache',comorb$cfh_comorbid_disease_diagnosis), 'tension headache', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic thromboembolic disease single web',comorb$cfh_comorbid_disease_diagnosis), 'CTEPH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic venouse insuficiency',comorb$cfh_comorbid_disease_diagnosis), 'chronic venous insufficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic viral hepatitis b',comorb$cfh_comorbid_disease_diagnosis), 'hepatitis b', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^chronic viral hepatitis c',comorb$cfh_comorbid_disease_diagnosis), 'hepatitis c', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('churash ploycythaemia (von hippel-lindau mutation)',comorb$cfh_comorbid_disease_diagnosis), 'ploycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cirrhotic liver disease',comorb$cfh_comorbid_disease_diagnosis), 'liver_chirrhosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^tbx4',comorb$cfh_comorbid_disease_diagnosis), 'TBX4_mutation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^BMPR2',comorb$cfh_comorbid_disease_diagnosis), 'BMPR2_mutation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('clotting disorder (antiphospholipid syndrome, apc resistance)',comorb$cfh_comorbid_disease_diagnosis), 'antiphospholipid_syndrome', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^colon carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'bowelCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^colonic polyp',comorb$cfh_comorbid_disease_diagnosis), 'colon polyp', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^colorectal adenocarcinoma',comorb$cfh_comorbid_disease_diagnosis), 'bowelCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^colorectal cancer',comorb$cfh_comorbid_disease_diagnosis), 'bowelCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^congestive cardiac failure',comorb$cfh_comorbid_disease_diagnosis), 'congestive_heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^congestive heart failure',comorb$cfh_comorbid_disease_diagnosis), 'congestive_heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^coronary artery calcification',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^coronary artery occlusion',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^covid',comorb$cfh_comorbid_disease_diagnosis), 'COVID_19', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^corona',comorb$cfh_comorbid_disease_diagnosis), 'COVID_19', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^crohn',comorb$cfh_comorbid_disease_diagnosis), 'crohns_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cryptogenic fibrosing alveolitis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ct thorax suggestive of a pulmonary vasculopathy rather',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary vasculopathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cutaneous sarcoidosis',comorb$cfh_comorbid_disease_diagnosis), 'sarcoidosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cyanosis with poylcythehaemia',comorb$cfh_comorbid_disease_diagnosis), 'poylcythehaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cytomegalovirus',comorb$cfh_comorbid_disease_diagnosis), 'cytomegalovirus', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cmv',comorb$cfh_comorbid_disease_diagnosis), 'cytomegalovirus', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('quervain',comorb$cfh_comorbid_disease_diagnosis), 'de_quervain_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^cystic liver disease',comorb$cfh_comorbid_disease_diagnosis), 'liver_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('tbx4',comorb$cfh_comorbid_disease_diagnosis), 'TBX4_mutation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^decompensated right heart failure',comorb$cfh_comorbid_disease_diagnosis), 'right_heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^delayed neurogenic syncope',comorb$cfh_comorbid_disease_diagnosis), 'syncope', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^deletion in chromosome 17',comorb$cfh_comorbid_disease_diagnosis), 'chromosome_17q_variant', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^depressive',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^deranged liver function',comorb$cfh_comorbid_disease_diagnosis), 'liver impairment', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabete mellitus type 2',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('diabetes (not stated which type)',comorb$cfh_comorbid_disease_diagnosis), 'diabetes', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes mellitus type ii',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes secondary to pancreatic insufficiency',comorb$cfh_comorbid_disease_diagnosis), 'diabetes', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes with neuropathy',comorb$cfh_comorbid_disease_diagnosis), 'diabetes & neuropathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes with retinopathy',comorb$cfh_comorbid_disease_diagnosis), 'diabetes & diabetic_retinopathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('diabetic (type ii)',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetic retinopathy',comorb$cfh_comorbid_disease_diagnosis), 'diabetic_retinopathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetis type 2',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dialysis-dependent renal insufficiency',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diffuse parenchymal lung disease',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dilated ascending aorta',comorb$cfh_comorbid_disease_diagnosis), 'type_A_aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ischemic dilated cardiomyopathy',comorb$cfh_comorbid_disease_diagnosis), 'dilated cardiomyopathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^idiopathic dilated cardiomyopathy',comorb$cfh_comorbid_disease_diagnosis), 'dilated cardiomyopathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non-ischaemic cardiomyopathy',comorb$cfh_comorbid_disease_diagnosis), 'cardiomyopathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^17q',comorb$cfh_comorbid_disease_diagnosis), 'chromosome_17q_variant', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^discoid eczema',comorb$cfh_comorbid_disease_diagnosis), 'eczema', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^disorder resulting from impaired renal tubular function',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diverted diverticulitis',comorb$cfh_comorbid_disease_diagnosis), 'diverticulitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^drug addiction',comorb$cfh_comorbid_disease_diagnosis), 'drug abuse', comorb$cfh_comorbid_disease_diagnosis)
#EdB: retain drug induced liver injury
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^drug induced liver injury',comorb$cfh_comorbid_disease_diagnosis), 'drug induced liver injury', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dysrrythmia',comorb$cfh_comorbid_disease_diagnosis), 'palpitations', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^dual chamber pacemaker',comorb$cfh_comorbid_disease_diagnosis), 'pacemaker', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ectopic palpitations',comorb$cfh_comorbid_disease_diagnosis), 'palpitations', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^eisenmenger',comorb$cfh_comorbid_disease_diagnosis), 'eisenmenger', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^elective caesarian section',comorb$cfh_comorbid_disease_diagnosis), 'caesarean section', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^elevated body mass index',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^elevated liver enzymes',comorb$cfh_comorbid_disease_diagnosis), 'liver impairment', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^end stage chronic kidney disease',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^end stage renal failure, renal crisis secondary to scleroderma',comorb$cfh_comorbid_disease_diagnosis), 'CKD & scleroderma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^endometrial carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'endometrialCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^endometriosis',comorb$cfh_comorbid_disease_diagnosis), 'endometriosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^essentil hyperpension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^essentil hypertension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^exudative left pleural effusion',comorb$cfh_comorbid_disease_diagnosis), 'pleural_effusion', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^familial hypercholesterolaemia',comorb$cfh_comorbid_disease_diagnosis), 'dyslipidaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^focal segmental glomerulosclerosis',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^folic acid deficiency',comorb$cfh_comorbid_disease_diagnosis), 'vit_b12_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('lymphoma',comorb$cfh_comorbid_disease_diagnosis), 'lymphoma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^g3 infiltrating ductal carcinoma, left breast ',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('g6pd deficiency (family history)',comorb$cfh_comorbid_disease_diagnosis), 'g6pd_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gallstone',comorb$cfh_comorbid_disease_diagnosis), 'cholelithiasis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gastric polyps',comorb$cfh_comorbid_disease_diagnosis), 'gastric polyp', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gastric reflux',comorb$cfh_comorbid_disease_diagnosis), 'GERD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gastric ulceration',comorb$cfh_comorbid_disease_diagnosis), 'gastric ulcer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gastric/duodenal ulcer',comorb$cfh_comorbid_disease_diagnosis), 'gastric ulcer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gastro',comorb$cfh_comorbid_disease_diagnosis), 'GERD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^generalised anxiety disorder',comorb$cfh_comorbid_disease_diagnosis), 'anxiety', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^guttate psoriasis',comorb$cfh_comorbid_disease_diagnosis), 'psoriasis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^haemorrhagic stroke',comorb$cfh_comorbid_disease_diagnosis), 'brain_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hashimoto',comorb$cfh_comorbid_disease_diagnosis), 'hashimoto', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hearing problems',comorb$cfh_comorbid_disease_diagnosis), 'hearing_loss', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^heart and lung transplant',comorb$cfh_comorbid_disease_diagnosis), 'heart_transplant & LTx', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('heart arrhythmias (av block first, second, third degree; sinoatrial block; sick sinus syndrome, ventricular tachycardia; av nodal reentrant tachycardia)',comorb$cfh_comorbid_disease_diagnosis), 'arrhytmia_unspecified', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^heart failure',comorb$cfh_comorbid_disease_diagnosis), 'heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^heartburn',comorb$cfh_comorbid_disease_diagnosis), 'GERD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^heavy alcohol user',comorb$cfh_comorbid_disease_diagnosis), 'alcohol abuse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hepatic hamangioma',comorb$cfh_comorbid_disease_diagnosis), 'hepatic haemangioma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hepatic steatosis',comorb$cfh_comorbid_disease_diagnosis), 'fatty liver', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hepatitis c genotype 2 cirrhosis',comorb$cfh_comorbid_disease_diagnosis), 'hepatitis c', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hereditary hemorrhagic telangiectasia',comorb$cfh_comorbid_disease_diagnosis), 'hereditary_haemorrhagic_telangiectasia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hereditery spherocytosis',comorb$cfh_comorbid_disease_diagnosis), 'hereditary spherocytosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('sox17',comorb$cfh_comorbid_disease_diagnosis), 'SOX17_variant', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('atp13a3',comorb$cfh_comorbid_disease_diagnosis), 'ATP13A3_variant', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('eif2ak4',comorb$cfh_comorbid_disease_diagnosis), 'EIF2AK4_varint', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hiatal hernia with gerd',comorb$cfh_comorbid_disease_diagnosis), 'GERD & hiatus hernia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^high blood pressure',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^high tsh felt to be secondary to his amiodarone',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^history of deliberate self-harm',comorb$cfh_comorbid_disease_diagnosis), 'deliberate self_harm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^history of panic attacks',comorb$cfh_comorbid_disease_diagnosis), 'anxiety', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^horse shoe kidney',comorb$cfh_comorbid_disease_diagnosis), 'horseshoe kidney', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^h-pylori',comorb$cfh_comorbid_disease_diagnosis), 'hpylori infection', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^htn-hypertension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hydronephrosis with pelviureteric junction obstruction',comorb$cfh_comorbid_disease_diagnosis), 'hydronephrosis & pelviureteric junction obstruction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypercalaemia',comorb$cfh_comorbid_disease_diagnosis), 'hypercalcaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypermobile joints',comorb$cfh_comorbid_disease_diagnosis), 'hypermobility', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypermobility syndrome',comorb$cfh_comorbid_disease_diagnosis), 'hypermobility', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hyperplasia of prostate',comorb$cfh_comorbid_disease_diagnosis), 'BPH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypertenstion',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('hypertensive cardiac disease (lv compliance disorder)',comorb$cfh_comorbid_disease_diagnosis), 'hypertensive_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypertensive renal disease',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypertensive retinopapthy',comorb$cfh_comorbid_disease_diagnosis), 'retinopapthy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hyperthyrodism',comorb$cfh_comorbid_disease_diagnosis), 'hyperthyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypertriglyceridemia',comorb$cfh_comorbid_disease_diagnosis), 'dyslipidaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hyperuricemia',comorb$cfh_comorbid_disease_diagnosis), 'hyperuricaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypothroidism',comorb$cfh_comorbid_disease_diagnosis), 'hypothroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^idiopathic pulmonary fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ihd with stents',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^in-situ thrombus',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^in situ pulmonary thrombus',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^increased bmi',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^inferior myocardial infarction',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^influenza',comorb$cfh_comorbid_disease_diagnosis), 'influenza', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^insulin dependant diabetes mellitus',comorb$cfh_comorbid_disease_diagnosis), 'insulin_dependent_dm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^insulin dependent diabetes mellitus',comorb$cfh_comorbid_disease_diagnosis), 'insulin_dependent_dm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^insulin dependent diabetic',comorb$cfh_comorbid_disease_diagnosis), 'insulin_dependent_dm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^interstitial lung fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^liver cirrhosis',comorb$cfh_comorbid_disease_diagnosis), 'liver_chirrhosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^intertstitial lung disease',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^intrapulmonary nodule',comorb$cfh_comorbid_disease_diagnosis), 'lung nodule', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^iron deficient microcytic anaemia',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^irregular menstrual bleeding',comorb$cfh_comorbid_disease_diagnosis), 'metrorrhagia ', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ischaemic stroke',comorb$cfh_comorbid_disease_diagnosis), 'cva', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ischemic stroke',comorb$cfh_comorbid_disease_diagnosis), 'cva', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^isthmus ablation',comorb$cfh_comorbid_disease_diagnosis), 'atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^itp',comorb$cfh_comorbid_disease_diagnosis), 'idiopathic thrombocytopenic purpura', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^iv cocaine user',comorb$cfh_comorbid_disease_diagnosis), 'cocaine_abuse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('iv opiods user (heroin)',comorb$cfh_comorbid_disease_diagnosis), 'heroin_abuse', comorb$cfh_comorbid_disease_diagnosis)
#mutations not clearly filtered, try again
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('bmpr2:smad9',comorb$cfh_comorbid_disease_diagnosis), 'BMPR2_SMAD9_variant', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('bmpr2',comorb$cfh_comorbid_disease_diagnosis), 'BMPR2_variant', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^keratitis caused by dry eyes',comorb$cfh_comorbid_disease_diagnosis), 'dry eyes', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^kyphoscoliosis of thoracic spine',comorb$cfh_comorbid_disease_diagnosis), 'kyphoscoliosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^lacunar infarct',comorb$cfh_comorbid_disease_diagnosis), 'cva', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^langerhans cell histocytosis',comorb$cfh_comorbid_disease_diagnosis), 'langerhans cell histiocytosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^large atrial septal defect',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left atrial appendage thrombus',comorb$cfh_comorbid_disease_diagnosis), 'cardiac_thrombus', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left eye cataract',comorb$cfh_comorbid_disease_diagnosis), 'cataract', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left femoral vein dvt',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left leg deep vein thrombosis',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left upper lobe nodule',comorb$cfh_comorbid_disease_diagnosis), 'lung nodule', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left ventricle hypertrophy',comorb$cfh_comorbid_disease_diagnosis), 'LVH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left ventricular hypertrophy',comorb$cfh_comorbid_disease_diagnosis), 'LVH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left ventricular dysfunction',comorb$cfh_comorbid_disease_diagnosis), 'left_heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left ventricular impairment',comorb$cfh_comorbid_disease_diagnosis), 'left_heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('vocal cord',comorb$cfh_comorbid_disease_diagnosis), 'vocal cord palsey', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^likely gilberts syndrome',comorb$cfh_comorbid_disease_diagnosis), 'gilbert_syndrome', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^lipoma',comorb$cfh_comorbid_disease_diagnosis), 'lipoma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^long-term asthma',comorb$cfh_comorbid_disease_diagnosis), 'asthma', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^localised polyangitis granulomatosis',comorb$cfh_comorbid_disease_diagnosis), 'GPA', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^localised primary arthritis',comorb$cfh_comorbid_disease_diagnosis), 'arthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^low iron levels',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary_fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^low mood',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^macrocytosis',comorb$cfh_comorbid_disease_diagnosis), 'macrocytosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^major depression',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^massive haemoptesis',comorb$cfh_comorbid_disease_diagnosis), 'haemoptysis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^massive upper gi bleeding',comorb$cfh_comorbid_disease_diagnosis), 'gi_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mediastinal adenopathy',comorb$cfh_comorbid_disease_diagnosis), 'mediastinal lymphadenopathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^meniere',comorb$cfh_comorbid_disease_diagnosis), 'meniere_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ménière disease',comorb$cfh_comorbid_disease_diagnosis), 'meniere_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^menhorraghia',comorb$cfh_comorbid_disease_diagnosis), 'menorrhagia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^menorrhagia with iron deficiency anaemia',comorb$cfh_comorbid_disease_diagnosis), 'menorrhagia & iron_deficiency_anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^menorrhagia due to warfarin',comorb$cfh_comorbid_disease_diagnosis), 'menorrhagia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mennorhagia',comorb$cfh_comorbid_disease_diagnosis), 'menorrhagia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mi',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^microcytic',comorb$cfh_comorbid_disease_diagnosis), 'anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild centrilobullar emphysema',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild chronic obstructive pulmonary disease',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild concentric left ventricular hypertrophy',comorb$cfh_comorbid_disease_diagnosis), 'LVH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild coronary atheroma',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild depression',comorb$cfh_comorbid_disease_diagnosis), 'deression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild interstitial lung disease with pulmonary fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild erythrocytosis',comorb$cfh_comorbid_disease_diagnosis), 'erythrocytosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild left ventricular hypertrophy',comorb$cfh_comorbid_disease_diagnosis), 'LVH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild lv hypertrophy',comorb$cfh_comorbid_disease_diagnosis), 'LVH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild obstructive pattern on lung function',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild osa',comorb$cfh_comorbid_disease_diagnosis), 'osas', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild paraseptal emphysema',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mild predominantly basal fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^minor basal interstitial changes, lung',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^minor emphysema of minor subpleural fibrosis ct',comorb$cfh_comorbid_disease_diagnosis), 'copd & ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^minor upper lobe emphysema',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mitral insufficiency',comorb$cfh_comorbid_disease_diagnosis), 'mitral regurgitation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mitral reurgitation',comorb$cfh_comorbid_disease_diagnosis), 'mitral regurgitation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mixed obstructive and central sleep ap',comorb$cfh_comorbid_disease_diagnosis), 'osa & central sleep apnea', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^moderate aortic regurgitation',comorb$cfh_comorbid_disease_diagnosis), 'aortic regurgitation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^moderate coronary artery disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^moderate obstructive airway disease, on lung function',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^moderate secundum asd',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^monoclonal gammopathy with undetermined significance',comorb$cfh_comorbid_disease_diagnosis), 'mgus', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mucinous cystadenoma',comorb$cfh_comorbid_disease_diagnosis), 'ovary_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^multivessel coronary artery disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^myeloproliferative disease',comorb$cfh_comorbid_disease_diagnosis), 'myeloproliferative disorder', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^myocardial infarct 1993',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^myocardial infarction ? 1983',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^nafld',comorb$cfh_comorbid_disease_diagnosis), 'fatty liver', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^nasal polyp',comorb$cfh_comorbid_disease_diagnosis), 'nasal polyp', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^nephrotic syndrome',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^neurotic depression',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^neurotic depressive reactive type',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^niddm',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non-insulin-dependent diabetes mellitus',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non insulin dependent diabetes mellitus',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non malignant ovarian cyst',comorb$cfh_comorbid_disease_diagnosis), 'ovary_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non visible haematuria',comorb$cfh_comorbid_disease_diagnosis), 'haematuria', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^non specific interstitial pneumonitis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^nontoxic diffuse goiter',comorb$cfh_comorbid_disease_diagnosis), 'goiter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^normocytic anaemia',comorb$cfh_comorbid_disease_diagnosis), 'anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^nstemi',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^obsesity',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^obestity',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^obesity- intragastric balloon inserted',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('obstructive airways disease (newly diagnosed) likely copd',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^obstructive lung disease',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^obstructuve sleep apnea',comorb$cfh_comorbid_disease_diagnosis), 'osas', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ocular sarcoidosis',comorb$cfh_comorbid_disease_diagnosis), 'sarcoidosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ocular hypertension',comorb$cfh_comorbid_disease_diagnosis), 'occular hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^oesophageal gastric disease',comorb$cfh_comorbid_disease_diagnosis), 'GERD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^oesophageal reflux',comorb$cfh_comorbid_disease_diagnosis), 'GERD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^old myocardial infarction',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^osetoarthritis',comorb$cfh_comorbid_disease_diagnosis), 'osteoarthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^osteoarthritis',comorb$cfh_comorbid_disease_diagnosis), 'osteoarthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^other chronic obstructive pulmonary disease',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^other hypothyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ovarian cyst',comorb$cfh_comorbid_disease_diagnosis), 'ovary_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ovary cyst surgery',comorb$cfh_comorbid_disease_diagnosis), 'ovary_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pacemaker',comorb$cfh_comorbid_disease_diagnosis), 'pacemaker', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pancytopeonia',comorb$cfh_comorbid_disease_diagnosis), 'pancytopenia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^obstructive airways disease',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^paroxysmal atrial fibrilation',comorb$cfh_comorbid_disease_diagnosis), 'afib', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^paroxysmal atrial tachycardia',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^paroxysmal supra ventricular tachycardia',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^partial blindness',comorb$cfh_comorbid_disease_diagnosis), 'partial_blindness', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^partial deafness',comorb$cfh_comorbid_disease_diagnosis), 'hearing_loss', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^past history atrial tachycardia and fainting',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pe',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^percutaneous coronary intervention',comorb$cfh_comorbid_disease_diagnosis), 'PCI', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^perennial rhinitis',comorb$cfh_comorbid_disease_diagnosis), 'rhinitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pericardial effusion',comorb$cfh_comorbid_disease_diagnosis), 'pericardial_effusion', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pericardial effusions',comorb$cfh_comorbid_disease_diagnosis), 'pericardial_effusion', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^peripheral artery occlusive disease',comorb$cfh_comorbid_disease_diagnosis), 'peripheral artery disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^peripheral artery disease',comorb$cfh_comorbid_disease_diagnosis), 'peripheral_vascular_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^permanent atrial fibrillation',comorb$cfh_comorbid_disease_diagnosis), 'afib', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary_fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^permanent pacemaker',comorb$cfh_comorbid_disease_diagnosis), 'pacemaker', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^persistent anxiety depression',comorb$cfh_comorbid_disease_diagnosis), 'anxiety & depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^phlebitis and thrombophlebitis',comorb$cfh_comorbid_disease_diagnosis), 'phlebitis & thrombophlebitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^plantar fasciitis',comorb$cfh_comorbid_disease_diagnosis), 'plantar fasciitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pleural effusion',comorb$cfh_comorbid_disease_diagnosis), 'pleural effusion', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pleural effusion',comorb$cfh_comorbid_disease_diagnosis), 'pleural_effusion', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pleural plaque',comorb$cfh_comorbid_disease_diagnosis), 'pleural plaque', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pleural parenchymal fibroelastosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pnuemonia',comorb$cfh_comorbid_disease_diagnosis), 'pneumonia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^polyarthralgia elevated antinuclear antibody titre',comorb$cfh_comorbid_disease_diagnosis), 'polyarthralgia & ANA_positivity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^emphysema',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary_fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^polycystic ovarian syndrome',comorb$cfh_comorbid_disease_diagnosis), 'PCOS', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^polycystic ovaries',comorb$cfh_comorbid_disease_diagnosis), 'PCOS', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^polycystic ovary disease',comorb$cfh_comorbid_disease_diagnosis), 'PCOS', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^polycyth',comorb$cfh_comorbid_disease_diagnosis), 'polycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^possible raynaud',comorb$cfh_comorbid_disease_diagnosis), 'raynauds_phenomenon', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^possible patent foramen ovale',comorb$cfh_comorbid_disease_diagnosis), 'pfo', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^possible systemic hypertension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^possible transient cerebral ischaemic attacks',comorb$cfh_comorbid_disease_diagnosis), 'tia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^post-procedural hypothyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^poylcythehaemia',comorb$cfh_comorbid_disease_diagnosis), 'polycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous anaemia',comorb$cfh_comorbid_disease_diagnosis), 'anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous appendectomy',comorb$cfh_comorbid_disease_diagnosis), 'appendicitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous atrial fibrillation',comorb$cfh_comorbid_disease_diagnosis), 'afib', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous atrial flutter',comorb$cfh_comorbid_disease_diagnosis), 'atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous carcinoma prostate',comorb$cfh_comorbid_disease_diagnosis), 'prostateCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous cervical cancer',comorb$cfh_comorbid_disease_diagnosis), 'cervicalCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous colonic cancer treated with right hemicolectomy',comorb$cfh_comorbid_disease_diagnosis), 'bowelCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous decompensation and right heart failure',comorb$cfh_comorbid_disease_diagnosis), 'right_heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous deep vein thrombosis',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous excessive alcohol intake',comorb$cfh_comorbid_disease_diagnosis), 'alcohol abuse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous hepatitis c',comorb$cfh_comorbid_disease_diagnosis), 'hepatitis c', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous high alcohol intake',comorb$cfh_comorbid_disease_diagnosis), 'alcohol abuse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous inferior myocardial infarction',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous intermittent anaemia',comorb$cfh_comorbid_disease_diagnosis), 'anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous investigation for anaemia',comorb$cfh_comorbid_disease_diagnosis), 'anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous iron deficiency',comorb$cfh_comorbid_disease_diagnosis), 'iron_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous left breast carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'breast_cancer', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous menorrhagia',comorb$cfh_comorbid_disease_diagnosis), 'menorrhagia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous cataract surgery',comorb$cfh_comorbid_disease_diagnosis), 'cataract', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous myocardial infarction',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous oesophageal varices bleed',comorb$cfh_comorbid_disease_diagnosis), 'oesophageal varices', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous ovarian carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'ovaryCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous pancytopenia',comorb$cfh_comorbid_disease_diagnosis), 'pancytopenia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous plantar fasciitis, left foot',comorb$cfh_comorbid_disease_diagnosis), 'plantar fasciitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous polycythaemia',comorb$cfh_comorbid_disease_diagnosis), 'polycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous severe depressive episode',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous significant alcohol intake',comorb$cfh_comorbid_disease_diagnosis), 'alcohol abuse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous thyroid toxicosis',comorb$cfh_comorbid_disease_diagnosis), 'thyrotoxicosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous thyrotoxicosis',comorb$cfh_comorbid_disease_diagnosis), 'thyrotoxicosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previously mildly deranged liver function',comorb$cfh_comorbid_disease_diagnosis), 'liver impairment', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^primary hyperparathyroidism',comorb$cfh_comorbid_disease_diagnosis), 'hyperparathyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^primary left upper lobe lung cancer',comorb$cfh_comorbid_disease_diagnosis), 'lungCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^prior left ventricular failure',comorb$cfh_comorbid_disease_diagnosis), 'left_heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^probable secondary polycythaemia',comorb$cfh_comorbid_disease_diagnosis), 'secondary_polycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^problematic oral candidiasis',comorb$cfh_comorbid_disease_diagnosis), 'oral candidiasis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^profound hypoxaemia and low transferrin saturation',comorb$cfh_comorbid_disease_diagnosis), 'hypoxia & iron_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^profound thyrotoxicosis',comorb$cfh_comorbid_disease_diagnosis), 'thyrotoxicosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^prostate cancer',comorb$cfh_comorbid_disease_diagnosis), 'prostateCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^prostatectomy',comorb$cfh_comorbid_disease_diagnosis), 'prostateCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^prostatic carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'prostateCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^prostatic hypertrophy',comorb$cfh_comorbid_disease_diagnosis), 'BPH', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^psorasis',comorb$cfh_comorbid_disease_diagnosis), 'psoriasis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^psoriasis nos',comorb$cfh_comorbid_disease_diagnosis), 'psoriasis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^psoriatic arthropathy',comorb$cfh_comorbid_disease_diagnosis), 'psoriatic arthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^psychotic depression',comorb$cfh_comorbid_disease_diagnosis), 'psychosis & depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^psychotic',comorb$cfh_comorbid_disease_diagnosis), 'psychosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary embolus',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary nodules',comorb$cfh_comorbid_disease_diagnosis), 'lung nodule', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary tb',comorb$cfh_comorbid_disease_diagnosis), 'tuberculosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary thromboembolism',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary tuberculosis',comorb$cfh_comorbid_disease_diagnosis), 'tuberculosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary veno-occlusive disease',comorb$cfh_comorbid_disease_diagnosis), 'PVOD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary venous occlusive disease',comorb$cfh_comorbid_disease_diagnosis), 'PVOD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pure hypercholesterolaemia',comorb$cfh_comorbid_disease_diagnosis), 'dyslipidaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^raised bmi',comorb$cfh_comorbid_disease_diagnosis), 'obesity', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^raynaoud',comorb$cfh_comorbid_disease_diagnosis), 'raynauds_phenomenon', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^raynud',comorb$cfh_comorbid_disease_diagnosis), 'raynauds_phenomenon', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recent mri scan fulfil criteria for arvc',comorb$cfh_comorbid_disease_diagnosis), 'arrhythmogenic right ventricular cardiomyopathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent anaemia',comorb$cfh_comorbid_disease_diagnosis), 'anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent atrial flutter with flutter ablation',comorb$cfh_comorbid_disease_diagnosis), 'atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent cardiac arrhythmias',comorb$cfh_comorbid_disease_diagnosis), 'afib & atrial_flutter', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent chest infection',comorb$cfh_comorbid_disease_diagnosis), 'recurrent chest infection', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent haemoptysis',comorb$cfh_comorbid_disease_diagnosis), 'haemoptysis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent pulmonary embolism',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent pyelonephritis',comorb$cfh_comorbid_disease_diagnosis), 'pyelonephritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent rectal bleeding secondary to lower gi polyps',comorb$cfh_comorbid_disease_diagnosis), 'gi_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent syncopy',comorb$cfh_comorbid_disease_diagnosis), 'syncope', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^renal artery stenosis with stent',comorb$cfh_comorbid_disease_diagnosis), 'renal_artery_stenosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^renal calculi bilateral renal cysts',comorb$cfh_comorbid_disease_diagnosis), 'renal calculus & renal_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^respiratory bronchiolitis associated idiopathic lung fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^respiratory bronchiolitis idiopathic lung fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^respiratory bronchiolitis interstitial lung disease',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^respiratory failure',comorb$cfh_comorbid_disease_diagnosis), 'respiratory failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^retinopapthy',comorb$cfh_comorbid_disease_diagnosis), 'retinopathy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^reynauds',comorb$cfh_comorbid_disease_diagnosis), 'raynauds_phenomenon', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^rhuematic fever',comorb$cfh_comorbid_disease_diagnosis), 'rheumatic fever', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right bundle branc block',comorb$cfh_comorbid_disease_diagnosis), 'rbbb', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right cerrebellar stroke',comorb$cfh_comorbid_disease_diagnosis), 'cva', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right carotid artery stenosis',comorb$cfh_comorbid_disease_diagnosis), 'carotid artery stenosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right heart failure',comorb$cfh_comorbid_disease_diagnosis), 'right_heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right coronary artery stent',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right lower lobe pulmonary nodule',comorb$cfh_comorbid_disease_diagnosis), 'lung nodule', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right middle lobe nodule',comorb$cfh_comorbid_disease_diagnosis), 'lung nodule', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right middle lobe pulmonary artery aneurysm',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary_artery_aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right nephrectomy due to hydro nephrosis and poor functioning kidney',comorb$cfh_comorbid_disease_diagnosis), 'hydropnephrosis & CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right pleural effusion',comorb$cfh_comorbid_disease_diagnosis), 'pleural_effusion', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right proximal subclavian, internal jugular and innominate ',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right subclavian thrombus and further pulmonary emboli',comorb$cfh_comorbid_disease_diagnosis), 'DVT & pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right upper lobe 8mm nodule',comorb$cfh_comorbid_disease_diagnosis), 'lung nodule', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right ventricular failure',comorb$cfh_comorbid_disease_diagnosis), 'right_heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^rul carcinoma-lung cancer',comorb$cfh_comorbid_disease_diagnosis), 'lungCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sarcoidosis',comorb$cfh_comorbid_disease_diagnosis), 'sarcoidosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sciatca',comorb$cfh_comorbid_disease_diagnosis), 'sciatica', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^secondary polycythaemia',comorb$cfh_comorbid_disease_diagnosis), 'secondary_polycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^secundum asd',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sensorineural deafness',comorb$cfh_comorbid_disease_diagnosis), 'hearing_loss', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^septal defect - patent foramen ovale',comorb$cfh_comorbid_disease_diagnosis), 'pfo', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^septic emboli',comorb$cfh_comorbid_disease_diagnosis), 'septic emboli', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^severe 2 vessel coronary artery disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^severe right coronary artery disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^severe tricuspid regurgitation',comorb$cfh_comorbid_disease_diagnosis), 'tricuspid regurgitation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^severely impaired right ventricular function',comorb$cfh_comorbid_disease_diagnosis), 'right_heart_failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^simple liver cyst',comorb$cfh_comorbid_disease_diagnosis), 'liver cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^simple renal cyst on ultrasound',comorb$cfh_comorbid_disease_diagnosis), 'renal_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^single major depressive episode',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^single vessel coronary artery disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^singles vessel artery disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sinus node re-entrant tachycardia ablated',comorb$cfh_comorbid_disease_diagnosis), 'SVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sjogren',comorb$cfh_comorbid_disease_diagnosis), 'sjogren_syndrome', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sleep apnoea',comorb$cfh_comorbid_disease_diagnosis), 'osa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^osas',comorb$cfh_comorbid_disease_diagnosis), 'osa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^small airway disease',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^small cerebral infarcts',comorb$cfh_comorbid_disease_diagnosis), 'cva', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^small component of thromboembolic disease',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary_fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^myocardial infarction',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^osteoartritis',comorb$cfh_comorbid_disease_diagnosis), 'osteoarthritis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^osteomyelitis',comorb$cfh_comorbid_disease_diagnosis), 'osteomyelitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^other and unspecified syphilis',comorb$cfh_comorbid_disease_diagnosis), 'syphilis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous tb adenitis treated 35 years ago',comorb$cfh_comorbid_disease_diagnosis), 'tuberculosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous testicular carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'testisCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^previous tuberculosis',comorb$cfh_comorbid_disease_diagnosis), 'tuberculosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^probable old tuberculosis',comorb$cfh_comorbid_disease_diagnosis), 'tuberculosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^profoundly deaf',comorb$cfh_comorbid_disease_diagnosis), 'hearing_loss', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^psitive rheumatoid factor',comorb$cfh_comorbid_disease_diagnosis), 'rheumatoid factor positive', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^secondary erythrocytosis',comorb$cfh_comorbid_disease_diagnosis), 'secondary_polycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^significant recent hypereosinophilia',comorb$cfh_comorbid_disease_diagnosis), 'hypereosinophilia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^small bowel obstruction secondary to proximal sigmoid',comorb$cfh_comorbid_disease_diagnosis), 'diverticulitis & small_bowel_obstruction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^small bowel resection for stricture',comorb$cfh_comorbid_disease_diagnosis), 'small_bowel_obstruction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^small left ovarian cyst',comorb$cfh_comorbid_disease_diagnosis), 'ovary_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^small lower abdominal aortic aneurysm',comorb$cfh_comorbid_disease_diagnosis), 'abdominal_aortic_aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^some features of pulmonary capilliary haemangiomatosis',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary capilliary haemangiomatosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^spenectomy',comorb$cfh_comorbid_disease_diagnosis), 'splenectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^spenomegaly',comorb$cfh_comorbid_disease_diagnosis), 'splenomegaly', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary_fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^spine fracture',comorb$cfh_comorbid_disease_diagnosis), 'spinal fracture', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pulmonary_fibrosis',comorb$cfh_comorbid_disease_diagnosis), 'ild', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^splenectomy following autoimmue haemolytic anemia',comorb$cfh_comorbid_disease_diagnosis), 'splenectomy & autoimmue haemolytic anemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^squamous cell carcinma anus',comorb$cfh_comorbid_disease_diagnosis), 'anusCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^state after coronary artery bypass surgery',comorb$cfh_comorbid_disease_diagnosis), 'CABG', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^state after deep vein thrombosis',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^state after ptca with stent implantation',comorb$cfh_comorbid_disease_diagnosis), 'PCI', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^stroke',comorb$cfh_comorbid_disease_diagnosis), 'cva', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^stemi',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sub clinical borderline hyperthyroidism',comorb$cfh_comorbid_disease_diagnosis), 'subclinical hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^subarachnoid haemorrhage and anterior',comorb$cfh_comorbid_disease_diagnosis), 'brain_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^subdural haematoma',comorb$cfh_comorbid_disease_diagnosis), 'brain_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^superior vena cava and azygos vein thrombosis relate',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^systemic lupus erythematosis',comorb$cfh_comorbid_disease_diagnosis), 'sle', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^systemic lupus erythematosus',comorb$cfh_comorbid_disease_diagnosis), 'sle', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^systemic lupus erythematous',comorb$cfh_comorbid_disease_diagnosis), 'sle', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^systemis hypertension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^sytemic hypertension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^systyemic hypertension',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^t2 dm',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^tb',comorb$cfh_comorbid_disease_diagnosis), 'tuberculosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^testicular cancer',comorb$cfh_comorbid_disease_diagnosis), 'testisCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^three-vessel coronary disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^three vessel coronary disease',comorb$cfh_comorbid_disease_diagnosis), 'ischaemic_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^three paroxysmal atrial fibrillation',comorb$cfh_comorbid_disease_diagnosis), 'afib', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thrombocytopenia',comorb$cfh_comorbid_disease_diagnosis), 'thrombocytopenia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thrombosed left brachiocephalic vein',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thrombosed distal right internal jugular vein',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thrombosed proximal svc',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thrombosed right brachiocephalic vein',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thrombpocytopenia',comorb$cfh_comorbid_disease_diagnosis), 'thrombocytopenia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thyroid cancer',comorb$cfh_comorbid_disease_diagnosis), 'thyroidCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thyroid nodule follicular neoplasm',comorb$cfh_comorbid_disease_diagnosis), 'thyroidCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thyrotoxicosis',comorb$cfh_comorbid_disease_diagnosis), 'thyrotoxicosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thyrotoxic',comorb$cfh_comorbid_disease_diagnosis), 'thyrotoxicosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^tia with left amaurosis fugax',comorb$cfh_comorbid_disease_diagnosis), 'tia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^tonsellectomy',comorb$cfh_comorbid_disease_diagnosis), 'tonsillectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^total abdominal hysterectomy',comorb$cfh_comorbid_disease_diagnosis), 'hysterectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^total hysterectomy',comorb$cfh_comorbid_disease_diagnosis), 'hysterectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^total thyroidectomy of retrosternal goiter',comorb$cfh_comorbid_disease_diagnosis), 'thyroidectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^transient cerebral ischemic attack',comorb$cfh_comorbid_disease_diagnosis), 'tia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^transient ischaeic accident secondary to',comorb$cfh_comorbid_disease_diagnosis), 'tia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^transitional cell carcinoma of the bladder',comorb$cfh_comorbid_disease_diagnosis), 'bladderCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^traumatic brain injury',comorb$cfh_comorbid_disease_diagnosis), 'tbi', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^traumatic head injury',comorb$cfh_comorbid_disease_diagnosis), 'tbi', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^triple vessel coronary artery bypass grafting',comorb$cfh_comorbid_disease_diagnosis), 'CABG', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^trisomy 21',comorb$cfh_comorbid_disease_diagnosis), 'trisomy 21', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^t21',comorb$cfh_comorbid_disease_diagnosis), 'trisomy 21', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^tuberculois meningitis',comorb$cfh_comorbid_disease_diagnosis), 'tuberculosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^tuberculosis',comorb$cfh_comorbid_disease_diagnosis), 'tuberculosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^two occurrences of acute myocardial infarction',comorb$cfh_comorbid_disease_diagnosis), 'myocardial infarction', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type 1 respiratory failure',comorb$cfh_comorbid_disease_diagnosis), 'type 1 respiratory failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type 2 respiratory failure',comorb$cfh_comorbid_disease_diagnosis), 'type 2 respiratory failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type i respiratory failure',comorb$cfh_comorbid_disease_diagnosis), 'type 1 respiratory failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type i diastolic lv dysfunction',comorb$cfh_comorbid_disease_diagnosis), 'diastolic dysfunction_LV', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type ii dm',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^type ii respiratory failure',comorb$cfh_comorbid_disease_diagnosis), 'type 2 respiratory failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^abnormal liver function',comorb$cfh_comorbid_disease_diagnosis), 'liver impairment', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^acute liver failure',comorb$cfh_comorbid_disease_diagnosis), 'acute liver failure', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^churash ploycythaemia',comorb$cfh_comorbid_disease_diagnosis), 'ploycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^clotting disorder',comorb$cfh_comorbid_disease_diagnosis), 'antiphospholipid_syndrome', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetic ',comorb$cfh_comorbid_disease_diagnosis), 'DM2', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes mellitus',comorb$cfh_comorbid_disease_diagnosis), 'diabetes', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^diabetes (not stated which type)',comorb$cfh_comorbid_disease_diagnosis), 'diabetes', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^myasthena gravis',comorb$cfh_comorbid_disease_diagnosis), 'myasthena gravis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^iv opiods use',comorb$cfh_comorbid_disease_diagnosis), 'heroin_abuse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ypertensive cardiac disease',comorb$cfh_comorbid_disease_diagnosis), 'hypertensive_heart_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^heart arrhythmias',comorb$cfh_comorbid_disease_diagnosis), 'arrhytmia_unspecified', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^) atrial septal aneurysm with small patent forame ovale',comorb$cfh_comorbid_disease_diagnosis), 'pfo & atrial septal aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^unrepaired atrial septal defect',comorb$cfh_comorbid_disease_diagnosis), 'asd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^upper right lobectomy for tb',comorb$cfh_comorbid_disease_diagnosis), 'tuberculosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vagal episodes',comorb$cfh_comorbid_disease_diagnosis), 'syncope', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^urticarial rash for lipodermatosclerosis',comorb$cfh_comorbid_disease_diagnosis), 'lipodermatosclerosis & urticaria', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vaginal prolapse',comorb$cfh_comorbid_disease_diagnosis), 'vaginal prolapse', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^varicose vein',comorb$cfh_comorbid_disease_diagnosis), 'varicose veins', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^venothromboembolisms',comorb$cfh_comorbid_disease_diagnosis), 'DVT', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^5 colorectal adenocarcinoma',comorb$cfh_comorbid_disease_diagnosis), 'bowelCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vitamin d deficient',comorb$cfh_comorbid_disease_diagnosis), 'vit_d_deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^vocal cord palsey',comorb$cfh_comorbid_disease_diagnosis), 'vocal chord palsy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^von willebrand',comorb$cfh_comorbid_disease_diagnosis), 'von_willebrands_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^widespread emphysema',comorb$cfh_comorbid_disease_diagnosis), 'copd', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^addison',comorb$cfh_comorbid_disease_diagnosis), 'addison_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^angioplasty to right femoral artery',comorb$cfh_comorbid_disease_diagnosis), 'peripheral_vascular_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^antiphospholipid syndrome',comorb$cfh_comorbid_disease_diagnosis), 'antiphospholipid_syndrome', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^anusCa',comorb$cfh_comorbid_disease_diagnosis), 'analCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^autoimmue haemolytic anemia',comorb$cfh_comorbid_disease_diagnosis), 'autoimmune haemolytic anaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^barret oesophagus',comorb$cfh_comorbid_disease_diagnosis), 'barrett_oesophagus', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^deafness',comorb$cfh_comorbid_disease_diagnosis), 'hearing_loss', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^erythrocytosis/monocytosis',comorb$cfh_comorbid_disease_diagnosis), 'polycythaemia & monocytosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^erythrocytosis',comorb$cfh_comorbid_disease_diagnosis), 'polycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^fusiform aneurysm of aortic arch',comorb$cfh_comorbid_disease_diagnosis), 'type_A_aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^gi bleed',comorb$cfh_comorbid_disease_diagnosis), 'gi_bleed', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hepatic haemangioma',comorb$cfh_comorbid_disease_diagnosis), 'hemangioma of liver', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^heterozygous missense variant of uncertain clinical significance smad9',comorb$cfh_comorbid_disease_diagnosis), 'SMAD9_variant', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hydropnephrosis',comorb$cfh_comorbid_disease_diagnosis), 'hydronephrosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypertensive disease',comorb$cfh_comorbid_disease_diagnosis), 'hypertension', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^hypothroidism',comorb$cfh_comorbid_disease_diagnosis), 'hypothyroidism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^intraatrial septal aneurysm',comorb$cfh_comorbid_disease_diagnosis), 'atrial septal aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^left-sided atrioventricular valvar regurgitation',comorb$cfh_comorbid_disease_diagnosis), 'mitral regurgitation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^jessner',comorb$cfh_comorbid_disease_diagnosis), 'jessners_lymphocytic_infiltration', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^liver cyst',comorb$cfh_comorbid_disease_diagnosis), 'liver_cyst', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^lung cancer with recent radiotherapy',comorb$cfh_comorbid_disease_diagnosis), 'lungCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^malignant neoplasm of cervix uteri',comorb$cfh_comorbid_disease_diagnosis), 'cervicalCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^malignant neoplasm of kidney, except renal pelvis',comorb$cfh_comorbid_disease_diagnosis), 'kidneyCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mcardle',comorb$cfh_comorbid_disease_diagnosis), 'mcardle_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^membranous nephropathy',comorb$cfh_comorbid_disease_diagnosis), 'CKD', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^low grade mgus',comorb$cfh_comorbid_disease_diagnosis), 'mgus', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^monoclonal gammopathy',comorb$cfh_comorbid_disease_diagnosis), 'mgus', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^mgus',comorb$cfh_comorbid_disease_diagnosis), 'MGUS', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^myalgic encephalomyelitis',comorb$cfh_comorbid_disease_diagnosis), 'chronic fatigue syndrome', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^noonan',comorb$cfh_comorbid_disease_diagnosis), 'noonan_syndrome', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ovarian cancer',comorb$cfh_comorbid_disease_diagnosis), 'ovaryCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^oophorectomy',comorb$cfh_comorbid_disease_diagnosis), 'ovariectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pneumococcal meningitis',comorb$cfh_comorbid_disease_diagnosis), 'meningitis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^low protein c level',comorb$cfh_comorbid_disease_diagnosis), 'protein c deficiency', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^radiotherapy for menorrhagia secondary to adenomyosis',comorb$cfh_comorbid_disease_diagnosis), 'menorrhagia & adenomyosis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^radioulnar synostosis with amegakaryocytic thrombocytopenia syndrome, mecom gene mutation',comorb$cfh_comorbid_disease_diagnosis), 'MECOM_variant', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^pathogenic variant in the mecom gene',comorb$cfh_comorbid_disease_diagnosis), 'MECOM_variant', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^raised breathlessness',comorb$cfh_comorbid_disease_diagnosis), 'dyspnoea', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ankle inury',comorb$cfh_comorbid_disease_diagnosis), 'ankle_fracture', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^breathless',comorb$cfh_comorbid_disease_diagnosis), 'dyspnoea', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent ocular hypertension and cataracts right eye',comorb$cfh_comorbid_disease_diagnosis), 'occular hypertension & cataract', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent chest infection',comorb$cfh_comorbid_disease_diagnosis), 'chest infection', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recent unexplained weight loss of 20kg',comorb$cfh_comorbid_disease_diagnosis), 'weight loss', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^recurrent peripheral oedema',comorb$cfh_comorbid_disease_diagnosis), 'peripheral oedema', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^renal calculus',comorb$cfh_comorbid_disease_diagnosis), 'nephrolithiasis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^renal cell carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'kidneyCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^renal stone disease',comorb$cfh_comorbid_disease_diagnosis), 'nephrolithiasis', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^restrictive spirometry and resting hypoxia',comorb$cfh_comorbid_disease_diagnosis), 'hypoxia & restrictive lung funcion', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^retinal occlusion',comorb$cfh_comorbid_disease_diagnosis), 'RVO', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^retinal vein thrombosis',comorb$cfh_comorbid_disease_diagnosis), 'RVO', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^rheumatoid factor positive',comorb$cfh_comorbid_disease_diagnosis), 'positive rheumatoid factor', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right carotid endartarectomy',comorb$cfh_comorbid_disease_diagnosis), 'carotid endartarectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right-sided cerebral atrophy',comorb$cfh_comorbid_disease_diagnosis), 'cerebral atrophy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right lower lobe pulmonary avm',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary av malformation', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^partial anomalous pulmonary venous drainage',comorb$cfh_comorbid_disease_diagnosis), 'anomalous pulmonary venous drainage', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^aneurysmatic pulmonary artery',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary_artery_aneurysm', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^partial anomalous pulmonary venous connection',comorb$cfh_comorbid_disease_diagnosis), 'anomalous pulmonary venous drainage', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^right upper lobectomy 2012 for adenocarcinoma of the lung',comorb$cfh_comorbid_disease_diagnosis), 'lungCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^squamous cell renal carcinoma',comorb$cfh_comorbid_disease_diagnosis), 'kidneyCa', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^stress related depression',comorb$cfh_comorbid_disease_diagnosis), 'depression', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^surgery for bilateral cataracts',comorb$cfh_comorbid_disease_diagnosis), 'cataract', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^surgical cholecystectomy',comorb$cfh_comorbid_disease_diagnosis), 'cholecystectomy', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thrombin pulmonary arteriesis',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^thrombin pulmonary arteriesis',comorb$cfh_comorbid_disease_diagnosis), 'pulmonary embolism', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^transaminitis',comorb$cfh_comorbid_disease_diagnosis), 'liver impairment', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^transposition of great arteries',comorb$cfh_comorbid_disease_diagnosis), 'transposition of great arteries', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^uncentric castelman',comorb$cfh_comorbid_disease_diagnosis), 'uncentric_castelmans_disease', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^weight loss + anorexia',comorb$cfh_comorbid_disease_diagnosis), 'weight loss & anorexia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^ploycythaemia',comorb$cfh_comorbid_disease_diagnosis), 'polycythaemia', comorb$cfh_comorbid_disease_diagnosis)
comorb$cfh_comorbid_disease_diagnosis <- ifelse(grepl('^genesis of right pulmonary artery',comorb$cfh_comorbid_disease_diagnosis), 'agenesis of right pulmonary artery', comorb$cfh_comorbid_disease_diagnosis)


#now split the rows for the double annotated diseases
library(splitstackshape)
comorb <- cSplit(comorb, c('cfh_comorbid_disease_diagnosis'), sep='&', 'long')

#make sure double diagnoses are removed again
comorb <- unique(comorb)
#removed some double diagnoses
#2192 comorbid diseases to 968 
abc <- as.data.frame(unique(comorb$cfh_comorbid_disease_diagnosis))


#now group diseases together
cv_electrophysiological <- c('1st_degree_heart_block','	2nd_degree_heart_block', 'afib', 'arrhythmogenic right ventricular cardiomyopathy', 'arrhytmia_unspecified', 'rbbb', 'wolff-parkinson-white syndrome',
                             'atrial_flutter', 'avnrt', 'avrt', 'bradycardia', 'complete_heart_block', 'heart_block', 'lbbb', 'marked asynchronous left ventricular contraction',
                             'pacemaker', 'paroxysmal tachycardia', 'paroxysmal ventricular tachycardia', 'premature atrial contractions', 'SVT', 'ventricular ectopy')

cv_ischaemic <- c('CABG', 'cardiac_arrest', 'cardiac_thrombus', 'ischaemic_heart_disease', 'myocardial infarction', 'PCI', 'carotid atheroma', 'carotid artery stenosis')

cv_heart_failure <- c('cardiomyopathy', 'cardiomegaly', 'diastolic dysfunction_LV', 'diastolic heart failure', 'dilated cardiomyopathy', 'tamponade',
                      'eisenmenger', 'heart_failure', 'heart_transplant', 'hfref', 'left_heart_failure', 'LVH', 'right_heart_failure', 'systolic dysfunction_LV')

cv_non_tricuspid_valvular_disease <- c('aortic regurgitation', 'aortic valve replacement','aortic_stenosis')


cv_structural <- c('abdominal_aortic_aneurysm','aberrant right coronary artery', 'aberrant subclavian artery', 'pulmonary_artery_aneurysm', 'right to left shunt (no asd)',
                   'aneurysm of heart', 'anomalous pulmonary venous drainage', 'aortic aneurysm', 'delayed closure of ductus arteriosus', 
                   'aorta-pulmonary fistula closed spontaneously in childhood', 'aortic coarctation- repaired', 'tricuspid valve repair',
                   'agenesis of right pulmonary artery', 'left atrial isomerism', 'asd', 'atrial septal aneurysm', 'closure of patent ductus arteriosus', 'pfo', 'pulmonary av malformation', 'type_A_aneurysm',
                   'bronchial artery aneurysm', 'transposition of great arteries', 'kartageners syndrome',
                   'left superior vena cava persisting to coronary sinus and right atrium', 'left svc draining to coronary sinus')


endocrine_general <- c('addison_disease', 'adrenal insufficiency', 'adrenal hypoplasia', 'adrenal adenoma', 'anterior pituitary insufficiency', 'diabetes insipidus', 'vit_d_deficiency', 
                       'hirsutism', 'hyperinsulinism', 'hyperparathyroidism', 'hypopituitarism', 'prolactinoma', 'secondary hypogonadism', 'osteopenia', 'osteoporosis')

diabetes <- c('DM1', 'DM2', 'insulin_dependent_dm','diabetes', 'diabetes (not stated which type)')

endocrine_thyroid <- c('goiter', 'graves_disease', 'hashimoto', 'hyperthyroidism', 'hypothyroidism', 'subclinical hypothyroidism', 'thyroid disease', 'thyroid nodule', 'thyroidectomy', 'thyroiditis', 'thyrotoxicosis')

hepatological <- c('acute liver failure','alcoholic liver disease', 'autoimmune hepatitis', 'hyperbilirubinaemia', 'jaundice', 'liver abscess', 'liver_cyst', 'liver disease',
                   'fatty liver', 'gilbert_syndrome', 'hemangioma of liver', 'hepatitis', 'hepatitis c','hepatitis b', 'hepatitis b/c', 'hepatomegaly', 'hepatotoxicity',
                   'liver haemangioma', 'drug induced liver injury', 'liver impairment', 'liver_chirrhosis', 'palmar erythema', 'possible hepatic arteriovenous malformation')

GI_other <- c('acute parotitis', 'acute cholecystitis', 'atrophic gastritis', 'barrett_oesophagus', 'celiac disease', 'oesophageal varices', 'ulcerative oesophagitis',
              'cholecystectomy', 'cholecystitis', 'cholelithiasis', 'coeliac disease', 'colitis', 'crohns_disease', 'oesophageal ulcer', 'ulcerative_colitis',
              'diverticulitis', 'diverticulosis', 'duodenal ulcer', 'duodenitis', 'dyspepsia', 'erosive gastritis and duodenitis', 
              'erosive oesophagitis', 'gastris and duodenitis', 'gastric ulcer','gastritis', 'GERD', 'gi_bleed', 'ischaemic colitis',
              'oesophagitis', 'primary biliary cirrhosis', 'proctocolitis')

haematological_malignant_and_premalignant <- c('ALL','AML', 'CLL', 'igg kappa monoclonal peak, myeloma non confirmed', 'igm paraproteinemia', 'lgl leukaemia', 'MGUS', 'lymphoma', 'lymphoproliferative disease', 't-cell lymphoproliferative disorder', 'waldenström macroglobulinaemia',
                                               'multiple myeloma', 'myelofibrosis','myeloproliferative disorder', 'secondary_polycythaemia', 'pulmonary maltoma with involvement of right lung', 't-cell lgl', 't-cell clonal lymphocytosis', 'uncentric_castelmans_disease', 'myelodysplastic syndrome')

haemotological_immune <- c('hypogammaglobulinaemia', 'hypereosinophilia', 'elevated igm', 'leukocytosis', 'low complement c4 levels', 'monocytosis', 'neutropenia', 'pancytopenia', 'ethnic neutropenia', 'splenectomy', 'splenomegaly', 'idiopathic thrombocytopenic purpura', 'autoimmune haemolytic anaemia', 'thrombotic thrombocytopenia')

haematological_benign <-c('anaemia', 'b12_anaemia', 'beta thalassaemia major', 'beta thalassaemia trait', 'vit_b12_deficiency',
                          'haemochromatosis', 'hereditary spherocytosis',  'thalassaemia trait', 'thalassemia',
                          'iron_deficiency', 'iron_deficiency_anaemia', 'secondary_polycythaemia', 'sickle cell trait')

gynaecological_hormonal <- c('amenorrhoea', 'dysmenorrhea', 'endometriosis', 'menorrhagia', 'metrorrhagia', 'oligomenorrhoea', 'PCOS', 'post-menopausal syndrome', 'premature menopause')

bone_marrow <- c('autologus bone marrow transplant', 'MECOM_variant', 'polycythaemia', 'thrombocytopenia')

pulmonary <- c('alveolar hypoventilation','ards', 'asbestos related pleural parenchymal lung disease', 'blind ending bronchus',
               'bronchiectasis', 'bronchiolitis', 'asthma', 'chronic respiratory failure', 'copd', 'LTx', 'type 2 respiratory failure',
               'haemoptysis', 'hypersensitivity pneumonitis', 'ild', 'occupation lung disease', 'sarcoidosis', 'alpha 1 antitrypsin deficiency',
               'pleural_effusion', 'pleural plaque', 'respiratory failure', 'hypoxia', 'restrictive lung function')

dermatological <- c('acne folliculitis', 'lichenoid dermatitis', 'atopic', 'atopic dermatitis', 'jessners_lymphocytic_infiltration', 'lipodermatosclerosis',
                    'dermatitis', 'eczema', 'erythema nodosum', 'granuloma telangiecticum', 'haemangioma', 'irritant dermatitis', 'leg ulcers',
                    'pruritus', 'psoriasis', 'chronic venous insufficiency', 'rosacea', 'vitiligo')

solid_malignancy <- c('adenocarcinoma nos','analCa', 'bladderCa', 'bowelCa', 'cervicalCa', 'oesophagusCa', 'rectalCa', 'testisCa', 
                      'endometrialCa', 'lung metastases', 'lungCa', 'meningioma', 'ovaryCa', 'prostateCa', 'kidneyCa', 'thyroidCa')

rheumatological <- c('algodystrophy of left knee', 'ankylosing spondylitis', 'arthralgia', 'arthritis', 'gout', 'GPA', 'IgA_vasculitis', 
                     'inflammatory arthritis', 'osteoarthritis', 'polymyalgia', 'polyarthralgia', 'psoriatic arthritis', 'rheumatic fever',
                     'rheumatoid arthritis', 'sjogren_syndrome', 'sle', 'temporal arteritis')

PAH_subtypes <- c('CTEPH', 'features of pvod', 'ipah with venous component', 'idiopathic pulmonary arterial hypertension with a reduced transfer factor', 'idiopathic pulmonary arterial hypertension with autoimmune background', 'pulmonary capilliary haemangiomatosis',
                  'idiopathic pulmonary hypertension with risk factors for left heart disease', 'porto pulmonary hypertension', 'possible minor venous component to ph', 'pphn of newborn', 'presumed congestive enteropathy with malnutrition', 'PVOD')

autoimmune_ctd <- c('ANA_positivity', 'anti-gastric parietal cell antibody postive', 'anti parietal cell antibodies without autoimmune gastritis', 'uveitis', 'vasculitis',
                    'antiphospholipid_syndrome', 'retinal vasculitis', 'undifferentiated connective tissue disease',
                    'conjunctivitis', 'pulmonary vasculopathy', 'connective tissue disease', 'crest syndrome', 'dry eyes', 'langerhans cell histiocytosis', 'positive rheumatoid factor',
                    'features of systemic sclerosis but autoantibody negative', 'lupus anticoagulant positive', 'overlap syndrome', 'raynauds_phenomenon', 'scleroderma')

infections_inflammatory_other <- c('aspargilloma', 'candidaemia with right atrial vegetation', 'cellulitis', 'chest infection', 'cocksackie b viral lung infection', 'atopic rhinoconjunctivitis',
                                   'glandular fever', 'myocarditis', 'myositis', 'osteomyelitis', 'thrombophlebitis', 'amyloidosis', 'sarcoidosis', 'calcified lymph nodes unknown cause (not sarcoidosis)')


thromboembolic_coagulation <- c('DVT','factor v leiden insufficiency', 'factor vii deficiency','heparin-induced thrombocytopenia', 'hereditary deficiency of factor x and xii', 'prothrombin (g20210a) mutation, heterozygote carrier', 
                                'protein c deficiency', 'occluded ivc', 'superior_vena_cava_syndrome', 'pulmonary embolism','protein s deficiency', 'von_willebrands_disease')
systemic_cardiovascular_disease_other <- c('FHx_cardiovasc', 'obesity', 'osa', 'peripheral_vascular_disease', 'renal_artery_stenosis')
dyslipidaemia <- c('dyslipidaemia')
htn <- c('hypertension', 'hypertensive cardiac disease (lv compliance disorder)')
neurological_disease_central <- c('1 heritable cerebral ataxia', 'abscess of brain', 'absence seizures', 'bells_palsy', 'brain_bleed', 'cerebellar ataxia', 'meningitis', 'tbi', 'viral encephalitis',
                                  'cerebral arteriovenous malformation', 'cerebral palsy', 'cerebrovascular disease', 'cva', 'diffuse cortical atrophy', 'ischaemic neuropathy', 'syncope', 'viral meningitis',
                                  'disorder of brain, unspecified', 'epilepsy', 'fronto temporal dementia', 'hemiparesis', 'hypoxic ischaemic encephalopathy', 'mononeuritis multiplex',
                                  'moya moya disease', 'multiple sclerosis', 'carotid endarterectomy', 'myasthena gravis', 'normal pressure hydrocephalus', 'parkinsons', 'cerebral atrophy', 'seizures', 'tia')
substance_abuse <- c('accidental overdose','alcohol abuse', 'cocaine_abuse', 'current smoker', 'drug abuse', 'drug induced cerebellar disease', 'ex-pipe smoker',
                     'heroin_abuse')
Chronic_nervous_system_disorder <-c('chronic fatigue syndrome')
psychosocial <- c('agoraphobia','anorexia', 'anxiety', 'avoidant personality disorder', 'bipolar', 'bulemia', 
                  'deliberate self_harm', 'depression', 'post traumatic stress disorder', 'psychosis', 'schizoaffective disorder', 'schizophrenia')
nephrological <- c('AKI', 'CKD', 'ectopic kidney', 'hydronephrosis', 'iga nephropathy', 'kidney congenitally absent, left', 'nephrocalcinosis','polycystic kidney disease', 'proteinuria', 'pyelonephritis')

#EdB: make separate bins genetic PAH and others
genetic_congenital_other <- c('alpha 1 antitrypsin deficiency', 'edward’ syndrome', 'genetic susceptibility',
                              'chromosomal abnormality, unspecified', 'chromosome_17q_variant', 'cohen syndrome', 'cutis marmorata telangectasia congenita',
                              'congenital abnormality of ivc','congenital asplenia', 'congenital macular retinopathy', 'congenital malformations of great veins', 'digeorge syndrome', 
                              'ehlers danlos', 'genetic carrier – for non-affected family member', 'genetic susceptibility to disease – for index case with known mutation', 'hereditary deafness',  
                              'kabuki syndrome', 'mcardle_disease', 'mthfr mutation carrier', 'pulmonary hamartoma left lower lobe', 'robertsonian translocation', 'translocation of chromosone 17-19', 'trisomy 21','turners syndrome')
genetic_congenital_pah <- c('BMPR2_variant', 'SMAD9_variant', 'TBX4_mutation','kcnk3 gene alteration of unknown significance', 'alk-1 gene mutation', 'ATP13A3_variant', 'bmp9', 'BMPR2_SMAD9_variant','EIF2AK4_varint','hereditary_haemorrhagic_telangiectasia', 'SMAD9_variant')
opthamological_vascular <- c('BRVO', 'central serious retinopathy (secondary to pph)', 'CRVO', 'diabetic_retinopathy', 'occular hypertension', 'RVO', 'retinopathy')

#now add the grouping cols to the actual data
comorbidity_cleaned <- comorb
comorbidity_cleaned$disease_class <- NA

comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% cv_electrophysiological, 'cv_electrophysiological', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% cv_ischaemic, 'cv_ischaemic', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% cv_heart_failure, 'cv_heart_failure', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% cv_structural, 'cv_structural', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% endocrine_general, 'endocrine_general', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% endocrine_thyroid, 'endocrine_thyroid', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% hepatological, 'hepatological', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% GI_other, 'GI_other', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% haematological_malignant_and_premalignant, 'haematological_malignant_and_premalignant', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% haemotological_immune, 'haemotological_immune', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% haematological_benign, 'haematological_benign', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% gynaecological_hormonal, 'gynaecological_hormonal', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% bone_marrow, 'bone_marrow', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% pulmonary, 'pulmonary', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% PAH_subtypes, 'PAH_subtypes', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% dermatological, 'dermatological', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% solid_malignancy, 'solid_malignancy', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% rheumatological, 'rheumatological', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% autoimmune_ctd, 'autoimmune_ctd_other', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% infections_inflammatory_other, 'infections_inflammatory_other', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% thromboembolic_coagulation, 'thromboembolic_coagulation', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% systemic_cardiovascular_disease_other, 'systemic_cardiovascular_disease_other', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% neurological_disease_central, 'neurological_disease_central', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% substance_abuse, 'substance_abuse', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% psychosocial, 'psychosocial', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% genetic_congenital_other, 'genetic_congenital_other', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% opthamological_vascular, 'opthamological_vascular', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% genetic_congenital_pah, 'genetic_congenital_pah', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% Chronic_nervous_system_disorder, 'Chronic_nervous_system_disorder', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% dyslipidaemia, 'dyslipidaemia', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% htn, 'htn', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% diabetes, 'diabetes', comorbidity_cleaned$disease_class)
comorbidity_cleaned$disease_class <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% cv_non_tricuspid_valvular_disease, 'cv_non_tricuspid_valvular_disease', comorbidity_cleaned$disease_class)



comorbidity_cleaned$cv_electrophysiological <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% cv_electrophysiological, 'yes', 'no')
comorbidity_cleaned$cv_ischaemic <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% cv_ischaemic, 'yes', 'no')
comorbidity_cleaned$cv_heart_failure <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% cv_heart_failure, 'yes', 'no')
comorbidity_cleaned$cv_structural <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% cv_structural, 'yes', 'no')
comorbidity_cleaned$endocrine_general <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% endocrine_general, 'yes', 'no')
comorbidity_cleaned$endocrine_thyroid <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% endocrine_thyroid, 'yes', 'no')
comorbidity_cleaned$hepatological <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% hepatological, 'yes', 'no')
comorbidity_cleaned$GI_other <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% GI_other, 'yes', 'no')
comorbidity_cleaned$haematological_malignant_and_premalignant <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% haematological_malignant_and_premalignant, 'yes', 'no')
comorbidity_cleaned$haemotological_immune <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% haemotological_immune, 'yes', 'no')
comorbidity_cleaned$haematological_benign <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% haematological_benign, 'yes', 'no')
comorbidity_cleaned$gynaecological_hormonal <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% gynaecological_hormonal, 'yes', 'no')
comorbidity_cleaned$bone_marrow <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% bone_marrow, 'yes', 'no')
comorbidity_cleaned$pulmonary <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% pulmonary, 'yes', 'no')
comorbidity_cleaned$PAH_subtypes <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% PAH_subtypes, 'yes', 'no')
comorbidity_cleaned$dermatological <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% dermatological, 'yes', 'no')
comorbidity_cleaned$solid_malignancy <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% solid_malignancy, 'yes', 'no')
comorbidity_cleaned$rheumatological <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% rheumatological, 'yes', 'no')
comorbidity_cleaned$autoimmune_ctd <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% autoimmune_ctd, 'yes', 'no')
comorbidity_cleaned$infections_inflammatory_other <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% infections_inflammatory_other, 'yes', 'no')
comorbidity_cleaned$thromboembolic_coagulation <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% thromboembolic_coagulation, 'yes', 'no')
comorbidity_cleaned$systemic_cardiovascular_disease_other <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% systemic_cardiovascular_disease_other, 'yes', 'no')
comorbidity_cleaned$neurological_disease_central <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% neurological_disease_central, 'yes', 'no')
comorbidity_cleaned$substance_abuse <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% substance_abuse, 'yes', 'no')
comorbidity_cleaned$psychosocial <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% psychosocial, 'yes', 'no')
comorbidity_cleaned$genetic_congenital_other <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% genetic_congenital_other, 'yes', 'no')
comorbidity_cleaned$opthamological_vascular <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% opthamological_vascular, 'yes', 'no')
comorbidity_cleaned$genetic_congenital_pah <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% genetic_congenital_pah, 'yes', 'no')
comorbidity_cleaned$Chronic_nervous_system_disorder <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% Chronic_nervous_system_disorder, 'yes', 'no')
comorbidity_cleaned$diabetes <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% diabetes, 'yes', 'no')
comorbidity_cleaned$cv_non_tricuspid_valvular_disease <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% cv_non_tricuspid_valvular_disease, 'yes', 'no')
comorbidity_cleaned$dyslipidaemia <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% dyslipidaemia, 'yes', 'no')
comorbidity_cleaned$htn <- ifelse(comorbidity_cleaned$cfh_comorbid_disease_diagnosis %in% htn, 'yes', 'no')


comorbidity_cleaned <- unique(comorbidity_cleaned)

write_rds(comorbidity_cleaned, file='cleaned_comorbidity_data_Vx_dateY.rds')

#==========================================================================================
#clean the clinical data at diagnosis (note: same script can be applied to follow-up data)
#however, checks included in script are needed
#clean the clinical data
clin_df2 <- data.checked
#subset df, remove certain cols that are double, have lots of NAs or are unuseful (like NT-pro-BNP precision)
subset_df <- clin_df2[,c(1, 6:14, 17:41, 50:67, 69:99, 101:296, 302:315, 330, 339:352)]
#eGFR does not seem to be right! 

#now make sure that UNK, etc are transformed into NA
#first transform the factors
subset_df[,c(1:9, 16, 24, 26:34, 39, 42, 47, 52, 57, 65, 71, 80, 89, 93, 98, 100, 104,106,108:111,115, 117:123, 130, 132, 136, 137, 139, 141:144, 146, 154, 190, 191, 203, 206, 214, 216:218, 228, 231, 240, 243:245, 255, 256, 267, 271, 275, 276, 280:293, 304, 305, 307:309)] <- lapply(subset_df[,c(1:9, 16, 24, 26:34, 39, 42, 47, 52, 57, 65, 71, 80, 89, 93, 98, 100, 104,106,108:111,115, 117:123, 130, 132, 136, 137, 139, 141:144, 146, 154, 190, 191, 203, 206, 214, 216:218, 228, 231, 240, 243:245, 255, 256, 267, 271, 275, 276, 280:293, 304, 305, 307:309)], as.factor)
subset_df[,c(12, 19, 25, 35, 43, 53,58, 66, 72, 81, 90, 94, 97, 114, 116, 131, 138, 145, 155,192,215, 239, 241, 242,246, 254, 257, 294, 296, 297 )] <- lapply(subset_df[,c(12, 19, 25, 35, 43, 53,58, 66, 72, 81, 90, 94, 97, 114, 116, 131, 138, 145, 155,192,215, 239, 241, 242,246, 254, 257, 294, 296, 297 )], as.character)

#do transformations earlier performed
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cbt_aab_ana', 'cbt_aab_anticardiolipin_ab', 'cbt_aab_antidsdna_ab', 'cbt_aab_antiscl70_ab', 'cbt_aab_anticentromere_ab', 'cbt_aab_antirho_ab', 'cbt_aab_antiena_ab', 'cbt_aab_anca', 'ep_1_done'), condition = ~.x == 'not done')
subset_df <- subset_df %>% replace_with_na_at(.vars = c('img_thromboembolic_disease', 'el_dominant_r_wave', 'el_rbbb', 'cfh_raynauds', 'cfe_hf_ascites', 'cfe_hf_ankle_swelling', 'cfe_spider_naevi', 'lf_supplemental_oxygen', 'hv_vasodilator_responder', 'ep_2_supplemental_oxygen', 'ep_1_done'), condition = ~.x == 'UNK')
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cfe_jugular_venous_pressure_select'), condition = ~.x == 'not-seen')
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cfe_jugular_venous_pressure_select'), condition = ~.x == 'not-recorded')
#add 0s in supplemental oxygen
subset_df$cfe_supplemental_oxygen_level <- ifelse(is.na(subset_df$cfe_supplemental_oxygen_level), 0, subset_df$cfe_supplemental_oxygen_level)

#exclude some values that seem impossible;
subset_df <- subset_df %>% replace_with_na_at(.vars = c('ec_left_atrial_size'), condition = ~.x > 200)
#also remove 0 and everyting smaller than 0
subset_df <- subset_df %>% replace_with_na_at(.vars = c('ec_left_atrial_size'), condition = ~.x <= 0)
subset_df <- subset_df %>% replace_with_na_at(.vars = c('ec_right_atrial_area'), condition = ~.x <= 0)
subset_df <- subset_df %>% replace_with_na_at(.vars = c('hb_rap_m'), condition = ~.x > 30)

#now the NAs are actual NAs
#make sure that the numeric data makes sense
lapply(subset_df, summary)

#to pvr_calc has a 0, svO2 has one with 14.90, CO has a value which is 0, same for PAP_d and PAP_S, PAWP and PCWP
#adjust these 
subset_df <- subset_df %>% replace_with_na_at(.vars = c('hb_pvr_calc', 'hb_cardiac_output_value_1', 'hb_pap_d', 'hb_pap_s', 'hb_pawp_m', 'hv_sa_o2', 'hb_cardiac_index_value_1', 'hv_pawp_m', 'hb_lvedp', 'hb_sp_mean', 'ep_2_anaer_petco2_at_kpa', 'ep_2_anaer_petco2_max_kpa', 'ep_2_derived_vo2_wr_slope', 'ep_2_peak_work', 'ep_2_rest_o2_pulse', 'ep_2_rest_spo2', 'ep_2_done'), condition = ~.x == 0)

#PVR is infinite in one case --> adjust
subset_df <- subset_df %>% replace_with_na_at(.vars = c('pvr'), condition = ~.x > 1000)
#PCWP is negative
subset_df <- subset_df %>% replace_with_na_at(.vars = c('pcwp_adj', 'hb_pvr_calc', 'hb_sa_o2', 'hb_pawp_m', 'el_qrs'), condition = ~.x < 0)
#lf_avg_saturation has minimum of 64%???
#EdB --> bin it, after checking not congenital heart disease patient.
#Glaswegian patient, no congenital heart disease or shunt registered
subset_df <- subset_df %>% replace_with_na_at(.vars = c('lf_avg_saturation'), condition = ~.x < 65)
#creat has impossibly high value --> exclude
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cbt_renal_creatinine_umolpl'), condition = ~.x > 2000)
subset_df <- subset_df %>% replace_with_na_at(.vars = c('hb_sv_o2', 'hb_sp_dias', 'ep_1_oxygen_saturation_pre'), condition = ~.x < 40)

#check again now
lapply(subset_df, summary)

#plot for visual inspection
num_df <- subset_df %>% select(where(is.numeric))

num_df %>% gather() %>% head()
ggplot(gather(num_df), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

#also re-check summary data
lapply(num_df, summary)

#remove values =<0
subset_df <- subset_df %>% replace_with_na_at(.vars = c('pcwp_adj', 'hv_pvr_calc', 'hv_sv_o2', 'hv_cardiac_output_value_1', 'hb_sa_o2', 'ep_1_corridor_length', 'el_qrs'), condition = ~.x <= 0)
#some values do not seem realistic
subset_df <- subset_df %>% replace_with_na_at(.vars = c('hb_pap_s'), condition = ~.x >1000)
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cbt_renal_creatinine_umolpl'), condition = ~.x <1)
subset_df <- subset_df %>% replace_with_na_at(.vars = c('bs_height'), condition = ~.x <10)
#EdB: double check PaO2 unit
#range is weird, not logical in mmHg --> bin
subset_df <- subset_df %>% replace_with_na_at(.vars = c('abg_acidity', 'abg_pao2'), condition = ~.x >40)
subset_df <- subset_df %>% replace_with_na_at(.vars = c('bs_bmi'), condition = ~.x >100)

#also re-check summary data
lapply(num_df, summary)

#explore distributions of variables with questionable units in more detail
hist(subset_df$cbt_card_ntprobnp_ngpl)
hist(subset_df$cbt_card_trop_ugpl)
hist(subset_df$bs_height)
hist(subset_df$abg_pao2)
#PaO2's of <5 seem not realistic to me (and tbh <8 are questionable I think...), same for this >20
subset_df <- subset_df %>% replace_with_na_at(.vars = c('abg_pao2'), condition = ~.x <5)
subset_df <- subset_df %>% replace_with_na_at(.vars = c('abg_pao2'), condition = ~.x >20)

#BSA <0.2 seems unlikely (even for neonates, filter this)
subset_df <- subset_df %>% replace_with_na_at(.vars = c('bs_bsa'), condition = ~.x <0.2)
#Haem/Haematocrit ratio of 0.0046 doesn't seem logical.....
hist(subset_df$cbt_haem_haematocrit_ratio)
#all the same unit, but probably something went wrong with conversion --> discuss with Mark

#ABG supplemetal oxygen level will be in both L and %....
#ABG acidity of 1.4 is impossible! Filter out anything <6
subset_df <- subset_df %>% replace_with_na_at(.vars = c('abg_acidity'), condition = ~.x <6)

#TIBC seems odd
hist(subset_df$cbt_iron_iron_bdg_cap_umolpl)
#distribution isn't horrible 

#urea seems odd
hist(subset_df$cbt_renal_urea_mmolpl)
#clinically still possible 
hist(subset_df$cbt_renal_creatinine_umolpl)
#creat <10 is just highly unlikely --> exclude 
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cbt_renal_creatinine_umolpl'), condition = ~.x <10)
#eGFR is wrong --> formula used probably inadequate! --> will re-calculate myself if needed
#diastolic BP <20 is impossible 
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cfe_bp_diastolic'), condition = ~.x <20)


subset_df <- subset_df %>% replace_with_na_at(.vars = c('ep_2_peak_spo2'), condition = ~.x <40)
subset_df <- subset_df %>% replace_with_na_at(.vars = c('hb_heart_rate'), condition = ~.x >300)
#LVEDP seem crazily high in max group
#same for max PAWP/PAP --> consider unit differences and not correctly transformed to clean df?
#general note: oxygen will be in l/min and in % --> making interpretation difficult...
#(cannot convert without knowing way of oxygen administration)
hist(subset_df$img_lvef)
#remove everything <10, but also, LVEF > 80 is unlikely 
subset_df <- subset_df %>% replace_with_na_at(.vars = c('img_lvef'), condition = ~.x <10)
hist(subset_df$img_lv_diastolic_vol)
hist(subset_df$img_lv_systolic_vol)
#diastolic volume cannot be >2x max systolic LV volume
#remove vars that high up
subset_df <- subset_df %>% replace_with_na_at(.vars = c('img_lv_diastolic_vol'), condition = ~.x >200)

#plot for visual inspection
num_df <- subset_df %>% select(where(is.numeric))

num_df %>% gather() %>% head()
ggplot(gather(num_df[,131:159]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

#also re-check summary data
lapply(num_df, summary)

#also check the non-numeric data
nonnum_df <- subset_df %>% select(where(is.character))
nonnum_df <- as.data.frame(lapply(nonnum_df, as.factor))
lapply(nonnum_df, summary)

#repeat this for factorial data
fact_df <- subset_df %>% select(where(is.factor))
lapply(fact_df, summary)

#vars where not done should be changed into NA
#cbt_sero_hep_b_sero
#cbt_sero_hiv_sero
#hb_study
#cbt_iron_transferrin_unit
#cbt_iron_ferritin_unit
#cbt_haem_hb_unit
#cbt_haem_haematocrit_unit
#cbt_haem_serum_protein
#cbt_card_urate_unit
#cbt_card_trop_unit
#cbt_card_ntprobnp_unit
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cbt_sero_hep_b_sero', 'cbt_sero_hiv_sero', 'hb_study', 'cbt_iron_transferrin_unit', 'cbt_iron_ferritin_unit', 'cbt_haem_hb_unit', 'cbt_haem_haematocrit_unit', 'cbt_haem_serum_protein', 'cbt_card_urate_unit', 'cbt_card_trop_unit', 'cbt_card_ntprobnp_unit'), condition = ~.x == 'not done')

#change UNK to NA for these vars
#hv_supplemental_oxygen
#ep_2_done
#ep_1_supplemental_oxygen
#el_done
#ec_done
#cfh_haemoptysis_hospitalization
#cfh_symptoms_onset_infection
#cfh_symptoms_onset
#cfe_supplemental_oxygen
#cfe_digital_clubbing
#abg_supplemental_oxygen
subset_df <- subset_df %>% replace_with_na_at(.vars = c('hv_supplemental_oxygen', 'ep_2_done', 'ep_1_supplemental_oxygen', 'el_done', 'ec_done', 'cfh_haemoptysis_hospitalization', 'cfh_symptoms_onset', 'cfh_symptoms_onset_infection', 'cfe_supplemental_oxygen', 'cfe_digital_clubbing', 'abg_supplemental_oxygen', 'hb_supplemental_oxygen'), condition = ~.x == 'UNK')


#in el_done, ec_done change 'no' to 'not done'
subset_df$el_done <- ifelse(subset_df$el_done == 'no', 'not done', subset_df$el_done)
subset_df$ec_done <- ifelse(subset_df$ec_done == 'no', 'not done', subset_df$ec_done)


#continue cleaning based on plots
lapply(num_df, summary)


#1) input eGFR
#add eGFR according to MDRD formula
subset_df$DOB <- as.Date(subset_df$DOB)
subset_df$cbt_renal_date <- as.Date(subset_df$cbt_renal_date)
subset_df$age_at_renal_bloods <- (subset_df$cbt_renal_date - subset_df$DOB)
subset_df[c('age_at_renal_bloods', 'day')] <- str_split_fixed(subset_df$age_at_renal_bloods, ' ', 2)
subset_df$age_at_renal_bloods <- as.numeric(subset_df$age_at_renal_bloods)
subset_df$age_at_renal_bloods <- subset_df$age_at_renal_bloods/365.2422

#calculate egfr (also adjust units; MDRD is in mL/min/1.73 m2)
#have to calculate as eGFR is not given for everyone with creatinine measured
#formula used from: https://www.kidney.org/content/mdrd-study-equation
subset_df$egfr_mdrd <- 175*((subset_df$cbt_renal_creatinine_umolpl/88.4)**-1.154) *((subset_df$age_at_renal_bloods)**-0.203)
#now correct for gender
subset_df$egfr_mdrd <- ifelse(subset_df$sex =='female', subset_df$egfr_mdrd*0.742, subset_df$egfr_mdrd*1)

summary(subset_df$egfr_mdrd)

#some really big eGFRs
t1 <- subset_df %>% select(id, age_diagnosis, cbt_renal_creatinine_umolpl, cbt_renal_creatinine_unit, egfr_mdrd)
t1 <- t1 %>% filter(egfr_mdrd > 150)
#sometimes creatinine of 200 is given eGFR of 900 
#in total 75 pts have eGFR >150

#the really high eGFRs are in kids --> assume eGFR cannot be >150 (for stats)
subset_df$egfr_mdrd <- ifelse(subset_df$egfr_mdrd >=150, 150, subset_df$egfr_mdrd)

#@Mark, surely PaO2 of <4 must be a venous/mixed gas? --> make it NA for now...
subset_df <- subset_df %>% replace_with_na_at(.vars = c('abg_pao2'), condition = ~.x <4)
#18 for PaO2 is high but in extreme hyperventilation/oxygen therapy not unthinkable...
#PaCO2 surely won't be <2???
subset_df <- subset_df %>% replace_with_na_at(.vars = c('abg_paco2'), condition = ~.x <2)

#ignore supplemental oxygen level --> both in % and in L/min --> without knowing device --> cannot convert
#ABG acidity now seems correct
#weights seem realistic --> low weights observed in neonates
#BSA/BMI/NT-Pro-BNP/Troponin all seem to make sense

#urate has a unit conversion problem!
#the units do not make sense!
#really small numbers are in umol/l in 2nd col but supposed to be in mmol/l
#for both units waaaaaay too small....
#not sure what to do? --> just discard?
#perhaps entered as umol/l but actually already in mmol/l and then converted wrongly?
#this would make a lot of sense as conversion is with a factor 1000
#Change
subset_df$cbt_card_urate_mmolpl <- ifelse(subset_df$cbt_card_urate_mmolpl <= 0.000910, subset_df$cbt_card_urate_mmolpl*1000, subset_df$cbt_card_urate_mmolpl)
hist(subset_df$cbt_card_urate_mmolpl)
#the distribution looks much more normal now too! 

#platelets --> seem to be okay, some weird outliers but not impossible
#RD%, WBC and

#H/H ratio likely error with both % and fraction (given some values >30)
subset_df$cbt_haem_haematocrit_ratio <- ifelse(subset_df$cbt_haem_haematocrit_ratio >30, subset_df$cbt_haem_haematocrit_ratio/100, subset_df$cbt_haem_haematocrit_ratio)
hist(subset_df$cbt_haem_haematocrit_ratio)
#some were probably a fraction but denoted as % and overcorrected
subset_df$cbt_haem_haematocrit_ratio <- ifelse(subset_df$cbt_haem_haematocrit_ratio <0.007, subset_df$cbt_haem_haematocrit_ratio*100, subset_df$cbt_haem_haematocrit_ratio)
hist(subset_df$cbt_haem_haematocrit_ratio, xlim = c(0,1))

#this makes more sense; only 1 pt with a value of around 4...
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cbt_haem_haematocrit_ratio'), condition = ~.x >3)


#for Hb values --> seems off --> select pts with weird values
df_test <- subset_df %>% filter(cbt_haem_hb_gpl < 80 | cbt_haem_hb_gpl > 1000)
df_test <- merge(data.clean.cohort, df_test, by='id')
#these are all UK patients..... So probably no international conversion problems
#probably a decimal error in the ones >1000 (some repeat visits show Hbs in range 10x less)
#similarly with the ones <20
subset_df$cbt_haem_hb_gpl <- ifelse(subset_df$cbt_haem_hb_gpl >1000, subset_df$cbt_haem_hb_gpl/10, subset_df$cbt_haem_hb_gpl)
subset_df$cbt_haem_hb_gpl <- ifelse(subset_df$cbt_haem_hb_gpl <20, subset_df$cbt_haem_hb_gpl*10, subset_df$cbt_haem_hb_gpl)
hist(subset_df$cbt_haem_hb_gpl)

#one kid with >800, no repeats, however, so far off the scale --> must be decimal error?
#kid also has no haematological malignancies recorded but has had surgery recorded--> anaemia more likely
subset_df$cbt_haem_hb_gpl <- ifelse(subset_df$cbt_haem_hb_gpl >800, subset_df$cbt_haem_hb_gpl/10, subset_df$cbt_haem_hb_gpl)
hist(subset_df$cbt_haem_hb_gpl)
#this gives much more realistic distributions! 

#CRP/sCRP makes sense
#iron makes sense, range is a bit broad though
#ferritin is possible range
#transferrin --> some high values, yet not impossible

#all cholesterol makes sense, apart from LDL
#all patients with LDL <1 (i.e. 0.2) have significant cardiovascular risk factors/cardiac events
#they also have high TC and fairly low LDL, some may be statin effects but not <0.05
#potentially --> converted from mmol/l one extra time if unit was denoted as mg/dl but actually was in mmol/l?? --> when coverting this gives normal values if
#on statins --> convert these with conversion factor back 
subset_df$cbt_lipids_ldl_cholesterol_mmolpl  <- ifelse(subset_df$cbt_lipids_ldl_cholesterol_mmolpl <0.05, subset_df$cbt_lipids_ldl_cholesterol_mmolpl*38.67, subset_df$cbt_lipids_ldl_cholesterol_mmolpl )
#note: this only converts 2 extreme outliers, more outliers are present.
hist(subset_df$cbt_lipids_ldl_cholesterol_mmolpl )
#triglycerides in normal range
#billirubin range can be possible
#LFTs are within expected range

#albumin quite a few <5 --> doesn't make sense even in chronic illness
#check
t <- subset_df %>% filter(cbt_liver_albumin_gpl <5)
t <- merge(data.clean.cohort, t, by='id')
#all Austrian patients --> will be a conversion error! 
subset_df$cbt_liver_albumin_gpl  <- ifelse(subset_df$cbt_liver_albumin_gpl <5, subset_df$cbt_liver_albumin_gpl*10, subset_df$cbt_liver_albumin_gpl)
hist(subset_df$cbt_liver_albumin_gpl)
#looks much more normal too
#total protein makes sense

#1 potassium of 1.8 recorded --> surely this cannot be in an outpatient
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cbt_renal_potassium_mmolpl'), condition = ~.x <2)

#urea is really high in some pts
t <- subset_df %>% filter(cbt_renal_urea_mmolpl >28)
t <- merge(data.clean.cohort, t, by='id')
#these patients are all from Graz --> probably systematic error with patients from Graz
#convert their labs from mg/dl to mmol/
#subset_df$cbt_renal_urea_mmolpl  <- ifelse(subset_df$cbt_renal_urea_mmolpl >28, subset_df$cbt_renal_urea_mmolpl*0.357, subset_df$cbt_renal_urea_mmolpl)
hist(subset_df$cbt_renal_urea_mmolpl)

a2 <- merge(subset_df, data.clean.cohort, by='id')
a2 <- a2 %>% filter(centre=='Graz')
hist(a2$cbt_renal_urea_mmolpl.x)
hist(a2$cbt_renal_creatinine_umolpl.x)
graz <- as.character(a2$id)

#given distribution of this in graz group --> normal, not bimodal. Must affect all patients from
#Graz --> do transformaion for all
subset_df$id <- as.character(subset_df$id)
subset_df$cbt_renal_urea_mmolpl  <- ifelse(subset_df$id %in% graz, subset_df$cbt_renal_urea_mmolpl*0.357, subset_df$cbt_renal_urea_mmolpl)
hist(subset_df$cbt_renal_urea_mmolpl)
subset_df$id <- as.factor(subset_df$id)

#ft4 and TSH check
t <- subset_df %>% filter(cbt_thyr_freet4_pmolpl >60)
t <- merge(data.clean.cohort, t, by='id')
#is a German patient, however, converting German units (ng/dl) to pmol/l doens't give sensible results --> keep in
#also check for low values
t <- subset_df %>% filter(cbt_thyr_freet4_pmolpl <6)
t <- merge(data.clean.cohort, t, by='id')
#this makes sense

t <- subset_df %>% filter(cbt_thyr_tsh_mupl >20)
t <- merge(data.clean.cohort, t, by='id')
#the ones with high TSH are UK based
t <- subset_df %>% filter(cbt_thyr_tsh_mupl <0.01)
t <- merge(data.clean.cohort, t, by='id')
#really low TSH values are all from Graz --> their units are 1000x smaller than ours (as they use ML)
subset_df$cbt_thyr_tsh_mupl  <- ifelse(subset_df$cbt_thyr_tsh_mupl <0.01, subset_df$cbt_thyr_tsh_mupl*1000, subset_df$cbt_thyr_tsh_mupl)
hist(subset_df$cbt_thyr_tsh_mupl)
#looks somewhat better! 

#rest SpO2 is okay; albeit somewhat low
#systolic BP is okay
#MAP is okay
#diastolic BP is okay
#HR has one of 17 --> unlikely --> exclude
subset_df <- subset_df %>% replace_with_na_at(.vars = c('cfe_heart_rate'), condition = ~.x <20)
#JVP with max of 20 seems acceptable
#left atrial size is probably incorrect due to a mix of volume and size but no clear cut in distrubution --> DO NOT USE
#QRS is possible now but not always realistic
subset_df$ec_tricuspid_apse <- ifelse(subset_df$ec_tricuspid_apse > 5, subset_df$ec_tricuspid_apse/10, subset_df$ec_tricuspid_apse)

#corridor length and distance in metres should be fine
#SpO2 pre and post sometimes are reallty low! Exclude all < 65
subset_df <- subset_df %>% replace_with_na_at(.vars = c('ep_1_oxygen_saturation_post'), condition = ~.x <65)
#ignoring supplemental oxygen level (sometimes % and sometimes l/min)

num_df <- subset_df %>% select(where(is.numeric))
lapply(num_df, summary)

#height/weight during EP2 make sense
#I think FEV1 makese sense (difficult to say without %)
#VC makes sense
#test bp
a <- num_df$cfe_bp_systolic - num_df$cfe_bp_diastolic
summary(a)
a2 <- num_df$ep_2_peak_sys - num_df$ep_2_peak_dias
summary(a2)
#this all makes sense! No negative values! 
#MAP during ep2 makes sense too
subset_df <- subset_df %>% replace_with_na_at(.vars = c('ep_2_anaer_eqco2_at'), condition = ~.x <2)
#other CPET haemodynamics seem fine to me
#I do not know what to do with peak rer/peak work/peak ventilation in CPET... difficult to grasp reference range
#same for: ep_2_derived_oues, ep_2_derived_ve_vco2_slope,ep_2_derived_vo2_wr_slope --> histograms do seem fairly realistic with some outliers

#KPA and mmHg are used interchangebly in EtCO2 --> doesn't make sense --> everything <10 is in KPA
subset_df$ep_2_anaer_petco2_at_kpa  <- ifelse(subset_df$ep_2_anaer_petco2_at_kpa >10, subset_df$ep_2_anaer_petco2_at_kpa/7.50061683, subset_df$ep_2_anaer_petco2_at_kpa)
hist(subset_df$ep_2_anaer_petco2_at_kpa)
#one variable still is 0.3 --> impossible even in KPA
subset_df <- subset_df %>% replace_with_na_at(.vars = c('ep_2_anaer_petco2_at_kpa'), condition = ~.x <0.5)

subset_df$ep_2_anaer_petco2_max_kpa  <- ifelse(subset_df$ep_2_anaer_petco2_max_kpa >10, subset_df$ep_2_anaer_petco2_max_kpa/7.50061683, subset_df$ep_2_anaer_petco2_max_kpa)
hist(subset_df$ep_2_anaer_petco2_max_kpa)
#this now makes much more sense! 
subset_df$ep_2_peak_petco2_kpa  <- ifelse(subset_df$ep_2_peak_petco2_kpa >10, subset_df$ep_2_peak_petco2_kpa/7.50061683, subset_df$ep_2_peak_petco2_kpa)
hist(subset_df$ep_2_peak_petco2_kpa)


#hb_heart rate makes sense and so do blood pressures
#PAWP of 90 and 140 do not make sense to me --> remove
subset_df <- subset_df %>% replace_with_na_at(.vars = c('hb_pawp_m'), condition = ~.x >85)
#wedge of 1 is really low --> @MRT: leave in?
hist(subset_df$hb_pawp_m)

#LVEDP makes sense except 1 of 134 (has low PAWP of 12...)
subset_df <- subset_df %>% replace_with_na_at(.vars = c('hb_lvedp'), condition = ~.x >100)
#some really high PAP S/D/M all present --> however, not impossible 
#RAP range seems possible, like CO and CI
#one really low CO though;
t <- subset_df %>% filter(hb_cardiac_output_value_1 < 1)
#really low value in a 6yr old child (0.7l/min), quite a few people have CO of <2 though... and some up to 11
#decided not to clean as it could still be possible

#SVO2 > 90 is highly unlikely --> remove
subset_df <- subset_df %>% replace_with_na_at(.vars = c('hb_sv_o2'), condition = ~.x >90)

#SVO2 > 90 is highly unlikely --> remove
subset_df <- subset_df %>% replace_with_na_at(.vars = c('hv_sv_o2'), condition = ~.x >90)
#this makes more sense

#PVR calc is weirdly low in some pts
t <- subset_df %>% filter(hb_pvr_calc < 40)
t <- merge(data.clean.cohort, t, by='id')
#they're all from Pavia and Giessen --> likely in mmHg.min/l
#so likely in WU --> multiply by 80 to get in dynes
subset_df$hb_pvr_calc  <- ifelse(subset_df$hb_pvr_calc < 40, subset_df$hb_pvr_calc*80, subset_df$hb_pvr_calc)
hist(subset_df$hb_pvr_calc)

#ignore supplemental oxygen levels

#one HV diastolic BP > systolic BP..
subset_df$hv_sp_syst  <- ifelse(subset_df$hv_sp_syst < subset_df$hv_sp_dias, NA, subset_df$hv_sp_syst)
hist(subset_df$hv_sp_syst)

#remove paired measurements from just now
subset_df$hv_sp_dias  <- ifelse(is.na(subset_df$hv_sp_syst), NA, subset_df$hv_sp_dias)
hist(subset_df$hv_sp_dias)
#values now make sense
#other haemodynamics in HV study make sense to me, ranges of CO/CI are comparable with other
#measurement data too
#again low SaO2 of 65% --> is unlikely.... -->  keep in for now

#PVR calc again has weird discrepancies
t <- subset_df %>% filter(hv_pvr_calc < 40)
t <- merge(data.clean.cohort, t, by='id')
#in this case all from Paris, Glasgow and Pavia --> assume this is systematic bias again --> so
#probably in WU --> change
subset_df$hv_pvr_calc  <- ifelse(subset_df$hv_pvr_calc < 40, subset_df$hv_pvr_calc*80, subset_df$hv_pvr_calc)
hist(subset_df$hv_pvr_calc)

#ignore supplemental oxygen (again as it's not informative)

#HR on imaging not always correct
subset_df <- subset_df %>% replace_with_na_at(.vars = c('img_heart_rate'), condition = ~.x <10)
#RVEF cannot be 1
subset_df <- subset_df %>% replace_with_na_at(.vars = c('img_rvef'), condition = ~.x <5)

#LVEF makes sense, LV systolic/diastolic volume don't make sense at 1 but it's because it's associaited with weird LVEF
#remove volumes if EF is na
subset_df$img_lv_systolic_vol  <- ifelse(is.na(subset_df$img_lvef), NA, subset_df$img_lv_systolic_vol)
subset_df$img_lv_diastolic_vol  <- ifelse(is.na(subset_df$img_lvef), NA, subset_df$img_lv_diastolic_vol)

subset_df$img_rv_systolic_vol  <- ifelse(is.na(subset_df$img_rvef), NA, subset_df$img_rv_systolic_vol)
subset_df$img_rv_diastolic_vol  <- ifelse(is.na(subset_df$img_rvef), NA, subset_df$img_rv_diastolic_vol)

num_df <- subset_df %>% select(where(is.numeric))
lapply(num_df, summary)

t <- num_df[,c(1,134:158)]
#FEV1 makes sort of sense --> weird outliers, but also difference %pred, so may be true?
#same for FVC --> tends to be slightly larger, but sometimes smaller than FEV1 --> @Mark is this possible?
#however 1 pt has FVC of 415! --> probably decimal error as %pred = 100
#--> adjust
subset_df$lf_fvc_liters  <- ifelse(subset_df$lf_fvc_liters > 400, subset_df$lf_fvc_liters/100, subset_df$lf_fvc_liters)
#this has consequences for one FEV1/FVC
subset_df$fev1_fvc  <- ifelse(subset_df$fev1_fvc < 0.009, subset_df$fev1_fvc*100, subset_df$fev1_fvc)
#TLC data makes sense
#KCO roughly makes sense, matches with % too, but some really high ones too
t2 <- subset_df %>% filter(lf_kco_mmol> 4)
t2 <- merge(data.clean.cohort, t2, by='id')
#high ones are mixed UK/Dutch --> so unlikely to be a unit problem.
#VA seems to make sense
#average Saturation again has some really low ones --> not sure if we should exclude SpO2 <65%?
#ignore oxygen level again

#new wedge/PVR make sense
#CI has one 0 --> not right
subset_df <- subset_df %>% replace_with_na_at(.vars = c('ci'), condition = ~.x <=0)

#is PP pulse pressure? and what are CA and RC? have some infinite values too
#6mwd seems to make sense to me
subset_df$spo2_diff_6mwd <- subset_df$ep_1_oxygen_saturation_pre - subset_df$ep_1_oxygen_saturation_post

#EdB paused here on 24-11-2023
num_df <- subset_df %>% select(where(is.numeric))

num_df %>% gather() %>% head()
ggplot(gather(num_df[,c(151:162)]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

write_rds(subset_df, file='v4_clean_clinical_data_first_visit_15Dec23.rds')


