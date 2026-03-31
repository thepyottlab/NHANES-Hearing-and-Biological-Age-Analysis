# Required packages
library(haven)
library(tidyverse)
library(here)
library(flexsurv)
# Set path to working directory
# setwd('/Users/landyqu/Desktop/NHANES Project/Data')

# 1999-2000 import --------------------------------------------------------
demographics_1999 <- read_xpt(here("Data", "Raw data", "Demographic 1999-2000.XPT"))
audiometry_1999 <- read_xpt(here("Data", "Raw data", "Audiometry 1999-2000.XPT"))
biochem_1999 <- read_xpt(here("Data", "Raw data", "Biochemistry 1999-2000.XPT"))
BP_1999 <- read_xpt(here("Data", "Raw data", "BP 1999-2000.XPT"))
CBC_1999 <- read_xpt(here("Data", "Raw data", "CBC 1999-2000.XPT"))
CRP_1999 <- read_xpt(here("Data", "Raw data", "CRP 1999-2000.XPT"))
hba1c_1999 <- read_xpt(here("Data", "Raw data", "Glycohemoglobin 1999-2000.XPT"))
totcholesterol_1999 <- read_xpt(here("Data", "Raw data", "Total cholesterol 1999-2000.XPT"))
telomere_1999 <- read_xpt(here("Data", "Raw data", "Telomere 1999-2000.XPT"))
cardiovas_1999 <- read_xpt(here("Data", "Raw data", "Medical Conditions Q 1999-2000.XPT"))
smoking_1999 <- read_xpt(here("Data", "Raw data", "Smoking Q 1999-2000.XPT"))
diabetes_1999 <- read_xpt(here("Data", "Raw data", "Diabetes Q 1999-2000.XPT"))
hypertension_1999 <- read_xpt(here("Data", "Raw data", "BP & Cholesterol Q 1999-2000.XPT"))
bmi_1999 <- read_xpt(here("Data", "Raw data", "Body Measures 1999-2000.XPT"))
ckd_1999 <- read_xpt(here("Data", "Raw data", "Kidney Conditions Q 1999-2000.XPT"))
hearing_1999 <- read_xpt(here("Data", "Raw data", "Hearing Q 1999-2000.XPT"))

# Select variables needed
demographics_1999 <- demographics_1999 %>% dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG, RIDRETH1, DMDEDUC2, INDFMINC)
audiometry_1999 <- audiometry_1999 %>% dplyr::select(SEQN, AUXU500R, AUXU1K1R, AUXU1K2R, AUXU2KR, AUXU3KR, AUXU4KR, AUXU6KR, AUXU8KR, AUXU500L, AUXU1K1L, AUXU1K2L, AUXU2KL, AUXU3KL, AUXU4KL, AUXU6KL, AUXU8KL)
biochem_1999 <- biochem_1999 %>% dplyr::select(SEQN, LBXSAL, LBXSAPSI, LBXSBU, LBXSGL, LBXSUA, LBXSCR, LBDSALSI, LBDSCRSI)
BP_1999 <- BP_1999 %>% dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)
CBC_1999 <- CBC_1999 %>% dplyr::select(SEQN, LBXWBCSI, LBXLYPCT, LBXMCVSI, LBXRDW)
CRP_1999 <- CRP_1999 %>% dplyr::select(SEQN, LBXCRP)
hba1c_1999 <- hba1c_1999 %>% dplyr::select(SEQN, LBXGH)
totcholesterol_1999 <- totcholesterol_1999 %>% dplyr::select(SEQN, LBXTC)
telomere_1999 <- telomere_1999 %>% dplyr::select(SEQN, TELOMEAN)
cardiovas_1999 <- cardiovas_1999 %>% dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F)
smoking_1999 <- smoking_1999 %>% dplyr::select(SEQN, SMQ020)
diabetes_1999 <- diabetes_1999 %>% dplyr::select(SEQN, DIQ010)
hypertension_1999 <- hypertension_1999 %>% dplyr::select(SEQN, BPQ020)
bmi_1999 <- bmi_1999 %>% dplyr::select(SEQN, BMXBMI)
ckd_1999 <- ckd_1999 %>% dplyr::select(SEQN, KIQ020)
hearing_1999 <- hearing_1999 %>% dplyr::select(SEQN, AUQ210, AUQ230)

# Combine data frames into dataset
require(purrr)
require(dplyr)
dataset_1999 <- list(
  demographics_1999, audiometry_1999, biochem_1999, BP_1999, CBC_1999,
  CRP_1999, hba1c_1999, totcholesterol_1999, telomere_1999, cardiovas_1999,
  smoking_1999, diabetes_1999, hypertension_1999, bmi_1999, ckd_1999, hearing_1999
) %>%
  reduce(left_join, by = "SEQN")

# Change sequence number to add year of data
dataset_1999$sampleID <- paste0("1999_", dataset_1999$SEQN)

# Add new column with just year number
dataset_1999$cohort <- 1999

# Exclude age >= 85 or 80 depending on year
dataset_1999 <- subset(dataset_1999, dataset_1999$RIDAGEYR < 85)

# Calculate average SBP
dataset_1999$sbp <- rowMeans(dataset_1999[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)

# Standardize creatinine per NHANES 1999-2000 analytic notes (https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB18.htm#LBXSCR)
dataset_1999$creat <- dataset_1999$LBXSCR * 1.013 + 0.147
dataset_1999$creat_umol <- dataset_1999$creat * 88.4

# Rename columns
dataset_1999 <- dataset_1999 %>%
  rename(
    age = RIDAGEYR,
    gender = RIAGENDR,
    pregnant = RIDEXPRG,
    race = RIDRETH1,
    education = DMDEDUC2,
    income = INDFMINC,
    audio_right_500 = AUXU500R,
    audio_right_1000 = AUXU1K1R,
    audio_right_1000_retest = AUXU1K2R,
    audio_right_2000 = AUXU2KR,
    audio_right_3000 = AUXU3KR,
    audio_right_4000 = AUXU4KR,
    audio_right_6000 = AUXU6KR,
    audio_right_8000 = AUXU8KR,
    audio_left_500 = AUXU500L,
    audio_left_1000 = AUXU1K1L,
    audio_left_1000_retest = AUXU1K2L,
    audio_left_2000 = AUXU2KL,
    audio_left_3000 = AUXU3KL,
    audio_left_4000 = AUXU4KL,
    audio_left_6000 = AUXU6KL,
    audio_left_8000 = AUXU8KL,
    albumin = LBXSAL,
    albumin_gL = LBDSALSI,
    alp = LBXSAPSI,
    bun = LBXSBU,
    glucose = LBXSGL,
    uap = LBXSUA,
    wbc = LBXWBCSI,
    lymph = LBXLYPCT,
    mcv = LBXMCVSI,
    rdw = LBXRDW,
    crp = LBXCRP,
    hba1c = LBXGH,
    totchol = LBXTC,
    telolength = TELOMEAN,
    bmi = BMXBMI,
    chf = MCQ160B,
    cad = MCQ160C,
    angina = MCQ160D,
    mi = MCQ160E,
    stroke = MCQ160F,
    smoking = SMQ020,
    diabetes = DIQ010,
    htn = BPQ020,
    kid_failure = KIQ020,
    firearm = AUQ210,
    offwork_exposure = AUQ230
  )

# 2001-2002 import --------------------------------------------------------
demographics_2001 <- read_xpt(here("Data", "Raw data", "Demographic 2001-2002.XPT"))
audiometry_2001 <- read_xpt(here("Data", "Raw data", "Audiometry 2001-2002.XPT"))
biochem_2001 <- read_xpt(here("Data", "Raw data", "Biochemistry 2001-2002.XPT"))
BP_2001 <- read_xpt(here("Data", "Raw data", "BP 2001-2002.XPT"))
CBC_2001 <- read_xpt(here("Data", "Raw data", "CBC 2001-2002.XPT"))
CRP_2001 <- read_xpt(here("Data", "Raw data", "CRP 2001-2002.XPT"))
hba1c_2001 <- read_xpt(here("Data", "Raw data", "Glycohemoglobin 2001-2002.XPT"))
totcholesterol_2001 <- read_xpt(here("Data", "Raw data", "Total cholesterol 2001-2002.XPT"))
telomere_2001 <- read_xpt(here("Data", "Raw data", "Telomere 2001-2002.XPT"))
cardiovas_2001 <- read_xpt(here("Data", "Raw data", "Medical Conditions Q 2001-2002.XPT"))
smoking_2001 <- read_xpt(here("Data", "Raw data", "Smoking Q 2001-2002.XPT"))
diabetes_2001 <- read_xpt(here("Data", "Raw data", "Diabetes Q 2001-2002.XPT"))
hypertension_2001 <- read_xpt(here("Data", "Raw data", "BP & Cholesterol Q 2001-2002.XPT"))
bmi_2001 <- read_xpt(here("Data", "Raw data", "Body Measures 2001-2002.XPT"))
ckd_2001 <- read_xpt(here("Data", "Raw data", "Kidney Conditions Q 2001-2002.XPT"))
hearing_2001 <- read_xpt(here("Data", "Raw data", "Hearing Q 2001-2002.XPT"))

demographics_2001 <- demographics_2001 %>% dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG, RIDRETH1, DMDEDUC2, INDFMINC)
audiometry_2001 <- audiometry_2001 %>% dplyr::select(SEQN, AUXU500R, AUXU1K1R, AUXU1K2R, AUXU2KR, AUXU3KR, AUXU4KR, AUXU6KR, AUXU8KR, AUXU500L, AUXU1K1L, AUXU1K2L, AUXU2KL, AUXU3KL, AUXU4KL, AUXU6KL, AUXU8KL)
biochem_2001 <- biochem_2001 %>% dplyr::select(SEQN, LBXSAL, LBDSAPSI, LBXSBU, LBXSGL, LBXSUA, LBDSCR, LBDSALSI, LBDSCRSI)
BP_2001 <- BP_2001 %>% dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)
CBC_2001 <- CBC_2001 %>% dplyr::select(SEQN, LBXWBCSI, LBXLYPCT, LBXMCVSI, LBXRDW)
CRP_2001 <- CRP_2001 %>% dplyr::select(SEQN, LBXCRP)
hba1c_2001 <- hba1c_2001 %>% dplyr::select(SEQN, LBXGH)
totcholesterol_2001 <- totcholesterol_2001 %>% dplyr::select(SEQN, LBXTC)
telomere_2001 <- telomere_2001 %>% dplyr::select(SEQN, TELOMEAN)
cardiovas_2001 <- cardiovas_2001 %>% dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F)
smoking_2001 <- smoking_2001 %>% dplyr::select(SEQN, SMQ020)
diabetes_2001 <- diabetes_2001 %>% dplyr::select(SEQN, DIQ010)
hypertension_2001 <- hypertension_2001 %>% dplyr::select(SEQN, BPQ020)
bmi_2001 <- bmi_2001 %>% dplyr::select(SEQN, BMXBMI)
ckd_2001 <- ckd_2001 %>% dplyr::select(SEQN, KIQ022, KIQ025)
hearing_2001 <- hearing_2001 %>% dplyr::select(SEQN, AUQ210, AUQ230)

require(purrr)
require(dplyr)
dataset_2001 <- list(
  demographics_2001, audiometry_2001, biochem_2001, BP_2001, CBC_2001,
  CRP_2001, hba1c_2001, totcholesterol_2001, telomere_2001, cardiovas_2001,
  smoking_2001, diabetes_2001, hypertension_2001, bmi_2001, ckd_2001, hearing_2001
) %>%
  reduce(left_join, by = "SEQN")

dataset_2001$sampleID <- paste0("2001_", dataset_2001$SEQN)

dataset_2001$cohort <- 2001

dataset_2001 <- subset(dataset_2001, dataset_2001$RIDAGEYR < 85)

dataset_2001$sbp <- rowMeans(dataset_2001[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)

dataset_2001 <- dataset_2001 %>%
  rename(
    age = RIDAGEYR,
    gender = RIAGENDR,
    pregnant = RIDEXPRG,
    race = RIDRETH1,
    education = DMDEDUC2,
    income = INDFMINC,
    audio_right_500 = AUXU500R,
    audio_right_1000 = AUXU1K1R,
    audio_right_1000_retest = AUXU1K2R,
    audio_right_2000 = AUXU2KR,
    audio_right_3000 = AUXU3KR,
    audio_right_4000 = AUXU4KR,
    audio_right_6000 = AUXU6KR,
    audio_right_8000 = AUXU8KR,
    audio_left_500 = AUXU500L,
    audio_left_1000 = AUXU1K1L,
    audio_left_1000_retest = AUXU1K2L,
    audio_left_2000 = AUXU2KL,
    audio_left_3000 = AUXU3KL,
    audio_left_4000 = AUXU4KL,
    audio_left_6000 = AUXU6KL,
    audio_left_8000 = AUXU8KL,
    albumin = LBXSAL,
    albumin_gL = LBDSALSI,
    alp = LBDSAPSI,
    bun = LBXSBU,
    glucose = LBXSGL,
    uap = LBXSUA,
    creat = LBDSCR,
    creat_umol = LBDSCRSI,
    wbc = LBXWBCSI,
    lymph = LBXLYPCT,
    mcv = LBXMCVSI,
    rdw = LBXRDW,
    crp = LBXCRP,
    hba1c = LBXGH,
    totchol = LBXTC,
    telolength = TELOMEAN,
    bmi = BMXBMI,
    chf = MCQ160B,
    cad = MCQ160C,
    angina = MCQ160D,
    mi = MCQ160E,
    stroke = MCQ160F,
    smoking = SMQ020,
    diabetes = DIQ010,
    htn = BPQ020,
    kid_failure = KIQ022,
    dialysis = KIQ025,
    firearm = AUQ210,
    offwork_exposure = AUQ230
  )

# 2003-2004 import --------------------------------------------------------
demographics_2003 <- read_xpt(here("Data", "Raw data", "Demographic 2003-2004.XPT"))
audiometry_2003 <- read_xpt(here("Data", "Raw data", "Audiometry 2003-2004.XPT"))
biochem_2003 <- read_xpt(here("Data", "Raw data", "Biochemistry 2003-2004.XPT"))
BP_2003 <- read_xpt(here("Data", "Raw data", "BP 2003-2004.XPT"))
CBC_2003 <- read_xpt(here("Data", "Raw data", "CBC 2003-2004.XPT"))
CRP_2003 <- read_xpt(here("Data", "Raw data", "CRP 2003-2004.XPT"))
hba1c_2003 <- read_xpt(here("Data", "Raw data", "Glycohemoglobin 2003-2004.XPT"))
totcholesterol_2003 <- read_xpt(here("Data", "Raw data", "Total cholesterol 2003-2004.XPT"))
cardiovas_2003 <- read_xpt(here("Data", "Raw data", "Medical Conditions Q 2003-2004.XPT"))
smoking_2003 <- read_xpt(here("Data", "Raw data", "Smoking Q 2003-2004.XPT"))
diabetes_2003 <- read_xpt(here("Data", "Raw data", "Diabetes Q 2003-2004.XPT"))
hypertension_2003 <- read_xpt(here("Data", "Raw data", "BP & Cholesterol Q 2003-2004.XPT"))
bmi_2003 <- read_xpt(here("Data", "Raw data", "Body Measures 2003-2004.XPT"))
ckd_2003 <- read_xpt(here("Data", "Raw data", "Kidney Conditions Q 2003-2004.XPT"))
hearing_2003 <- read_xpt(here("Data", "Raw data", "Hearing Q 2003-2004.XPT"))

demographics_2003 <- demographics_2003 %>% dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG, RIDRETH1, DMDEDUC2, INDFMINC)
audiometry_2003 <- audiometry_2003 %>% dplyr::select(SEQN, AUXU500R, AUXU1K1R, AUXU1K2R, AUXU2KR, AUXU3KR, AUXU4KR, AUXU6KR, AUXU8KR, AUXU500L, AUXU1K1L, AUXU1K2L, AUXU2KL, AUXU3KL, AUXU4KL, AUXU6KL, AUXU8KL)
biochem_2003 <- biochem_2003 %>% dplyr::select(SEQN, LBXSAL, LBXSAPSI, LBXSBU, LBXSGL, LBXSUA, LBXSCR, LBDSALSI, LBDSCRSI)
BP_2003 <- BP_2003 %>% dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)
CBC_2003 <- CBC_2003 %>% dplyr::select(SEQN, LBXWBCSI, LBXLYPCT, LBXMCVSI, LBXRDW)
CRP_2003 <- CRP_2003 %>% dplyr::select(SEQN, LBXCRP)
hba1c_2003 <- hba1c_2003 %>% dplyr::select(SEQN, LBXGH)
totcholesterol_2003 <- totcholesterol_2003 %>% dplyr::select(SEQN, LBXTC)
cardiovas_2003 <- cardiovas_2003 %>% dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F)
smoking_2003 <- smoking_2003 %>% dplyr::select(SEQN, SMQ020)
diabetes_2003 <- diabetes_2003 %>% dplyr::select(SEQN, DIQ010)
hypertension_2003 <- hypertension_2003 %>% dplyr::select(SEQN, BPQ020)
bmi_2003 <- bmi_2003 %>% dplyr::select(SEQN, BMXBMI)
ckd_2003 <- ckd_2003 %>% dplyr::select(SEQN, KIQ022, KIQ025)
hearing_2003 <- hearing_2003 %>% dplyr::select(SEQN, AUQ210, AUQ230)

require(purrr)
require(dplyr)
dataset_2003 <- list(
  demographics_2003, audiometry_2003, biochem_2003, BP_2003, CBC_2003,
  CRP_2003, hba1c_2003, totcholesterol_2003, cardiovas_2003, smoking_2003,
  diabetes_2003, hypertension_2003, bmi_2003, ckd_2003, hearing_2003
) %>%
  reduce(left_join, by = "SEQN")

dataset_2003$sampleID <- paste0("2003_", dataset_2003$SEQN)

dataset_2003$cohort <- 2003

dataset_2003 <- subset(dataset_2003, dataset_2003$RIDAGEYR < 85)

dataset_2003$sbp <- rowMeans(dataset_2003[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)

dataset_2003 <- dataset_2003 %>%
  rename(
    age = RIDAGEYR,
    gender = RIAGENDR,
    pregnant = RIDEXPRG,
    race = RIDRETH1,
    education = DMDEDUC2,
    income = INDFMINC,
    audio_right_500 = AUXU500R,
    audio_right_1000 = AUXU1K1R,
    audio_right_1000_retest = AUXU1K2R,
    audio_right_2000 = AUXU2KR,
    audio_right_3000 = AUXU3KR,
    audio_right_4000 = AUXU4KR,
    audio_right_6000 = AUXU6KR,
    audio_right_8000 = AUXU8KR,
    audio_left_500 = AUXU500L,
    audio_left_1000 = AUXU1K1L,
    audio_left_1000_retest = AUXU1K2L,
    audio_left_2000 = AUXU2KL,
    audio_left_3000 = AUXU3KL,
    audio_left_4000 = AUXU4KL,
    audio_left_6000 = AUXU6KL,
    audio_left_8000 = AUXU8KL,
    albumin = LBXSAL,
    albumin_gL = LBDSALSI,
    alp = LBXSAPSI,
    bun = LBXSBU,
    glucose = LBXSGL,
    uap = LBXSUA,
    creat = LBXSCR,
    creat_umol = LBDSCRSI,
    wbc = LBXWBCSI,
    lymph = LBXLYPCT,
    mcv = LBXMCVSI,
    rdw = LBXRDW,
    crp = LBXCRP,
    hba1c = LBXGH,
    totchol = LBXTC,
    bmi = BMXBMI,
    chf = MCQ160B,
    cad = MCQ160C,
    angina = MCQ160D,
    mi = MCQ160E,
    stroke = MCQ160F,
    smoking = SMQ020,
    diabetes = DIQ010,
    htn = BPQ020,
    kid_failure = KIQ022,
    dialysis = KIQ025,
    firearm = AUQ210,
    offwork_exposure = AUQ230
  )

# 2005-2006 import --------------------------------------------------------
demographics_2005 <- read_xpt(here("Data", "Raw data", "Demographic 2005-2006.XPT"))
audiometry_2005 <- read_xpt(here("Data", "Raw data", "Audiometry 2005-2006.XPT"))
biochem_2005 <- read_xpt(here("Data", "Raw data", "Biochemistry 2005-2006.XPT"))
BP_2005 <- read_xpt(here("Data", "Raw data", "BP 2005-2006.XPT"))
CBC_2005 <- read_xpt(here("Data", "Raw data", "CBC 2005-2006.XPT"))
CRP_2005 <- read_xpt(here("Data", "Raw data", "CRP 2005-2006.XPT"))
hba1c_2005 <- read_xpt(here("Data", "Raw data", "Glycohemoglobin 2005-2006.XPT"))
totcholesterol_2005 <- read_xpt(here("Data", "Raw data", "Total cholesterol 2005-2006.XPT"))
cardiovas_2005 <- read_xpt(here("Data", "Raw data", "Medical Conditions Q 2005-2006.XPT"))
smoking_2005 <- read_xpt(here("Data", "Raw data", "Smoking Q 2005-2006.XPT"))
diabetes_2005 <- read_xpt(here("Data", "Raw data", "Diabetes Q 2005-2006.XPT"))
hypertension_2005 <- read_xpt(here("Data", "Raw data", "BP & Cholesterol Q 2005-2006.XPT"))
bmi_2005 <- read_xpt(here("Data", "Raw data", "Body Measures 2005-2006.XPT"))
ckd_2005 <- read_xpt(here("Data", "Raw data", "Kidney Conditions Q 2005-2006.XPT"))
hearing_2005 <- read_xpt(here("Data", "Raw data", "Hearing Q 2005-2006.XPT"))

demographics_2005 <- demographics_2005 %>% dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG, RIDRETH1, DMDEDUC2, INDFMINC)
audiometry_2005 <- audiometry_2005 %>% dplyr::select(SEQN, AUXU500R, AUXU1K1R, AUXU1K2R, AUXU2KR, AUXU3KR, AUXU4KR, AUXU6KR, AUXU8KR, AUXU500L, AUXU1K1L, AUXU1K2L, AUXU2KL, AUXU3KL, AUXU4KL, AUXU6KL, AUXU8KL)
biochem_2005 <- biochem_2005 %>% dplyr::select(SEQN, LBXSAL, LBXSAPSI, LBXSBU, LBXSGL, LBXSUA, LBXSCR, LBDSALSI, LBDSCRSI)
BP_2005 <- BP_2005 %>% dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)
CBC_2005 <- CBC_2005 %>% dplyr::select(SEQN, LBXWBCSI, LBXLYPCT, LBXMCVSI, LBXRDW)
CRP_2005 <- CRP_2005 %>% dplyr::select(SEQN, LBXCRP)
hba1c_2005 <- hba1c_2005 %>% dplyr::select(SEQN, LBXGH)
totcholesterol_2005 <- totcholesterol_2005 %>% dplyr::select(SEQN, LBXTC)
cardiovas_2005 <- cardiovas_2005 %>% dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F)
smoking_2005 <- smoking_2005 %>% dplyr::select(SEQN, SMQ020)
diabetes_2005 <- diabetes_2005 %>% dplyr::select(SEQN, DIQ010)
hypertension_2005 <- hypertension_2005 %>% dplyr::select(SEQN, BPQ020)
bmi_2005 <- bmi_2005 %>% dplyr::select(SEQN, BMXBMI)
ckd_2005 <- ckd_2005 %>% dplyr::select(SEQN, KIQ022, KIQ025)
hearing_2005 <- hearing_2005 %>% dplyr::select(SEQN, AUQ211, AUQ231, AUQ290)

require(purrr)
require(dplyr)
dataset_2005 <- list(
  demographics_2005, audiometry_2005, biochem_2005, BP_2005, CBC_2005,
  CRP_2005, hba1c_2005, totcholesterol_2005, cardiovas_2005, smoking_2005,
  diabetes_2005, hypertension_2005, bmi_2005, ckd_2005, hearing_2005
) %>%
  reduce(left_join, by = "SEQN")

dataset_2005$sampleID <- paste0("2005_", dataset_2005$SEQN)

dataset_2005$cohort <- 2005

dataset_2005 <- subset(dataset_2005, dataset_2005$RIDAGEYR < 85)

dataset_2005$sbp <- rowMeans(dataset_2005[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)

# Standardize creatinine per NHANES 2005-2006 analytic notes (https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BIOPRO_D.htm#Analytic_Notes)
dataset_2005$creat <- dataset_2005$LBXSCR * 0.978 - 0.016
dataset_2005$creat_umol <- dataset_2005$creat * 88.4

dataset_2005 <- dataset_2005 %>%
  rename(
    age = RIDAGEYR,
    gender = RIAGENDR,
    pregnant = RIDEXPRG,
    race = RIDRETH1,
    education = DMDEDUC2,
    income = INDFMINC,
    audio_right_500 = AUXU500R,
    audio_right_1000 = AUXU1K1R,
    audio_right_1000_retest = AUXU1K2R,
    audio_right_2000 = AUXU2KR,
    audio_right_3000 = AUXU3KR,
    audio_right_4000 = AUXU4KR,
    audio_right_6000 = AUXU6KR,
    audio_right_8000 = AUXU8KR,
    audio_left_500 = AUXU500L,
    audio_left_1000 = AUXU1K1L,
    audio_left_1000_retest = AUXU1K2L,
    audio_left_2000 = AUXU2KL,
    audio_left_3000 = AUXU3KL,
    audio_left_4000 = AUXU4KL,
    audio_left_6000 = AUXU6KL,
    audio_left_8000 = AUXU8KL,
    albumin = LBXSAL,
    albumin_gL = LBDSALSI,
    alp = LBXSAPSI,
    bun = LBXSBU,
    glucose = LBXSGL,
    uap = LBXSUA,
    wbc = LBXWBCSI,
    lymph = LBXLYPCT,
    mcv = LBXMCVSI,
    rdw = LBXRDW,
    crp = LBXCRP,
    hba1c = LBXGH,
    totchol = LBXTC,
    bmi = BMXBMI,
    chf = MCQ160B,
    cad = MCQ160C,
    angina = MCQ160D,
    mi = MCQ160E,
    stroke = MCQ160F,
    smoking = SMQ020,
    diabetes = DIQ010,
    htn = BPQ020,
    kid_failure = KIQ022,
    dialysis = KIQ025,
    firearm = AUQ211,
    offwork_exposure = AUQ231,
    job_exposure_loud = AUQ290
  )

# 2007-2008 import --------------------------------------------------------
demographics_2007 <- read_xpt(here("Data", "Raw data", "Demographic 2007-2008.XPT"))
audiometry_2007 <- read_xpt(here("Data", "Raw data", "Audiometry 2007-2008.XPT"))
biochem_2007 <- read_xpt(here("Data", "Raw data", "Biochemistry 2007-2008.XPT"))
BP_2007 <- read_xpt(here("Data", "Raw data", "BP 2007-2008.XPT"))
CBC_2007 <- read_xpt(here("Data", "Raw data", "CBC 2007-2008.XPT"))
CRP_2007 <- read_xpt(here("Data", "Raw data", "CRP 2007-2008.XPT"))
hba1c_2007 <- read_xpt(here("Data", "Raw data", "Glycohemoglobin 2007-2008.XPT"))
totcholesterol_2007 <- read_xpt(here("Data", "Raw data", "Total cholesterol 2007-2008.XPT"))
cardiovas_2007 <- read_xpt(here("Data", "Raw data", "Medical Conditions Q 2007-2008.XPT"))
smoking_2007 <- read_xpt(here("Data", "Raw data", "Smoking Q 2007-2008.XPT"))
diabetes_2007 <- read_xpt(here("Data", "Raw data", "Diabetes Q 2007-2008.XPT"))
hypertension_2007 <- read_xpt(here("Data", "Raw data", "BP & Cholesterol Q 2007-2008.XPT"))
bmi_2007 <- read_xpt(here("Data", "Raw data", "Body Measures 2007-2008.XPT"))
ckd_2007 <- read_xpt(here("Data", "Raw data", "Kidney Conditions Q 2007-2008.XPT"))
hearing_2007 <- read_xpt(here("Data", "Raw data", "Hearing Q 2007-2008.XPT"))

demographics_2007 <- demographics_2007 %>% dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG, RIDRETH1, DMDEDUC2, INDFMIN2)
audiometry_2007 <- audiometry_2007 %>% dplyr::select(SEQN, AUXU500R, AUXU1K1R, AUXU1K2R, AUXU2KR, AUXU3KR, AUXU4KR, AUXU6KR, AUXU8KR, AUXU500L, AUXU1K1L, AUXU1K2L, AUXU2KL, AUXU3KL, AUXU4KL, AUXU6KL, AUXU8KL)
biochem_2007 <- biochem_2007 %>% dplyr::select(SEQN, LBXSAL, LBXSAPSI, LBXSBU, LBXSGL, LBXSUA, LBXSCR, LBDSALSI, LBDSCRSI)
BP_2007 <- BP_2007 %>% dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)
CBC_2007 <- CBC_2007 %>% dplyr::select(SEQN, LBXWBCSI, LBXLYPCT, LBXMCVSI, LBXRDW)
CRP_2007 <- CRP_2007 %>% dplyr::select(SEQN, LBXCRP)
hba1c_2007 <- hba1c_2007 %>% dplyr::select(SEQN, LBXGH)
totcholesterol_2007 <- totcholesterol_2007 %>% dplyr::select(SEQN, LBXTC)
cardiovas_2007 <- cardiovas_2007 %>% dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F)
smoking_2007 <- smoking_2007 %>% dplyr::select(SEQN, SMQ020)
diabetes_2007 <- diabetes_2007 %>% dplyr::select(SEQN, DIQ010)
hypertension_2007 <- hypertension_2007 %>% dplyr::select(SEQN, BPQ020)
bmi_2007 <- bmi_2007 %>% dplyr::select(SEQN, BMXBMI)
ckd_2007 <- ckd_2007 %>% dplyr::select(SEQN, KIQ022, KIQ025)
hearing_2007 <- hearing_2007 %>% dplyr::select(SEQN, AUQ211, AUQ231, AUQ290)

require(purrr)
require(dplyr)
dataset_2007 <- list(
  demographics_2007, audiometry_2007, biochem_2007, BP_2007, CBC_2007,
  CRP_2007, hba1c_2007, totcholesterol_2007, cardiovas_2007, smoking_2007,
  diabetes_2007, hypertension_2007, bmi_2007, ckd_2007, hearing_2007
) %>%
  reduce(left_join, by = "SEQN")

dataset_2007$sampleID <- paste0("2007_", dataset_2007$SEQN)

dataset_2007$cohort <- 2007

dataset_2007 <- subset(dataset_2007, dataset_2007$RIDAGEYR < 80)

dataset_2007$sbp <- rowMeans(dataset_2007[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)

dataset_2007 <- dataset_2007 %>%
  rename(
    age = RIDAGEYR,
    gender = RIAGENDR,
    pregnant = RIDEXPRG,
    race = RIDRETH1,
    education = DMDEDUC2,
    income = INDFMIN2,
    audio_right_500 = AUXU500R,
    audio_right_1000 = AUXU1K1R,
    audio_right_1000_retest = AUXU1K2R,
    audio_right_2000 = AUXU2KR,
    audio_right_3000 = AUXU3KR,
    audio_right_4000 = AUXU4KR,
    audio_right_6000 = AUXU6KR,
    audio_right_8000 = AUXU8KR,
    audio_left_500 = AUXU500L,
    audio_left_1000 = AUXU1K1L,
    audio_left_1000_retest = AUXU1K2L,
    audio_left_2000 = AUXU2KL,
    audio_left_3000 = AUXU3KL,
    audio_left_4000 = AUXU4KL,
    audio_left_6000 = AUXU6KL,
    audio_left_8000 = AUXU8KL,
    albumin = LBXSAL,
    albumin_gL = LBDSALSI,
    alp = LBXSAPSI,
    bun = LBXSBU,
    glucose = LBXSGL,
    uap = LBXSUA,
    creat = LBXSCR,
    creat_umol = LBDSCRSI,
    wbc = LBXWBCSI,
    lymph = LBXLYPCT,
    mcv = LBXMCVSI,
    rdw = LBXRDW,
    crp = LBXCRP,
    hba1c = LBXGH,
    totchol = LBXTC,
    bmi = BMXBMI,
    chf = MCQ160B,
    cad = MCQ160C,
    angina = MCQ160D,
    mi = MCQ160E,
    stroke = MCQ160F,
    smoking = SMQ020,
    diabetes = DIQ010,
    htn = BPQ020,
    kid_failure = KIQ022,
    dialysis = KIQ025,
    firearm = AUQ211,
    offwork_exposure = AUQ231,
    job_exposure_loud = AUQ290
  )

# 2009-2010 import --------------------------------------------------------
demographics_2009 <- read_xpt(here("Data", "Raw data", "Demographic 2009-2010.XPT"))
audiometry_2009 <- read_xpt(here("Data", "Raw data", "Audiometry 2009-2010.XPT"))
biochem_2009 <- read_xpt(here("Data", "Raw data", "Biochemistry 2009-2010.XPT"))
BP_2009 <- read_xpt(here("Data", "Raw data", "BP 2009-2010.XPT"))
CBC_2009 <- read_xpt(here("Data", "Raw data", "CBC 2009-2010.XPT"))
CRP_2009 <- read_xpt(here("Data", "Raw data", "CRP 2009-2010.XPT"))
hba1c_2009 <- read_xpt(here("Data", "Raw data", "Glycohemoglobin 2009-2010.XPT"))
totcholesterol_2009 <- read_xpt(here("Data", "Raw data", "Total cholesterol 2009-2010.XPT"))
cardiovas_2009 <- read_xpt(here("Data", "Raw data", "Medical Conditions Q 2009-2010.XPT"))
smoking_2009 <- read_xpt(here("Data", "Raw data", "Smoking Q 2009-2010.XPT"))
diabetes_2009 <- read_xpt(here("Data", "Raw data", "Diabetes Q 2009-2010.XPT"))
hypertension_2009 <- read_xpt(here("Data", "Raw data", "BP & Cholesterol Q 2009-2010.XPT"))
bmi_2009 <- read_xpt(here("Data", "Raw data", "Body Measures 2009-2010.XPT"))
ckd_2009 <- read_xpt(here("Data", "Raw data", "Kidney Conditions Q 2009-2010.XPT"))
hearing_2009 <- read_xpt(here("Data", "Raw data", "Hearing Q 2009-2010.XPT"))

demographics_2009 <- demographics_2009 %>% dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG, RIDRETH1, DMDEDUC2, INDFMIN2)
audiometry_2009 <- audiometry_2009 %>% dplyr::select(SEQN, AUXU500R, AUXU1K1R, AUXU1K2R, AUXU2KR, AUXU3KR, AUXU4KR, AUXU6KR, AUXU8KR, AUXU500L, AUXU1K1L, AUXU1K2L, AUXU2KL, AUXU3KL, AUXU4KL, AUXU6KL, AUXU8KL)
biochem_2009 <- biochem_2009 %>% dplyr::select(SEQN, LBXSAL, LBXSAPSI, LBXSBU, LBXSGL, LBXSUA, LBXSCR, LBDSALSI, LBDSCRSI)
BP_2009 <- BP_2009 %>% dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)
CBC_2009 <- CBC_2009 %>% dplyr::select(SEQN, LBXWBCSI, LBXLYPCT, LBXMCVSI, LBXRDW)
CRP_2009 <- CRP_2009 %>% dplyr::select(SEQN, LBXCRP)
hba1c_2009 <- hba1c_2009 %>% dplyr::select(SEQN, LBXGH)
totcholesterol_2009 <- totcholesterol_2009 %>% dplyr::select(SEQN, LBXTC)
cardiovas_2009 <- cardiovas_2009 %>% dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F)
smoking_2009 <- smoking_2009 %>% dplyr::select(SEQN, SMQ020)
diabetes_2009 <- diabetes_2009 %>% dplyr::select(SEQN, DIQ010)
hypertension_2009 <- hypertension_2009 %>% dplyr::select(SEQN, BPQ020)
bmi_2009 <- bmi_2009 %>% dplyr::select(SEQN, BMXBMI)
ckd_2009 <- ckd_2009 %>% dplyr::select(SEQN, KIQ022, KIQ025)
hearing_2009 <- hearing_2009 %>% dplyr::select(SEQN, AUQ211, AUQ231, AUQ290)

require(purrr)
require(dplyr)
dataset_2009 <- list(
  demographics_2009, audiometry_2009, biochem_2009, BP_2009, CBC_2009,
  CRP_2009, hba1c_2009, totcholesterol_2009, cardiovas_2009, smoking_2009,
  diabetes_2009, hypertension_2009, bmi_2009, ckd_2009, hearing_2009
) %>%
  reduce(left_join, by = "SEQN")

dataset_2009$sampleID <- paste0("2009_", dataset_2009$SEQN)

dataset_2009$cohort <- 2009

dataset_2009 <- subset(dataset_2009, dataset_2009$RIDAGEYR < 80)

dataset_2009$sbp <- rowMeans(dataset_2009[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)

dataset_2009 <- dataset_2009 %>%
  rename(
    age = RIDAGEYR,
    gender = RIAGENDR,
    pregnant = RIDEXPRG,
    race = RIDRETH1,
    education = DMDEDUC2,
    income = INDFMIN2,
    audio_right_500 = AUXU500R,
    audio_right_1000 = AUXU1K1R,
    audio_right_1000_retest = AUXU1K2R,
    audio_right_2000 = AUXU2KR,
    audio_right_3000 = AUXU3KR,
    audio_right_4000 = AUXU4KR,
    audio_right_6000 = AUXU6KR,
    audio_right_8000 = AUXU8KR,
    audio_left_500 = AUXU500L,
    audio_left_1000 = AUXU1K1L,
    audio_left_1000_retest = AUXU1K2L,
    audio_left_2000 = AUXU2KL,
    audio_left_3000 = AUXU3KL,
    audio_left_4000 = AUXU4KL,
    audio_left_6000 = AUXU6KL,
    audio_left_8000 = AUXU8KL,
    albumin = LBXSAL,
    albumin_gL = LBDSALSI,
    alp = LBXSAPSI,
    bun = LBXSBU,
    glucose = LBXSGL,
    uap = LBXSUA,
    creat = LBXSCR,
    creat_umol = LBDSCRSI,
    wbc = LBXWBCSI,
    lymph = LBXLYPCT,
    mcv = LBXMCVSI,
    rdw = LBXRDW,
    crp = LBXCRP,
    hba1c = LBXGH,
    totchol = LBXTC,
    bmi = BMXBMI,
    chf = MCQ160B,
    cad = MCQ160C,
    angina = MCQ160D,
    mi = MCQ160E,
    stroke = MCQ160F,
    smoking = SMQ020,
    diabetes = DIQ010,
    htn = BPQ020,
    kid_failure = KIQ022,
    dialysis = KIQ025,
    firearm = AUQ211,
    offwork_exposure = AUQ231,
    job_exposure_loud = AUQ290
  )

# 2011-2012 import --------------------------------------------------------
demographics_2011 <- read_xpt(here("Data", "Raw data", "Demographic 2011-2012.XPT"))
audiometry_2011 <- read_xpt(here("Data", "Raw data", "Audiometry 2011-2012.XPT"))
biochem_2011 <- read_xpt(here("Data", "Raw data", "Biochemistry 2011-2012.XPT"))
BP_2011 <- read_xpt(here("Data", "Raw data", "BP 2011-2012.XPT"))
CBC_2011 <- read_xpt(here("Data", "Raw data", "CBC 2011-2012.XPT"))
hba1c_2011 <- read_xpt(here("Data", "Raw data", "Glycohemoglobin 2011-2012.XPT"))
totcholesterol_2011 <- read_xpt(here("Data", "Raw data", "Total cholesterol 2011-2012.XPT"))
cardiovas_2011 <- read_xpt(here("Data", "Raw data", "Medical Conditions Q 2011-2012.XPT"))
smoking_2011 <- read_xpt(here("Data", "Raw data", "Smoking Q 2011-2012.XPT"))
diabetes_2011 <- read_xpt(here("Data", "Raw data", "Diabetes Q 2011-2012.XPT"))
hypertension_2011 <- read_xpt(here("Data", "Raw data", "BP & Cholesterol Q 2011-2012.XPT"))
bmi_2011 <- read_xpt(here("Data", "Raw data", "Body Measures 2011-2012.XPT"))
ckd_2011 <- read_xpt(here("Data", "Raw data", "Kidney Conditions Q 2011-2012.XPT"))
hearing_2011 <- read_xpt(here("Data", "Raw data", "Hearing Q 2011-2012.XPT"))

demographics_2011 <- demographics_2011 %>% dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG, RIDRETH1, DMDEDUC2, INDFMIN2)
audiometry_2011 <- audiometry_2011 %>% dplyr::select(SEQN, AUXU500R, AUXU1K1R, AUXU1K2R, AUXU2KR, AUXU3KR, AUXU4KR, AUXU6KR, AUXU8KR, AUXU500L, AUXU1K1L, AUXU1K2L, AUXU2KL, AUXU3KL, AUXU4KL, AUXU6KL, AUXU8KL)
biochem_2011 <- biochem_2011 %>% dplyr::select(SEQN, LBXSAL, LBXSAPSI, LBXSBU, LBXSGL, LBXSUA, LBXSCR, LBDSALSI, LBDSCRSI)
BP_2011 <- BP_2011 %>% dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)
CBC_2011 <- CBC_2011 %>% dplyr::select(SEQN, LBXWBCSI, LBXLYPCT, LBXMCVSI, LBXRDW)
hba1c_2011 <- hba1c_2011 %>% dplyr::select(SEQN, LBXGH)
totcholesterol_2011 <- totcholesterol_2011 %>% dplyr::select(SEQN, LBXTC)
cardiovas_2011 <- cardiovas_2011 %>% dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F)
smoking_2011 <- smoking_2011 %>% dplyr::select(SEQN, SMQ020)
diabetes_2011 <- diabetes_2011 %>% dplyr::select(SEQN, DIQ010)
hypertension_2011 <- hypertension_2011 %>% dplyr::select(SEQN, BPQ020)
bmi_2011 <- bmi_2011 %>% dplyr::select(SEQN, BMXBMI)
ckd_2011 <- ckd_2011 %>% dplyr::select(SEQN, KIQ022, KIQ025)
hearing_2011 <- hearing_2011 %>% dplyr::select(SEQN, AUQ300, AUQ370, AUQ330, AUQ350)

require(purrr)
require(dplyr)
dataset_2011 <- list(
  demographics_2011, audiometry_2011, biochem_2011, BP_2011, CBC_2011,
  hba1c_2011, totcholesterol_2011, cardiovas_2011, smoking_2011,
  diabetes_2011, hypertension_2011, bmi_2011, ckd_2011, hearing_2011
) %>%
  reduce(left_join, by = "SEQN")

dataset_2011$sampleID <- paste0("2011_", dataset_2011$SEQN)

dataset_2011$cohort <- 2011

dataset_2011 <- subset(dataset_2011, dataset_2011$RIDAGEYR < 80)

dataset_2011$sbp <- rowMeans(dataset_2011[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)

dataset_2011 <- dataset_2011 %>%
  rename(
    age = RIDAGEYR,
    gender = RIAGENDR,
    pregnant = RIDEXPRG,
    race = RIDRETH1,
    education = DMDEDUC2,
    income = INDFMIN2,
    audio_right_500 = AUXU500R,
    audio_right_1000 = AUXU1K1R,
    audio_right_1000_retest = AUXU1K2R,
    audio_right_2000 = AUXU2KR,
    audio_right_3000 = AUXU3KR,
    audio_right_4000 = AUXU4KR,
    audio_right_6000 = AUXU6KR,
    audio_right_8000 = AUXU8KR,
    audio_left_500 = AUXU500L,
    audio_left_1000 = AUXU1K1L,
    audio_left_1000_retest = AUXU1K2L,
    audio_left_2000 = AUXU2KL,
    audio_left_3000 = AUXU3KL,
    audio_left_4000 = AUXU4KL,
    audio_left_6000 = AUXU6KL,
    audio_left_8000 = AUXU8KL,
    albumin = LBXSAL,
    albumin_gL = LBDSALSI,
    alp = LBXSAPSI,
    bun = LBXSBU,
    glucose = LBXSGL,
    uap = LBXSUA,
    creat = LBXSCR,
    creat_umol = LBDSCRSI,
    wbc = LBXWBCSI,
    lymph = LBXLYPCT,
    mcv = LBXMCVSI,
    rdw = LBXRDW,
    hba1c = LBXGH,
    totchol = LBXTC,
    bmi = BMXBMI,
    chf = MCQ160B,
    cad = MCQ160C,
    angina = MCQ160D,
    mi = MCQ160E,
    stroke = MCQ160F,
    smoking = SMQ020,
    diabetes = DIQ010,
    htn = BPQ020,
    kid_failure = KIQ022,
    dialysis = KIQ025,
    firearm = AUQ300,
    offwork_exposure = AUQ370,
    job_exposure_loud = AUQ330,
    job_exposure_very_loud = AUQ350
  )

# 2015-2016 import --------------------------------------------------------
demographics_2015 <- read_xpt(here("Data", "Raw data", "Demographic 2015-2016.XPT"))
audiometry_2015 <- read_xpt(here("Data", "Raw data", "Audiometry 2015-2016.XPT"))
biochem_2015 <- read_xpt(here("Data", "Raw data", "Biochemistry 2015-2016.XPT"))
BP_2015 <- read_xpt(here("Data", "Raw data", "BP 2015-2016.XPT"))
CBC_2015 <- read_xpt(here("Data", "Raw data", "CBC 2015-2016.XPT"))
CRP_2015 <- read_xpt(here("Data", "Raw data", "CRP 2015-2016.XPT"))
hba1c_2015 <- read_xpt(here("Data", "Raw data", "Glycohemoglobin 2015-2016.XPT"))
totcholesterol_2015 <- read_xpt(here("Data", "Raw data", "Total cholesterol 2015-2016.XPT"))
cardiovas_2015 <- read_xpt(here("Data", "Raw data", "Medical Conditions Q 2015-2016.XPT"))
smoking_2015 <- read_xpt(here("Data", "Raw data", "Smoking Q 2015-2016.XPT"))
diabetes_2015 <- read_xpt(here("Data", "Raw data", "Diabetes Q 2015-2016.XPT"))
hypertension_2015 <- read_xpt(here("Data", "Raw data", "BP & Cholesterol Q 2015-2016.XPT"))
bmi_2015 <- read_xpt(here("Data", "Raw data", "Body Measures 2015-2016.XPT"))
ckd_2015 <- read_xpt(here("Data", "Raw data", "Kidney Conditions Q 2015-2016.XPT"))
hearing_2015 <- read_xpt(here("Data", "Raw data", "Hearing Q 2015-2016.XPT"))

demographics_2015 <- demographics_2015 %>% dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG, RIDRETH1, DMDEDUC2, INDFMIN2)
audiometry_2015 <- audiometry_2015 %>% dplyr::select(SEQN, AUXU500R, AUXU1K1R, AUXU1K2R, AUXU2KR, AUXU3KR, AUXU4KR, AUXU6KR, AUXU8KR, AUXU500L, AUXU1K1L, AUXU1K2L, AUXU2KL, AUXU3KL, AUXU4KL, AUXU6KL, AUXU8KL)
biochem_2015 <- biochem_2015 %>% dplyr::select(SEQN, LBXSAL, LBXSAPSI, LBXSBU, LBXSGL, LBXSUA, LBXSCR, LBDSALSI, LBDSCRSI)
BP_2015 <- BP_2015 %>% dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)
CBC_2015 <- CBC_2015 %>% dplyr::select(SEQN, LBXWBCSI, LBXLYPCT, LBXMCVSI, LBXRDW)
CRP_2015 <- CRP_2015 %>% dplyr::select(SEQN, LBXHSCRP)
hba1c_2015 <- hba1c_2015 %>% dplyr::select(SEQN, LBXGH)
totcholesterol_2015 <- totcholesterol_2015 %>% dplyr::select(SEQN, LBXTC)
cardiovas_2015 <- cardiovas_2015 %>% dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F)
smoking_2015 <- smoking_2015 %>% dplyr::select(SEQN, SMQ020)
diabetes_2015 <- diabetes_2015 %>% dplyr::select(SEQN, DIQ010)
hypertension_2015 <- hypertension_2015 %>% dplyr::select(SEQN, BPQ020)
bmi_2015 <- bmi_2015 %>% dplyr::select(SEQN, BMXBMI)
ckd_2015 <- ckd_2015 %>% dplyr::select(SEQN, KIQ022, KIQ025)
hearing_2015 <- hearing_2015 %>% dplyr::select(SEQN, AUQ300, AUQ370, AUQ331, AUQ350)

require(purrr)
require(dplyr)
dataset_2015 <- list(
  demographics_2015, audiometry_2015, biochem_2015, BP_2015, CBC_2015,
  CRP_2015, hba1c_2015, totcholesterol_2015, cardiovas_2015, smoking_2015,
  diabetes_2015, hypertension_2015, bmi_2015, ckd_2015, hearing_2015
) %>%
  reduce(left_join, by = "SEQN")

dataset_2015$sampleID <- paste0("2015_", dataset_2015$SEQN)

dataset_2015$cohort <- 2015

dataset_2015 <- subset(dataset_2015, dataset_2015$RIDAGEYR < 80)

# Convert unit of high-sensitivity CRP from mg/L to mg/dL
dataset_2015$crp <- dataset_2015$LBXHSCRP / 10

dataset_2015$sbp <- rowMeans(dataset_2015[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)

dataset_2015 <- dataset_2015 %>%
  rename(
    age = RIDAGEYR,
    gender = RIAGENDR,
    pregnant = RIDEXPRG,
    race = RIDRETH1,
    education = DMDEDUC2,
    income = INDFMIN2,
    audio_right_500 = AUXU500R,
    audio_right_1000 = AUXU1K1R,
    audio_right_1000_retest = AUXU1K2R,
    audio_right_2000 = AUXU2KR,
    audio_right_3000 = AUXU3KR,
    audio_right_4000 = AUXU4KR,
    audio_right_6000 = AUXU6KR,
    audio_right_8000 = AUXU8KR,
    audio_left_500 = AUXU500L,
    audio_left_1000 = AUXU1K1L,
    audio_left_1000_retest = AUXU1K2L,
    audio_left_2000 = AUXU2KL,
    audio_left_3000 = AUXU3KL,
    audio_left_4000 = AUXU4KL,
    audio_left_6000 = AUXU6KL,
    audio_left_8000 = AUXU8KL,
    albumin = LBXSAL,
    albumin_gL = LBDSALSI,
    alp = LBXSAPSI,
    bun = LBXSBU,
    glucose = LBXSGL,
    uap = LBXSUA,
    creat = LBXSCR,
    creat_umol = LBDSCRSI,
    wbc = LBXWBCSI,
    lymph = LBXLYPCT,
    mcv = LBXMCVSI,
    rdw = LBXRDW,
    hba1c = LBXGH,
    totchol = LBXTC,
    bmi = BMXBMI,
    chf = MCQ160B,
    cad = MCQ160C,
    angina = MCQ160D,
    mi = MCQ160E,
    stroke = MCQ160F,
    smoking = SMQ020,
    diabetes = DIQ010,
    htn = BPQ020,
    kid_failure = KIQ022,
    dialysis = KIQ025,
    firearm = AUQ300,
    offwork_exposure = AUQ370,
    job_exposure_loud = AUQ331,
    job_exposure_very_loud = AUQ350
  )

# 2017-2020 import --------------------------------------------------------
demographics_2017 <- read_xpt(here("Data", "Raw data", "Demographic 2017-2020.XPT"))
audiometry_2017 <- read_xpt(here("Data", "Raw data", "Audiometry 2017-2020.XPT"))
biochem_2017 <- read_xpt(here("Data", "Raw data", "Biochemistry 2017-2020.XPT"))
BP_2017 <- read_xpt(here("Data", "Raw data", "BP 2017-2020.XPT"))
CBC_2017 <- read_xpt(here("Data", "Raw data", "CBC 2017-2020.XPT"))
CRP_2017 <- read_xpt(here("Data", "Raw data", "CRP 2017-2020.XPT"))
hba1c_2017 <- read_xpt(here("Data", "Raw data", "Glycohemoglobin 2017-2020.XPT"))
totcholesterol_2017 <- read_xpt(here("Data", "Raw data", "Total cholesterol 2017-2020.XPT"))
cardiovas_2017 <- read_xpt(here("Data", "Raw data", "Medical Conditions Q 2017-2020.XPT"))
smoking_2017 <- read_xpt(here("Data", "Raw data", "Smoking Q 2017-2020.XPT"))
diabetes_2017 <- read_xpt(here("Data", "Raw data", "Diabetes Q 2017-2020.XPT"))
hypertension_2017 <- read_xpt(here("Data", "Raw data", "BP & Cholesterol Q 2017-2020.XPT"))
bmi_2017 <- read_xpt(here("Data", "Raw data", "Body Measures 2017-2020.XPT"))
ckd_2017 <- read_xpt(here("Data", "Raw data", "Kidney Conditions Q 2017-2020.XPT"))
hearing_2017 <- read_xpt(here("Data", "Raw data", "Hearing Q 2017-2020.XPT"))

demographics_2017 <- demographics_2017 %>% dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG, RIDRETH1, DMDEDUC2)
audiometry_2017 <- audiometry_2017 %>% dplyr::select(SEQN, AUXU500R, AUXU1K1R, AUXU1K2R, AUXU2KR, AUXU3KR, AUXU4KR, AUXU6KR, AUXU8KR, AUXU500L, AUXU1K1L, AUXU1K2L, AUXU2KL, AUXU3KL, AUXU4KL, AUXU6KL, AUXU8KL)
biochem_2017 <- biochem_2017 %>% dplyr::select(SEQN, LBXSAL, LBXSAPSI, LBXSBU, LBXSGL, LBXSUA, LBXSCR, LBDSALSI, LBDSCRSI)
BP_2017 <- BP_2017 %>% dplyr::select(SEQN, BPXOSY1, BPXOSY2, BPXOSY3)
CBC_2017 <- CBC_2017 %>% dplyr::select(SEQN, LBXWBCSI, LBXLYPCT, LBXMCVSI, LBXRDW)
CRP_2017 <- CRP_2017 %>% dplyr::select(SEQN, LBXHSCRP)
hba1c_2017 <- hba1c_2017 %>% dplyr::select(SEQN, LBXGH)
totcholesterol_2017 <- totcholesterol_2017 %>% dplyr::select(SEQN, LBXTC)
cardiovas_2017 <- cardiovas_2017 %>% dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F)
smoking_2017 <- smoking_2017 %>% dplyr::select(SEQN, SMQ020)
diabetes_2017 <- diabetes_2017 %>% dplyr::select(SEQN, DIQ010)
hypertension_2017 <- hypertension_2017 %>% dplyr::select(SEQN, BPQ020)
bmi_2017 <- bmi_2017 %>% dplyr::select(SEQN, BMXBMI)
ckd_2017 <- ckd_2017 %>% dplyr::select(SEQN, KIQ022, KIQ025)
hearing_2017 <- hearing_2017 %>% dplyr::select(SEQN, AUQ300, AUQ370, AUQ330, AUQ350)

require(purrr)
require(dplyr)
dataset_2017 <- list(
  demographics_2017, audiometry_2017, biochem_2017, BP_2017, CBC_2017,
  CRP_2017, hba1c_2017, totcholesterol_2017, cardiovas_2017, smoking_2017,
  diabetes_2017, hypertension_2017, bmi_2017, ckd_2017, hearing_2017
) %>%
  reduce(left_join, by = "SEQN")

dataset_2017$sampleID <- paste0("2017_", dataset_2017$SEQN)

dataset_2017$cohort <- 2017

dataset_2017 <- subset(dataset_2017, dataset_2017$RIDAGEYR < 80)

# Convert unit of high-sensitivity CRP from mg/L to mg/dL
dataset_2017$crp <- dataset_2017$LBXHSCRP / 10

# Different blood pressure measurement method
dataset_2017$sbp <- rowMeans(dataset_2017[, c("BPXOSY1", "BPXOSY2", "BPXOSY3")], na.rm = TRUE)

dataset_2017 <- dataset_2017 %>%
  rename(
    age = RIDAGEYR,
    gender = RIAGENDR,
    pregnant = RIDEXPRG,
    race = RIDRETH1,
    education = DMDEDUC2,
    audio_right_500 = AUXU500R,
    audio_right_1000 = AUXU1K1R,
    audio_right_1000_retest = AUXU1K2R,
    audio_right_2000 = AUXU2KR,
    audio_right_3000 = AUXU3KR,
    audio_right_4000 = AUXU4KR,
    audio_right_6000 = AUXU6KR,
    audio_right_8000 = AUXU8KR,
    audio_left_500 = AUXU500L,
    audio_left_1000 = AUXU1K1L,
    audio_left_1000_retest = AUXU1K2L,
    audio_left_2000 = AUXU2KL,
    audio_left_3000 = AUXU3KL,
    audio_left_4000 = AUXU4KL,
    audio_left_6000 = AUXU6KL,
    audio_left_8000 = AUXU8KL,
    albumin = LBXSAL,
    albumin_gL = LBDSALSI,
    alp = LBXSAPSI,
    bun = LBXSBU,
    glucose = LBXSGL,
    uap = LBXSUA,
    creat = LBXSCR,
    creat_umol = LBDSCRSI,
    wbc = LBXWBCSI,
    lymph = LBXLYPCT,
    mcv = LBXMCVSI,
    rdw = LBXRDW,
    hba1c = LBXGH,
    totchol = LBXTC,
    bmi = BMXBMI,
    chf = MCQ160B,
    cad = MCQ160C,
    angina = MCQ160D,
    mi = MCQ160E,
    stroke = MCQ160F,
    smoking = SMQ020,
    diabetes = DIQ010,
    htn = BPQ020,
    kid_failure = KIQ022,
    dialysis = KIQ025,
    firearm = AUQ300,
    offwork_exposure = AUQ370,
    job_exposure_loud = AUQ330,
    job_exposure_very_loud = AUQ350
  )

# Combine & clean ---------------------------------------------------------
# Combine all years into one large dataset
dataset_all <- bind_rows(dataset_1999, dataset_2001, dataset_2003, dataset_2005, dataset_2007, dataset_2009, dataset_2011, dataset_2015, dataset_2017)

# Discard rows with audiometry value NA, 666 (no response) or 888 (could not obtain)
dataset_all_clean <- dataset_all[!(is.na(dataset_all$audio_right_500) | dataset_all$audio_right_500 == 666 | dataset_all$audio_right_500 == 888 |
  is.na(dataset_all$audio_right_1000) | dataset_all$audio_right_1000 == 666 | dataset_all$audio_right_1000 == 888 |
  is.na(dataset_all$audio_right_1000_retest) | dataset_all$audio_right_1000_retest == 666 | dataset_all$audio_right_1000_retest == 888 |
  is.na(dataset_all$audio_right_2000) | dataset_all$audio_right_2000 == 666 | dataset_all$audio_right_2000 == 888 |
  is.na(dataset_all$audio_right_3000) | dataset_all$audio_right_3000 == 666 | dataset_all$audio_right_3000 == 888 |
  is.na(dataset_all$audio_right_4000) | dataset_all$audio_right_4000 == 666 | dataset_all$audio_right_4000 == 888 |
  is.na(dataset_all$audio_right_6000) | dataset_all$audio_right_6000 == 666 | dataset_all$audio_right_6000 == 888 |
  is.na(dataset_all$audio_right_8000) | dataset_all$audio_right_8000 == 666 | dataset_all$audio_right_8000 == 888 |
  is.na(dataset_all$audio_left_500) | dataset_all$audio_left_500 == 666 | dataset_all$audio_left_500 == 888 |
  is.na(dataset_all$audio_left_1000) | dataset_all$audio_left_1000 == 666 | dataset_all$audio_left_1000 == 888 |
  is.na(dataset_all$audio_left_1000_retest) | dataset_all$audio_left_1000_retest == 666 | dataset_all$audio_left_1000_retest == 888 |
  is.na(dataset_all$audio_left_2000) | dataset_all$audio_left_2000 == 666 | dataset_all$audio_left_2000 == 888 |
  is.na(dataset_all$audio_left_3000) | dataset_all$audio_left_3000 == 666 | dataset_all$audio_left_3000 == 888 |
  is.na(dataset_all$audio_left_4000) | dataset_all$audio_left_4000 == 666 | dataset_all$audio_left_4000 == 888 |
  is.na(dataset_all$audio_left_6000) | dataset_all$audio_left_6000 == 666 | dataset_all$audio_left_6000 == 888 |
  is.na(dataset_all$audio_left_8000) | dataset_all$audio_left_8000 == 666 | dataset_all$audio_left_8000 == 888), ]


# Age over 20
dataset_all_clean <- subset(dataset_all_clean, age >= 20)

# Exclude pregnancy
dataset_all_clean <- subset(dataset_all_clean, pregnant != 1 | is.na(pregnant))

# Test-retest difference at 1000Hz
dataset_all_clean$test_retest_diff_left <- abs(dataset_all_clean$audio_left_1000 - dataset_all_clean$audio_left_1000_retest)
dataset_all_clean$test_retest_diff_right <- abs(dataset_all_clean$audio_right_1000 - dataset_all_clean$audio_right_1000_retest)

# Exclude subjects with a >= 10 dB test-retest difference
# (Reference - Prevalence of Hearing Loss in US Children and Adolescents: Findings From NHANES 1988-2010, Brooke M. Su, Dylan K. Chan)
dataset_all_clean <- subset(dataset_all_clean, test_retest_diff_left < 10 & test_retest_diff_right < 10)

# Calculate PTA at 500, 1000, 2000 and 4000 Hz for right & left ear
dataset_all_clean$pta_r <- (dataset_all_clean$audio_right_500 + dataset_all_clean$audio_right_1000 + dataset_all_clean$audio_right_2000 + dataset_all_clean$audio_right_4000) / 4
dataset_all_clean$pta_l <- (dataset_all_clean$audio_left_500 + dataset_all_clean$audio_left_1000 + dataset_all_clean$audio_left_2000 + dataset_all_clean$audio_left_4000) / 4

# Calculate difference in Fletcher Index between two ears
dataset_all_clean$pta_diff <- abs(dataset_all_clean$pta_r - dataset_all_clean$pta_l)

# Plot histogram for difference in Fletcher Index between ears
# library(ggplot2)
# data = data.frame(value = dataset_all_clean$FI_mid_diff)
# ggplot(data, aes(x = value)) + geom_histogram(binwidth = 1) + ggtitle("FI difference between ears")

# Exclude subjects with asymmetric hearing loss, defined as a difference >= 15 dB in PTA between ears
dataset_all_clean <- subset(dataset_all_clean, pta_diff < 15)

# Make 'chronic kidney disease' covariable by pooling 'kid_failure' and 'dialysis' 1 = yes, 2 = no
# dataset_all_clean <- mutate(dataset_all_clean, ckd = case_when(dataset_all_clean$kid_failure == 1 | dataset_all_clean$dialysis == 1 ~ 1,
# dataset_all_clean$kid_failure == 2 | dataset_all_clean$dialysis == 2 ~ 2))

# Make 'noise exposure' covariable by pooling firearm usage, offwork exposure and job exposure
# dataset_all_clean <- mutate(dataset_all_clean, noise_exposure = case_when(dataset_all_clean$firearm == 1 | dataset_all_clean$offwork_exposure == 1 | dataset_all_clean$job_exposure_loud == 1 | dataset_all_clean$job_exposure_very_loud == 1 ~ 1,
# dataset_all_clean$firearm == 2 | dataset_all_clean$offwork_exposure == 2 | dataset_all_clean$job_exposure_loud == 2 | dataset_all_clean$job_exposure_very_loud == 2 ~ 2))

# Exclude 'refused' or 'don't know' in covariables
# dataset_all_clean <- dataset_all_clean[!(dataset_all_clean$education == 7 | dataset_all_clean$education == 9 |
# dataset_all_clean$income == 77 | dataset_all_clean$income == 99 |dataset_all_clean$income == 12 | dataset_all_clean$income == 13 |
# dataset_all_clean$chf == 7 | dataset_all_clean$chf == 9 |
# dataset_all_clean$cad == 7 | dataset_all_clean$cad == 9 |
# dataset_all_clean$angina == 7 | dataset_all_clean$angina == 9 |
# dataset_all_clean$mi == 7 | dataset_all_clean$mi == 9 |
# dataset_all_clean$stroke == 7 | dataset_all_clean$stroke == 9 |
# dataset_all_clean$smoking == 7 | dataset_all_clean$smoking == 9 |
# dataset_all_clean$diabetes == 7 | dataset_all_clean$diabetes == 9 |
# dataset_all_clean$htn == 7 | dataset_all_clean$htn == 9),]

# Drop columns no longer needed
drop <- c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4", "LBXHSCRP", "BPXOSY1", "BPXOSY2", "BPXOSY3", "LBXSCR", "LBDSCRSI")
dataset_all_clean <- dataset_all_clean[, !(names(dataset_all_clean) %in% drop)]

# Removing NAs
dataset_all_clean <- dataset_all_clean[complete.cases(dataset_all_clean[, c("albumin", "alp", "totchol", "crp", "creat", "hba1c", "sbp", "bun", "uap", "lymph", "mcv", "wbc")]), ]

# Log transform
dataset_all_clean$lncrp <- log(dataset_all_clean$crp + 1)
dataset_all_clean$lncreat <- log(dataset_all_clean$creat + 1)

# Cleaning biomarker outliers ---------------------------------------------
male <- subset(dataset_all_clean, gender == 1)
female <- subset(dataset_all_clean, gender == 2)

albumin_mean_m <- mean(male$albumin, na.rm = TRUE)
albumin_sd_m <- sd(male$albumin, na.rm = TRUE)
albumin_mean_f <- mean(female$albumin, na.rm = TRUE)
albumin_sd_f <- sd(female$albumin, na.rm = TRUE)

alp_mean_m <- mean(male$alp, na.rm = TRUE)
alp_sd_m <- sd(male$alp, na.rm = TRUE)
alp_mean_f <- mean(female$alp, na.rm = TRUE)
alp_sd_f <- sd(female$alp, na.rm = TRUE)

crp_mean_m <- mean(male$lncrp, na.rm = TRUE)
crp_sd_m <- sd(male$lncrp, na.rm = TRUE)
crp_mean_f <- mean(female$lncrp, na.rm = TRUE)
crp_sd_f <- sd(female$lncrp, na.rm = TRUE)

totchol_mean_m <- mean(male$totchol, na.rm = TRUE)
totchol_sd_m <- sd(male$totchol, na.rm = TRUE)
totchol_mean_f <- mean(female$totchol, na.rm = TRUE)
totchol_sd_f <- sd(female$totchol, na.rm = TRUE)

creat_mean_m <- mean(male$creat, na.rm = TRUE)
creat_sd_m <- sd(male$creat, na.rm = TRUE)
creat_mean_f <- mean(female$creat, na.rm = TRUE)
creat_sd_f <- sd(female$creat, na.rm = TRUE)

hba1c_mean_m <- mean(male$hba1c, na.rm = TRUE)
hba1c_sd_m <- sd(male$hba1c, na.rm = TRUE)
hba1c_mean_f <- mean(female$hba1c, na.rm = TRUE)
hba1c_sd_f <- sd(female$hba1c, na.rm = TRUE)

sbp_mean_m <- mean(male$sbp, na.rm = TRUE)
sbp_sd_m <- sd(male$sbp, na.rm = TRUE)
sbp_mean_f <- mean(female$sbp, na.rm = TRUE)
sbp_sd_f <- sd(female$sbp, na.rm = TRUE)

bun_mean_m <- mean(male$bun, na.rm = TRUE)
bun_sd_m <- sd(male$bun, na.rm = TRUE)
bun_mean_f <- mean(female$bun, na.rm = TRUE)
bun_sd_f <- sd(female$bun, na.rm = TRUE)

uap_mean_m <- mean(male$uap, na.rm = TRUE)
uap_sd_m <- sd(male$uap, na.rm = TRUE)
uap_mean_f <- mean(female$uap, na.rm = TRUE)
uap_sd_f <- sd(female$uap, na.rm = TRUE)

lymph_mean_m <- mean(male$lymph, na.rm = TRUE)
lymph_sd_m <- sd(male$lymph, na.rm = TRUE)
lymph_mean_f <- mean(female$lymph, na.rm = TRUE)
lymph_sd_f <- sd(female$lymph, na.rm = TRUE)

mcv_mean_m <- mean(male$mcv, na.rm = TRUE)
mcv_sd_m <- sd(male$mcv, na.rm = TRUE)
mcv_mean_f <- mean(female$mcv, na.rm = TRUE)
mcv_sd_f <- sd(female$mcv, na.rm = TRUE)

wbc_mean_m <- mean(male$wbc, na.rm = TRUE)
wbc_sd_m <- sd(male$wbc, na.rm = TRUE)
wbc_mean_f <- mean(female$wbc, na.rm = TRUE)
wbc_sd_f <- sd(female$wbc, na.rm = TRUE)

male_new <- subset(male, albumin >= albumin_mean_m - 5 * albumin_sd_m & albumin <= albumin_mean_m + 5 * albumin_sd_m &
  alp >= alp_mean_m - 5 * alp_sd_m & alp <= alp_mean_m + 5 * alp_sd_m &
  totchol >= totchol_mean_m - 5 * totchol_sd_m & totchol <= totchol_mean_m + 5 * totchol_sd_m &
  lncrp >= crp_mean_f - 5 * crp_sd_f & lncrp <= crp_mean_f + 5 * crp_sd_f &
  creat >= creat_mean_m - 5 * creat_sd_m & creat <= creat_mean_m + 5 * creat_sd_m &
  hba1c >= hba1c_mean_m - 5 * hba1c_sd_m & hba1c <= hba1c_mean_m + 5 * hba1c_sd_m &
  sbp >= sbp_mean_m - 5 * sbp_sd_m & sbp <= sbp_mean_m + 5 * sbp_sd_m &
  bun >= bun_mean_m - 5 * bun_sd_m & bun <= bun_mean_m + 5 * bun_sd_m &
  uap >= uap_mean_m - 5 * uap_sd_m & uap <= uap_mean_m + 5 * uap_sd_m &
  lymph >= lymph_mean_m - 5 * lymph_sd_m & lymph <= lymph_mean_m + 5 * lymph_sd_m &
  mcv >= mcv_mean_m - 5 * mcv_sd_m & mcv <= mcv_mean_m + 5 * mcv_sd_m &
  wbc >= wbc_mean_m - 5 * wbc_sd_m & wbc <= wbc_mean_m + 5 * wbc_sd_m)

female_new <- subset(female, albumin >= albumin_mean_f - 5 * albumin_sd_f & albumin <= albumin_mean_f + 5 * albumin_sd_f &
  alp >= alp_mean_f - 5 * alp_sd_f & alp <= alp_mean_f + 5 * alp_sd_f &
  totchol >= totchol_mean_f - 5 * totchol_sd_f & totchol <= totchol_mean_f + 5 * totchol_sd_f &
  lncrp >= crp_mean_f - 5 * crp_sd_f & lncrp <= crp_mean_f + 5 * crp_sd_f &
  creat >= creat_mean_m - 5 * creat_sd_m & creat <= creat_mean_m + 5 * creat_sd_m &
  hba1c >= hba1c_mean_f - 5 * hba1c_sd_f & hba1c <= hba1c_mean_f + 5 * hba1c_sd_f &
  sbp >= sbp_mean_f - 5 * sbp_sd_f & sbp <= sbp_mean_f + 5 * sbp_sd_f &
  bun >= bun_mean_f - 5 * bun_sd_f & bun <= bun_mean_f + 5 * bun_sd_f &
  uap >= uap_mean_f - 5 * uap_sd_f & uap <= uap_mean_f + 5 * uap_sd_f &
  lymph >= lymph_mean_f - 5 * lymph_sd_f & lymph <= lymph_mean_f + 5 * lymph_sd_f &
  mcv >= mcv_mean_f - 5 * mcv_sd_f & mcv <= mcv_mean_f + 5 * mcv_sd_f &
  wbc >= wbc_mean_f - 5 * wbc_sd_f & wbc <= wbc_mean_f + 5 * wbc_sd_f)

dataset_all_clean <- rbind(male_new, female_new)

# Calculate biological age ----------------------------------------------------------
library(dplyr)
r_files <- list.files(here("Scripts", "Modified Kwon Scripts"),
  pattern = "\\.R$", full.names = TRUE
)
lapply(r_files, source, local = TRUE, echo = FALSE)

load(here("Data", "NHANES3.rda"))
load(here("Data", "NHANES3_HDTrain.rda"))
load(here("Data", "NHANES4.rda"))

# HD using NHANES
hd <- hd_nhanes(biomarkers = c("albumin", "alp", "lncrp", "totchol", "creat", "hba1c", "sbp", "bun", "uap", "lymph", "mcv", "wbc"))

# KDM bioage using NHANES
kdm <- kdm_nhanes(biomarkers = c("albumin", "alp", "lncrp", "totchol", "creat", "hba1c", "sbp", "bun", "uap", "lymph", "mcv", "wbc"))

# phenoage using NHANES
phenoage <- phenoage_nhanes(biomarkers = c("albumin_gL", "alp", "lncrp", "totchol", "creat_umol", "hba1c", "sbp", "bun", "uap", "lymph", "mcv", "wbc"))

# HD
hd_fem <- hd_calc(
  data = dataset_all_clean %>%
    filter(gender == 2),
  reference = NHANES3_HDTrain %>%
    filter(gender == 2),
  biomarkers = c("albumin", "alp", "lncrp", "totchol", "creat", "hba1c", "sbp", "bun", "uap", "lymph", "mcv", "wbc")
)

hd_male <- hd_calc(
  data = dataset_all_clean %>%
    filter(gender == 1),
  reference = NHANES3_HDTrain %>%
    filter(gender == 1),
  biomarkers = c("albumin", "alp", "lncrp", "totchol", "creat", "hba1c", "sbp", "bun", "uap", "lymph", "mcv", "wbc")
)

hd_data <- rbind(hd_fem$data, hd_male$data)

# KDM
kdm_fem <- kdm_calc(
  data = dataset_all_clean %>%
    filter(gender == 2),
  biomarkers = c("albumin", "alp", "lncrp", "totchol", "creat", "hba1c", "sbp", "bun", "uap", "lymph", "mcv", "wbc"),
  fit = kdm$fit$female,
  s_ba2 = kdm$fit$female$s_ba2
)

kdm_male <- kdm_calc(
  data = dataset_all_clean %>%
    filter(gender == 1),
  biomarkers = c("albumin", "alp", "lncrp", "totchol", "creat", "hba1c", "sbp", "bun", "uap", "lymph", "mcv", "wbc"),
  fit = kdm$fit$male,
  s_ba2 = kdm$fit$male$s_ba2
)

kdm_data <- rbind(kdm_fem$data, kdm_male$data)

# Phenotypical age
phenoage_own <- phenoage_calc(
  data = dataset_all_clean,
  biomarkers = c("albumin_gL", "alp", "lncrp", "totchol", "creat_umol", "hba1c", "sbp", "bun", "uap", "lymph", "mcv", "wbc"),
  fit = phenoage$fit
)

phenoage_data <- phenoage_own$data

# Pull the full dataset with bioage
dataset_bioage <- left_join(dataset_all_clean, hd_data[, c("sampleID", "hd", "hd_log")], by = "sampleID") %>%
  left_join(., kdm_data[, c("sampleID", "kdm", "kdm_advance")], by = "sampleID") %>%
  left_join(., phenoage_data[, c("sampleID", "phenoage", "phenoage_advance")], by = "sampleID")


# # Scatterplots for chronological age vs. bioage
library(ggplot2)
# ggplot(dataset_bioage, aes(x=age, y=kdm)) + geom_point() + geom_smooth(method=lm , color="red", se=FALSE)
# ggplot(dataset_bioage, aes(x=age, y=phenoage)) + geom_point() + geom_smooth(method=lm , color="red", se=FALSE)
# ggplot(dataset_bioage, aes(x=age, y=hd)) + geom_point() + geom_smooth(method=lm , color="red", se=FALSE)
# ggplot(dataset_bioage, aes(x=age, y=hd_log)) + geom_point() + geom_smooth(method=lm , color="red", se=FALSE)
#
# cor.test(dataset_bioage$age, dataset_bioage$kdm, method = "spearman", exact = FALSE)
# cor.test(dataset_bioage$age, dataset_bioage$phenoage, method = "spearman", exact = FALSE)
# cor.test(dataset_bioage$age, dataset_bioage$hd, method = "spearman", exact = FALSE)
# cor.test(dataset_bioage$age, dataset_bioage$hd_log, method = "spearman", exact = FALSE)

# Calculate Fletcher indices ----------------------------------------------------------------
# Low Fletcher Index (500/1000/2000 Hz)
dataset_bioage$FI_low_r <- (dataset_bioage$audio_right_500 + dataset_bioage$audio_right_1000 + dataset_bioage$audio_right_2000) / 3
dataset_bioage$FI_low_l <- (dataset_bioage$audio_left_500 + dataset_bioage$audio_left_1000 + dataset_bioage$audio_left_2000) / 3
dataset_bioage$FI_low <- (dataset_bioage$FI_low_l + dataset_bioage$FI_low_r) / 2

# High Fletcher Index (4000/6000/8000 Hz)
dataset_bioage$FI_high_r <- (dataset_bioage$audio_right_4000 + dataset_bioage$audio_right_6000 + dataset_bioage$audio_right_8000) / 3
dataset_bioage$FI_high_l <- (dataset_bioage$audio_left_4000 + dataset_bioage$audio_left_6000 + dataset_bioage$audio_left_8000) / 3
dataset_bioage$FI_high <- (dataset_bioage$FI_high_l + dataset_bioage$FI_high_r) / 2

dataset_bioage$pta_1000 <- (dataset_bioage$audio_right_1000 + dataset_bioage$audio_left_1000) / 2
dataset_bioage$pta_8000 <- (dataset_bioage$audio_right_8000 + dataset_bioage$audio_left_8000) / 2

dataset_bioage$telolength_log <- log(dataset_bioage$telolength)

write.csv(dataset_bioage, file = here("Data", "dataset_bioage.csv"), row.names = FALSE)
