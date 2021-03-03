# Set up working directories, load packages, import data, merge datasets
library(dplyr)
library(psych)

getwd()
setwd("/Users/mellyzors/Desktop/fathering") #note enable this line to set wd

options(scipen=999)

#load full assessment file
path1 <- file.path("/Users/mellyzors/Desktop/fathering/master.csv")
master <- read.csv(path1, stringsAsFactors = FALSE)

#create a sheet to contain all variables to be recoded
selfish_data <- master


## Selfishness Questionnaire (SQ) Raine & Uh, 2018 - 24 items; (scale: 0 through 2)

### Egocentric subscale: (2, 6, 10, 13, 15, 18, 21, 23)
### Adaptive subscale: (1, 4, 7, 9, 12, 17, 20, 24)
### Pathologic subscale: (3, 5, 8, 11, 14, 16, 19, 22)
### Total: sum of all 3

selfish_data <- selfish_data %>%
  rowwise() %>%
  mutate(EGO_SQ = mean(c(SQ2, SQ6, SQ10, SQ13, SQ15, SQ18, SQ21, SQ23), na.rm=TRUE)*8) %>%
  mutate(ADAPT_SQ = mean(c(SQ1, SQ4, SQ7, SQ9, SQ12, SQ17, SQ20, SQ24), na.rm=TRUE)*8) %>%
  mutate(PATHO_SQ = mean(c(SQ3, SQ5, SQ8, SQ11, SQ14, SQ16, SQ19, SQ22), na.rm=TRUE)*8) %>%
  mutate(TOTAL_SQ = sum(EGO_SQ, ADAPT_SQ, PATHO_SQ, na.rm=TRUE))


## Alabama Parenting Questionnaire (APQ) Frick, 1991; 42 items
### 5 point likert scale: 1 (Never), 2 (Almost Never), 3 (Sometimes), 4 (Often), 5 (Always).

### Involvement (1, 4, 7, 9, 11, 14, 15, 20, 23, 26) 10 items.
### Positive Parenting (2, 5, 13, 16, 18, 27) 6 items.
### Poor Monitoring/Supervision (6, 10, 17, 19, 21, 24, 28, 29, 30, 32) 10 items.
### Inconsistent Discipline (3, 8, 12, 22, 25, 31) 6 items.
### Corporal Punishment (33, 35, 38) 3 items.
### Other Discipline Practices* (34, 36, 37, 39, 40, 41, 42) 7 items.

#### No reverse coding necessary.
#### Sum all items in the scale to obtain a total scale score
#### You may subtract this score by the number of items in the subscale so that the score range begins at zero)
#### Higher scores in the positive scales (involvement, positive parenting) show efficient parenting practices
#### Higher scores in the negative scales indicate inefficient practices
#### Other Discipline Practices is not a scale, but provides information on an item by item basis

#reverse code positive items for selfish project analysis
selfish_data$APQ1r <- car::recode(selfish_data$APQ1, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ4r <- car::recode(selfish_data$APQ4, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ7r <- car::recode(selfish_data$APQ7, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ9r <- car::recode(selfish_data$APQ9, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ11r <- car::recode(selfish_data$APQ11, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ14r <- car::recode(selfish_data$APQ14, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ15r <- car::recode(selfish_data$APQ15, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ20r <- car::recode(selfish_data$APQ20, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ23r <- car::recode(selfish_data$APQ23, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ26r <- car::recode(selfish_data$APQ26, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ2r <- car::recode(selfish_data$APQ2, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ5r <- car::recode(selfish_data$APQ5, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ13r <- car::recode(selfish_data$APQ13, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ16r <- car::recode(selfish_data$APQ16, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ18r <- car::recode(selfish_data$APQ18, "1=5; 2=4; 3=3; 4=2; 5=1")
selfish_data$APQ27r <- car::recode(selfish_data$APQ27, "1=5; 2=4; 3=3; 4=2; 5=1")

selfish_data <-selfish_data %>%
  rowwise() %>%
  mutate(APQ_INVr = mean(c(APQ1r, APQ4r, APQ7r, APQ9r, APQ11r, APQ14r, APQ15r, APQ20r, APQ23r, APQ26r), na.rm=TRUE)*10) %>%
  mutate(APQ_PPr = mean(c(APQ2r, APQ5r, APQ13r, APQ16r, APQ18r, APQ27r), na.rm=TRUE)*6) %>%
  mutate(APQ_PMS = mean(c(APQ6, APQ10, APQ17, APQ19, APQ21, APQ24, APQ28, APQ29, APQ30, APQ32), na.rm=TRUE)*10) %>%
  mutate(APQ_ID = mean(c(APQ3, APQ8, APQ12, APQ22, APQ25, APQ31), na.rm=TRUE)*6) %>%
  mutate(APQ_CP = mean(c(APQ33, APQ35, APQ38), na.rm=TRUE)*3) %>%
  mutate(APQ_OT = mean(c(APQ34, APQ36, APQ37, APQ39, APQ40, APQ41, APQ42), na.rm=TRUE)*7) %>%
  mutate(APQ_POSITIVEr = sum(APQ_INVr, APQ_PPr, na.rm=TRUE)) %>%
  mutate(APQ_NEGATIVE = sum(APQ_PMS, APQ_ID, APQ_CP, na.rm=TRUE))


## Inventory of Callous Unemotional Traits (ICU) Frick, 2004; 24 items(scale: 0 through 3)

## Original:
### Callous: 2, 4, 7, 8, 9, 10, 12, 18, 11, 20, 21 (11 items)
### Uncaring: 3r, 5r, 13r, 15r, 16r, 17r, 23, 24 (8 items)
### Unemotional: 1r, 6, 14r, 19r, 22 (5 items)
#### Sum for total

## New factor structure:
### Callous: 4, 7, 9, 11, 12, 18, 20 (7 items)
### Uncaring: 3r, 5r, 13r, 15r, 16r, 17r, 23r, 24r (8 items)
### Unemotional: 1r, 14r, 19r, 22 (4 items)
#### Sum for total

#CAREGIVER VERSION
#reverse code
selfish_data$P_ICU1r <- car::recode(selfish_data$P_ICU1, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU3r <- car::recode(selfish_data$P_ICU3, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU5r <- car::recode(selfish_data$P_ICU5, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU8r <- car::recode(selfish_data$P_ICU8, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU13r <- car::recode(selfish_data$P_ICU13, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU14r <- car::recode(selfish_data$P_ICU14, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU15r <- car::recode(selfish_data$P_ICU15, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU16r <- car::recode(selfish_data$P_ICU16, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU17r <- car::recode(selfish_data$P_ICU17, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU19r <- car::recode(selfish_data$P_ICU19, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU23r <- car::recode(selfish_data$P_ICU23, "0=3; 1=2; 2=1; 3=0")
selfish_data$P_ICU24r <- car::recode(selfish_data$P_ICU24, "0=3; 1=2; 2=1; 3=0")

#original
selfish_data <-selfish_data %>%
  rowwise() %>%
  mutate(pICU_CA = mean(c(P_ICU4,P_ICU8r,P_ICU9,P_ICU18,P_ICU11,P_ICU21,P_ICU7,P_ICU20,P_ICU2,P_ICU10,P_ICU12), na.rm=TRUE)*11) %>%
  mutate(pICU_UC = mean(c(P_ICU15r,P_ICU23r,P_ICU16r,P_ICU3r,P_ICU17r,P_ICU24r,P_ICU13r,P_ICU5r), na.rm=TRUE)*8) %>%
  mutate(pICU_UE = mean(c(P_ICU1r,P_ICU19r,P_ICU6,P_ICU22,P_ICU14r), na.rm=TRUE)*5) %>%
  mutate(pICU = sum(pICU_CA,pICU_UC,pICU_UE, na.rm=TRUE))

#new factor
selfish_data <-selfish_data %>%
  rowwise() %>%
  mutate(pICU_CA_newf = mean(c(P_ICU4,P_ICU7,P_ICU9,P_ICU11,P_ICU12,P_ICU18,P_ICU20), na.rm=TRUE)*7) %>%
  mutate(pICU_UC_newf = mean(c(P_ICU3r,P_ICU5r,P_ICU13r,P_ICU15r,P_ICU16r,P_ICU17r,P_ICU23r,P_ICU24r), na.rm=TRUE)*8) %>%
  mutate(pICU_UE_newf = mean(c(P_ICU1r,P_ICU14r,P_ICU19r,P_ICU22), na.rm=TRUE)*4) %>%
  mutate(pICU_newf = sum(pICU_CA_newf,pICU_UC_newf,pICU_UE_newf, na.rm=TRUE))


#SELF-REPORT (YOUTH) VERSION
#reverse code
selfish_data$ICU1r <- car::recode(selfish_data$ICU1, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU3r <- car::recode(selfish_data$ICU3, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU5r <- car::recode(selfish_data$ICU5, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU8r <- car::recode(selfish_data$ICU8, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU13r <- car::recode(selfish_data$ICU13, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU14r <- car::recode(selfish_data$ICU14, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU15r <- car::recode(selfish_data$ICU15, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU16r <- car::recode(selfish_data$ICU16, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU17r <- car::recode(selfish_data$ICU17, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU19r <- car::recode(selfish_data$ICU19, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU23r <- car::recode(selfish_data$ICU23, "0=3; 1=2; 2=1; 3=0")
selfish_data$ICU24r <- car::recode(selfish_data$ICU24, "0=3; 1=2; 2=1; 3=0")

#original
selfish_data <-selfish_data %>%
  rowwise() %>%
  mutate(ICU_CA = mean(c(ICU4,ICU8r,ICU9,ICU18,ICU11,ICU21,ICU7,ICU20,ICU2,ICU10,ICU12), na.rm=TRUE)*11) %>%
  mutate(ICU_UC = mean(c(ICU15r,ICU23r,ICU16r,ICU3r,ICU17r,ICU24r,ICU13r,ICU5r), na.rm=TRUE)*8) %>%
  mutate(ICU_UE = mean(c(ICU1r,ICU19r,ICU6,ICU22,ICU14r), na.rm=TRUE)*5) %>%
  mutate(ICU = sum(ICU_CA,ICU_UC,ICU_UE, na.rm=TRUE))

#new factor
selfish_data <-selfish_data %>%
  rowwise() %>%
  mutate(ICU_CA_newf = mean(c(ICU4,ICU7,ICU9,ICU11,ICU12,ICU18,ICU20), na.rm=TRUE)*7) %>%
  mutate(ICU_UC_newf = mean(c(ICU3r,ICU5r,ICU13r,ICU15r,ICU16r,ICU17r,ICU23r,ICU24r), na.rm=TRUE)*8) %>%
  mutate(ICU_UE_newf = mean(c(ICU1r,ICU14r,ICU19r,ICU22), na.rm=TRUE)*4) %>%
  mutate(ICU_newf = sum(ICU_CA_newf,ICU_UC_newf,ICU_UE_newf, na.rm=TRUE))


## APSD (Antisocial Process Screening Device) Frick & Hare, 2001; 12 items (scale: 0 through 2)

### Narcissism: 4, 6, 8, 11, 12, 13 (7 items)
### Impulsivity: 1, 3, 7, 10, 14 (5 items)

#CAREGIVER VERSION
selfish_data <-selfish_data %>%
  rowwise() %>%
  mutate(pAPSD_NARC = mean(c(P_APSD4,P_APSD6,P_APSD8,P_APSD9,P_APSD11,P_APSD12,P_APSD13), na.rm=TRUE)*7) %>%
  mutate(pAPSD_IMP = mean(c(P_APSD1,P_APSD3,P_APSD7,P_APSD10,P_APSD14), na.rm=TRUE)*5)

#SELF-RERPORT (YOUTH) VERSION
selfish_data <-selfish_data %>%
  rowwise() %>%
  mutate(APSD_NARC = mean(c(APSD4,APSD6,APSD8,APSD9,APSD11,APSD12,APSD13), na.rm=TRUE)*7) %>%
  mutate(APSD_IMP = mean(c(APSD1,APSD3,APSD7,APSD10,APSD14), na.rm=TRUE)*5)

## Inventory of parent and peer attachment (IPPA) - Armsden & Greensberg, 1989; (scale: 0, 1, 2)

#### Parent attachment
##### Trust (10 items) - 1,2,3r,4,10r,13,14,21,23,24
##### Communication (10 items) - 5r,6,7r,8,15r, 16,17,20,26,28
##### Alienation (8 items) - 9r,11r,12r,18r,19r,22r,25r,27r

#compute scores
selfish_data <- selfish_data %>%
  mutate_at(vars(IPPAR3,IPPAR5,IPPAR7,IPPAR9,IPPAR10,IPPAR11,IPPAR12,IPPAR15,IPPAR18,IPPAR19,IPPAR22,IPPAR25,IPPAR27,IPPAR32,IPPAR33,IPPAR37,IPPAR38,IPPAR39,IPPAR46,IPPAR50,IPPAR51), car::recode, "0=2; 1=1; 2=0")
selfish_data <- selfish_data %>%
  rowwise() %>%
  mutate(IPPA_PTRUST = mean(c(IPPAR1,IPPAR2,IPPAR3,IPPAR4,IPPAR10,IPPAR13,IPPAR14,IPPAR21,IPPAR23,IPPAR24), na.rm=TRUE)*10) %>%
  mutate(IPPA_PCOM = mean(c(IPPAR5,IPPAR6,IPPAR7,IPPAR8,IPPAR15,IPPAR16,IPPAR17,IPPAR20,IPPAR26,IPPAR28), na.rm=TRUE)*10) %>%
  mutate(IPPA_PALIEN = mean(c(IPPAR9,IPPAR11,IPPAR12,IPPAR18,IPPAR19,IPPAR22,IPPAR25,IPPAR27), na.rm=TRUE)*8) %>%
  mutate(IPPA_PA_tot = sum(IPPA_PTRUST, IPPA_PCOM,IPPA_PALIEN, na.rm=TRUE))



#run this only after all relevant variables have been coded via codevars.R

#rename variables for readability prior to creating sheet with subset of data
selfish_data <- selfish_data %>%
  rename(CAREGIVERSEX = DEMO1) %>%
  rename(SEX = DEMO2) %>%
  rename(RACE = DEMO3A) %>%
  rename(AGE = DEMO6A)

#select relevant variables and filter out those without physio data in wave 3
selfish_data_clean <- selfish_data %>%
  select(FUV3ID, CAREGIVERSEX, SEX, RACE, AGE, EGO_SQ, ADAPT_SQ, PATHO_SQ,
         TOTAL_SQ, APQ_INVr, APQ_PPr, APQ_PMS, APQ_ID, APQ_CP, APQ_OT, APQ_POSITIVEr, APQ_NEGATIVE,
         pICU_CA, pICU_UC, pICU_UE, pICU, pICU_CA_newf, pICU_UC_newf, pICU_UE_newf, pICU_newf, pAPSD_NARC, pAPSD_IMP, ICU_CA, ICU_UC, ICU_UE, ICU, ICU_CA_newf,
         ICU_UC_newf, ICU_UE_newf, ICU_newf, APSD_NARC, APSD_IMP, IPPA_PTRUST, IPPA_PCOM, IPPA_PALIEN, IPPA_PA_tot)

head(selfish_data_clean) #take a look at data to see if selection and filter were successful

#save file
write.csv(selfish_data_clean,"/Users/mellyzors/Desktop/fathering/selfish_data_clean.csv", row.names = FALSE)

#filter & create subsets of data by child sexes OR congruent respondent sexes
selfish_data_males <- selfish_data_clean %>%
  filter(SEX == "1")

selfish_data_females <- selfish_data_clean %>%
  filter(SEX == "2")

selfish_data_congru <- selfish_data_clean %>%
  filter(CAREGIVERSEX == SEX)

## load, merge previous datas
w1 <- file.path("/Users/mellyzors/Desktop/ICU_APSD_W1.csv")

read.csv(w1, stringsAsFactors = FALSE)

WAVE1 <- read.csv(w1, stringsAsFactors = FALSE)

w2 <- file.path("/Users/mellyzors/Desktop/ICU_APSD_W2.csv")

read.csv(w2, stringsAsFactors = FALSE)

WAVE2 <- read.csv(w2, stringsAsFactors = FALSE)

WAVE1WAVE2 <- merge(WAVE1, WAVE2, by="FUV3ID", all = TRUE)

selfish_data_clean2 <- merge(WAVE1WAVE2, selfish_data_clean,
                             by="FUV3ID", all = TRUE)

#drop cases without w3 data
library(tidyr)
selfish_data_clean2_w3 <- selfish_data_clean2 %>% drop_na(pICU)

#save file
write.csv(selfish_data_clean2_w3,"/Users/mellyzors/Desktop/fathering/selfish_data_clean2_w3.csv", row.names = FALSE)
