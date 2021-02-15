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
        TOTAL_SQ, APQ_INV, APQ_PP, APQ_PMS, APQ_ID, APQ_CP, APQ_OT, APQ_POSITIVE, APQ_NEGATIVE,
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
