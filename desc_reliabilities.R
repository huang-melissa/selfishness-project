#descriptives for paper
table(selfish_data_clean2_w3$SEX)
describe(selfish_data_clean2_w3$AGE)
table(selfish_data_clean2_w3$RACE)

#cronbach alphas

#first recode ICU

master_r <- master
#CAREGIVER VERSION
#reverse code
master_r$P_ICU1 <- car::recode(master_r$P_ICU1, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU3 <- car::recode(master_r$P_ICU3, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU5 <- car::recode(master_r$P_ICU5, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU8 <- car::recode(master_r$P_ICU8, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU13 <- car::recode(master_r$P_ICU13, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU14 <- car::recode(master_r$P_ICU14, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU15 <- car::recode(master_r$P_ICU15, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU16 <- car::recode(master_r$P_ICU16, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU17 <- car::recode(master_r$P_ICU17, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU19 <- car::recode(master_r$P_ICU19, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU23 <- car::recode(master_r$P_ICU23, "0=3; 1=2; 2=1; 3=0")
master_r$P_ICU24 <- car::recode(master_r$P_ICU24, "0=3; 1=2; 2=1; 3=0")

#create standalone dataframes and obtain alphas

#ICU

P_ICU <- master_r %>%
  select(P_ICU1:P_ICU24) %>%
  mutate_all(unclass)

alpha(P_ICU)


#APSD IMPULSIVITY (DARING-IMPULSIVE)

APSD_IMP <- master_r %>%
  select(P_APSD1,P_APSD3,P_APSD7,P_APSD10,P_APSD14) %>%
  mutate_all(unclass)

alpha(APSD_IMP)


#APSD NARCISSISM (GRANDIOSE-MANIPULATIVE)

APSD_NARC <- master_r %>%
  select(P_APSD4,P_APSD6,P_APSD8,P_APSD9,P_APSD11,P_APSD12,P_APSD13) %>%
  mutate_all(unclass)

alpha(APSD_NARC)


#APQ

#INVOLVEMENT
APQ_INV <- master_r %>%
  select(APQ1, APQ4, APQ7, APQ9, APQ11, APQ14, APQ15, APQ20, APQ23, APQ26) %>%
  mutate_all(unclass)

alpha(APQ_INV)

#POSITIVE PARENTING
APQ_PP <- master_r %>%
  select(APQ2, APQ5, APQ13, APQ16, APQ18, APQ27) %>%
  mutate_all(unclass)

alpha(APQ_PP)


#POOR MONITORING/SUPERVISION
APQ_PMS <- master_r %>%
  select(APQ6, APQ10, APQ17, APQ19, APQ21, APQ24, APQ28, APQ29, APQ30, APQ32) %>%
  mutate_all(unclass)

alpha(APQ_PMS)


#INCONSISTENT DISCIPLINE

APQ_ID <- master_r %>%
  select(APQ3, APQ8, APQ12, APQ22, APQ25, APQ31) %>%
  mutate_all(unclass)

#CORPORAL PUNISHMENT

alpha(APQ_CP)

APQ_CP <- master_r %>%
  select(APQ33, APQ35, APQ38) %>%
  mutate_all(unclass)

alpha(APQ_CP)
