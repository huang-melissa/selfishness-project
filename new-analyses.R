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
