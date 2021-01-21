#ANALYSIS
#only proceed to this step when clean, filtered datasets have been created

#load libraries
library(Hmisc)
library(corrplot)

#determine whether separating by sex is necessary by conducting independent sample t-tests
t.test(selfish_data_clean$TOTAL_SQ~selfish_data_clean$SEX)
t.test(selfish_data_clean$EGO_SQ~selfish_data_clean$SEX)
t.test(selfish_data_clean$PATHO_SQ~selfish_data_clean$SEX)
t.test(selfish_data_clean$ADAPT_SQ~selfish_data_clean$SEX)
t.test(selfish_data_clean$APQ_NEGATIVE~selfish_data_clean$SEX)
t.test(selfish_data_clean$pICU~selfish_data_clean$SEX)
t.test(selfish_data_clean$pICU_CA~selfish_data_clean$SEX)
t.test(selfish_data_clean$pICU_UC~selfish_data_clean$SEX)
t.test(selfish_data_clean$pICU_UE~selfish_data_clean$SEX)
#all t-tests ns,thus analyses thereafter will be conducted on full clean dataset


#Entire correlation matrix, p-values for correlations, and plot

selfish_cor.matrix <- cor(selfish_data_clean, use='pairwise') #use arguments 'everything' 'complete.obs' 'pairwise'
round(selfish_data_clean, 2)

#p-values for correlations
rcorr(selfish_cor.matrix, type = c("pearson","spearman"))

#correlation plot, with numbers
corrplot(selfish_cor.matrix, type = "upper", method='number', order ="hclust",
         tl.col = "black", tl.cex = 0.40, number.cex=0.40)

#individual pairs
#here check to make sure all 3 correlations in proposed model are sig. before proceeding to mediation

#check correlation among parent vs self-reported orig. ICU
cor.test(selfish_data_clean$pICU, selfish_data_clean$ICU, use = "complete.obs")
cor.test(selfish_data_clean$pICU_CA, selfish_data_clean$ICU_CA, use = "complete.obs")
cor.test(selfish_data_clean$pICU_UC, selfish_data_clean$ICU_UC, use = "complete.obs")
cor.test(selfish_data_clean$pICU_UE, selfish_data_clean$ICU_UE, use = "complete.obs")


#TOTAL ICU as DV
#model 1.1a: total sq, negative parenting, total ICU (check parent (prefix p) vs self & orig. VS new factor (suffix _newf)
##change ICU variable prefix and suffix as necessary, as stated in above comment
cor.test(selfish_data_clean$TOTAL_SQ, selfish_data_clean$pICU, use = "complete.obs")
cor.test(selfish_data_clean$TOTAL_SQ, selfish_data_clean$APQ_NEGATIVE, use = "complete.obs")
cor.test(selfish_data_clean$pICU, selfish_data_clean$APQ_NEGATIVE, use = "complete.obs")
#more robust correlations with orig ICU, therefore subsequent analyses with orig.
#adapt sq not inclu. due to ns correlations with either ICU report & inconsis. with apriori hypothesis
#due to self-report ICU & APQ NS, no need to run self-report in models 1.1

#model 1.1b: replace with ego sq
cor.test(selfish_data_clean$EGO_SQ, selfish_data_clean$pICU, use = "complete.obs")
cor.test(selfish_data_clean$EGO_SQ, selfish_data_clean$APQ_NEGATIVE, use = "complete.obs")

#model 1.1c: replace with patho sq
cor.test(selfish_data_clean$PATHO_SQ, selfish_data_clean$pICU, use = "complete.obs")
cor.test(selfish_data_clean$PATHO_SQ, selfish_data_clean$APQ_NEGATIVE, use = "complete.obs")


#CA ICU SUBSCALE AS DV

#due to self-report ICU_CA & APQ NS, no need to run self-report in models 1.2

#model 1.2a: total sq, negative parenting, CA ICU
cor.test(selfish_data_clean$TOTAL_SQ, selfish_data_clean$pICU_CA, use = "complete.obs")
cor.test(selfish_data_clean$pICU_CA, selfish_data_clean$APQ_NEGATIVE, use = "complete.obs")

#model 1.2b: replace with ego sq
cor.test(selfish_data_clean$EGO_SQ, selfish_data_clean$pICU_CA, use = "complete.obs")

#model 1.2c: replace with patho sq
cor.test(selfish_data_clean$PATHO_SQ, selfish_data_clean$pICU_CA, use = "complete.obs")


#UC ICU SUBSCALE AS DV

#due to self-report ICU_UC & APQ NS, no need to run self-report in models 1.3

#model 1.3a: total sq, negative parenting, CA ICU
cor.test(selfish_data_clean$TOTAL_SQ, selfish_data_clean$pICU_UC, use = "complete.obs")
cor.test(selfish_data_clean$pICU_UC, selfish_data_clean$APQ_NEGATIVE, use = "complete.obs")

#model 1.3b: replace with ego sq
cor.test(selfish_data_clean$EGO_SQ, selfish_data_clean$pICU_UC, use = "complete.obs")

#model 1.3c: replace with patho sq
cor.test(selfish_data_clean$PATHO_SQ, selfish_data_clean$pICU_UC, use = "complete.obs")


#UE ICU SUBSCALE AS DV

#due to self-report ICU_UE & APQ NS, no need to run self-report in models 1.4

#model 1.4a: total sq, negative parenting, CA ICU
cor.test(selfish_data_clean$TOTAL_SQ, selfish_data_clean$pICU_UE, use = "complete.obs")
cor.test(selfish_data_clean$pICU_UE, selfish_data_clean$APQ_NEGATIVE, use = "complete.obs")

#model 1.4b: replace with ego sq
cor.test(selfish_data_clean$EGO_SQ, selfish_data_clean$pICU_UE, use = "complete.obs")

#model 1.4c: replace with patho sq
cor.test(selfish_data_clean$PATHO_SQ, selfish_data_clean$pICU_UE, use = "complete.obs")

#based on correlation analysis run the following models for mediation: 1.1a, 1.1b, 1.2a, 1.2b, 1.2c, 1.3b, 1.4b (parent-report only)

#MEDIATE, REGRESS, PLOT

#load libraries
library(diagram)
library(lme4)
library(QuantPsyc)

#these subsequent analyses require A.F Hayes' process macro for R
#run the script for process and then run the following model
#regression needs to be run because process in R does not provide coeff for x to y without m

#mediation model 1.1a
process(selfish_data_clean, y="pICU", x="TOTAL_SQ", m="APQ_NEGATIVE", cov=c("SEX","RACE","AGE"), model=4, stand=1)

#regression model 1.1a
m1.1a1 <- lm(pICU ~ AGE + SEX + RACE + TOTAL_SQ, na.action = na.exclude, data = selfish_data_clean)
summary(m1.1a1)
lm.beta(m1.1a1)

m1.1a2 <- lm(pICU ~ AGE + SEX + RACE + TOTAL_SQ + APQ_NEGATIVE, na.action = na.exclude, data = selfish_data_clean)
summary(m1.1a2)
lm.beta(m1.1a2)

#plot model 1.1a; replace ## with standardized coefficients 
coeff1.1a <- c(0, "'.##'", 0, #x to m
               0, 0, 0,
               "'.##'", "'.## (.##)'", 0) #m to y, then x to y
matrix1.1a<- matrix(nrow=3, ncol=3, byrow = TRUE, data=coeff1.1a)
plot1.1a<- plotmat(matrix1.1a, pos=c(1,2),
                   name=c("Mediator","Independent Variable", "Dependent Variable"), #m, x, y
                   box.type ="rect", box.size = 0.15, box.prop=0.05, curve=0, arr.length=0.2, arr.width=0.1)


#repeat lines 119 thru 138 for subsequent models
