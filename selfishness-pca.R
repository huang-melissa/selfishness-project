library(stats)
library(dplyr)
library(psych)
library(nFactors)

#conduct PCA on SQ

selfish_latent_all <- selfish_data[,c(436:459)] #full sq

#only maladaptive q's
selfish_latent_maladapt <- selfish_data[,c("SQ2", "SQ3", "SQ5", "SQ6", "SQ8", "SQ10", "SQ11",
                                  "SQ13", "SQ14", "SQ15", "SQ16", "SQ18", "SQ19", "SQ21",
                                  "SQ22", "SQ23")]

head(selfish_latent_all) #take a look at data to see if selection successful


#principle component analysis

sq_all.pca <- princomp(na.omit(selfish_latent_all), cor=FALSE) #use covariance matrix since vars same scale
summary(sq_all.pca)
loadings(sq_all.pca)
plot(sq_all.pca) #bar
plot(sq_all.pca, type="lines") #scree
sq_all.pca$scores
biplot(sq_all.pca)

# Varimax Rotated Principal Components
# retaining 2 components
sq_all.pca <- principal(na.omit(selfish_latent_all), nfactors=2, rotate="varimax")
sq_all.pca # print results

# Determine Number of Factors to Extract
ev <- eigen(cor(na.omit(selfish_latent_all))) # get eigenvalues
ap <- parallel(subject=nrow(na.omit(selfish_latent_all)),var=ncol(selfish_latent_all),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


#exploratory factor analysis

# retaining 3 components
sq_all.fa3 <- factanal(na.omit(selfish_latent_all), factors=3, rotation="varimax", scores="regression")

# retaining 2 components
sq_all.fa2 <- factanal(na.omit(selfish_latent_all), factors=2, rotation="varimax", scores="regression")
