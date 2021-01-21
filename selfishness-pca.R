#conduct PCA on SQ

selfish_latent_all <- selfish_data[,c(436:459)] #full sq

#only maladaptive q's
selfish_latent_maladapt <- selfish_data[,c("SQ2", "SQ3", "SQ5", "SQ6", "SQ8", "SQ10", "SQ11",
                                  "SQ13", "SQ14", "SQ15", "SQ16", "SQ18", "SQ19", "SQ21",
                                  "SQ22", "SQ23")]

head(selfish_latent_all) #take a look at data to see if selection successful


#principle component analysis

fit <- princomp(na.omit(selfish_latent_all), cor=FALSE) #use covariance matrix since vars same scale
summary(fit)
loadings(fit)
plot(fit, type="lines")
fit$scores
biplot(fit)

# Varimax Rotated Principal Components
# retaining 2 components
fit <- principal(na.omit(selfish_latent_all), nfactors=2, rotate="varimax")
fit # print results

# Determine Number of Factors to Extract
ev <- eigen(cor(na.omit(selfish_latent_all))) # get eigenvalues
ap <- parallel(subject=nrow(na.omit(selfish_latent_all)),var=ncol(selfish_latent_all),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
