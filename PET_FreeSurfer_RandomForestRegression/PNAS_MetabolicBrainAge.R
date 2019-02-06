# Metabolic Brain Age
#
# Data is from several PET based studies obtained at
# the Neuroimaging Laboratories at Washington University
# School of Medicine in St. Louis, MO., published in 
# Cell Metabolism 2017 (Goyal, Vlassenko et al.)
#
# Code started: 08/23/2017
# Last updated: 11/08/2018
#
# Note that this data is deidentified without any IDs

setwd("~/R/Lifespan Metabolism/Metabolic Brain Age/PNAS")
par.defaults = par(no.readonly=TRUE)

library(ggplot2)
library(grid)
library(reshape)
library(randomForest)
library(preprocessCore)


# Color blind friendly palette
cbP = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pie(rep(1,8), col=cbP)

# Load the data file, from Cell Metabolism 2017
load("CoreData_DeID.RData")

# ID, Age, Gender, Study, PIB (Note that ID is "NA" as this data is deidentified)
samples = adultmetabFULL[,1:5]

# Age, group, and sex distribution of subjects included
ggplot(adultmetabFULL, aes(x=Age, fill=Gender)) +
  geom_histogram(binwidth=10,alpha=0.5,position="identity") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.key.size = unit(4,"mm")) +
  xlab("Age (Years)") +
  ylab("Number of study participants") +
  scale_fill_manual(values=cbP[7:6], 
                    name="Sex/Gender",
                    breaks=c("F","M"),
                    labels=c("Females", "Males"))


ggplot(adultmetabFULL, aes(x=Age, fill=Study)) +
  geom_histogram(binwidth=10,alpha=0.5,position="identity") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.key.size = unit(4,"mm")) +
  xlab("Age (Years)") +
  ylab("Number of study participants") +
  scale_fill_manual(values=cbP[2:8], 
                    name="Study",
                    labels=c("ACS","DIAN","N33","PET/CT","AM/PM","WSCT"))


min(adultmetabFULL$Age)
max(adultmetabFULL$Age)

# Note that age range is 20-82
# Note relatively less participants in 40-50 year olds.

table(adultmetabFULL$Study)
table(adultmetabFULL$Study[adultmetabFULL$PIB=="Pos"])

table(adultmetabFULL$Gender)
# Note 138 females and 96 males

adultmetabFULL$Age[is.na(adultmetabFULL$PIB)]
# Only 48 did not get amyloid imaging, nearly all under age of 36, except 3 subjects.

# DATA NORMALIZATION

# Age and sex / gender explain much of the variance in adult human brain metabolism.
# Some of the results below were previously published in Goyal, Vlassenko et al. Cell Metabolism
# 2017, but are replicated here as they inform metabolic brain age derivation

# The first step is to perform quantile normalization
# Important to do this independently for each metabolic parameter,
# to isolate the topographic information int he data

# We will include gray and white matter. The following are column numbers
# GM: AG 10-52, CMRGlc 89-131, CMRO2 168-209, CBF 247-289
# WM: AG 53-88, CMRGlc 132-167, CMRO2 210-246, CBF 290-325

# Note that the normalization is done across subjects, 
# e.g., the mean and SD of AG is equal for each subject (not across regions)

AG.normFULL = t(normalize.quantiles(t(adultmetabFULL[,10:88])))
CMRGlc.normFULL = t(normalize.quantiles(t(adultmetabFULL[,89:167])))
CMRO2.normFULL = t(normalize.quantiles(t(adultmetabFULL[,168:246])))
CBF.normFULL = t(normalize.quantiles(t(adultmetabFULL[,247:325])))
all.normFULL = cbind(AG.normFULL, CMRGlc.normFULL, CMRO2.normFULL, CBF.normFULL)

AG.norm = t(normalize.quantiles(t(adultmetab[,10:88])))
CMRGlc.norm = t(normalize.quantiles(t(adultmetab[,89:167])))
CMRO2.norm = t(normalize.quantiles(t(adultmetab[,168:246])))
CBF.norm = t(normalize.quantiles(t(adultmetab[,247:325])))
all.norm = cbind(AG.norm, CMRGlc.norm, CMRO2.norm, CBF.norm)

# Perform multidimensional scaling of normalized data across subjects
subjectdist.norm = dist(all.norm)
loc = cmdscale(subjectdist.norm)
xreg = loc[,1]
yreg = loc[,2]

MDS = data.frame(Age = adultmetab$Age, PC1 = loc[,1], PC2 = loc[,2], Sex = adultmetab$Gender)
cor.test(MDS[,1],MDS[,2])
cor.test(MDS[,1],MDS[,3])

range01 <- function(x)(x-min(x))/diff(range(x))
cRamp <- function(x){
  cols <- colorRamp(rainbow(6))(range01(x))
  apply(cols, 1, function(xt)rgb(xt[1],xt[2],xt[3],maxColorValue=255))
}
# Note this plot was published as supplemental info in Goyal, Vlassenko et al. 2017
plot(xreg, yreg, xlab = "First Principal Coordinate", ylab = "Second Principal Coordinate",
     main = "MDS of individuals after quantile normalization",
     col = cRamp(adultmetab$Age),
     type = "p", pch = 19, cex = 1.7,
     cex.lab = 1.4, xlim = c(-100,175))
legend("right",c("20s","30s","40s","50s","60s","70s","80s"),
       fill = rainbow(7), title = "Age")

plot(xreg, yreg, xlab = "First Principal Coordinate", ylab = "Second Principal Coordinate",
     main = "MDS of individuals after quantile normalization",
     col = c("cadetblue3","coral")[adultmetab$Gender],
     type = "p", pch = 19, cex = 1.5,
     cex.lab = 1.4, xlim = c(-100,175))
legend("right",c("Female","Male"),
       fill = c("cadetblue3","coral"), title = "Sex/Gender")

t.test(MDS[adultmetab$Gender=="F",2],MDS[adultmetab$Gender=="M",2])
t.test(MDS[adultmetab$Gender=="F",3],MDS[adultmetab$Gender=="M",3])
# Note that main gender effect is along PC1 similar to age
# hinting that females might have younger looking brains

# Now PCA to determine how much variance explained by individual components
pca1 = prcomp(all.norm, center=TRUE, scale.=TRUE)
summary(pca1)
plot(pca1, type = "l")
qplot(c(1:184),summary(pca1)$importance[3,]*100,
      xlab = "Principal Component", ylab = "Explained variance (%)",
      size = I(3), color = I("skyblue"))
# As published previously, many principal components required to achieve high explained variance.
# Thus the data is highly complex

# The above results have been published (Goyal, Vlassenko et al. Cell Metabolism 2017).
# The results below have not been.

# METABOLIC BRAIN AGE: Derivation with Random Forest Regression

# Random Forest Regression, Bias Corrected (Zhang and Lu 2011 J Applied Stats)
# Define function that does RF and bias correction on training data
# Use predict function with bias correction on test data

# The following trains the bias corrected random forest regression (rfbc) function
rfbc = function(trainingdata,actualtrain,ntree1=10000,importance1=TRUE,spar1=1){
  
  rfbctrained = randomForest(trainingdata,
                             actualtrain,
                             ntree=ntree1,
                             importance=importance1)
  
  # Now need to compute bias with cubic spline
  biasfix = smooth.spline(actualtrain~rfbctrained$predicted, spar=spar1)
  return(list("RFR" = rfbctrained, "BF" = biasfix))
}

# The following tests new data with trained rfbc function
rfbctest = function(rfbctrained,newdata){
  
  # Predict newfit non-bias corrected based on rfbctrained
  newfitbiased = predict(rfbctrained$RFR,newdata)
  
  # Correct prediction using trained bias correction
  newfit = predict(rfbctrained$BF,newfitbiased)
  
  return(newfit$y)
}

# The following wrapper does 3 things:
# Trains rfbc (input training data)
# Tests rfbc with correlations (input test data)
# Output tests and trains and corresponding x (e.g., actual age) data
# "Fit" data corresponds to predicted age

rfbcwrap = function(trainingdata, trainx, testdata, testx){
  trained = rfbc(trainingdata,trainx)
  testfit = rfbctest(trained,testdata)
  trainfit = predict(trained$BF,trained$RFR$predicted)$y
  return(list("testfit"=testfit,"trainfit"=trainfit,"trainx"=trainx,"testx"=testx))
}

# Defaults for ggplots of RFBC results
pointsize=2.5; lmsize=1.2; axti = 15; axtx = 15; bfill = 'lightgray';

# RUN RFBC ON DATA AND PERFORM INTERNAL VALIDATION

# Run main test via split validation, 80/20

traintest = runif(dim(adultmetab)[1])<0.80
table(traintest)
rfmain = rfbcwrap(all.norm[which(traintest==TRUE),],adultmetab$Age[traintest==TRUE],
                  all.norm[which(traintest==FALSE),],adultmetab$Age[traintest==FALSE])

cor.test(rfmain$testfit,rfmain$testx)
sd(rfmain$testfit-rfmain$testx)
min(rfmain$testfit-rfmain$testx)
max(rfmain$testfit-rfmain$testx)

# Run rfbc via split validation, 70/30 for males and females independently

adultmetabMales = all.norm[adultmetab$Gender=="M",]
MalesAge = adultmetab$Age[adultmetab$Gender=="M"]
traintest = runif(dim(adultmetabMales)[1])<0.70
table(traintest)
rfmain = rfbcwrap(adultmetabMales[which(traintest==TRUE),],MalesAge[traintest==TRUE],
                  adultmetabMales[which(traintest==FALSE),],MalesAge[traintest==FALSE])

cor.test(rfmain$testfit,rfmain$testx)
sd(rfmain$testfit-rfmain$testx)
min(rfmain$testfit-rfmain$testx)
max(rfmain$testfit-rfmain$testx)

adultmetabFemales = all.norm[adultmetab$Gender=="F",]
FemalesAge = adultmetab$Age[adultmetab$Gender=="F"]
traintest = runif(dim(adultmetabFemales)[1])<0.70
table(traintest)
rfmain = rfbcwrap(adultmetabFemales[which(traintest==TRUE),],FemalesAge[traintest==TRUE],
                  adultmetabFemales[which(traintest==FALSE),],FemalesAge[traintest==FALSE])

cor.test(rfmain$testfit,rfmain$testx)
sd(rfmain$testfit-rfmain$testx)
min(rfmain$testfit-rfmain$testx)
max(rfmain$testfit-rfmain$testx)

# For males, 70/30 split shows a correlation of 0.81
# For females, 70/30 split shows a correlation of 0.87

# Now do 10 fold cross validation
# Randomly create 10-fold vector
tenfoldx10 = matrix(NA,10,4)
colnames(tenfoldx10) = c("MeanR","SD","Min","Max")

for (nn in 1:10){
  
  fold10 = vector(mode="numeric",length=184)
  fold10[1:18]=1;fold10[19:36]=2;fold10[37:55]=3;fold10[56:73]=4;fold10[74:92]=5;
  fold10[93:110]=6;fold10[111:129]=7;fold10[130:148]=8;fold10[149:166]=9;fold10[167:184]=10;
  
  # Now randomized fold10
  fold10 = fold10[sample(length(fold10))]
  table(fold10)
  
  rfresults = matrix(NA,10,4)
  date()
  for(n in 1:10){
    rfmain = rfbcwrap(all.norm[which(fold10!=n),],adultmetab$Age[fold10!=n],
                      all.norm[which(fold10==n),],adultmetab$Age[fold10==n])
    rfresults[n,1] = cor(rfmain$testfit,rfmain$testx)
    rfresults[n,2] = sd(rfmain$testfit-rfmain$testx)
    rfresults[n,3] = min(rfmain$testfit-rfmain$testx)
    rfresults[n,4] = max(rfmain$testfit-rfmain$testx)
    print(n)
  }
  date()
  
  # 10-fold validation takes 6-7 minutes. Thus can do 10 runs per hour, or 100 runs in 10 hours.
  
  tenfoldx10[nn,] = colMeans(rfresults)
  
}

colMeans(tenfoldx10)

# RESUME HERE

# So based on colMeans 10-fold, R = 0.88-0.90 (mean 0.89), sd = 8.7y, min = -17.9y, max = 15.9y

# Now do more typical 10-fold, collating all test data to test accuracy

fold10results = matrix(NA,184,2)
date()
startind = 1;
for(n in 1:10){
  rfmain = rfbcwrap(all.norm[which(fold10!=n),],adultmetab$Age[fold10!=n],
                    all.norm[which(fold10==n),],adultmetab$Age[fold10==n])
  endind = sum(table(fold10)[1:n])
  fold10results[startind:endind,1] = rfmain$testx
  fold10results[startind:endind,2] = rfmain$testfit
  startind = sum(table(fold10)[1:n])+1
  print(n)
}
date()

cor(fold10results)

# Note that collating the test results produces a very similar correlation.
# Discussions with lab feel that first method is perhaps slightly more rigourous.

# Now do bootstrap. Randomly reassort actual ages. Note that this script will likely take a long time (10 hours on my Surface Pro).

# In case you do not want to run the bootstrap again:
# BootResults = read.csv(file="BootResults.csv")[,2:101]

BootResults = matrix(NA,10,100)
date()
for (m in 1:100){
  NewAge = sample(adultmetab$Age)
  rfresults = matrix(NA,10,4)
  for(n in 1:10){
    rfmain = rfbcwrap(all.norm[which(fold10!=n),],NewAge[fold10!=n],
                      all.norm[which(fold10==n),],NewAge[fold10==n])
    rfresults[n,1] = cor(rfmain$testfit,rfmain$testx)
    rfresults[n,2] = sd(rfmain$testfit-rfmain$testx)
    rfresults[n,3] = min(rfmain$testfit-rfmain$testx)
    rfresults[n,4] = max(rfmain$testfit-rfmain$testx)
  }
  BootResults[,m] = rfresults[,1]
  print(m)
}
date()


# Results from bootstrap analysis
min(colMeans(BootResults))
max(colMeans(BootResults))
sd(colMeans(BootResults))

# Create ggplot for collated test data above
# Also note longitudinal cases
rfplot = data.frame("Actual"=fold10results[,1],
                    "Predicted" = fold10results[,2])

# Defaults for ggplots of RFBC results
pointsize=2.5; lmsize=1.2; axti = 15; axtx = 15; bfill = 'lightgray';

ggplot(rfplot,aes(Actual,Predicted)) +
  geom_point(size=pointsize) +
  geom_smooth(method="lm",size=lmsize) +
  coord_cartesian(xlim = c(15,85),ylim = c(15,85)) +
  theme(axis.title = element_text(size = axti),
        axis.title.y = element_text(vjust=0.5),
        axis.title.x = element_text(vjust=0.3),
        axis.text = element_text(size = axtx,color="black"),
        plot.margin=unit(c(.2,.3,.1,.1),"cm"),
        panel.background=element_rect(fill=bfill),
        legend.position = "top") +
  labs(color="") +
  scale_color_manual(values=cbP[6:7]) +
  xlab("Actual Age (Years)") +
  ylab("Predicted Age (Years)") 

ggsave("RFRtestcases.tiff",width=85,height=95,units="mm",dpi=1200)

# Identify longitudinal participants, and superimpose lines

repRF = rfbcwrap(all.norm,adultmetab$Age,
                 all.norm[repeatAGrows[,2],],adultmetab$Age[repeatAGrows[,2]])

mean(repRF$trainfit[repeatAGrows[,2]]-repRF$trainfit[repeatAGrows[,1]])
mean(repRF$trainx[repeatAGrows[,2]]-repRF$trainx[repeatAGrows[,1]])
sd(repRF$trainfit[repeatAGrows[,2]]-repRF$trainfit[repeatAGrows[,1]])

t.test(repRF$trainfit[repeatAGrows[,1]],
       repRF$trainfit[repeatAGrows[,2]],
       paired=TRUE, var.equal=TRUE,alternative="less")

cor.test(repRF$trainfit[repeatAGrows[,2]],repRF$trainfit[repeatAGrows[,1]])

# Very high correlation! R > 0.80, p < 0.0001

# Now check if they all go in the same direction

repplot = data.frame("ID" = c((1:19),(1:19)),
                     "Scan" = c(rep("Initial",19),rep("Repeat",19)),
                     "AgeDif" = c(repRF$trainfit[repeatAGrows[,1]]-repRF$trainx[repeatAGrows[,1]],
                                  repRF$trainfit[repeatAGrows[,2]]-repRF$trainx[repeatAGrows[,2]]))

cor.test(repRF$trainfit[repeatAGrows[,1]]-repRF$trainx[repeatAGrows[,1]],
         repRF$trainfit[repeatAGrows[,2]]-repRF$trainx[repeatAGrows[,2]])

# Delta metabolic brain age also correlates, r~0.75, p < 0.005

ggplot(repplot,aes(Scan,AgeDif,color=ID))+
  geom_point(size=3)+
  geom_line(aes(group=ID)) +
  coord_cartesian(ylim = c(-20,10)) +
  theme(axis.title = element_text(size = axti),
        axis.title.y = element_text(vjust=0.5),
        axis.title.x = element_text(vjust=-0.3),
        axis.text = element_text(size = axtx,color="black"),
        plot.margin=unit(c(.2,.3,.3,.1),"cm"),
        panel.background=element_rect(fill=bfill),
        legend.position="none") +
  xlab("Scan session (mean 1.6 years apart)") +
  ylab("Predicted - Actual Age (Years)") 

ggsave("Repeat19Sticks.tiff",width=85,height=95,units="mm",dpi=1200)


# Show test-retest correlations versus normals

repplot2 = data.frame("Participant" = seq(1,19,1),
                      "Initial" = repplot[1:19,3],
                      "Repeat" = repplot[20:38,3])

ggplot(repplot2,aes(Initial, Repeat)) +
  geom_point(size=2) +
  geom_smooth(method="lm",size=lmsize) +
  coord_cartesian(xlim = c(-18,12),ylim = c(-20,12)) +
  theme(axis.title = element_text(size = axti),
        axis.title.y = element_text(vjust=1.1,hjust=0.8),
        axis.title.x = element_text(vjust=0.3),
        axis.text = element_text(size = axtx,color="black"),
        panel.background=element_rect(fill=bfill)) +
  xlab("Initial Predicted - Actual Age (Years)") +
  ylab("Repeat Predicted - Actual Age (Years)") 

ggsave("Repeat19Panel.tiff",width=85,height=95,units="mm",dpi=1200)


# AMYLOID

# Amyloid deposition does not influence Random Forest predictions of metabolic brain age.
#  - Show amyloid+ versus amyloid- RF plot. 

Amyloid = rfbcwrap(all.normFULL[which(adultmetabFULL$PIB=="Neg"),],
                   adultmetabFULL$Age[which(adultmetabFULL$PIB=="Neg")],
                   all.normFULL[which(adultmetabFULL$PIB=="Pos"),],
                   adultmetabFULL$Age[which(adultmetabFULL$PIB=="Pos")])

rfplot = data.frame("Actual"=c(Amyloid$trainx,Amyloid$testx),
                    "Predicted" = c(Amyloid$trainfit,Amyloid$testfit),
                    "Type" = c(rep("Amyloid-",length(Amyloid$trainfit)),
                               rep("Amyloid+",length(Amyloid$testfit))))

cor.test(Amyloid$testfit,Amyloid$testx)

# Note that separate cohort of amyloid positive individuals also show correlation
# between metabolic brain age and actual age, albeit less so (R > 0.51, p < 0.005)

ggplot(rfplot,aes(Actual,Predicted,color=Type)) +
  geom_point(size=pointsize) +
  geom_smooth(method="lm",size=lmsize,data=rfplot[rfplot$Type=="Amyloid-",]) +
  coord_cartesian(xlim = c(15,85),ylim = c(15,85)) +
  theme(axis.title = element_text(size = axti),
        axis.title.y = element_text(vjust=0.5),
        axis.title.x = element_text(vjust=0.3),
        axis.text = element_text(size = axtx,color="black"),
        plot.margin=unit(c(.2,.3,.1,.1),"cm"),
        panel.background=element_rect(fill=bfill),
        legend.position = "top") +
  labs(color="") +
  scale_color_manual(values=cbP[c(6,5)]) +
  xlab("Actual Age (Years)") +
  ylab("Predicted Age (Years)") 

ggsave("RFRAmyloid.tiff",width=85,height=95,units="mm",dpi =1200)

min(adultmetabFULL$Age[which(adultmetabFULL$PIB=="Pos")])
max(adultmetabFULL$Age[which(adultmetabFULL$PIB=="Pos")])

t.test(Amyloid$trainfit[which(Amyloid$trainx>59)],Amyloid$testfit[which(Amyloid$testx>59)])

# No significant difference in predicted age for amyloid positive versus negative (p~0.20)
# Note that looking at the graph, amyloid+ certainly does not increase the age


# SEX DIFFERENCES


# Sex / gender influences Random Forest predictions of metabolic brain age.
#  - Show female versus male RF plot. Then show MBA differences in a bar graph.
#  - Supplement shows differences in mean cortical AG (true) and in young adults only.


MF = rfbcwrap(all.norm[which(adultmetab$Gender=="M"),],adultmetab$Age[which(adultmetab$Gender=="M")],
              all.norm[which(adultmetab$Gender=="F"),],adultmetab$Age[which(adultmetab$Gender=="F")])

# Trainx = male actual age
# Trainfit = male predicted age
# Testx = female actual age
# Testfit = female predicted age

cor.test(MF$testfit,MF$testx)
t.test(MF$trainfit-MF$trainx,MF$testfit-MF$testx)

# F -3.8y as compared to M, p = 0.008

rfplot = data.frame("Actual"=c(MF$trainx,MF$testx),
                    "Predicted" = c(MF$trainfit,MF$testfit),
                    "Type" = c(rep("Males",length(MF$trainfit)),
                               rep("Females",length(MF$testfit))))


ggplot(rfplot,aes(x=Actual,y=Predicted,color=Type)) +
  geom_point(size=pointsize) +
  geom_smooth(method="lm",size=lmsize) +
  coord_cartesian(xlim = c(15,85),ylim = c(15,85)) +
  theme(axis.title = element_text(size = axti),
        axis.title.y = element_text(vjust=0.5),
        axis.title.x = element_text(vjust=0.3),
        axis.text = element_text(size = axtx,color="black"),
        plot.margin=unit(c(.2,.3,.1,.1),"cm"),
        panel.background=element_rect(fill=bfill),
        legend.position = "top") +
  labs(color="") +
  scale_color_manual(values=cbP[c(8,3)]) +
  xlab("Actual Age (Years)") +
  ylab("Predicted Age (Years)") 

ggsave("RFRGenderBO.tiff",width=85,height=95,units="mm",dpi=1200)

MFplot2 = data.frame("AgeDif" = c(MF$testfit-MF$testx,MF$trainfit-MF$trainx),
                     "Type" = c(rep("Females",length(MF$testfit)),
                                rep("Males",length(MF$trainfit))))


ggplot(MFplot2, aes(x=Type, y = AgeDif, color=Type)) +
  geom_boxplot(alpha=0.3) +
  geom_jitter() +
  theme(axis.title = element_text(size = axti),
        axis.title.y = element_text(vjust=0.5),
        axis.title.x = element_text(vjust=0.3),
        axis.text = element_text(size = axtx,color="black"),
        plot.margin=unit(c(.2,.3,.1,.1),"cm"),
        panel.background=element_rect(fill=bfill),
        legend.position = "top") +
  scale_color_manual(values=cbP[c(8,3)])

ggsave("RFRGenderJitter.tiff",width=75,height=95,units="mm",dpi=1200)


# Females appear ~3.8 years younger (CI 1.0-6.6 years), p < 0.01
# Effect size is 3.8y / 8.7y = 0.43 (Cohen's D)

# Confirm relationship above going the opposite way

FM = rfbcwrap(all.norm[which(adultmetab$Gender=="F"),],adultmetab$Age[which(adultmetab$Gender=="F")],
              all.norm[which(adultmetab$Gender=="M"),],adultmetab$Age[which(adultmetab$Gender=="M")])

cor.test(FM$testfit,FM$testx)
t.test(FM$trainfit-FM$trainx,FM$testfit-FM$testx,alternative = c("less"))
# Okay to use 1-tail test given desire to confirm result above.

# Going the other direction confirms the result (p=0.035), though the effect
# is not as large: year difference ~2.4 years.

# Look at distribution of delta metabolic brain ages
hist(MF$trainfit-MF$trainx)
hist(MF$testfit-MF$testx)

cMFplot = data.frame(x = c(MF$trainfit-MF$trainx,MF$testfit-MF$testx), 
                     type = rep(c("M","F"),c(length(MF$trainx),length(MF$testx))))
ggplot(cMFplot) + 
  geom_density(aes(x=x, colour=type, fill=type), alpha=0.5) +
  theme(legend.position = "top")

ggsave("RFRGenderDensity.tiff",width=85,height=95,units="mm",dpi=1200)

# Note the difference is in the mean, less so for the mode!

# Look at results when training on whole data set

rftot = rfbcwrap(all.norm,adultmetab$Age,
                 all.norm[1,],adultmetab$Age[1])
cor.test(rftot$trainfit,rftot$trainx)
mean(rftot$trainfit[adultmetab$Gender=="M"]-rftot$trainx[adultmetab$Gender=="M"])
mean(rftot$trainfit[adultmetab$Gender=="F"]-rftot$trainx[adultmetab$Gender=="F"])

# Difference is only 2.2y when trained on the whole group.
t.test(rftot$trainfit[adultmetab$Gender=="M"]-rftot$trainx[adultmetab$Gender=="M"],
       rftot$trainfit[adultmetab$Gender=="F"]-rftot$trainx[adultmetab$Gender=="F"],
       alternative = c("greater"))
# This is also marginally significant (p=0.05, one-tailed, varies according to instance)

# Can Random Forest identify females vs males?
MFpredict = randomForest(all.norm,adultmetab$Gender,ntree=10000,importance=TRUE)
length(which(MFpredict$predicted==MFpredict$y))/184
MFpredict2 = randomForest(cbind(adultmetab$Age,all.norm),adultmetab$Gender,ntree=10000,importance=TRUE)
length(which(MFpredict2$predicted==MFpredict2$y))/184
# Interesting! RF was quite poor with 64-66% prediction accuracy. I suspect this is because aging changes predominate

# Median absolute deviation
mad(rftot$trainfit-rftot$trainx)
mad(rftot$trainfit-rftot$trainx, constant=1)
sd(rftot$trainfit-rftot$trainx)
# So scaled median absolute deviation is similar to SD (7.9y). 
# Pure MAD is only ~5.4y, which is about twice that of MF differences.

# THE FOLLOWING SCRIPT REPRESENTS PERMUTATION TESTS TO TRY AND REVEAL WHETHER
# SPECIFIC REGIONS AND TYPES OF METABOLISM ARE DRIVING THE RESULTS

# SOME OF THESE SCRIPTS MAY TAKE A LONG TIME

rftot = rfbc(all.norm,adultmetab$Age)
ageimp = cbind(names(adultmetab[10:325]),rftot$RFR$importance[,1])
ageimp = ageimp[sort(as.numeric(ageimp[,2]),decreasing = TRUE, index.return=TRUE)$ix,]
ageimp[,2] = as.numeric(ageimp[,2])
ageimp[1:20,]
# Note however that the problem with this is that with quantile normalized data, as regions
# go up or down, others are displaced. So this might not be very accurate.
# That said, CBF.pallidum, AG.ACC, and CMRGlu.pallidum/parahippocampus/temporalpole have
# strong effects.
# Note also that AG posterior cingulate is high on the list.

# Another strategy is to try to delete components to see how they influence prediction accruacy.
# This would require re-quantile normalization.
# Try this with metabolism first (AG,CMRglu,CMRO2,CBF). Then try with 1 region at a time.

rftot = rfbcwrap(all.norm,adultmetab$Age,
                 all.norm[1,],adultmetab$Age[1])
cor.test(rftot$trainfit,rftot$trainx)
# r ~ 0.89
all.normAGout = all.norm[,-c(1:79)]
rfAGout = rfbcwrap(all.normAGout,adultmetab$Age,all.normAGout[1,],adultmetab$Age[1])
cor.test(rfAGout$trainfit,rfAGout$trainx)
# r = 0.874, 0.872, 0.873
all.normGlcout = all.norm[,-c(80:158)]
rfGlcout = rfbcwrap(all.normGlcout,adultmetab$Age,all.normGlcout[1,],adultmetab$Age[1])
cor.test(rfGlcout$trainfit,rfGlcout$trainx)
# r = 0.886, 0.886, 0.886
all.normO2out = all.norm[,-c(159:237)]
rfO2out = rfbcwrap(all.normO2out,adultmetab$Age,all.normO2out[1,],adultmetab$Age[1])
cor.test(rfO2out$trainfit,rfO2out$trainx)
# r = 0.896, 0.896, 0.896
all.normCBFout = all.norm[,-c(238:316)]
rfCBFout = rfbcwrap(all.normCBFout,adultmetab$Age,all.normCBFout[1,],adultmetab$Age[1])
cor.test(rfCBFout$trainfit,rfCBFout$trainx)
# r = 0.881, 0.881, 0.882

# Iterate
rfmetabresults = matrix(NA,5,10)
rownames(rfmetabresults) = c("Allin","AGout","CMRGlcout","CMRO2out","CBFout")
for (n in 1:10){
  rftot = rfbcwrap(all.norm,adultmetab$Age,
                   all.norm[1,],adultmetab$Age[1])
  rfmetabresults[1,n]=cor(rftot$trainfit,rftot$trainx)
  
  all.normAGout = all.norm[,-c(1:79)]
  rfAGout = rfbcwrap(all.normAGout,adultmetab$Age,all.normAGout[1,],adultmetab$Age[1])
  cor.test(rfAGout$trainfit,rfAGout$trainx)
  rfmetabresults[2,n]=cor(rfAGout$trainfit,rfAGout$trainx)
  
  all.normGlcout = all.norm[,-c(80:158)]
  rfGlcout = rfbcwrap(all.normGlcout,adultmetab$Age,all.normGlcout[1,],adultmetab$Age[1])
  rfmetabresults[3,n]=cor(rfGlcout$trainfit,rfGlcout$trainx)
  
  all.normO2out = all.norm[,-c(159:237)]
  rfO2out = rfbcwrap(all.normO2out,adultmetab$Age,all.normO2out[1,],adultmetab$Age[1])
  rfmetabresults[4,n]=cor(rfO2out$trainfit,rfO2out$trainx)
  
  all.normCBFout = all.norm[,-c(238:316)]
  rfCBFout = rfbcwrap(all.normCBFout,adultmetab$Age,all.normCBFout[1,],adultmetab$Age[1])
  rfmetabresults[5,n]=cor(rfCBFout$trainfit,rfCBFout$trainx)
  print(n)
}
rfmetabresults = read.csv(file="rfmetabresults.csv")[,2:11]
row.names(rfmetabresults) = read.csv(file="rfmetabresults.csv")[,1]
boxplot(t(rfmetabresults))
write.csv(rfmetabresults,file="rfmetabresults.csv")

# This shows that AG is the most informative metabolic parameter, then CBF,
# then CMRGlc. CMRO2, if anything, might even worsen assessment (redundant with AG).

# Next regional random predictions
permutes = 1000
rfrandresults = matrix(NA,permutes,40)
rfrandcor = vector('numeric',permutes)
date()
for (n in 1:permutes){
  randdump = sample(79,40);
  AGrand = t(normalize.quantiles(t(adultmetab[,randdump+9])))
  CMRGlurand = t(normalize.quantiles(t(adultmetab[,randdump+9+79])))
  CMRO2rand = t(normalize.quantiles(t(adultmetab[,randdump+9+79+79])))
  CBFrand = t(normalize.quantiles(t(adultmetab[,randdump+9+79+79])))
  rand.norm = cbind(AGrand, CMRGlurand, CMRO2rand, CBFrand)
  rfrandresults[n,] = randdump
  rfrand = rfbcwrap(rand.norm,adultmetab$Age,rand.norm[1,],adultmetab$Age)
  rfrandcor[n] = cor(rfrand$trainfit,rfrand$trainx)
  print(n)
}
date()
write.csv(cbind(rfrandcor,rfrandresults),file="rfrand3.csv")

# Do the same now for comparing males vs females
permutes = 1000
sl = 60
MFrandresults = matrix(NA,permutes,sl)
MFranddif = vector('numeric',permutes)
date()
for (n in 1:permutes){
  randdump = sample(79,sl);
  AGrand = t(normalize.quantiles(t(adultmetab[,randdump+9])))
  CMRGlurand = t(normalize.quantiles(t(adultmetab[,randdump+9+79])))
  CMRO2rand = t(normalize.quantiles(t(adultmetab[,randdump+9+79+79])))
  CBFrand = t(normalize.quantiles(t(adultmetab[,randdump+9+79+79])))
  rand.norm = cbind(AGrand, CMRGlurand, CMRO2rand, CBFrand)
  MFrandresults[n,] = randdump
  MFrand = rfbcwrap(rand.norm[adultmetab$Gender=="M",],adultmetab$Age[adultmetab$Gender=="M"],
                    rand.norm[adultmetab$Gender=="F",],adultmetab$Age[adultmetab$Gender=="F"])
  MFranddif[n] = mean(MFrand$trainfit-MFrand$trainx)-mean(MFrand$testfit-MFrand$testx)
  print(n)
}
date()
write.csv(cbind(MFranddif,MFrandresults),file="MFrand1.csv")

# This performs 1000 permutations with just 60 of the regions.
# Interesting that mean dif was 4.7y (1.4-7.9y).
# This suggests that the effect cannot be ascribed to a few regions!

# Next step is to load the random results and determine what regions are 
# most reponsible for accuracy of RF, and for the MvF difference

rfrandom = read.csv("rfrand.csv")
rfrandom = rbind(rfrandom,read.csv("rfrand2.csv"))
rfrandom = rbind(rfrandom,read.csv("rfrand3.csv"))
names(rfrandom)
rfrandomcor = rfrandom[,2]
rfrandomreg = rfrandom[,3:42]
rm(rfrandom)
hist(rfrandomcor)
rfregcor = matrix(NA,79,3)
colnames(rfregcor) = c("RegInd","Regionname","Meancorr")
for (n in 1:79){
  rfregcor[n,1] = n;
  rfregcor[n,2] = names(adultmetab)[n+9];
  rfregcor[n,3] = mean(rfrandomcor[which(rfrandomreg==n,arr.ind=TRUE)[,1]])
}
rfregcor[sort(rfregcor[,3],decreasing=TRUE,index.return=TRUE)$ix,]
hist(rfrandomcor)

# So gray matter more than white matter (except corpus callosum); anterior cingulate, middle temporal,
# superior frontal, insular and fusiform and parahippocampal gyri; and globus pallidus and cerebellum.
# No obvious pattern, but very interesting!

# Now look at men v women.

MFrandom = read.csv("MFrand1.csv")
names(MFrandom); dim(MFrandom)
MFranddif = MFrandom$MFranddif
MFrandreg = MFrandom[,3:62]
hist(MFranddif, xlab="Metabolic Age Difference: Males - Females (Years)")
mean(MFranddif)
min(MFranddif)
max(MFranddif)
# Very interesting! In 1000 iterations, *none* found females to be older than males.
# Also mean difference was 4.7y (range 1.4-7.9y)

# Now what regions correlate with highest age difference?
MFregdif = matrix(NA,79,3)
colnames(MFregdif) = c("RegInd","Regionname","Meandif")
for (n in 1:79){
  MFregdif[n,1] = n;
  MFregdif[n,2] = names(adultmetab)[n+9];
  MFregdif[n,3] = mean(MFranddif[which(MFrandreg==n,arr.ind=TRUE)[,1]])
}
MFregdif[sort(MFregdif[,3],decreasing=TRUE,index.return=TRUE)$ix,]

# Do the reverse (i.e, missing regions, mean difference in age)
MFregdif2 = matrix(NA,79,3)
colnames(MFregdif2) = c("RegInd","Regionname","Meandif2")
for (n in 1:79){
  MFregdif2[n,1] = n;
  MFregdif2[n,2] = names(adultmetab)[n+9];
  MFregdif2[n,3] = mean(MFranddif[-which(MFrandreg==n,arr.ind=TRUE)[,1]])
}
MFregdif2[sort(MFregdif[,3],decreasing=TRUE,index.return=TRUE)$ix,]

# Repeat leave one metabolic parameter out test
# for male-female differences

MFmetabresults = matrix(NA,5,10)
rownames(MFmetabresults) = c("Allin","AGout","CMRGlcout","CMRO2out","CBFout")
for (n in 1:10){
  MFtot = rfbcwrap(all.norm[which(adultmetab$Gender=="M"),],adultmetab$Age[which(adultmetab$Gender=="M")],
                   all.norm[which(adultmetab$Gender=="F"),],adultmetab$Age[which(adultmetab$Gender=="F")])
  MFmetabresults[1,n]=mean(MFtot$trainfit-MFtot$trainx)-mean(MFtot$testfit-MFtot$testx)
  
  all.normAGout = all.norm[,-c(1:79)]
  MFAGout = rfbcwrap(all.normAGout[which(adultmetab$Gender=="M"),],adultmetab$Age[which(adultmetab$Gender=="M")],
                     all.normAGout[which(adultmetab$Gender=="F"),],adultmetab$Age[which(adultmetab$Gender=="F")])
  MFmetabresults[2,n]=mean(MFAGout$trainfit-MFAGout$trainx)-mean(MFAGout$testfit-MFAGout$testx)
  
  all.normGlcout = all.norm[,-c(80:158)]
  MFGlcout = rfbcwrap(all.normGlcout[which(adultmetab$Gender=="M"),],adultmetab$Age[which(adultmetab$Gender=="M")],
                      all.normGlcout[which(adultmetab$Gender=="F"),],adultmetab$Age[which(adultmetab$Gender=="F")])
  MFmetabresults[3,n]=mean(MFGlcout$trainfit-MFGlcout$trainx)-mean(MFGlcout$testfit-MFGlcout$testx)
  
  all.normO2out = all.norm[,-c(159:237)]
  MFO2out = rfbcwrap(all.normO2out[which(adultmetab$Gender=="M"),],adultmetab$Age[which(adultmetab$Gender=="M")],
                     all.normO2out[which(adultmetab$Gender=="F"),],adultmetab$Age[which(adultmetab$Gender=="F")])
  MFmetabresults[4,n]=mean(MFO2out$trainfit-MFO2out$trainx)-mean(MFO2out$testfit-MFO2out$testx)
  
  all.normCBFout = all.norm[,-c(238:316)]
  MFCBFout = rfbcwrap(all.normCBFout[which(adultmetab$Gender=="M"),],adultmetab$Age[which(adultmetab$Gender=="M")],
                      all.normCBFout[which(adultmetab$Gender=="F"),],adultmetab$Age[which(adultmetab$Gender=="F")])
  MFmetabresults[5,n]=mean(MFCBFout$trainfit-MFCBFout$trainx)-mean(MFCBFout$testfit-MFCBFout$testx)
  
  print(n)
}

boxplot(t(MFmetabresults))
write.csv(MFmetabresults,file="MFmetabresults.csv")

rowMeans(MFmetabresults)
# Allin = 3.65y, AGout = 3.26y, CMRGlcout = 2.72y, CMRO2out = 3.47y, CBFout = 5.34y
# Suggests that CMRGlc is a bigger driver in the difference between males and females
# And that CBF actually equalizes the age difference a bit

# Boruta

library(Boruta)
colnames(all.norm) = colnames(adultmetab)[10:325]
impFeatures = Boruta(all.norm,adultmetab$Age, maxRuns=1000)
print(impFeatures)
colnames(all.norm)[which(impFeatures$finalDecision=="Confirmed")]
colnames(all.norm)[which(impFeatures$finalDecision=="Tentative")]

plot(impFeatures)

impFeatures.df = attStats(impFeatures)
names(impFeatures.df)
print(impFeatures.df[which(impFeatures.df$decision=="Confirmed"),])

# Most important features include AG in the cingulate and superior frontal gyrus
# CMRGlc in the temporal lobes (parahippocampal, temporal pole)
# and CBF in the basal ganglia

# Function to plot females vs males for specific parameter

plotMF = function(datatoplot,region,MValue){
  dataplot = data.frame("MetabValue" = datatoplot[,region],
                        "Age" = adultmetab$Age,
                        "Gender" = adultmetab$Gender) 
  ggplot(dataplot, aes(x=Age, y=MetabValue, color=Gender)) +
    geom_point(size=pointsize) +
    geom_smooth(method="lm",size=lmsize) +
    theme(axis.title = element_text(size = axti),
          axis.title.y = element_text(vjust=0.5),
          axis.title.x = element_text(vjust=0.3),
          axis.text = element_text(size = axtx,color="black"),
          plot.margin=unit(c(.2,.3,.1,.1),"cm"),
          panel.background=element_rect(fill=bfill),
          legend.position = "top") +
    labs(color="") +
    scale_color_manual(values=cbP[c(8,3)]) +
    xlab("Actual Age (Years)") +
    ylab(MValue) 
  glm()
}

plotMF(adultmetab[,10:325],
       which(test$finalDecision=="Confirmed")[20],
       colnames(all.norm)[which(test$finalDecision=="Confirmed")[20]])

# AMYLOID WITHOUT CBF
pointsize=2.5; lmsize=1.2; axti = 15; axtx = 15; bfill = 'lightgray';

# Check to see if amyloid affects Metab Brain Age without CBF data

all.normFULLsinCBF = all.normFULL[,-c(238:316)]

Amyloid = rfbcwrap(all.normFULLsinCBF[which(adultmetabFULL$PIB=="Neg"),],
                   adultmetabFULL$Age[which(adultmetabFULL$PIB=="Neg")],
                   all.normFULLsinCBF[which(adultmetabFULL$PIB=="Pos"),],
                   adultmetabFULL$Age[which(adultmetabFULL$PIB=="Pos")])

rfplot = data.frame("Actual"=c(Amyloid$trainx,Amyloid$testx),
                    "Predicted" = c(Amyloid$trainfit,Amyloid$testfit),
                    "Type" = c(rep("Amyloid-",length(Amyloid$trainfit)),
                               rep("Amyloid+",length(Amyloid$testfit))))

cor.test(Amyloid$testfit,Amyloid$testx)

ggplot(rfplot,aes(Actual,Predicted,color=Type)) +
  geom_point(size=pointsize) +
  geom_smooth(method="lm",size=lmsize,data=rfplot[rfplot$Type=="Amyloid-",]) +
  coord_cartesian(xlim = c(15,85),ylim = c(15,85)) +
  theme(axis.title = element_text(size = axti),
        axis.title.y = element_text(vjust=0.5),
        axis.title.x = element_text(vjust=0.3),
        axis.text = element_text(size = axtx,color="black"),
        plot.margin=unit(c(.2,.3,.1,.1),"cm"),
        panel.background=element_rect(fill=bfill),
        legend.position = "top") +
  labs(color="") +
  scale_color_manual(values=cbP[c(6,5)]) +
  xlab("Actual Age (Years)") +
  ylab("Predicted Age (Years)") 

t.test(Amyloid$trainfit[which(Amyloid$trainx>59)],Amyloid$testfit[which(Amyloid$testx>59)],alternative = "greater")

# Again, amyloid does not predict a higher metabolic brain age (if anything, it might predict a slightly
# lower metabolic brain age, though this is probably due to the sex differences)


# USE BAYESIAN ANALYSIS TO DETERMINE LIKELIHOOD THAT F.MBA < M.MBA BASED ON DATA

MF = rfbcwrap(all.norm[which(adultmetab$Gender=="M"),],adultmetab$Age[which(adultmetab$Gender=="M")],
              all.norm[which(adultmetab$Gender=="F"),],adultmetab$Age[which(adultmetab$Gender=="F")])

# Trainx = male actual age
# Trainfit = male predicted age
# Testx = female actual age
# Testfit = female predicted age

cor.test(MF$testfit,MF$testx)
t.test(MF$trainfit-MF$trainx,MF$testfit-MF$testx)
FMmeandif = mean(MF$trainfit-MF$trainx)-mean(MF$testfit-MF$testx)
# F -3.8y as compared to M, p < 0.008

FMstderr = sd(MF$testfit-MF$testx)/sqrt(length(MF$testfit))
# Since male MBA is assumed to be 'zero', can only use female MBA
# to calculate standard error

library(BayesCombo)
pph(beta = FMmeandif, se.beta = FMstderr,
    H0 = c(0,0), H.priors = rep(1/3,3))$pphs
# So very high posterior probability


# Make brain maps to map out importance values
"%>%" = function(x,f) do.call(f,list(x))
library(ggseg)
ggseg(atlas="dkt",mapping=aes(fill=area),position="stacked")
ggseg(atlas="aseg",mapping=aes(fill=area),position="stacked")
ImpGGSEG = read.csv(file="ImportanceData.csv")

ImpPlot = data.frame(cbind(area=as.character(ImpGGSEG$Region),MeanImp=ImpGGSEG$Importance,Metabolism=as.character(ImpGGSEG$Mode)),
                     stringsAsFactors=F)

ggseg(data = ImpPlot[1:39,], atlas = "dkt",
      mapping=aes(fill=as.numeric(MeanImp)), position="stacked", color="darkgrey",size=.1) +
  scale_fill_gradientn(colors=c("dodgerblue4","lightblue","firebrick","goldenrod"),limits=c(-1,10), name="Importance") +
  ggtitle("CMRO2")

ggseg(data = ImpPlot[40:78,], atlas = "dkt",
      mapping=aes(fill=as.numeric(MeanImp)), position="stacked", color="darkgrey",size=.1) +
  scale_fill_gradientn(colors=c("dodgerblue4","lightblue","firebrick","goldenrod"),limits=c(-1,10), name="Importance") +
  ggtitle("CBF")

ggseg(data = ImpPlot[79:117,], atlas = "dkt",
      mapping=aes(fill=as.numeric(MeanImp)), position="stacked", color="darkgrey",size=.1) +
  scale_fill_gradientn(colors=c("dodgerblue4","lightblue","firebrick","goldenrod"),limits=c(-1,10), name="Importance") +
  ggtitle("CMRGlc")

ggseg(data = ImpPlot[118:156,], atlas = "dkt",
      mapping=aes(fill=as.numeric(MeanImp)), position="stacked", color="darkgrey",size=.1) +
  scale_fill_gradientn(colors=c("dodgerblue4","lightblue","firebrick","goldenrod"),limits=c(-1,10), name="Importance") +
  ggtitle("AG")

ggseg(data = ImpPlot[1:39,], atlas = "aseg",
      mapping=aes(fill=as.numeric(MeanImp)), position="stacked", color="darkgrey",size=.1) +
  scale_fill_gradientn(colors=c("dodgerblue4","lightblue","firebrick","goldenrod"),limits=c(-1,10), name="Importance") +
  ggtitle("CMRO2")

ggseg(data = ImpPlot[40:78,], atlas = "aseg",
      mapping=aes(fill=as.numeric(MeanImp)), position="stacked", color="darkgrey",size=.1) +
  scale_fill_gradientn(colors=c("dodgerblue4","lightblue","firebrick","goldenrod"),limits=c(-1,10), name="Importance") +
  ggtitle("CBF")

ggseg(data = ImpPlot[79:117,], atlas = "aseg",
      mapping=aes(fill=as.numeric(MeanImp)), position="stacked", color="darkgrey",size=.1) +
  scale_fill_gradientn(colors=c("dodgerblue4","lightblue","firebrick","goldenrod"),limits=c(-1,10), name="Importance") +
  ggtitle("CMRGlc")

ggseg(data = ImpPlot[118:156,], atlas = "aseg",
      mapping=aes(fill=as.numeric(MeanImp)), position="stacked", color="darkgrey",size=.1) +
  scale_fill_gradientn(colors=c("dodgerblue4","lightblue","firebrick","goldenrod"),limits=c(-1,10), name="Importance") +
  ggtitle("AG")
