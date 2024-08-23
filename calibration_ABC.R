#------------ 1. define parameter space -------------#
input.names<- c("LossFatFem", "LossFatMal", "LossFatPreco",
                #"NFryMax",
                "activitySummer", "activitySmolt1", "activitySmoltN", "activityWinter",
                "sp0", "sp1M", "sp1S", "sp1", "spn", "spnM",
                "meanMatThrMalParr", "meanMatThrMalAnad", "meanMatThrFemAnad",
                "meanSmoltiThrFem","meanSmoltiThrMal")
                #"maxRIV","kappaRIV","sigRIV")
# minmaxparams<-matrix(c(1,100,0,  1,  0.9,0.5,0.5,0,   
#                        0.9885538, 0.9882199, 0.9992182, 0.994421, 0.9989116,0.9964198,   
#                        0,40,80,80,80,  4.75,0.00095,3.515,
#                        2,300,5,  2,  1,0.8,0.8,0.3,   
#                        0.9896325, 0.9893161, 1, 0.9955241, 0.9994609, 0.9975251,    
#                        1.5,60,90,100,100  ,5.25,0.00105,3.885
# ), 22)
#rownames(minmaxparams) <- input.names

minparams<-c(1,100,0,  #loss fat
             #1,  #NfryMax
             0.9,0.5,0.5,0,   #activityState
             0.9885538, 0.9882199, 0.9992182, 0.994421, 0.9989116,0.9964198,   #sp
             0,30,80,80,80)  #meanTraits
             #4.75,0.00095,3.515)  #trade-off
maxparams<-c(3,400,10,  #loss fat
             #2,  #NfryMax
             1,1,1,0.5,   #activityState
             0.9896325, 0.9893161, 1, 0.9955241, 0.9994609, 0.9975251,   #sp 
             1.5,70,100,120,120)  #meanTraits
             #,5.25,0.00105,3.885)  #trade-off


#------------ 2. Latin hypercube sampling -------------#

#---- TGP PACKAGE - cannot augment the design by keeping already sampled values ----#
# library(tgp)
# set.seed(123)
# matrix_param <- lhs(n=1, rect=minmaxparams) ## cannot augment the design
# colnames(matrix_param) <- input.names
# save(matrix_param,file="calibration_ABC_matrix_param.RData") #the matrix of parameters combinations

#---- LHS PACKAGE - CAN augment the design by keeping already sampled values ----#
library(lhs)
# Define the number of samples and the number of variables
n <- 10000      # Number of samples
k <- length(input.names)     # Number of variables

# Generate the LHS sample (values will be between 0 and 1)
set.seed(123)
lhs_sample <- randomLHS(n, k) #uniform 0-1

# Scale the LHS sample to the defined min-max ranges
scaled_sample <- matrix(NA, nrow=n, ncol=k)
for (i in 1:k) {
  scaled_sample[, i] <- lhs_sample[, i] * (maxparams[i] - minparams[i]) + minparams[i]
}
matrix_param <- scaled_sample
colnames(matrix_param) <- input.names
save(matrix_param,file="calibration_ABC_matrix_param_10000.RData") #the matrix of parameters combinations

# # --->>> If want to increase the design
# set.seed(123)
# lhs_sample_10000 <- augmentLHS(lhs_sample, m = 5000)
# scaled_sample <- matrix(NA, nrow=n+5000, ncol=k)
# for (i in 1:k) {
#   scaled_sample[, i] <- lhs_sample_10000[, i] * (maxparams[i] - minparams[i]) + minparams[i]
# }
# matrix_param <- scaled_sample
# colnames(matrix_param) <- input.names
# save(matrix_param,file="calibration_ABC_matrix_param_10000.RData") #the matrix of parameters combinations
# 
# # --->>> If want to increase the design second time
# set.seed(123)
# lhs_sample_20000 <- augmentLHS(lhs_sample_10000, m = 10000)
# scaled_sample <- matrix(NA, nrow=n+15000, ncol=k)
# for (i in 1:k) {
#   scaled_sample[, i] <- lhs_sample_20000[, i] * (maxparams[i] - minparams[i]) + minparams[i]
# }
# matrix_param <- scaled_sample
# colnames(matrix_param) <- input.names
# save(matrix_param,file="calibration_ABC_matrix_param_20000.RData") #the matrix of parameters combinations


#------------ 4. need to run model for the n rows of this matrix -------------#
#need to create config AND inventory files for each row
matrix_param
## Run the submit_job.sh for an array of matrix rows



#------------ 5. get output (corresponds to the nrow of the matrix) -------------#
load("calibration_ABC_matrix_param_10000.RData")
matrix <- matrix_param

output <- "~/Documents/Capsis/calibration/Scorff/code/forPaperCapsis/CALIBRATION-ABC/output/"
#mean of summary statistics over all years, not only the last 5 years
NbOSW <- MeanFLOSW <- MeanWOSW <- PropMaleOSW <- NULL
NbMSW <- MeanFLMSW <- MeanWMSW <- PropMaleMSW <- PropIteroparousMSW <- NULL
NbParr0 <- PropMature0 <- MeanFLParr0 <- MeanWParr0 <- NULL
NbParr1 <- PropMature1 <- MeanFLParr1 <- MeanWParr1 <- NULL
NbSmolt0 <- MeanFLSmolt0 <- MeanWSmolt0 <- PropMaleSmolt0 <- NULL
NbSmolt1 <- MeanFLSmolt1 <- MeanWSmolt1 <- PropMaleSmolt1 <- NULL

for (row in 1:nrow(matrix)) {
  summary_years <- read.table(paste0(output,"ibaSummaryExport_calibration_ABC_row",row,"_sim_1.txt"),header=TRUE)
  #Adults
  NbOSW <- c(NbOSW, mean(summary_years$NbOSW[which(summary_years$doy == 319)]))
  PropMaleOSW <- c(PropMaleOSW, mean(summary_years$PropMaleOSW[which(summary_years$doy == 319)]))
  MeanFLOSW <- c(MeanFLOSW, mean(summary_years$MeanFLOSW[which(summary_years$doy == 319)]))
  MeanWOSW <- c(MeanWOSW, mean(summary_years$MeanWOSW[which(summary_years$doy == 319)]))
  
  NbMSW <- c(NbMSW, mean(summary_years$NbMSW[which(summary_years$doy == 319)]))
  PropIteroparousMSW <- c(PropIteroparousMSW, mean(summary_years$PropIteroparousMSW[which(summary_years$doy == 319)]))
  PropMaleMSW <- c(PropMaleMSW, mean(summary_years$PropMaleMSW[which(summary_years$doy == 319)]))
  MeanFLMSW <- c(MeanFLMSW, mean(summary_years$MeanFLMSW[which(summary_years$doy == 319)]))
  MeanWMSW <- c(MeanWMSW, mean(summary_years$MeanWMSW[which(summary_years$doy == 319)]))
  
  #Parr0
  NbParr0 <- c(NbParr0, mean(summary_years$NbParr0[which(summary_years$doy == 267)]))
  ##some problems in PropMature0 > 1 !! for Nparr=0?? but indicates a meanFL??!!! something weird here
  summary_years$PropMature0[which(summary_years$PropMature0 > 1)] <- 0
  PropMature0 <- c(PropMature0, mean(summary_years$PropMature0[which(summary_years$doy == 267)]))
  MeanFLParr0 <- c(MeanFLParr0, mean(summary_years$MeanFLParr0[which(summary_years$doy == 267)]))
  MeanWParr0 <- c(MeanWParr0, mean(summary_years$MeanWParr0[which(summary_years$doy == 267)]))
  
  #Parr1
  NbParr1 <- c(NbParr1, mean(summary_years$NbParr1[which(summary_years$doy == 267)]))
  PropMature1 <- c(PropMature1, mean(summary_years$PropMature1[which(summary_years$doy == 267)]))
  MeanFLParr1 <- c(MeanFLParr1, mean(summary_years$MeanFLParr1[which(summary_years$doy == 267)]))
  MeanWParr1 <- c(MeanWParr1, mean(summary_years$MeanWParr1[which(summary_years$doy == 267)]))
  
  #Smolt0
  NbSmolt0 <- c(NbSmolt0, mean(summary_years$NbSmolt0[which(summary_years$doy == 75)]))
  MeanFLSmolt0 <- c(MeanFLSmolt0, mean(summary_years$MeanFLSmolt0[which(summary_years$doy == 75)]))
  MeanWSmolt0 <- c(MeanWSmolt0, mean(summary_years$MeanWSmolt0[which(summary_years$doy == 75)]))
  PropMaleSmolt0 <- c(PropMaleSmolt0, mean(summary_years$PropMaleSmolt0[which(summary_years$doy == 75)]))
  
  #Smolt1
  NbSmolt1 <- c(NbSmolt1, mean(summary_years$NbSmolt1[which(summary_years$doy == 75)]))
  MeanFLSmolt1 <- c(MeanFLSmolt1, mean(summary_years$MeanFLSmolt1[which(summary_years$doy == 75)]))
  MeanWSmolt1 <- c(MeanWSmolt1, mean(summary_years$MeanWSmolt1[which(summary_years$doy == 75)]))
  PropMaleSmolt1 <- c(PropMaleSmolt1, mean(summary_years$PropMaleSmolt1[which(summary_years$doy == 75)]))
  
}

model <- c(1:nrow(matrix))
sumstats <- cbind(NbOSW, PropMaleOSW, MeanFLOSW, MeanWOSW,
                  NbMSW, PropMaleMSW, PropIteroparousMSW, MeanFLMSW, MeanWMSW,
                  NbParr0, PropMature0, MeanFLParr0, MeanWParr0,
                  NbParr1, PropMature1, MeanFLParr1, MeanWParr1,
                  NbSmolt0, PropMaleSmolt0, MeanFLSmolt0, MeanWSmolt0,
                  NbSmolt1, PropMaleSmolt1, MeanFLSmolt1, MeanWSmolt1)

ref_table <- as.data.frame(cbind(model,sumstats))
ref_table$model <- as.factor(ref_table$model)
save(ref_table,file="calibration_ABC_ref_table_10000.RData") 


###------------------------ OBSERVED DATA --------------------------###
load("~/Documents/Capsis/calibration/Scorff/data/DB_Scorff_SAT_2005to2023.Rdata")
# remove first years
df <- df[df$year>1993,]

load("~/Documents/Capsis/calibration/Scorff/data/Results_tacon_2022.RData")
n_parr <- fit$sims.list$ntot_Sc

load("~/Documents/Capsis/calibration/Scorff/data/Results_smolt_2022.RData")
n <- fit$sims.list$Nc
smolt1 <- n[,,1]
smolt2 <- n[,,2]

load("~/Documents/Capsis/calibration/Scorff/data/Results_adult_2022.RData")
OneSW <- fit$sims.list$n_1SW #PFA #fit$sims.list$e_1SW (echappement: post fisheries)
MSW <- fit$sims.list$n_MSW #PFA #fit$sims.list$e_MSW (echappement: post fisheries)
load("~/Documents/Capsis/calibration/Scorff/data/p_male_Scorff.Rdata")


#Parr0
stats <- apply(n_parr,2,median)
NbParr0_Obs <- mean(stats) / 4 #scaling factor

parr0 <- subset(df, mca_code_methode=="IAS" & age_riv=="0+" & (month>8 & month<11))#df$cap_lf<200 & df$month>8 & df$month<12)# & df$pit_read=='PO')
L.mean <- aggregate(cap_lf~year,data=parr0,median)
W.mean <- aggregate(cap_poids~year,data=parr0,median)
MeanFLParr0_Obs <- mean(L.mean$cap_lf)
MeanWParr0_Obs <- mean(W.mean$cap_poids)

res <- aggregate(cap_lf~year+ps_get_sdv_txt,data=parr0, FUN="length")#,function(x) quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
JSIN <- subset(res,ps_get_sdv_txt=="JSIN");JSIN<-merge(data.frame(year=1993:2022),JSIN, by = "year", all = TRUE)
JSSN <- subset(res,ps_get_sdv_txt=="JSSN");JSSN<-merge(data.frame(year=1993:2022),JSSN, by = "year", all = TRUE)
mat <- JSSN$cap_lf / (JSIN$cap_lf+JSSN$cap_lf)
PropMature0_Obs <- mean(mat, na.rm=T)

#Parr1
NbParr1_Obs <- 3600 / 4 #scaling factor

parr1 <- subset(df, mca_code_methode=="IAS" & age_riv=="1" & (month>8 & month<11))#df$cap_lf<200 & df$month>8 & df$month<12)# & df$pit_read=='PO')
L.mean <- aggregate(cap_lf~year,data=parr1,median)
W.mean <- aggregate(cap_poids~year,data=parr1,median)
MeanFLParr1_Obs <- mean(L.mean$cap_lf)
MeanWParr1_Obs <- mean(W.mean$cap_poids)

res <- aggregate(cap_lf~year+ps_get_sdv_txt,data=parr1, FUN="length")#,function(x) quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
JSIN <- subset(res,ps_get_sdv_txt=="JSIN");JSIN<-merge(data.frame(year=1993:2022),JSIN, by = "year", all = TRUE)
JSSN <- subset(res,ps_get_sdv_txt=="JSSN");JSSN<-merge(data.frame(year=1993:2022),JSSN, by = "year", all = TRUE)
mat <- JSSN$cap_lf / (JSIN$cap_lf+JSSN$cap_lf)
PropMature1_Obs <- mean(mat, na.rm=T)

#Smolt0
stats <- apply(smolt1,2,median)
NbSmolt0_Obs <- mean(stats) / 4

smolt1 <- subset(df, df$mca_code_methode == "PGD"
                 & df$cap_lf>90 & df$cap_lf<200
                 & df$month>=2 & df$month<7
                 & age_riv=="1" & df$year < 2023)#df$cap_lf<200 & df$month>8 & df$month<12)# & df$pit_read=='PO')
L.mean <- aggregate(cap_lf~year,data=smolt1,median)
W.mean <- aggregate(cap_poids~year,data=smolt1,median)
MeanFLSmolt0_Obs <- mean(L.mean$cap_lf[-1])
MeanWSmolt0_Obs <- mean(W.mean$cap_poids[-1])

PropMaleSmolt0_Obs <- 0.43

#Smolt1
stats <- apply(smolt2,2,median)
NbSmolt1_Obs <- mean(stats) / 4

smolt2 <- subset(df, df$mca_code_methode == "PGD"
                 & df$cap_lf>90 & df$cap_lf<250
                 & df$month>=2 & df$month<8
                 & age_riv=="2" & df$year < 2023)#df$cap_lf<200 & df$month>8 & df$month<12)# & df$pit_read=='PO')
L.mean <- aggregate(cap_lf~year,data=smolt2,median)
W.mean <- aggregate(cap_poids~year,data=smolt2,median)
MeanFLSmolt1_Obs <- mean(L.mean$cap_lf)
MeanWSmolt1_Obs <- mean(W.mean$cap_poids, na.rm=T)

PropMaleSmolt1_Obs <- 0.4


#Adults
stats1 <- apply(OneSW,2,quantile,probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
stats2 <- apply(MSW,2,quantile,probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
NbOSW_Obs <- mean(stats1[3,]) / 4
NbMSW_Obs <- mean(stats2[3,]) / 4

OneSW <- subset(df, mca_code_methode == "PGM"
                & cap_lf>400 
                & age_mer=="1+"
                & year <2023
)
L.mean <- aggregate(cap_lf~year,data=OneSW,median)
W.mean <- aggregate(cap_poids~year,data=OneSW,median)
MeanFLOSW_Obs <- mean(L.mean$cap_lf)
MeanWOSW_Obs <- mean(W.mean$cap_poids)
PropMaleOSW_Obs <- mean(p_male_1SW[,"50%"])

MSW <- subset(df, mca_code_methode == "PGM"
              & cap_lf>400 
              & age_mer!="1+"
              & year <2023
)
L.mean <- aggregate(cap_lf~year,data=MSW,median)
W.mean <- aggregate(cap_poids~year,data=MSW,median)
MeanFLMSW_Obs <- mean(L.mean$cap_lf)
MeanWMSW_Obs <- mean(W.mean$cap_poids)
PropMaleMSW_Obs <- mean(p_male_MSW[,"50%"])

nb_iteroparous <- c(0,3,4,0,10,4,5,3,2,15,22,11,10,39,24,23,27,5,22,15,5,7,2,0,2,2,4,2)
nb_MSW <- c(95,85,75,25,100,50,40,20,60,65,125,105,92,87,120,70,220,150,105,140,175,85,145,150,45,42,40,90)
nb_1SW <- c(780,700, 440, 580,250,280,300,590,220,1100,440,860,470,280,250,760,380,380,610,680,600,620,950,300,240,450,400,160)
PropIteroparousMSW_Obs <- mean(nb_iteroparous / nb_MSW)


target <- as.data.frame(cbind(NbOSW_Obs, PropMaleOSW_Obs, MeanFLOSW_Obs, MeanWOSW_Obs,
                              NbMSW_Obs, PropMaleMSW_Obs, PropIteroparousMSW_Obs, MeanFLMSW_Obs, MeanWMSW_Obs,
                              NbParr0_Obs, PropMature0_Obs, MeanFLParr0_Obs, MeanWParr0_Obs,
                              NbParr1_Obs, PropMature1_Obs, MeanFLParr1_Obs, MeanWParr1_Obs,
                              NbSmolt0_Obs, PropMaleSmolt0_Obs, MeanFLSmolt0_Obs, MeanWSmolt0_Obs,
                              NbSmolt1_Obs, PropMaleSmolt1_Obs, MeanFLSmolt1_Obs, MeanWSmolt1_Obs))
colnames(target) <- c("NbOSW", "PropMaleOSW", "MeanFLOSW", "MeanWOSW",
                      "NbMSW", "PropMaleMSW", "PropIteroparousMSW", "MeanFLMSW", "MeanWMSW",
                      "NbParr0", "PropMature0", "MeanFLParr0", "MeanWParr0",
                      "NbParr1", "PropMature1", "MeanFLParr1", "MeanWParr1",
                      "NbSmolt0", "PropMaleSmolt0", "MeanFLSmolt0", "MeanWSmolt0",
                      "NbSmolt1", "PropMaleSmolt1", "MeanFLSmolt1", "MeanWSmolt1")
save(target, file="target_ABC.RData")

####----------------------- ABC MODEL --------------------###

load("~/Documents/Capsis/calibration/Scorff/code/forPaperCapsis/CALIBRATION-ABC/target_ABC.RData")
load("~/Documents/Capsis/calibration/Scorff/code/forPaperCapsis/CALIBRATION-ABC/calibration_ABC_ref_table_10000.RData")


n_cols <- ncol(ref_table[,-1])
# Set up the plotting area (e.g., 2x3 layout)
par(mfrow = c(4,7))  # Adjust to change the layout
# Loop through each column to create individual boxplots
for (i in 1:n_cols) {
  boxplot(ref_table[,-1][, i], 
          main = colnames(ref_table[,-1])[i], 
          col = "lightblue",
          ylab = colnames(ref_table[,-1])[i])
  
  # Add the corresponding point from the vector
  points(1, target[i], 
         col = "red", 
         pch = 19, 
         cex = 1.5)
}


input <- as.data.frame(matrix)
vline_values <- c(LossFatFem=1.5, LossFatMal=200, LossFatPreco=0,
                  NFryMax=1,
                  activitySummer=0.95, activitySmolt1=0.65, activitySmoltN=0.7, activityWinter=0.1,
                  sp0=0.98912, sp1M=0.9887953, sp1S=0.9998, sp1=0.995, spn=0.9992, spnM=0.997,
                  meanMatThrMalParr=0.5, meanMatThrMalAnad=50, meanMatThrFemAnad=85,
                  meanSmoltiThrFem=89, meanSmoltiThrMal=89,
                  maxRIV=5, kappaRIV=0.001, sigRIV=3.7)
vline_df <- data.frame(parameter = names(vline_values), vline_value = vline_values)

#----3/ Posterior distribution of several parameters (abc)----#
library(abc)
library(dplyr)

##--- REJECTION ---##
#rejection algorithm, simulation is accepted if euclidean distance small, tolerance is the % of accepted simulations
#The rejection method is the simplest form of ABC. It involves generating many simulated datasets 
#from the prior distribution, then accepting only those simulations that are close to the 
#observed data based on some tolerance level.
rej <- abc(target, param=matrix, sumstat=ref_table[,-1], tol=.01, method="rejection")
post.sum <- summary(rej, intvl=.95) #posterior summary

## a/ get the mode of the updated distributions
post.sum["Mode:",]

## b/ get the medians of all distributions
post.sum["Median:",]

#hist(rej, breaks=30) #posterior histograms

#correlations between parameters?
posterior_samples <- rej$unadj.values #accepted values
cor_matrix <- cor(posterior_samples)
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black") # Add correlation coefficients

# Script pour matrice de corr?lation avec coefficient de corr?lation
panel.smooth<-function (x, y, col = par("col"), bg = NA, pch = ".", cex = 1, col.smooth = "red", span = 2/3, iter = 3)  {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth)
}

panel.cor.scale <- function(x, y, digits=2, prefix="", cex.cor)     {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8#/strwidth(txt)
  text(0.5, 0.5, txt)#, cex = cex * abs(r))
}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)     {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8#/strwidth(txt)
  text(0.5, 0.5, txt)#, cex = cex )
  
  #        myPal <- colorRampPalette( c("green", "red") ) # construit une palette de couleurs : ici d?grad? du vert au rouge pour la repr?sentation
  #                   Signif <- symnum(r, corr = TRUE, na = FALSE,
  #                   cutpoints = c(0.1,0.3, 0.6, 0.8, 0.9),
  #                   symbols = c(myPal(5)[1],myPal(5)[2],myPal(5)[3],myPal(5)[4],myPal(5)[5],myPal(5)[6]))
  #                   polygon(c(0,1,1,0),c(0,0,1,1),border="black",col=Signif)   
  #                   text(0.5, 0.5, txt, cex = cex,col="black")
  #       test <- cor.test(x,y)
  #    # borrowed from printCoefmat
  #    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
  #                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  #                  symbols = c("***", "**", "*", ".", " "))
  #               text(.8, .8, Signif, cex=cex, col=2)
}
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)
  #add points on the density plot for the mean and median
  #   points( c(mean(x), median(x)), col=c('red', 'orange'), pch=16)
  #add a boxplot
  #   boxplot(x, horizontal=TRUE, boxwex=0.2, add=T,border=2)
}

pairs.panels <- function (x,y,smooth=TRUE,scale=FALSE) 
{if (smooth ){
  if (scale) {
    pairs(x,diag.panel=panel.hist,lower.panel=panel.cor.scale,upper.panel=panel.smooth)
  }
  else {pairs(x,diag.panel=panel.hist,lower.panel=panel.cor,upper.panel=panel.smooth)
  } #else  {pairs(x,diag.panel=panel.hist,lower.panel=panel.cor,upper.panel=panel.smooth)
}
  else      #smooth is not true
  { if (scale) {pairs(x,diag.panel=panel.hist,lower.panel=panel.cor.scale)
  } else  {pairs(x,diag.panel=panel.hist,lower.panel=panel.cor) }
  } #end of else (smooth)
}   #end of function

pairs.panels(posterior_samples)


results.abc.rej <- tibble::as_tibble(rej$unadj.values) %>% # results from rejection method
  tidyr::gather(parameter, value) %>% 
  dplyr::mutate(method="Posterior") %>% 
  dplyr::bind_rows(input %>%                # initial parameter distribution (lhs)
                     tidyr::gather(parameter, value) %>% 
                     dplyr::mutate(method="Prior"))

# Merge the vline_df with the results to use in the ggplot
results.abc.rej <- results.abc.rej %>%
  dplyr::left_join(vline_df, by = "parameter")

ggplot2::ggplot(results.abc.rej) +
  ggplot2::facet_wrap(~parameter, scales="free") +
  ggplot2::geom_density(ggplot2::aes(x=value, fill=method), alpha=0.2) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = vline_value), color = "red", linetype = "dashed") +  # Add vertical line
  ggplot2::theme_bw() +
  ggplot2::scale_fill_manual(values=c("red","black")) + 
  ggplot2::labs(fill="Rejection method")

##--- LINEAR REGRESSION ---##
#local linear regression corrects for the imperfect match, the accepted parameter values are weighted by a smooth function 
#and corrected according to a linear transform
#local linear regression without correction for heteroscedasticity
#The loclinear method is a regression adjustment technique used to improve the accuracy of the rejection 
#method. After the initial rejection step, it fits a local linear regression to adjust the 
#accepted parameter values, thereby providing a better approximation of the posterior distribution.
lin <- abc(target, param=matrix, sumstat=ref_table[,-1], tol=.01, hcorr = FALSE, method="loclinear")
summary(lin)
hist(lin, breaks=30)
plot(lin, param = matrix) #diagnostic: prior, posterior distributions and qqplots
#correlations between parameters?
posterior_samples <- lin$adj.values
cor_matrix <- cor(posterior_samples)
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black") # Add correlation coefficients
pairs.panels(posterior_samples)

#local linear regression with correction for heteroscedasticity
# linhc <- abc(target, param=matrix, sumstat=ref_table[,-1], tol=.1, hcorr = TRUE, method="loclinear")
# summary(linhc)
# plot(linhc, param = matrix) #diagnostic: prior, posterior distributions and qqplots
##deconne completement!!

results.abc.lin <- tibble::as_tibble(lin$adj.values) %>% # results from linear method
  tidyr::gather(parameter, value) %>% 
  dplyr::mutate(method="Posterior linear") %>% 
  # dplyr::bind_rows(tibble::as_tibble(linhc$adj.values) %>% # results from local linear regression method with correction
  #                    tidyr::gather(parameter, value) %>% 
  #                    dplyr::mutate(method="Posterior linear corr.")) %>% 
  dplyr::bind_rows(input %>%                # initial parameter distribution (lhs)
                     tidyr::gather(parameter, value) %>% 
                     dplyr::mutate(method="Prior"))
# Merge the vline_df with the results to use in the ggplot
results.abc.lin <- results.abc.lin %>%
  dplyr::left_join(vline_df, by = "parameter")

ggplot2::ggplot(results.abc.lin) +
  ggplot2::facet_wrap(~parameter, scales="free") +
  ggplot2::geom_density(ggplot2::aes(x=value, fill=method), alpha=0.2) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = vline_value), color = "red", linetype = "dashed") +  # Add vertical line
  ggplot2::theme_bw() +
  ggplot2::scale_fill_manual(values=c("blue","black")) +
  ggplot2::labs(fill="Linear method")


##transformation
logit <- function(p) {
  log(p / (1 - p))
}
inverse_logit <- function(x) {
  exp(x) / (1 + exp(x))
}
apply_transformations <- function(params) {
  transformed_params <- params
  transformed_params[,"activitySmolt1"] <- logit(params[,"activitySmolt1"])  # Apply inverse logit transformation
  transformed_params[,"activitySmoltN"] <- logit(params[,"activitySmoltN"])  # Apply inverse logit transformation
  transformed_params[,"activitySummer"] <- logit(params[,"activitySummer"])  # Apply inverse logit transformation
  transformed_params[,"activityWinter"] <- logit(params[,"activityWinter"])  # Apply inverse logit transformation
  transformed_params[,"sp0"] <- logit(params[,"sp0"])  # Apply inverse logit transformation
  transformed_params[,"sp1"] <- logit(params[,"sp1"])  # Apply inverse logit transformation
  transformed_params[,"sp1M"] <- logit(params[,"sp1M"])  # Apply inverse logit transformation
  transformed_params[,"sp1S"] <- logit(params[,"sp1S"])  # Apply inverse logit transformation
  transformed_params[,"spn"] <- logit(params[,"spn"])  # Apply inverse logit transformation
  transformed_params[,"spnM"] <- logit(params[,"spnM"])  # Apply inverse logit transformation
  return(transformed_params)
}
inverse_transformations <- function(params) {
  inverse_params <- params
  inverse_params[,"activitySmolt1"] <- inverse_logit(params[,"activitySmolt1"])
  inverse_params[,"activitySmoltN"] <- inverse_logit(params[,"activitySmoltN"])
  inverse_params[,"activitySummer"] <- inverse_logit(params[,"activitySummer"])
  inverse_params[,"activityWinter"] <- inverse_logit(params[,"activityWinter"])
  inverse_params[,"sp0"] <- inverse_logit(params[,"sp0"])
  inverse_params[,"sp1"] <- inverse_logit(params[,"sp1"])
  inverse_params[,"sp1M"] <- inverse_logit(params[,"sp1M"])
  inverse_params[,"sp1S"] <- inverse_logit(params[,"sp1S"])
  inverse_params[,"spn"] <- inverse_logit(params[,"spn"])
  inverse_params[,"spnM"] <- inverse_logit(params[,"spnM"])
  return(inverse_params)
}
# Transform the parameters
transformed_params <- apply_transformations(matrix)
lin <- abc(target, param=transformed_params, sumstat=ref_table[,-1], tol=.1, hcorr = FALSE, method="loclinear")
results_transformed <- lin$adj.values
# Apply inverse transformation to results
results_original_scale <- inverse_transformations(results_transformed)

results.abc.lin <- tibble::as_tibble(results_original_scale) %>% # results from linear method
  tidyr::gather(parameter, value) %>% 
  dplyr::mutate(method="Posterior linear") %>% 
  # dplyr::bind_rows(tibble::as_tibble(linhc$adj.values) %>% # results from local linear regression method with correction
  #                    tidyr::gather(parameter, value) %>% 
  #                    dplyr::mutate(method="Posterior linear corr.")) %>% 
  dplyr::bind_rows(input %>%                # initial parameter distribution (lhs)
                     tidyr::gather(parameter, value) %>% 
                     dplyr::mutate(method="Prior"))
# Merge the vline_df with the results to use in the ggplot
results.abc.lin <- results.abc.lin %>%
  dplyr::left_join(vline_df, by = "parameter")

ggplot2::ggplot(results.abc.lin) +
  ggplot2::facet_wrap(~parameter, scales="free") +
  ggplot2::geom_density(ggplot2::aes(x=value, fill=method), alpha=0.2) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = vline_value), color = "red", linetype = "dashed") +  # Add vertical line
  ggplot2::theme_bw() +
  ggplot2::scale_fill_manual(values=c("blue","black")) +
  ggplot2::labs(fill="Linear method")



###--- NEURAL NETWORK ---###
#neural network: non linear regression correction method to minimize departure from non linearity
#The neural network method uses machine learning (specifically neural networks) to model the relationship 
#between the parameters and the summary statistics. This method involves training a neural network to 
#predict parameter values based on the summary statistics from the simulations.
nnet <- abc(target, param=matrix, sumstat=ref_table[,-1], tol=.01, hcorr = TRUE, method="neuralnet")
summary(nnet)
hist(nnet, breaks=30)
plot(nnet, param = matrix) #diagnostic: prior, posterior distributions and qqplots
#correlations between parameters?
posterior_samples <- nnet$adj.values
cor_matrix <- cor(posterior_samples)
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black") # Add correlation coefficients
pairs.panels(posterior_samples)


results.abc.nnet <- tibble::as_tibble(nnet$adj.values) %>% # results from neural network method
  tidyr::gather(parameter, value) %>% 
  dplyr::mutate(method="Posterior") %>% 
  dplyr::bind_rows(input %>%                # initial parameter distribution (lhs)
                     tidyr::gather(parameter, value) %>% 
                     dplyr::mutate(method="Prior"))
# Merge the vline_df with the results to use in the ggplot
results.abc.nnet <- results.abc.nnet %>%
  dplyr::left_join(vline_df, by = "parameter")

ggplot2::ggplot(results.abc.nnet) +
  ggplot2::facet_wrap(~parameter, scales="free") +
  ggplot2::geom_density(ggplot2::aes(x=value, fill=method), alpha=0.2) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = vline_value), color = "red", linetype = "dashed") +  # Add vertical line
  ggplot2::theme_bw() +
  ggplot2::scale_fill_manual(values=c("orange","black")) +
  ggplot2::labs(fill="Neural network method")



pow <- function(x,y) {x^y}
coefSURV=function(pGres, kappaRIV, sigRIV) {(exp(-kappaRIV*pow(pGres,sigRIV))-exp(-kappaRIV))/(1-exp(-kappaRIV))}

# trade_off
maxRIV=5 # maxRIV maximum growth rate, aded for the incorporation of trade-off growth/survival
sigRIV=3.7 #10 # sigRIV shape of the trade-off function
kappaRIV=0.001 #921.6 # kappaRIV shape of the trade-off function

# Growth potential
#pG=sort(rnorm(10000,0,0.25))
pG=sort(runif(1000,-1,1))
pGres=exp(pG)/maxRIV;
pGres=ifelse(pGres>1,1,pGres)

plot(pG,coefSURV(pGres, kappaRIV, sigRIV),type='l',ylab="Survival coefficient",xlab="Growth potential",main="River trade-off",xlim=c(-1,1))

summary(lin)
maxRIV2=5.3497 # maxRIV maximum growth rate, aded for the incorporation of trade-off growth/survival
sigRIV2=2.8387 #10 # sigRIV shape of the trade-off function
kappaRIV2=0.00104 #921.6 # kappaRIV shape of the trade-off function
pG2=sort(runif(1000,-1,1))
pGres2=exp(pG2)/maxRIV2;
pGres2=ifelse(pGres2>1,1,pGres2)

lines(pG2,coefSURV(pGres2, kappaRIV2, sigRIV2), col="red", type="l")
legend("bottomleft",c("prior","posterior linear"),fill=c("black","red"))
