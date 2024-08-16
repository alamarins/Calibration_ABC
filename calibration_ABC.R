#------------ 1. define parameter space -------------#
input.names<- c("LossFatFem", "LossFatMal", "LossFatPreco",
                "NFryMax",
                "activitySummer", "activitySmolt1", "activitySmoltN", "activityWinter",
                "sp0", "sp1M", "sp1S", "sp1", "spn", "spnM",
                "meanMatThrMalParr", "meanMatThrMalAnad", "meanMatThrFemAnad",
                "meanSmoltiThrFem","meanSmoltiThrMal",
                "maxRIV","kappaRIV","sigRIV")
# minmaxparams<-matrix(c(1,100,0,  1,  0.9,0.5,0.5,0,   
#                        0.9885538, 0.9882199, 0.9992182, 0.994421, 0.9989116,0.9964198,   
#                        0,40,80,80,80,  4.75,0.00095,3.515,
#                        2,300,5,  2,  1,0.8,0.8,0.3,   
#                        0.9896325, 0.9893161, 1, 0.9955241, 0.9994609, 0.9975251,    
#                        1.5,60,90,100,100  ,5.25,0.00105,3.885
# ), 22)
#rownames(minmaxparams) <- input.names

minparams<-c(1,100,0,  #loss fat
             1,  #NfryMax
             0.9,0.5,0.5,0,   #activityState
             0.9885538, 0.9882199, 0.9992182, 0.994421, 0.9989116,0.9964198,   #sp
             0,40,80,80,80,  #meanTraits
             4.75,0.00095,3.515).  #trade-off
maxparams<-c(2,300,5,  #loss fat
             2,  #NfryMax
             1,0.8,0.8,0.3,   #activityState
             0.9896325, 0.9893161, 1, 0.9955241, 0.9994609, 0.9975251,   #sp 
             1.5,60,90,100,100  #meanTraits
             ,5.25,0.00105,3.885)  #trade-off


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
n <- 1000      # Number of samples
k <- 22     # Number of variables

# Generate the LHS sample (values will be between 0 and 1)
set.seed(123)
lhs_sample <- randomLHS(n, k) #uniform 0-1

# --->>> If want to increase the design
#lhs_sample2 <- augmentLHS(lhs_sample, m = 1)

# Scale the LHS sample to the defined min-max ranges
scaled_sample <- matrix(NA, nrow=n, ncol=k)
for (i in 1:k) {
  scaled_sample[, i] <- lhs_sample[, i] * (maxparams[i] - minparams[i]) + minparams[i]
}
matrix_param <- scaled_sample
colnames(matrix_param) <- input.names
save(matrix_param,file="calibration_ABC_matrix_param.RData") #the matrix of parameters combinations


#------------ 4. need to run model for the n rows of this matrix -------------#
#need to create config AND inventory files for each row
matrix_param
## Run the submit_job.sh for an array of matrix rows



#------------ 5. get output (corresponds to the nrow of the matrix) -------------#
load("calibration_ABC_matrix_param.RData")

output <- "~/Documents/Capsis/calibration/Scorff/code/forPaperCapsis/CALIBRATION-ABC/output/"
#mean of summary statistics over all years, not only the last 5 years
NbReturns <- PropMSW <- MeanFLOSW <- VarFLOSW <- NULL
NbParr0 <- PropMature0 <- MeanFLParr0 <- VarFLParr0 <- NULL
NbSmolt0 <- MeanFLSmolt0 <- VarFLSmolt0 <- NULL
for (row in 1:nrow(so$X)) {
  summary_years <- read.table(paste0(output,"ibaSummaryExport_sensitivity_row",row,"_sim_1.txt"),header=TRUE)
  #Adults
  NbReturns <- c(NbReturns, mean(summary_years$NbReturns[which(summary_years$doy == 319)]))
  PropMSW <- c(PropMSW, mean(summary_years$PropMSW[which(summary_years$doy == 319)]))
  MeanFLOSW <- c(MeanFLOSW, mean(summary_years$MeanFLOSW[which(summary_years$doy == 319)]))
  VarFLOSW <- c(VarFLOSW, mean(summary_years$VarFLOSW[which(summary_years$doy == 319)]))
  #Parr0
  NbParr0 <- c(NbParr0, mean(summary_years$NbParr0[which(summary_years$doy == 267)]))
  ##some problems in PropMature0 > 1 !! for Nparr=0?? but indicates a meanFL??!!! something weird here
  summary_years$PropMature0[which(summary_years$PropMature0 > 1)] <- 0
  PropMature0 <- c(PropMature0, mean(summary_years$PropMature0[which(summary_years$doy == 267)]))
  MeanFLParr0 <- c(MeanFLParr0, mean(summary_years$MeanFLParr0[which(summary_years$doy == 267)]))
  VarFLParr0 <- c(VarFLParr0, mean(summary_years$VarFLParr0[which(summary_years$doy == 267)]))
  #Smolt0
  NbSmolt0 <- c(NbSmolt0, mean(summary_years$NbSmolt0[which(summary_years$doy == 75)]))
  MeanFLSmolt0 <- c(MeanFLSmolt0, mean(summary_years$MeanFLSmolt0[which(summary_years$doy == 75)]))
  VarFLSmolt0 <- c(VarFLSmolt0, mean(summary_years$VarFLSmolt0[which(summary_years$doy == 75)]))
}


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


#Parr0
stats <- apply(n_parr,2,median)
NbParr0_Obs <- mean(stats) / 4 #scaling factor

parr0 <- subset(df, mca_code_methode=="IAS" & age_riv=="0+" & (month>8 & month<11))#df$cap_lf<200 & df$month>8 & df$month<12)# & df$pit_read=='PO')
res.mean <- aggregate(cap_lf~year,data=parr0,mean)
res.var <- aggregate(cap_lf~year,data=parr0,var)
MeanFLParr0_Obs <- mean(res.mean$cap_lf)
VarFLParr0_Obs <- mean(res.var$cap_lf)

res <- aggregate(cap_lf~year+ps_get_sdv_txt,data=parr0, FUN="length")#,function(x) quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
JSIN <- subset(res,ps_get_sdv_txt=="JSIN");JSIN<-merge(data.frame(year=1993:2022),JSIN, by = "year", all = TRUE)
JSSN <- subset(res,ps_get_sdv_txt=="JSSN");JSSN<-merge(data.frame(year=1993:2022),JSSN, by = "year", all = TRUE)
mat <- JSSN$cap_lf / (JSIN$cap_lf+JSSN$cap_lf)
PropMature0_Obs <- mean(mat, na.rm=T)

#Smolt0
stats <- apply(smolt1,2,median)
NbSmolt0_Obs <- mean(stats) / 4

smolt1 <- subset(df, df$mca_code_methode == "PGD"
                 & df$cap_lf>90 & df$cap_lf<200
                 & df$month>=2 & df$month<7
                 & age_riv=="1" & df$year < 2023)#df$cap_lf<200 & df$month>8 & df$month<12)# & df$pit_read=='PO')
res.mean <- aggregate(cap_lf~year,data=smolt1,mean)
res.var <- aggregate(cap_lf~year,data=smolt1,var)
MeanFLSmolt0_Obs <- mean(res.mean$cap_lf)
VarFLSmolt0_Obs <- mean(res.var$cap_lf)

#Adults
stats1 <- apply(OneSW,2,quantile,probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
stats2 <- apply(MSW,2,quantile,probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
stats <- stats1 + stats2
NbReturns_Obs <- mean(stats[3,]) / 4

stats <- stats2 / (stats1 + stats2)
PropMSW_Obs <- mean(stats[3,])

OneSW <- subset(df, mca_code_methode == "PGM"
                & cap_lf>400 
                & age_mer=="1+"
                & year <2023
)
res.mean <- aggregate(cap_lf~year,data=OneSW,mean)
res.var <- aggregate(cap_lf~year,data=OneSW,var)
MeanFLOSW_Obs <- mean(res.mean$cap_lf)
VarFLOSW_Obs <- mean(res.var$cap_lf)

target <- as.data.frame(cbind(NbReturns_Obs, PropMSW_Obs, MeanFLOSW_Obs, VarFLOSW_Obs,
                              NbParr0_Obs, PropMature0_Obs, MeanFLParr0_Obs, VarFLParr0_Obs,
                              NbSmolt0_Obs, MeanFLSmolt0_Obs, VarFLSmolt0_Obs))
colnames(target) <- c("NbReturns", "PropMSW", "MeanFLOSW", "VarFLOSW",
                      "NbParr0", "PropMature0", "MeanFLParr0", "VarFLParr0",
                      "NbSmolt0", "MeanFLSmolt0", "VarFLSmolt0")

save(target, file="abc_test/target.RData")

target2 <- as.data.frame(cbind(NbReturns_Obs, PropMSW_Obs, MeanFLOSW_Obs,
                               NbParr0_Obs, PropMature0_Obs, MeanFLParr0_Obs,
                               NbSmolt0_Obs, MeanFLSmolt0_Obs))
colnames(target2) <- c("NbReturns", "PropMSW", "MeanFLOSW",
                       "NbParr0", "PropMature0", "MeanFLParr0",
                       "NbSmolt0", "MeanFLSmolt0")
save(target2, file="abc_test/target_notvariance.RData")

####----------------------- ABC MODEL --------------------###

model <- c(1:nrow(so$X))
sumstats <- cbind(NbReturns, PropMSW, MeanFLOSW, VarFLOSW,
                  NbParr0, PropMature0, MeanFLParr0, VarFLParr0,
                  NbSmolt0, MeanFLSmolt0, VarFLSmolt0)

ref_table <- as.data.frame(cbind(model,sumstats))
ref_table$model <- as.factor(ref_table$model)
save(ref_table, file="abc_test/ref_table.RData")

sumstats <- cbind(NbReturns, PropMSW, MeanFLOSW,
                  NbParr0, PropMature0, MeanFLParr0,
                  NbSmolt0, MeanFLSmolt0)
ref_table2 <- as.data.frame(cbind(model,sumstats))
ref_table2$model <- as.factor(ref_table2$model)
save(ref_table2, file="abc_test/ref_table_notvariance.RData")

#--------1/ Model selection method (abcrf)------#

#library(abcrf)
# #Train the random forest model
# model_RF <- abcrf(formula = model~.,
#                   data = ref_table,
#                   ntree = 1000,
#                   lda=F,paral = T)
# plot(model_RF, training=ref_table)
# save(model_RF, file="model_RF.RData")
# 
# #Prediction model for observed dataset
# model_selection_result_RF <- predict(object= model_RF,
#                                      obs = target,
#                                      training = ref_table,
#                                      ntree = 1000,
#                                      paral = T,
#                                      paral.predict = T)
# 
# #allocation :indices of the selected models for each observed data set
# model_selection_result_RF$allocation #5082
# 
# #model1
# so$X[5082,]
# ref_table[5082,]
# target

#----2/ Posterior distribution of one parameter (regAbcrf)----#

#----3/ Posterior distribution of several parameters (abc)----#
library(abc)
#rejection algorithm, simulation is accepted if euclidean distance small, tolerance is the % of accepted simulations
rej <- abc(target, param=so$X, sumstat=ref_table[,-1], tol=.1, method="rejection")
rej2 <- abc(target2, param=so$X, sumstat=ref_table2[,-1], tol=.1, method="rejection")
summary(rej2, intvl=.95) #posterior summary
hist(rej, breaks=30) #posterior histograms

#local linear regression corrects for the imperfect match, the accepted parameter values are weighted by a smooth function 
#and corrected according to a linear transform
#local linear regression without correction for heteroscedasticity
lin <- abc(target, param=so$X, sumstat=ref_table[,-1], tol=.1, hcorr = FALSE, method="loclinear")
lin2 <- abc(target2, param=so$X, sumstat=ref_table2[,-1], tol=.1, hcorr = FALSE, method="loclinear")
summary(lin)
hist(lin, breaks=30)
plot(lin, param = so$X) #diagnostic: prior, posterior distributions and qqplots

#local linear regression with correction for heteroscedasticity
linhc <- abc(target, param=so$X, sumstat=ref_table[,-1], tol=.1, hcorr = TRUE, method="loclinear")
linhc2 <- abc(target2, param=so$X, sumstat=ref_table2[,-1], tol=.1, hcorr = TRUE, method="loclinear")
summary(linhc)

#neural network: non linear regression correction method to minimize departure from non linearity
nnet <- abc(target, param=so$X, sumstat=ref_table[,-1], tol=.1, hcorr = TRUE, method="neuralnet")
nnet2 <- abc(target2, param=so$X, sumstat=ref_table2[,-1], tol=.1, hcorr = TRUE, method="neuralnet")
summary(nnet)
hist(nnet, breaks=30)
plot(nnet, param = so$X) #diagnostic: prior, posterior distributions and qqplots

library(dplyr)
input <- as.data.frame(so$X)
results.abc.rej2 <- tibble::as_tibble(rej2$unadj.values) %>% # results from rejection method
  tidyr::gather(parameter, value) %>% 
  dplyr::mutate(method="post.rejection") %>% 
  dplyr::bind_rows(input %>%                # initial parameter distribution (lhs)
                     tidyr::gather(parameter, value) %>% 
                     dplyr::mutate(method="prior"))
ggplot2::ggplot(results.abc.rej2) +
  ggplot2::facet_wrap(~parameter, scales="free") +
  ggplot2::geom_density(ggplot2::aes(x=value, fill=method), alpha=0.2) +
  ggplot2::theme_bw() +
  ggplot2::scale_fill_manual(values=c("red","black"))


results.abc.lin2 <- tibble::as_tibble(lin2$adj.values) %>% # results from linear method
  tidyr::gather(parameter, value) %>% 
  dplyr::mutate(method="post.loclinear") %>% 
  dplyr::bind_rows(tibble::as_tibble(linhc2$adj.values) %>% # results from local linear regression method with correction
                     tidyr::gather(parameter, value) %>% 
                     dplyr::mutate(method="post.loclinear.hcorr")) %>% 
  dplyr::bind_rows(input %>%                # initial parameter distribution (lhs)
                     tidyr::gather(parameter, value) %>% 
                     dplyr::mutate(method="prior"))
ggplot2::ggplot(results.abc.lin2) +
  ggplot2::facet_wrap(~parameter, scales="free") +
  ggplot2::geom_density(ggplot2::aes(x=value, fill=method), alpha=0.2) +
  ggplot2::theme_bw() +
  ggplot2::scale_fill_manual(values=c("blue","green","black"))

results.abc.nnet2 <- tibble::as_tibble(nnet2$adj.values) %>% # results from neural network method
  tidyr::gather(parameter, value) %>% 
  dplyr::mutate(method="post.nnetwork") %>% 
  dplyr::bind_rows(input %>%                # initial parameter distribution (lhs)
                     tidyr::gather(parameter, value) %>% 
                     dplyr::mutate(method="prior"))

ggplot2::ggplot(results.abc.nnet2) +
  ggplot2::facet_wrap(~parameter, scales="free") +
  ggplot2::geom_density(ggplot2::aes(x=value, fill=method), alpha=0.2) +
  ggplot2::theme_bw() +
  ggplot2::scale_fill_manual(values=c("orange","black"))


