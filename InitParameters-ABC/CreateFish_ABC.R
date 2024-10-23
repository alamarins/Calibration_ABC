#id female age_days nuclear mCytoplasmic pCytoplasmic mId pId creationDate riverId reachId birthriverId birthreachId atSea seaRegionId weight forkLength  fat  state durationInSea durationInRiverAsParr

#### FUNCTIONS #####
## Create allele sequences
#AllLocus=2
#n_allele=2
# create_seq_allele <- function(AllLocus, n_allele){
#   seq_allele <- sample(1:2,n_allele*AllLocus,1)
#   #seq_allele <- toString(seq_allele)
#   #seq_allele <- gsub(",",";",seq_allele)
#   return(seq_allele)
# }

## Function to create genotype for each individual
create_nuclear_seq <- function(GeneticTraits, AllelesFrequencies){
  #nuclear=NULL
  nlocus=NULL
  i=0
  genotype=NULL
  for (trait in GeneticTraits$ParametersNames){ # loop over genetic traits
    i=i+1 # counter over traits
    nbLocus <-GeneticTraits$nbLocus[i] # number of locus by traits
    p <- AllelesFrequencies[trait,] # allele frequencies for the trait
    seq_allele=NULL
    for (j in 1:nbLocus){
      tmp <- sample(1:2,2,prob=c(p[j],1-p[j]),replace=TRUE) # sample 2 alleles at locus j based on frequencies p
      seq_allele <- c(seq_allele,tmp) # add alleles to sequence (over locus)
    }
    genotype<-c(genotype,seq_allele) # add sequence over traits
  }
  genotype <- toString(genotype) # convert to character
  genotype <- gsub(", ",";",genotype) # separate alleles
  return(genotype)
}


## Weight-Length relationship
FL = function(W,fat,lwa,lwb,pPercFm){
  fl = exp(lwa +lwb*log((W-fat)/(1-pPercFm)))
  return(fl)
} 




### SIMULATION PARAMETERS ###
#initialDoy = 91

#### POPULATIONS PARAMETERS
# n_rivers =  2
# n_reaches = c(2,2)
# area = c(8000,300)
# prop=.2
#source("CreateRiver.R")


#AllLocus=c(2,2,1,1,1,1,1,1) # multilocus: number of locus for each traits
AllLocus = sum(nbLocus)+nbMsat+nbSNP
#AllLocus = sum(nbLocus)
n_allele=2 # biallelic model

#genotypes
# allele1Neutral=matrix(nrow=nbIndividuals, ncol = (nbMsat+nbSNP) )
# allele2Neutral=matrix(nrow=nbIndividuals, ncol = (nbMsat+nbSNP) )
# 
# if(nbMsat>0){
#   for(i in 1:nbMsat) {
#     allele1Neutral[,i]<- 11+round(runif(nbIndividuals, min=0, max=9))
#     allele2Neutral[,i]<- 11+round(runif(nbIndividuals, min=0, max=9))
#   }
#   #allele1Neutral[,AllLocus_FCRITBB+AllLocusg1max+1]
# }
# 
# freqSNP=runif(nbSNP, min=0, max=1)
# for(i in 1:nbSNP) {
#   allele1Neutral[,nbMsat+i]<- 1+rbinom(nbIndividuals, size=1, prob=freqSNP[i])
#   allele2Neutral[,nbMsat+i]<- 1+rbinom(nbIndividuals, size=1, prob=freqSNP[i])
# }
# #allele1Neutral[,AllLocus_FCRITBB+ AllLocusg1max+nbMsat+1]
# 
# 
# 
# allele1=cbind(allele1_FCRITBB, allele1_g1max,allele1Neutral)
# allele2=cbind(allele2_FCRITBB, allele2_g1max,allele2Neutral)



# From Cyril Piou (see IBASAM)
  #female = ( runif(1, 0.,1.) < FemaleProb );
  #parr= ( runif(0.,1.) < ParrProb );
  #mature= ( runif(0.,1.) < MatureProb );
  #at_sea= ( runif(0.,1.) < AtSeaProb );
  #smolt= ( runif(0.,1.) < SmoltProb );
  #if(smolt && parr)smolt=false;
  #if(at_sea && (parr || smolt))at_sea=false;
  #if(mature && smolt)mature=false;

reach_probabilities <- rep(0, nrow(ReachesArea))
accessible_reaches <- which(Weirs$upstreamRate[match(ReachesArea$fatherID, Weirs$ID)] == 1)
reach_probabilities[accessible_reaches] <- ReachesArea$habitatArea[accessible_reaches] / sum(ReachesArea$habitatArea[accessible_reaches])

juv=parr=smolt=anadromous_1SW=anadromous_MSW=anadromous_1SW_sea=list()

  for ( river in 1:n_rivers){
  
    ### 1. ADD JUVENILES
    
    # Parameters
    Wmean = 8 # to get ~25mm of length
    Wsd = 1 # 0.087 estimated from Scorf data MCMC 09032009
    Fat_m = 1.25
    Fat_sd = 0.2
    Fat_add_fem = 0 
    Femaleprob = 0.5# femaleprob   -> start condition hypothesis
    Parrprob = 1.0# parrprob      -> start condition hypothesis
    Matureprob = c(0.05, 0)# matureprob (male, female)   -> start condition hypothesis
    At_seaprob = 0.0# at_seaprob    -> start condition hypothesis
    Smoltprob = 0.5# smoltprob      -> start condition hypothesis
    max_days_from_emergence = 10 # max days after creation date (e.g. April 1st)
    
    
    
#### FRY / PARR 0+    
  #N=50
  N = 20740*prop #10740 #round(TotalArea[river]*8*0.15) # N -> here set given that we get ~3% of emerging parrs reaching smolt run stage and we assume a 0.45% from eggs to smolt survival (0.15=0.0045/0.03), the conservation limit of egg deposition is 4.75eggs/m?, here assumed to be the limit of conservation for Scorff

  if (N == 0 ) { print(paste0("No juveniles created for river ",river)) } else {
    
  id <- 1:N
  female <- tolower( runif(N, 0.,1.) < Femaleprob );
  age_days <- initialDoy - 90 #ceiling(runif(N,85,100))#sample(1:max_days_from_emergence,N,1)
  
  riverId <- river#sample(1:n_river,N_parr0,1)
  #reachId <- sample(2:n_reaches[river],N,1)
  if(n_reaches[river] == 2) {
    reachId <- rep(2,N)
    birthreachId <- reachId
  } else {
    #reachId <- sample(2:n_reaches[river],N,replace=T)
    reachId <- sample(2:n_reaches[river],N,replace=T, prob=reach_probabilities[-1])
    birthreachId <- reachId
  }
  birthriverId <- river#sample(1:n_rivers,N,1)
  #birthreachId <- sample(2:n_reaches[river],N,1)

  weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
  fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
  pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
  forkLength = FL(weight,fat,sp_lwa_parr,sp_lwb_parr,sp_pPercFm) # Fish size
  #In log((W - fat)/(1 - pPercFm)) : NaNs produced
  while (any(is.na(forkLength))) {
    weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
    fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
    pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
    forkLength = FL(weight,fat,sp_lwa_anadromous,sp_lwb_anadromous,sp_pPercFm) # Fish size
    
    #print_color("\n WARNING: NA in forkLength computation in river parr0+- resampling weight and forkLength ","red")
  }
  
  mature = tolower(runif(N, 0.,1.) < ifelse(female,Matureprob[2],Matureprob[1]) );
  #state = "STATE_PARR"
  
  # nuclear=NULL
  # for (i in 1:N){
  #   seq_allele <- create_seq_allele(AllLocus,n_allele)
  #   seq_allele <- toString(seq_allele)
  #   seq_allele <- gsub(", ",";",seq_allele)
  #   nuclear = rbind(nuclear,paste0("{", seq_allele ,"}"))
  # }s
  
  nuclear=NULL
  for (i in 1:N){
    seq <- create_nuclear_seq(GeneticTraits,AllelesFrequencies)
    nuclear<-rbind(nuclear,paste0("{", seq ,"}"))
  }
  
  
  juv[[river]]<-data.frame(
    id = id
    , female =female
    , age_days = age_days
    
    , nuclear = nuclear
    , mCytoplasmic="{}"
    , pCytoplasmic="{}"
    
    , mId = -1
    , pId = -1
    
    , creationDate = 0
    
    , riverId = riverId
    , reachId = reachId
    , birthriverId = birthriverId
    , birthreachId = birthreachId
    
    , atSea = "false"
    , seaRegionId = 0
    
    , weight = round(weight,2)
    , forkLength = round(forkLength,2)
    , fat = fat
    
    #, departureDate = "null"
    #, pPercF = round(pPercF,2)
    
    #, mature = mature
    , state = "STATE_PARR"#sample(c("STATE_PARR","STATE_SMOLT"),10,replace=TRUE,prob=c(1-Smoltprob,Smoltprob))
    
    , durationInSea=0
    , durationInRiverAsParr=0
    
    )
} # end if



### 2. ADD PARR 1+ ####

# start condition hypothesis
#N = 10
N = 2000*prop #500 #round(TotalArea[river]* 8* 0.15 * 0.011) # N emergence *  0.011 here from  SP0^183 * SP1^182  when SPO is for 5.3% survival from emergence to parr 0 in october

  if (N == 0 ) { print(paste0("No PARR 1+ created for river ",river)) }
  else {

Wmean = 30 #7.7# Wmean
Wsd = 3 #1.8# Wsd
Femaleprob = 0.5# femaleprob   -> start condition hypothesis
Parrprob = 1.0# parrprob      -> start condition hypothesis
Matureprob = c(0.4, 0) #0.0# matureprob (male, female)   -> start condition hypothesis
At_seaprob = 0.0# at_seaprob    -> start condition hypothesis
Smoltprob = 0.0# smoltprob      -> start condition hypothesis
AgeRiver = 1.0# ageRiver       -> start condition hypothesis
AgeSea = 0.0# ageSea         -> start condition hypothesis
Fat_m = 5 #0.64# Fat_m         -> start condition hypothesis
Fat_sd = 0.5 #0.28# Fat_sd        -> start condition hypothesis
Fat_add_fem = 0.0# Fat_add_fem   -> start condition hypothesis

id <- 1:N
female <- tolower( runif(N, 0.,1.) < Femaleprob );
age_days <- initialDoy - 90 + 365 #ceiling(runif(N,85,100))+365 #(AgeRiver+AgeSea)*365 + sample(1:max_days_from_emergence,N,1)

riverId <- river#sample(1:n_river,N_parr0,1)
#reachId <- sample(2:n_reaches[river],N,1)
if(n_reaches[river] == 2) {
  reachId <- rep(2,N)
  birthreachId <- reachId
} else {
  #reachId <- sample(2:n_reaches[river],N,replace=T)
  reachId <- sample(2:n_reaches[river],N,replace=T, prob=reach_probabilities[-1])
  birthreachId <- reachId
}
birthriverId <- river#sample(1:n_rivers,N,1)
#birthreachId <- sample(2:n_reaches[river],N,1)

weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
#pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
forkLength = FL(weight,fat,sp_lwa_parr,sp_lwb_parr,sp_pPercFm) # Fish size
while (any(is.na(forkLength))) {
  weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
  fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
  pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
  forkLength = FL(weight,fat,sp_lwa_anadromous,sp_lwb_anadromous,sp_pPercFm) # Fish size
  
  #print_color("\n WARNING: NA in forkLength computation in river parr1+- resampling weight and forkLength ","red")
}

mature = tolower(runif(N, 0.,1.) < ifelse(female,Matureprob[2],Matureprob[1]) );


# nuclear=NULL
# for (i in 1:N){
#   seq_allele <- create_seq_allele(AllLocus,n_allele)
#   seq_allele <- toString(seq_allele)
#   seq_allele <- gsub(", ",";",seq_allele)
#   nuclear = rbind(nuclear,paste0("{", seq_allele ,"}"))
# }

nuclear=NULL
for (i in 1:N){
  seq <- create_nuclear_seq(GeneticTraits,AllelesFrequencies)
  nuclear<-rbind(nuclear,paste0("{", seq ,"}"))
}

parr[[river]]<-data.frame(
  id = id
  , female =female
  , age_days = age_days

  , nuclear = nuclear
  , mCytoplasmic = "{}"
  , pCytoplasmic = "{}"

  , mId = -1
  , pId = -1

  , creationDate = 0

  , riverId = riverId
  , reachId = reachId
  , birthriverId = birthriverId
  , birthreachId = birthreachId

  , atSea = "false"
  , seaRegionId = 0

  , weight = round(weight,2)
  , forkLength = round(forkLength,2)
  , fat = fat

  #, departureDate = "null"

  #, mature = mature
  , state = "STATE_PARR"

  , durationInSea=0
  , durationInRiverAsParr = 1

)  } # end if

#   
#   
#   
#   ### 3. ADD SMOLT ####
#   
#   # start condition hypothesis
#   #N = 10
#   N = 5768  #round(TotalArea[river]* 8* 0.15 * 0.03) 
#   # N -> here set given that we get ~3% of emerging parrs reaching smolt run stage
#   
#   if (N == 0 ) { print(paste0("No SMOLT created for river ",river)) }
#   else {
#      
#     Wmean = 30.8 # Wmean / mean of smolts 1 of scorff 2000   (not really normally distributed but best year (middle of distribution))
#     Wsd = 5# Wsd / sd of smolts 1 of scorff 2000 ~ /2 (because else too small sizes occurs)
#     Femaleprob = 0.5# femaleprob   -> start condition hypothesis
#     Parrprob = 0.0# parrprob      -> start condition hypothesis
#     Matureprob = 0.0# matureprob    -> start condition hypothesis
#     At_seaprob = 0.0# at_seaprob    -> start condition hypothesis
#     Smoltprob = 1.0# smoltprob      -> start condition hypothesis
#     AgeRiver = 1.0# ageRiver       -> start condition hypothesis
#     AgeSea = 0.0# ageSea         -> start condition hypothesis
#     Fat_m = 2# Fat_m         -> start condition hypothesis
#     Fat_sd = 1.6# Fat_sd        -> start condition hypothesis
#     Fat_add_fem = 0.0# Fat_add_fem   -> start condition hypothesis
#     
#     id <- 1:N
#     female <- tolower( runif(N, 0.,1.) < Femaleprob );
#     age_days <- (AgeRiver+AgeSea)*365 + sample(1:max_days_from_emergence,N,1)
#     
#     riverId <- river#sample(1:n_river,N_parr0,1)
#     reachId <- sample(2:n_reaches[river],N,1)
#     birthriverId <- river#sample(1:n_rivers,N,1)
#     birthreachId <- sample(2:n_reaches[river],N,1)
#     
#     weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
#     fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
#     pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
#     forkLength = FL(weight,fat,sp_lwa_parr,sp_lwb_parr,sp_pPercFm) # Fish size
#     
#     mature = tolower(runif(N, 0.,1.) < MatureProb );
#     
#     nuclear=NULL
#     for (i in 1:N){
#       seq_allele <- create_seq_allele(AllLocus,n_allele)
#       seq_allele <- toString(seq_allele)
#       seq_allele <- gsub(", ",";",seq_allele) 
#       nuclear = rbind(nuclear,paste0("{", seq_allele ,"}"))
#     }
# 
#     
#     smolt[[river]]<-data.frame(
#       id = id
#       , female =female
#       , age_days = age_days
#       
#       , nuclear = nuclear
#       , mCytoplasmic = "{}"
#       , pCytoplasmic = "{}"
#       
#       , mId = -1
#       , pId = -1
#       
#       , creationDate = 0
#       
#       , riverId = riverId
#       , reachId = reachId
#       , birthriverId = birthriverId
#       , birthreachId = birthreachId
#       
#       , atSea = "false"
#       , seaRegionId = 0
#       
#       , weight = round(weight,2)
#       , forkLength = round(forkLength,2)
#       , fat = fat
#       
#       #, departureDate = "null"
#       
#       #, mature = mature
#       , state = "STATE_SMOLT"
#       
#       , durationInSea=0
#       , durationInRiverAsParr=age_days
#       
#     )  } # end if
#   
#   
#   
  ### 4. ADD 1SW ####

  # start condition hypothesis
  #N=0
  N = 507*prop #651 #round(TotalArea[river]* 8* 0.15 * 0.003)
# N -> 0.03*0.10 (0.10 of "ocean survival" from scorff data)


  if (N == 0 ) { print(paste0("No 1SW created for river ",river)) }
  else {

    Wmean = 2500 # Wmean
    Wsd = 500 # Wsd
    Femaleprob = 0.467# femaleprob   -> start condition hypothesis
    Parrprob = 0.0# parrprob      -> start condition hypothesis
    Matureprob = 1.0# matureprob    -> start condition hypothesis
    At_seaprob = 0.0# at_seaprob    -> start condition hypothesis
    Smoltprob = 1.0# smoltprob      -> start condition hypothesis
    AgeRiver = 1.0# ageRiver       -> start condition hypothesis
    AgeSea = 1.0# ageSea         -> start condition hypothesis
    Fat_m = 500 #370# Fat_m         -> start condition hypothesis
    Fat_sd = 100# Fat_sd        -> start condition hypothesis
    Fat_add_fem = 350# Fat_add_fem   -> start condition hypothesis

    id <- 1:N
    female <- tolower( runif(N, 0.,1.) < Femaleprob );
    age_days <- initialDoy - 90 + 2*365  # ceiling(runif(N,85,100)) + (AgeRiver+AgeSea)*365 #+ sample(1:max_days_from_emergence,N,1) # /!\ to check


    riverId <- river#sample(1:n_river,N_parr0,1)
    #reachId <- sample(2:n_reaches[river],N,1)
    if(n_reaches[river] == 2) {
      reachId <- rep(2,N)
      birthreachId <- reachId
    } else {
      #reachId <- sample(2:n_reaches[river],N,replace=T)
      reachId <- sample(2:n_reaches[river],N,replace=T, prob=reach_probabilities[-1])
      birthreachId <- reachId
    }
    birthriverId <- river#sample(1:n_rivers,N,1)
    #birthreachId <- sample(2:n_reaches[river],N,1)

    weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
    fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
    pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
    forkLength = FL(weight,fat,sp_lwa_anadromous,sp_lwb_anadromous,sp_pPercFm) # Fish size
    while (any(is.na(forkLength))) {
      weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
      fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
      pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
      forkLength = FL(weight,fat,sp_lwa_anadromous,sp_lwb_anadromous,sp_pPercFm) # Fish size
      
      #print_color("\n WARNING: NA in forkLength computation of river 1SW- resampling weight and forkLength ","red")
    }
    
    mature = tolower(runif(N, 0.,1.) < Matureprob );

    durationInSea = 1

    # nuclear=NULL
    # for (i in 1:N){
    #   seq_allele <- create_seq_allele(AllLocus,n_allele)
    #   seq_allele <- toString(seq_allele)
    #   seq_allele <- gsub(", ",";",seq_allele)
    #   nuclear = rbind(nuclear,paste0("{", seq_allele ,"}"))
    # }
    
    nuclear=NULL
    for (i in 1:N){
      seq <- create_nuclear_seq(GeneticTraits,AllelesFrequencies)
      nuclear<-rbind(nuclear,paste0("{", seq ,"}"))
    }

    anadromous_1SW[[river]]<-data.frame(
      id = id
      , female =female
      , age_days = age_days

      , nuclear = nuclear
      , mCytoplasmic = "{}"
      , pCytoplasmic = "{}"

      , mId = -1
      , pId = -1

      , creationDate = 0

      , riverId = riverId
      , reachId = reachId
      , birthriverId = birthriverId
      , birthreachId = birthreachId

      , atSea = "false"
      , seaRegionId = 1

      , weight = round(weight,2)
      , forkLength = round(forkLength,2)
      , fat = fat

      #, departureDate = paste0(initialYear-1,".",90)
      #, pPercF = round(pPercF,2)

      #, mature = "true"
      , state = "STATE_ANADROMOUS"

      , durationInSea = durationInSea #270  # /!\ to check / From Cyril Piou: 125.0, //nb of days additional of ocean life for grilse... could become stochastic!   120 from scorff data!
      , durationInRiverAsParr = 1 # /!\ to check

    )  } # end if




  ### 5. ADD MSW ####

  # start condition hypothesis
  #N=0
  N = 92*prop #200 #round(TotalArea[river]* 8* 0.15 * 0.0005) # N -> 0.03*0.017 (0.017 of "ocean survival" from scorff data)

  if (N == 0 ) { print(paste0("No MSW created for river ",river)) }
  else {

    Wmean = 4560 # Wmean
    Wsd = 760 # Wsd
    Femaleprob = 0.81# femaleprob   -> start condition hypothesis
    Parrprob = 0.0# parrprob      -> start condition hypothesis
    Matureprob = 1.0# matureprob    -> start condition hypothesis
    At_seaprob = 0.0# at_seaprob    -> start condition hypothesis
    Smoltprob = 1.0# smoltprob      -> start condition hypothesis
    AgeRiver = 1.0# ageRiver       -> start condition hypothesis
    AgeSea = 2.0# ageSea         -> start condition hypothesis
    Fat_m = 590# Fat_m         -> start condition hypothesis
    Fat_sd = 110# Fat_sd        -> start condition hypothesis
    Fat_add_fem = 240# Fat_add_fem   -> start condition hypothesis

    id <- 1:N
    female <- tolower( runif(N, 0.,1.) < Femaleprob );
    age_days <- initialDoy - 90 + 3*365 #ceiling(runif(N,85,100)) + (AgeRiver+AgeSea)*365 #+ sample(1:max_days_from_emergence,N,1) # /!\ to check

    riverId <- river#sample(1:n_river,N_parr0,1)
    #reachId <- sample(2:n_reaches[river],N,1)
    if(n_reaches[river] == 2) {
      reachId <- rep(2,N)
      birthreachId <- reachId
    } else {
      #reachId <- sample(2:n_reaches[river],N,replace=T)
      reachId <- sample(2:n_reaches[river],N,replace=T, prob=reach_probabilities[-1])
      birthreachId <- reachId
    }
    birthriverId <- river#sample(1:n_rivers,N,1)
    #birthreachId <- sample(2:n_reaches[river],N,1)

    weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
    fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
    pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
    forkLength = FL(weight,fat,sp_lwa_anadromous,sp_lwb_anadromous,sp_pPercFm) # Fish size
    while (any(is.na(forkLength))) {
      weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
      fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
      pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
      forkLength = FL(weight,fat,sp_lwa_anadromous,sp_lwb_anadromous,sp_pPercFm) # Fish size
      
      #print_color("\n WARNING: NA in forkLength computation of river MSW - resampling weight and forkLength ","red")
    }
    
    mature = tolower(runif(N, 0.,1.) < Matureprob );

    durationInSea = 2

    # nuclear=NULL
    # for (i in 1:N){
    #   seq_allele <- create_seq_allele(AllLocus,n_allele)
    #   seq_allele <- toString(seq_allele)
    #   seq_allele <- gsub(", ",";",seq_allele)
    #   nuclear = rbind(nuclear,paste0("{", seq_allele ,"}"))
    # }
    
    nuclear=NULL
    for (i in 1:N){
      seq <- create_nuclear_seq(GeneticTraits,AllelesFrequencies)
      nuclear<-rbind(nuclear,paste0("{", seq ,"}"))
    }

    anadromous_MSW[[river]]<-data.frame(
      id = id
      , female =female
      , age_days = age_days

      , nuclear = nuclear
      , mCytoplasmic = "{}"
      , pCytoplasmic = "{}"

      , mId = -1
      , pId = -1

      , creationDate = 0

      , riverId = riverId
      , reachId = reachId
      , birthriverId = birthriverId
      , birthreachId = birthreachId

      , atSea = "false"
      , seaRegionId = 2

      , weight = round(weight,2)
      , forkLength = round(forkLength,2)
      , fat = fat

      #, departureDate = paste0(initialYear-2,".",90)
      #, pPercF = round(pPercF,2) # mean and sd at population level of individual capacity to store growth into fat reserves


      #, mature = "true"
      , state = "STATE_ANADROMOUS"

      , durationInSea = durationInSea #270  # /!\ to check
      , durationInRiverAsParr= 1 # /!\ to check

    )
    } # end if


  
  } # end loop river


#### FISH AT SEA ??? ###

for (searegion in 1:1) {
  ### 4. ADD 1SW ####
  
  # start condition hypothesis
  #N=0
  N = 1000*prop #round(TotalArea[river]* 8* 0.15 * 0.003)
  # N -> 0.03*0.10 (0.10 of "ocean survival" from scorff data)
  
  
  if (N == 0 ) { 
    print(paste0("No 1SW created for searegion ",searegion)) 
  } else {
    
    Wmean = 350 # Wmean
    Wsd = 80 # Wsd
    Femaleprob = 0.467# femaleprob   -> start condition hypothesis
    Parrprob = 0.0# parrprob      -> start condition hypothesis
    Matureprob = 0# matureprob    -> start condition hypothesis
    At_seaprob = 1# at_seaprob    -> start condition hypothesis
    Smoltprob = 1.0# smoltprob      -> start condition hypothesis
    AgeRiver = 1.0# ageRiver       -> start condition hypothesis
    AgeSea = 0# ageSea         -> start condition hypothesis
    Fat_m = 40# Fat_m         -> start condition hypothesis
    Fat_sd = 10# Fat_sd        -> start condition hypothesis
    Fat_add_fem = 40# Fat_add_fem   -> start condition hypothesis
    
    id <- 1:N
    female <- tolower( runif(N, 0.,1.) < Femaleprob );
    age_days <- initialDoy - 90 + 1*365  # ceiling(runif(N,85,100)) + (AgeRiver+AgeSea)*365 #+ sample(1:max_days_from_emergence,N,1) # /!\ to check
    
    
    searegionId <- searegion#sample(1:n_river,N_parr0,1)
    #reachId <- sample(2:n_reaches[river],N,1)
    if(n_reaches[river] == 2) {
      reachId <- rep(2,N)
      birthreachId <- reachId
    } else {
      #reachId <- sample(2:n_reaches[river],N,replace=T)
      reachId <- sample(2:n_reaches[river],N,replace=T, prob=reach_probabilities[-1])
      birthreachId <- reachId
    }
    birthriverId <- river#sample(1:n_rivers,N,1)
    #birthreachId <- sample(2:n_reaches[river],N,1)
    
    weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
    fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
    pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
    forkLength = FL(weight,fat,sp_lwa_anadromous,sp_lwb_anadromous,sp_pPercFm) # Fish size
    while (any(is.na(forkLength))) {
      weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
      fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
      pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
      forkLength = FL(weight,fat,sp_lwa_anadromous,sp_lwb_anadromous,sp_pPercFm) # Fish size
      
      #print_color("\n WARNING: NA in forkLength computation of sea 1SW- resampling weight and forkLength ","red")
    }
    
    mature = tolower(runif(N, 0.,1.) < Matureprob );
    
    durationInSea = 0
    
    # nuclear=NULL
    # for (i in 1:N){
    #   seq_allele <- create_seq_allele(AllLocus,n_allele)
    #   seq_allele <- toString(seq_allele)
    #   seq_allele <- gsub(", ",";",seq_allele)
    #   nuclear = rbind(nuclear,paste0("{", seq_allele ,"}"))
    # }
    
    nuclear=NULL
    for (i in 1:N){
      seq <- create_nuclear_seq(GeneticTraits,AllelesFrequencies)
      nuclear<-rbind(nuclear,paste0("{", seq ,"}"))
    }
    
    anadromous_1SW_sea[[searegion]]<-data.frame(
      id = id
      , female =female
      , age_days = age_days
      
      , nuclear = nuclear
      , mCytoplasmic = "{}"
      , pCytoplasmic = "{}"
      
      , mId = -1
      , pId = -1
      
      , creationDate = 0
      
      , riverId = riverId
      , reachId = reachId
      , birthriverId = birthriverId
      , birthreachId = birthreachId
      
      , atSea = "true"
      , seaRegionId = searegionId
      
      , weight = round(weight,2)
      , forkLength = round(forkLength,2)
      , fat = fat
      
      #, departureDate = paste0(initialYear-1,".",90)
      #, pPercF = round(pPercF,2)
      
      #, mature = "true"
      , state = "STATE_ANADROMOUS"
      
      , durationInSea = durationInSea #270  # /!\ to check / From Cyril Piou: 125.0, //nb of days additional of ocean life for grilse... could become stochastic!   120 from scorff data!
      , durationInRiverAsParr = 1 # /!\ to check
      
    )  } # end if
  
  
  
  
  # ### 5. ADD MSW ####
  # 
  # # start condition hypothesis
  # #N=0
  # N = 200*prop #round(TotalArea[river]* 8* 0.15 * 0.0005) # N -> 0.03*0.017 (0.017 of "ocean survival" from scorff data)
  # 
  # if (N == 0 ) { print(paste0("No MSW created for river ",river)) }
  # else {
  #   
  #   Wmean = 4560 # Wmean
  #   Wsd = 760 # Wsd
  #   Femaleprob = 0.81# femaleprob   -> start condition hypothesis
  #   Parrprob = 0.0# parrprob      -> start condition hypothesis
  #   Matureprob = 1.0# matureprob    -> start condition hypothesis
  #   At_seaprob = 0.0# at_seaprob    -> start condition hypothesis
  #   Smoltprob = 1.0# smoltprob      -> start condition hypothesis
  #   AgeRiver = 1.0# ageRiver       -> start condition hypothesis
  #   AgeSea = 2.0# ageSea         -> start condition hypothesis
  #   Fat_m = 590# Fat_m         -> start condition hypothesis
  #   Fat_sd = 110# Fat_sd        -> start condition hypothesis
  #   Fat_add_fem = 240# Fat_add_fem   -> start condition hypothesis
  #   
  #   id <- 1:N
  #   female <- tolower( runif(N, 0.,1.) < Femaleprob );
  #   age_days <- initialDoy - 90 + 3*365 #ceiling(runif(N,85,100)) + (AgeRiver+AgeSea)*365 #+ sample(1:max_days_from_emergence,N,1) # /!\ to check
  #   
  #   riverId <- river#sample(1:n_river,N_parr0,1)
  #   #reachId <- sample(2:n_reaches[river],N,1)
  #   if(n_reaches[river] == 2) {
  #     reachId <- rep(2,N)
  #     birthreachId <- rep(2,N)
  #   } else {
  #     reachId <- sample(2:n_reaches[river],N,1)
  #     birthreachId <- sample(2:n_reaches[river],N,1)
  #   }
  #   birthriverId <- river#sample(1:n_rivers,N,1)
  #   #birthreachId <- sample(2:n_reaches[river],N,1)
  #   
  #   weight = abs(rnorm(N,Wmean,Wsd)) # abs() because must be >0
  #   fat = abs(rnorm(N, Fat_m,Fat_sd)) # abs() because must be >0
  #   pPercF = abs(rnorm(N, sp_pPercFm, sp_pPercFsd)) # mean and sd at population level of individual capacity to store growth into fat reserves
  #   forkLength = FL(weight,fat,sp_lwa_anadromous,sp_lwb_anadromous,sp_pPercFm) # Fish size
  #   
  #   mature = tolower(runif(N, 0.,1.) < MatureProb );
  #   
  #   durationInSea = 2
  #   
  #   # nuclear=NULL
  #   # for (i in 1:N){
  #   #   seq_allele <- create_seq_allele(AllLocus,n_allele)
  #   #   seq_allele <- toString(seq_allele)
  #   #   seq_allele <- gsub(", ",";",seq_allele)
  #   #   nuclear = rbind(nuclear,paste0("{", seq_allele ,"}"))
  #   # }
  #   
  #   nuclear=NULL
  #   for (i in 1:N){
  #     seq <- create_nuclear_seq(GeneticTraits,AllelesFrequencies)
  #     nuclear<-rbind(nuclear,paste0("{", seq ,"}"))
  #   }
  #   
  #   anadromous_MSW[[river]]<-data.frame(
  #     id = id
  #     , female =female
  #     , age_days = age_days
  #     
  #     , nuclear = nuclear
  #     , mCytoplasmic = "{}"
  #     , pCytoplasmic = "{}"
  #     
  #     , mId = -1
  #     , pId = -1
  #     
  #     , creationDate = 0
  #     
  #     , riverId = riverId
  #     , reachId = reachId
  #     , birthriverId = birthriverId
  #     , birthreachId = birthreachId
  #     
  #     , atSea = "false"
  #     , seaRegionId = 2
  #     
  #     , weight = round(weight,2)
  #     , forkLength = round(forkLength,2)
  #     , fat = fat
  #     
  #     #, departureDate = paste0(initialYear-2,".",90)
  #     #, pPercF = round(pPercF,2) # mean and sd at population level of individual capacity to store growth into fat reserves
  #     
  #     
  #     #, mature = "true"
  #     , state = "STATE_ANADROMOUS"
  #     
  #     , durationInSea = durationInSea #270  # /!\ to check
  #     , durationInRiverAsParr= age_days - durationInSea # /!\ to check
  #     
  #   )
  # } # end if
} #end for searegions


data_juv <- do.call("rbind",juv)
data_parr <- do.call("rbind",parr)
#data_smolt <- do.call("rbind",smolt)
data_anadromous_1SW <- do.call("rbind",anadromous_1SW)
data_anadromous_MSW <- do.call("rbind",anadromous_MSW)
data_anadromous_1SW_sea <- do.call("rbind",anadromous_1SW_sea)
data_all <- rbind(data_juv,data_parr,  data_anadromous_1SW, data_anadromous_MSW, data_anadromous_1SW_sea) #
data_all$id <- 1:nrow(data_all) # different ids for all individuals
write.table(data_all, file=paste0("initIndividuals_ABC",row,".inv"), sep="\t", row.names = FALSE)


  
  


