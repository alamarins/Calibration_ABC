############# Neutral allele ############

nbMsat= 0   # number of neutral microsatellite(SSR) loci  / nombre de loci neutres microsatellites (SSR)
nbAllPerMsat= 0 # number of alleles/locus at SSR / nombre d'alleles/locus  aux SSR
nbSNP= 0 # number of neutral SNP loci  / nombre de loci neutres type SNP
nbAllPerSNP= 2  # number of alleles/locus at SNP / nombre d'alleles/locus  aux SNP

distanceClassBounds = c(10,25,50,75,100,125,150,175,200,250)
nbDistanceClassBounds=length(distanceClassBounds)

############# Adaptive traits ############

GeneticTraits <- read.table(file="genetic/GeneticParameters.csv",sep=";",header=TRUE)

numberOfGeneticTraits <- length(GeneticTraits$ParametersNames)
nbLocus <- GeneticTraits$nbLocus
nbAllelePerLoc <- GeneticTraits$nbAllele
alleleEffectMultiplCoeff <- GeneticTraits$alleleEffectMultiplCoeff
#dominanceEffectMultiplCoeff <- GeneticTraits$dominanceEffectMultiplCoeff

GeneticTraits$Mean_value[which(GeneticTraits$ParametersNames == "maturationThresholdMaleParr")] <- 0.4805 #matrix_param[row,"meanMatThrMalParr"]
GeneticTraits$Mean_value[which(GeneticTraits$ParametersNames == "maturationThresholdMaleAnadromous")] <- 45.9667 #matrix_param[row,"meanMatThrMalAnad"]
GeneticTraits$Mean_value[which(GeneticTraits$ParametersNames == "maturationThresholdFemaleAnadromous")] <- 87.2227 #matrix_param[row,"meanMatThrFemAnad"]
GeneticTraits$Mean_value[which(GeneticTraits$ParametersNames == "smoltificationThresholdMale")] <- 89.0086 #matrix_param[row,"meanSmoltiThrMal"]
GeneticTraits$Mean_value[which(GeneticTraits$ParametersNames == "smoltificationThresholdFemale")] <- 90.8907 #matrix_param[row,"meanSmoltiThrFem"]

############# Locus effect ############

beta = GeneticTraits$beta # parameter of the negative exponential / give the effect of each locus
# if beta = 0, all locus have the same effect (1/nbLocus)

############# Dominance ############

# if scaledDominanceDeviation = 0 -> no dominance
# if scaledDominanceDeviation = 1 -> complete dominance
# if scaledDominanceDeviation > 1 -> overdominance
# if scaledDominanceDeviation < 1 -> incomplete dominance
scaledDominanceDeviation <- read.table(file="genetic/scaledDominanceDeviationValues.csv",sep="\t",header=TRUE)

############# Allele frequencies ############

AllelesFrequencies <- read.table(file="genetic/AllelesFrequencies.csv",sep="\t",header=TRUE)


###############################################
###STEP 2 = Computation / calculs
###############################################

# Les informations sur la valeur adaptative des allèles
# Ces informations sont :
#   - les valeurs des allèles (effets uniquement additifs)
# - l’héritabilité théorique du paramètre
# - la variance environnementale totale du paramètre σ 2 total
# - la part de la variance environnementale inter Step xσ 2
# int er
# Les valeurs des allèles sont utilisées pour le calcul des valeurs génétiques. Elles sont supposées invariables dans le temps.
# Les autres informations sont utilisées pour le calcul des valeurs environnementales et phénotypiques.
# Le principe de calcul de la valeur environnementale est basé sur l’hypothèse que la variance environnementale d’un paramètre se décompose en une variance environnementale intra Step3 et une variance environnementale inter Step.
# Par exemple, si le paramètre étudié est la potentialité de croissance, la variance environnementale de ce paramètre est la somme :
#   - de la variance inter Step qui représente la variabilité annuelle de l’effet du milieu liée, par exemple, à des variations de pluviométrie d’une année à l’autre
# - et de la variance intra Step qui représente la variabilité spatiale de l’effet du milieu liée, par exemple, aux variations des conditions stationnelles supposées constantes dans le temps et qui ne seraient pas prises en compte dans le module
# La valeur environnementale est alors la somme de :
#   - la valeur environnementale fixe, tirée dans une loi normale de moyenne 0 et de variance la variance environnementale intra Step ( σ 2 = σ 2 (1 − xσ 2 ) ),
# intra total inter
# - et de la valeur environnementale inter Step, tirée dans une loi normale de moyenne 0
# et de variance la variance environnementale inter Step ( σ 2 = σ 2 ( xσ 2 ) ). inter total inter
# L’utilisateur doit toujours fournir la part de la variance environnementale inter Step. En revanche il peut soit fournir la variance environnementale totale du paramètre, soit l’héritabilité théorique du paramètre. 
# La bibliothèque en déduira la variance environnementale totale d’après l’équation héritabilité = variance génétique / (variance génétique + variance environnementale), 
# la variance génétique étant calculée sur l’ensemble des arbres (individuels et moyens) du peuplement initial sous l’hypothèse de panmixie.



####Initialisation of allelic effects / Initialisation des effets des alleles
Nind = 1000


target_mean = GeneticTraits$Mean_value #* GeneticTraits$alleleEffectMultiplCoeff# Observed mean value
target_sd = GeneticTraits$CoefVariation* target_mean # Standard deviation / coef Vairation = st deviation/mean
#target_sd[7] = 0.2 #for growth potential at log scale
#target_sd[8] = 0.2 #for growth potential at log scale
target_Var=target_sd^2 # Variance
heritability <- GeneticTraits$heritability
GenetVar <- target_Var * heritability
totEnvirVar <- target_Var*(1-heritability)   # total environmental variance

effect_Locus = array(,dim=c(numberOfGeneticTraits, max(nbLocus)));
rownames(effect_Locus)<-GeneticTraits$ParametersNames
colnames(effect_Locus)<-paste0("Locus_",1:max(nbLocus))

effect_Allele = array(,dim=c(numberOfGeneticTraits, max(nbLocus)));
rownames(effect_Allele)<-GeneticTraits$ParametersNames
colnames(effect_Allele)<-paste0("Locus_",1:max(nbLocus))

genetValOfTraits = array(,dim=c(Nind, numberOfGeneticTraits));
colnames(genetValOfTraits)<-GeneticTraits$ParametersNames
phenoValOfTraits =genetValOfTraits


pdf(file="genetic/geneticEffects.pdf")
par(mfrow=c(1,1))
for (trait in 1:numberOfGeneticTraits){
  
  allele1=matrix(nrow=Nind, ncol = nbLocus[trait])
  allele2=matrix(nrow=Nind, ncol = nbLocus[trait])
  
  if (GeneticTraits$heritability[trait]>0) {
    mean_val=0
    sd_val=0
    step=0

    step=step+1

    ## Locus effect
    beta = GeneticTraits$beta[trait] # parameter of the negative exponential / give the effect of each locus
    # if beta = 0, all locus have the same effect (1/nbLocus)
    
    # Effect of each locus on the trait
    x=seq(1,nbLocus[trait],1)
    effect_Locus_tmp = exp(x*beta)
    effect_Locus_tmp = effect_Locus_tmp / sum(effect_Locus_tmp)
    effect_Locus[trait,1:nbLocus[trait]] <- effect_Locus_tmp
    
    
    
    ## Calculate allele effects for each locus = average contribution to genotypic value for allele i at locus j
    
    for(locus in 1:nbLocus[trait]) {
      p <- AllelesFrequencies[trait,locus]
      q <- 1 - p
      k <- scaledDominanceDeviation[trait,1+locus]
      
      effect_Allele[trait,locus] <- sqrt((GenetVar[trait] * effect_Locus[trait,locus]) / ((2*p*q*(1+k*(p - q))^2) + (2*p*q*k)^2))
    }
    
    
    ## Get alleles values (1 or 2)
    for(l in 1:nbLocus[trait]) {
      allele1[,l] = sample(c(1,2),Nind,replace=TRUE) # get allele values (0 or 1)
      allele2[,l] = sample(c(1,2),Nind,replace=TRUE) # get allele values (0 or 1)
    }
    
    # simulate genetic values for 1000 individuals
    for (i in 1:Nind) {
      
      genetVal=0
      for (locus in 1:nbLocus[trait]) {
        
        effect_Locus_tmp <- effect_Locus[trait,locus]
        effect_Allele_tmp <- effect_Allele[trait,locus]
        k <- scaledDominanceDeviation[trait,1+locus]
        #k <- scaledDominanceDeviation[trait,1+locus] * scaledDominanceDeviation[trait,1]
        
        locusVal = target_mean[trait] * effect_Locus_tmp # mean genetic value at the locus l
        
        sumAll = allele1[i,locus] + allele2[i,locus]
        # if homozygotous
        if (sumAll==2){
          locusVal = locusVal - effect_Allele_tmp
        } else if (sumAll==4){
          locusVal = locusVal + effect_Allele_tmp
        } else {
          locusVal = locusVal + (effect_Allele_tmp*k) # TO CHECK
        }
        genetVal = genetVal + locusVal
      } # end loop locus
      
      genetValOfTraits[i,trait] <- genetVal
      
      # Pred_val[i]= genetVal+target_mean[trait]
      
      phenoValOfTraits[i,trait] <- genetValOfTraits[i,trait] + rnorm(1,0,sqrt(totEnvirVar[trait]))
      
      # if(trait == 7 || 8){#growth potential at log scale
      #   
      # } else {
      # while(phenoValOfTraits[i,trait]<0){
      #     phenoValOfTraits[i,trait] <- genetValOfTraits[i,trait] + rnorm(1,0,sqrt(totEnvirVar[trait]))
      # }
      #}
      
    } # end loop individuals
    
  } else {
    
    genetVal=rep(target_mean[trait], 100)
    #effect_Locus=freq_Locus*0
    mean_val=target_mean[trait]
    sd_val=0
    
    genetValOfTraits[,trait] <- genetVal
    phenoValOfTraits[,trait] <- genetValOfTraits[,trait] + rnorm(100,0,sqrt(totEnvirVar[trait]))
  }
  
  #par(mfrow=c(1,2))
  #plot(1:nbLocus[trait],effect_Locus_tmp,type="b",xlab="# Loci",ylab="Effect (i.e. contribution to genetic value)",main=GeneticTraits$ParametersNames[trait])
  #barplot(effect_Locus_tmp,xlab="# Loci",ylab="Effect (i.e. contribution to genetic value)",main=GeneticTraits$ParametersNames[trait])
  
  hist(phenoValOfTraits[,trait], main="", xlab=GeneticTraits$ParametersNames[trait], cex.lab=1.5, cex.axis=1.2, breaks=10, freq = FALSE)
  abline(v=mean(phenoValOfTraits[,trait]), lwd=2, col="red")
  mean_val=mean(phenoValOfTraits[,trait])
  mtext(text=paste("mean_val=", round(mean_val,2), sep=" "), side= 3, col="red", cex=1.2)
  
  #hist( effect_Allele_tmp, main="", xlab="Allelic effects", cex.lab=1.5, cex.axis=1.2, breaks=20)
  
} # end loop trait

dev.off()

any(phenoValOfTraits<0)

mean_val = apply(phenoValOfTraits,2,mean)
abs((target_mean-mean_val)/target_mean)>0.01

sd_val = apply(phenoValOfTraits,2,sd)
target_sd
abs((target_sd-sd_val)/target_sd)
