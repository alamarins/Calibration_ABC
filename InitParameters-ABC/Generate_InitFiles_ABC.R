#############################################################################################
######################Generate inventory file for Capsis, module IBASAM######################
#############################################################################################

setwd(paste0(project_workingdir,"/InitParameters-ABC"))
options(max.print=999999)
options(width=1e4)
options(scipen=999)

##################################################
##################### CONFIG FILE ################
##################################################

###STEP1 = LOADING Parameters [change here their value]/ chargement des parametres [ecrire ici leur valeur]

source("ConfigParameters.R")

###STEP2 = PRINT Paramaters

sink(paste0("config_ABC",row,".txt"))

#HEADERS and simple parameters / EN TETES et parametres simples
cat("// Capsis 4.1 - module IBASAM - config parameters")
cat("\n\n//<General>");
cat("\n //Contributors: mb+al+fc - 2023");

cat("\n\n//--------SIMULATION parameters---------//")
cat("\n//Initial date, e.g. 91 = 1 April (if not a leap year)")
cat(paste(
  "\ninitialYear = ", initialYear
  ,"\ninitialDoy = ", initialDoy
  , sep=" "));


cat("\n\n//--------LIFE HISTORY parameters---------//")

cat("\n\t//TIME WINDOWS")
cat(paste0(
  "\n precociousMaturationWindow = (", precociousMaturationWindow[1],",",precociousMaturationWindow[2],")"
  ,"\n seawardMigrationDecisionWindow = (", seawardMigrationDecisionWindow[1],",",seawardMigrationDecisionWindow[2],")"
  ,"\n seawardEffectiveMigrationWindow = (", seawardEffectiveMigrationWindow[1],",",seawardEffectiveMigrationWindow[2],")"
  ,"\n // Maturation window at sea (47 days starting Oct 1st):"
  ,"\n riverwardMigrationDecisionWindow = (", riverwardMigrationDecisionWindow[1],",",riverwardMigrationDecisionWindow[2],")"
  ,"\n // Reproduction window:"
  ,"\n reproductionWindow = (", reproductionWindow[1],",",reproductionWindow[2],")"
  ,"\n fishingWindow = (", fishingWindow[1],",",fishingWindow[2],")"
  #, sep=" ")
  ,"\n"));


cat("\n\t//REPRODUCTION")
cat(paste0(
  "\n sp_rateDateRepro = ", sp_rateDateRepro
  ,"\n sp_shapeDateRepro = ", sp_shapeDateRepro
  ,"\n sp_spawning_nAnadromousMaleMean = ", sp_spawning_nAnadromousMaleMean
  ,"\n sp_spawning_nprecociousMaleMean = ", sp_spawning_nprecociousMaleMean
  ,"\n"
  ,"\n // Species parameters for reproduction"
  ,"\n // aFert: parameter adjusting competitive advantage due to weight among males"
  ,"\n sp_aFert = ", sp_aFert
  ,"\n // fat loss after reproduction (female: loose only their eggs; anad males: 59% of FmID; preco males: average reserve for 1+ in november)"
  ,"\n sp_Loss_Fat_Female = ", sp_Loss_Fat_Female
  ,"\n sp_Loss_Fat_AnadromousMale = ", sp_Loss_Fat_AnadromousMale
  ,"\n sp_Loss_Fat_PrecociousMale = ", sp_Loss_Fat_PrecociousMale
  ,"\n // mortality after reproduction"
  ,"\n sp_ReproMortalityFemale = ", sp_ReproMortalityFemale
  ,"\n sp_ReproMortalityMale = ", sp_ReproMortalityMale
  ,"\n"
  ,"\n //species parameters mean egg weight produced by female"
  ,"\n sp_WtoNegg_a = ", sp_WtoNegg_a
  ,"\n sp_WtoNegg_b = ", sp_WtoNegg_b
  ,"\n sp_Min_egg_W_pop = ", sp_Min_egg_W_pop
  ,"\n sp_Max_egg_W_pop = ", sp_Max_egg_W_pop
  ,"\n sp_WtoWegg_a = ", sp_WtoWegg_a
  ,"\n sp_WtoWegg_c = ", sp_WtoWegg_c
  ,"\n sp_CVEmergenceWeight = ", sp_CVEmergenceWeight  
  #, sep=" ")
  ,"\n"));

cat("\n\n\t//EMERGENCE")
cat(paste0(
  "\n //species parameters temperature effect eggs survival"
  ,"\n sp_aRt = ", sp_aRt
  ,"\n sp_bRt = ", sp_bRt
  ,"\n sp_cRt = ", sp_cRt
  ,"\n sp_dRt = ", sp_dRt
  ,"\n"
  ,"\n //species parameters flow effect eggs survival"
  ,"\n sp_coeffCriticalFlowSurvival_Inf = ", sp_coeffCriticalFlowSurvival_Inf
  ,"\n sp_coeffCriticalFlowSurvival_Sup = ", sp_coeffCriticalFlowSurvival_Sup
  ,"\n sp_betaNoiseReddSurvival = ", sp_betaNoiseReddSurvival
  ,"\n sp_betaReddFlowSurvival = ", sp_betaReddFlowSurvival
  ,"\n"
  ,"\n //species parameter for emergence: accumulated temperature units (ATUs), i.e. degree days"
  #,"\n sp_emergenceTemperatureThreshold = ", sp_emergenceTemperatureThreshold
  ,"\n sp_DDemergeMin = ", sp_DDemergeMin
  ,"\n sp_DDemergeMax = ", sp_DDemergeMax
  ,"\n"
  ,"\n //species parameters density effect eggs survival
    // TO CHECK: adjusted on Scorff data (al+mb 07-2020)
    // number of fry / m2"
  ,"\n sp_densityFryMax = ", sp_densityFryMax
  ,"\n sp_alphaBH = ", sp_alphaBH
  #, sep=" ")
  ,"\n"));

cat("\n\t//GROWTH")
cat(paste0(
  "\n //species parameters temperature effect river growth"
  ,"\n sp_dr = ", sp_dr
  ,"\n sp_Tlr =", sp_Tlr
  ,"\n sp_Tur =", sp_Tur
  ,"\n sp_gr =", sp_gr
  ,"\n sp_b =", sp_b
  ,"\n"
  ,"\n //species parameters activity effect river growth"
  ,"\n sp_activityState_summer =", sp_activityState_summer
  ,"\n sp_activityState_smolt1 =", sp_activityState_smolt1
  ,"\n sp_activityState_smoltN =", sp_activityState_smoltN
  ,"\n sp_activityState_winter =", sp_activityState_winter
  ,"\n"
  ,"\n //species parameter density effect river growth"
  ,"\n sp_betaDens =", sp_betaDens
  ,"\n sp_exponentEffectiveDensity =", sp_exponentEffectiveDensity
  ,"\n"
  ,"\n //species parameter flow effect river growth"
  ,"\n sp_coeffCriticalFlowGrowth =", sp_coeffCriticalFlowGrowth
  ,"\n"
  ,"\n //species parameters allocation process growth"
  ,"\n sp_pPercFm =", sp_pPercFm
  ,"\n sp_pPercFsd =", sp_pPercFsd
  ,"\n sp_matPercF_females =", sp_matPercF_females
  ,"\n sp_matPercF_males =", sp_matPercF_males
  ,"\n"
  ,"\n sp_lwa_parr =", sp_lwa_parr
  ,"\n sp_lwa_smolt =", sp_lwa_smolt
  ,"\n sp_lwa_anadromous =", sp_lwa_anadromous
  ,"\n sp_lwb_parr =", sp_lwb_parr
  ,"\n sp_lwb_smolt =", sp_lwb_smolt
  ,"\n sp_lwb_anadromous =", sp_lwb_anadromous
  ,"\n"
  ,"\n //species parameters sea growth"
  ,"\n sp_Kg =", sp_Kg
  ,"\n sp_Wmax =", sp_Wmax
  ,"\n"
  ,"\n //species parameters for growth-survival trade-off"
  ,"\n //In river"
  ,"\n sp_maxRIV =", sp_maxRIV
  ,"\n sp_kappaRIV =", sp_kappaRIV
  ,"\n sp_sigRIV =", sp_sigRIV
  ,"\n //In sea"
  ,"\n sp_maxSEA =", sp_maxSEA
  ,"\n sp_kappaSEA =", sp_kappaSEA
  ,"\n sp_sigSEA =", sp_sigSEA
  
  #, sep=" ")
  ,"\n"));




cat("\n\t//SURVIVAL")
cat(paste0(
  "\n //Parameters for daily survival in river (state dependent)"
  ,"\n sp_Sp0 = ", sp_Sp0
  ,"\n sp_Sp1M =", sp_Sp1M
  ,"\n sp_Sp1S =", sp_Sp1S
  ,"\n sp_Sp1 =", sp_Sp1
  ,"\n sp_Spn = ", sp_Spn
  ,"\n sp_SpnM =", sp_SpnM
  ,"\n sp_SpOSW =", sp_SpOSW  
  ,"\n sp_SpMSW =", sp_SpMSW
  ,"\n"
  ,"\n //Parameters for daily survival in sea (size dependent)"
  ,"\n sp_RickA =", sp_RickA
  ,"\n sp_RickB =", sp_RickB
  #, sep=" ")
  ,"\n"));

cat("\n\t//MATURATION")
cat(paste0(
  "\n //species nDays window and projection for maturation process"
  ,"\n //In river (precocious maturation)"
  ,"\n sp_nDaysWindowRiverMaturation = ", sp_nDaysWindowRiverMaturation
  ,"\n sp_nDaysProjectionRiverMaturation =", sp_nDaysProjectionRiverMaturation
  ,"\n //In sea"
  ,"\n sp_nDaysWindowSeaMaturation =", sp_nDaysWindowSeaMaturation
  ,"\n sp_nDaysProjectionSeaMaturation =", sp_nDaysProjectionSeaMaturation
  #, sep=" ")
  ,"\n"));


cat("\n\t//MIGRATION")
cat(paste0(
  #"\n //species smoltification reaction norm population parameters"
  #,"\n sp_alphaS = ", sp_alphaS
  #,"\n sp_Lmid =", sp_Lmid
  "\n //species Weibull parameters for seaward migration time"
  ,"\n sp_rateDateSeawardMigration_smolt0 = ", sp_rateDateSeawardMigration_smolt0
  ,"\n sp_rateDateSeawardMigration_smolt1 = ", sp_rateDateSeawardMigration_smolt1
  ,"\n sp_rateDateSeawardMigration_smolt2 = ", sp_rateDateSeawardMigration_smolt2
  ,"\n sp_shapeDateSeawardMigration_smolt0 = ", sp_shapeDateSeawardMigration_smolt0
  ,"\n sp_shapeDateSeawardMigration_smolt1 = ", sp_shapeDateSeawardMigration_smolt1
  ,"\n sp_shapeDateSeawardMigration_smolt2 = ", sp_shapeDateSeawardMigration_smolt2
  ,"\n"
  ,"\n //species Weibull parameters for riverward migration time"
  #,"\n sp_riverWardMigrationTime_1seaWinter =", sp_riverWardMigrationTime_1seaWinter
  #,"\n sp_riverWardMigrationTime_multipleSeaWinters =", sp_riverWardMigrationTime_multipleSeaWinters
  ,"\n sp_rateDateRiverwardMigration_OSW = ", sp_rateDateRiverwardMigration_OSW
  ,"\n sp_rateDateRiverwardMigration_MSW = ", sp_rateDateRiverwardMigration_MSW
  ,"\n sp_shapeDateRiverwardMigration_OSW = ", sp_shapeDateRiverwardMigration_OSW
  ,"\n sp_shapeDateRiverwardMigration_MSW = ", sp_shapeDateRiverwardMigration_MSW
  ,"\n"
  ,"\n //species parameters for dispersal kernel"
  ,"\n sp_muKernel = ", sp_muKernel
  ,"\n sp_betaKernel = ", sp_betaKernel
  ,"\n"));


sink()

file.copy(paste0("config_ABC",row,".txt"),paste0(capsis_workingdir,"/config_ABC",row,".txt"),overwrite = TRUE)


#############---------------------------------------------------------------------------#################

#####################################################
##################### INVENTORY FILE ################
#####################################################

##STEP1 = LOADING parameters

####--------SPECIES parameters---------####
speciesCode = 1
speciesName = "salmo_salar"

####-------- SPATIAL parameters---------####
# Simulate a proportion of the total habitat area observed
prop = 0.25
source("SpatialParameters.R")


####--------GENETIC parameters---------####
source("GeneticParameters.R")
oneTraitMaturation = TRUE
oneTraitSmoltification = TRUE
oneTraitGrowthPotential = TRUE

##STEP2 = Generating individuals/ tirage des individus

source("CreateFish_ABC.R")

##STEP3 = PRINT inventory file

sink(paste0("inventory_ABC",row,".txt"))

cat("// Capsis 4.1 - module IBASAM - spatial and genetic parameters + virtual inventory")
cat("\n\n//<General>");
cat("\n //Contributors: mb+al+fc - 2023");

cat("\n\n//--------SPATIAL parameters---------//")

# cat("\n 
# // RIVERS
# // each river contains a collection of reaches and weirs
# // int	char	Vertex3D
# // ID	name	coordinates
# 1\tScorff\t(6,7,0)
# 2\tBlavet\t(5,4,0)
# ");

rivers <- paste0(ID_river[1],"\t",name[1],"\t",PropRiversArea[1],"\t",coord_river[1],"\t",distance[1])
if(n_rivers > 1) {
  for(i in 2:n_rivers) {
    river <- paste0(ID_river[i],"\t",name[i],"\t",PropRiversArea[i],"\t",coord_river[i],"\t",distance[i])
    rivers <- paste0(rivers, "\n", river)
  }
}

cat(paste0("\n 
// RIVERS
// each river contains a collection of reaches and weirs
// int	char  int	Vertex3D  dist1;dist2...
// ID	name  habitatArea(m2)	coordinates distanceToTheOtherRivers
",rivers,"
"));

# cat(paste0("\n 
# //\tREACHES
# //data\ttypes
# //int	string	int\tint	int	int	int	Vertex3D
# //ID	fatherID	riverID\torder	habitatArea(m2)\tmodule(m3/s)	length(m)	meanWidth(m) fishingRate_1SW fishingRate_MSW	coordinates
# ",ID[1],"\t",fatherID[1],"\t",riverID[1],"\t",order[1],"\t",PropReachesArea[1],"\t",module[1],"\t",length[1],"\t",width[1],"\t",fR_1SW[1],"\t",fR_MSW[1],"\t",coord[1],"
# ",ID[2],"\t",fatherID[2],"\t",riverID[2],"\t",order[2],"\t",PropReachesArea[2],"\t",module[2],"\t",length[2],"\t",width[2],"\t",fR_1SW[2],"\t",fR_MSW[2],"\t",coord[2],"
# ",ID[3],"\t",fatherID[3],"\t",riverID[3],"\t",order[3],"\t",PropReachesArea[3],"\t",module[3],"\t",length[3],"\t",width[3],"\t",fR_1SW[3],"\t",fR_MSW[3],"\t",coord[3],"
# ",ID[4],"\t",fatherID[4],"\t",riverID[4],"\t",order[4],"\t",PropReachesArea[4],"\t",module[4],"\t",length[4],"\t",width[4],"\t",fR_1SW[4],"\t",fR_MSW[4],"\t",coord[4],"
# ",ID[5],"\t",fatherID[5],"\t",riverID[5],"\t",order[5],"\t",PropReachesArea[5],"\t",module[5],"\t",length[5],"\t",width[5],"\t",fR_1SW[5],"\t",fR_MSW[5],"\t",coord[5],"
# ",ID[6],"\t",fatherID[6],"\t",riverID[6],"\t",order[6],"\t",PropReachesArea[6],"\t",module[6],"\t",length[6],"\t",width[6],"\t",fR_1SW[6],"\t",fR_MSW[6],"\t",coord[6],"
# "));

reaches <- paste0(ID[1],"\t",fatherID[1],"\t",riverID[1],"\t",order[1],"\t",PropReachesArea[1],"\t",module[1],"\t",length[1],"\t",width[1],"\t",coord[1])
for(i in 2:(sum(n_reaches))) {
  reach <- paste0(ID[i],"\t",fatherID[i],"\t",riverID[i],"\t",order[i],"\t",PropReachesArea[i],"\t",module[i],"\t",length[i],"\t",width[i],"\t",coord[i])
  reaches <- paste0(reaches, "\n", reach)
}
cat(paste0("\n 
//\tREACHES
//data\ttypes
//int	string	int\tint	int	int	int	Vertex3D
//ID	fatherID	riverID\torder	habitatArea(m2)\tmodule(m3/s)	length(m)	meanWidth(m)	coordinates
",reaches,"
"));


# cat("\n 
# //WEIRS
# //Here are the existing weirs and the virtual weirs
# //int	int	riverID	boolean	bytes	bytes	Vertex3D
# //ID	father	int 	fishPass	upstreamRate	downstreamRate	coordinates
# 1	0	1	0	0	(6,7,0)
# 2	1	1	0	0	(8,7,0)
# 
# 1	0	2	0	0	(5,4,0)
# 2	1	2	0	0	(6,4,0)
# //3	2	2	0	0	(9,4,0)
# ");


weirs <- paste0(ID_weir[1],"\t",fatherID_weir[1],"\t",riverID_weir[1],"\t",upstreamRate[1],"\t",downstreamRate[1],"\t",coord_weir[1])
if(n_weirs > 1) {
  for(i in 2:n_weirs) {
    weir <- paste0(ID_weir[i],"\t",fatherID_weir[i],"\t",riverID_weir[i],"\t",upstreamRate[i],"\t",downstreamRate[i],"\t",coord_weir[i])
    weirs <- paste0(weirs, "\n", weir)
  }
}

cat(paste0("\n 
//WEIRS
//Here are the existing weirs and the virtual weirs
//int	int	int	bytes	bytes	Vertex3D
//ID	father	riverID	upstreamRate	downstreamRate	coordinates
",weirs,"
"));


# cat("\n
# //SEA REGIONS
# // The sea region with id 1 is the first one, where the fish migrate first from the rivers
# // The id is the rank of the seas: fish move from 1 to 2, from 2 to 3, etc.
# //int	char	Vertex3D
# //ID	Name	coordinates
# 1	Feroe	{(2,4,0);(3,4,0);(3,5,0);(4,5,0);(4,6,0);(3,6,0);(2,6,0);(2,5,0);(2,4,0)}
# 2	Greenland	{(1,8,0);(2,8,0);(3,8,0);(3,9,0);(3,10,0);(2,10,0);(1,10,0);(1,9,0);(1,8,0)}
# ");

searegions <- paste0(ID_searegion[1],"\t",name_searegion[1],"\t",coord_searegion[1])
if(n_searegions > 1){
  for(i in 2:n_searegions) {
    searegion <- paste0(ID_searegion[i],"\t",name_searegion[i],"\t",coord_searegion[i])
    searegions <- paste0(searegions, "\n", searegion)
  }
}

cat(paste0("\n 
//SEA REGIONS
// The sea region with id 1 is the first one, where the fish migrate first from the rivers
// The id is the rank of the seas: fish move from 1 to 2, from 2 to 3, etc.
//int	char	Vertex3D
//ID	Name	coordinates
",searegions,"
"));

cat("\n\n//--------GENETIC parameters---------//")

cat(paste0(
  "\n//numberOfGeneticTraits =", numberOfGeneticTraits
))

result="\n//nbLocus_byTrait={"
for (i in 1:(numberOfGeneticTraits-1)) {
  result=paste(result,nbLocus[i], ";",sep="")
}
result=paste(result,nbLocus[numberOfGeneticTraits],"}",sep="")
cat(result)


result="\n//nbAllelePerLocus={"
for (i in 1:(numberOfGeneticTraits-1)) {
  result=paste(result,nbAllelePerLoc[i], ";",sep="")
}
result=paste(result,nbAllelePerLoc[numberOfGeneticTraits],"}",sep="")
cat(result)

cat(paste0(
  "\n//nbLocusMicrosat =", nbMsat
  ,"\n//nbLocusNeutralSNP =", nbSNP
))

result="\n//distanceClassBoundsForSGS={"
for (i in 1:(nbDistanceClassBounds-1)) {
  result=paste(result,distanceClassBounds[i], ";",sep="")
}
result=paste(result,distanceClassBounds[nbDistanceClassBounds],"}",sep="")
cat(result)



#Genetic Map / carte genetique
cat("\n\n//Genetic Map 
    \n//value speciesName	geneticMap	allelesNuclear	allelesMCytoplasmic	allelesPCytoplasmic");
cat(paste0("\n",speciesCode,"\t",speciesName,"\t{}\t{"))
#allelesNuclear
stringGenetMap=character(0)
counter=0
for (trait in 1:numberOfGeneticTraits){
  for (i in 1:nbLocus[trait])
  {
    counter = counter +1
    if (counter < sum(nbLocus)){
      stringGenetMap=paste0(stringGenetMap,"[",1,",",2,"];")
    } else {
      stringGenetMap=paste0(stringGenetMap,"[",1,",",2,"]")
    }
  }
}
cat(stringGenetMap)


# Neutral MicroSat
if(nbMsat>0){
  for (i in 1:nbMsat)
  {
    for (j in 1:(nbAllPerMsat-1))
    {
      stringGenetMap=paste0(stringGenetMap,"[",1,",",2,"];")
    }
  }
  cat(stringGenetMap)
}

# Neutral SNPs
if(nbSNP>0){
  for (i in 1:(nbSNP-1))
  {
    for (j in 1:(nbAllPerSNP-1))
    {
      stringGenetMap=paste0(stringGenetMap,"[",1,",",2,"];")
    }
    stringGenetMap=paste0(stringGenetMap,"[1,2]")
  }
  cat(stringGenetMap)
}
# last locus for neutral SNP / allelesMCytoplasmic / allelesPCytoplasmic
cat("}\t{}\t{}")

#RECORDs/ Enregistrements        
cat("\n\n//<Records>")

# Dominance effects / Dominance alleliques
cat("\n\n// Dominance effects for each trait : [locus number, dominance effect allele 1, dominance effect allele 2]")
cat("\n// if scaledDominanceDeviation = 0 -> no dominance
// if scaledDominanceDeviation = 1 -> complete dominance
// if scaledDominanceDeviation > 1 -> overdominance
// if scaledDominanceDeviation < 1 -> incomplete dominance
// /!\ DominanceEffect = effectCoefficient * nuclear dominance effect")
cat("\n//speciesCode	parameter	effectCoefficient nuclear dominance effect	mcyto dominance effect	pcyto dominance effect","\n")

counter=0
for (trait in 1:(numberOfGeneticTraits)) {
  
  stringLocusEffect=character((0))
  stringLocusEffect = paste0(
    speciesCode
    ,"\t",GeneticTraits$ParametersNames[trait][1]
    ,"\t",scaledDominanceDeviation[trait,1]
    ,"\t{"
  )
  
  for (i in 1:nbLocus[trait]) {
    
    counter <- counter + 1 # get the total number of locus
    
    all1 <- 0 # Recessif
    all2 <- scaledDominanceDeviation[trait,i+1] # Dominant
    
    if(oneTraitMaturation == TRUE & trait %in% c(1:4)) { #al-12.09.23
      stringLocusEffect=paste(stringLocusEffect,"[",(counter - nbLocus[trait]*(trait-1)),",",all1,",",all2,"]",sep="")
    } else {
      if(oneTraitSmoltification == TRUE & trait %in% c(5:6)) { #al-12.09.23
        stringLocusEffect=paste(stringLocusEffect,"[",(counter - nbLocus[trait]*(trait-2)),",",all1,",",all2,"]",sep="")
      } else {
        if(oneTraitGrowthPotential == TRUE & trait %in% c(7:8)) { #al-14.11.23
          stringLocusEffect=paste(stringLocusEffect,"[",(counter - nbLocus[trait]*(trait-3)),",",all1,",",all2,"]",sep="")
        } else {
          stringLocusEffect=paste(stringLocusEffect,"[",(counter),",",all1,",",all2,"]",sep="")
        }
      }
    }
    if (i<nbLocus[trait]) stringLocusEffect=paste(stringLocusEffect,";",sep="")
  }
  stringLocusEffect=paste0(stringLocusEffect,"}\t{}\t{}")
  cat(stringLocusEffect,"\n")
}



# allele effects / effets alleliques
cat("\n\n// Allele effects for each trait : [locus number, allele effect allele 1, allele effect allele 2]")
cat("\n//speciesCode	parameter	effectCoefficient nuclear allele effect	mcyto allele effect	pcyto allele effect	heritability	environmentalVariance	interEnvironmentalVariance","\n")

counter=0
for (trait in 1:(numberOfGeneticTraits)) {
  
  stringLocusEffect=character((0))
  stringLocusEffect = paste0(
    speciesCode
    ,"\t",GeneticTraits$ParametersNames[trait][1]
    ,"\t",1/GeneticTraits$alleleEffectMultiplCoeff[trait]
    ,"\t{"
  )
  
  for (i in 1:nbLocus[trait]) {
    
    counter <- counter + 1 # get the total number of locus
    
    if(heritability[trait]>0) {
      all1=((target_mean[trait]*effect_Locus[trait,i])-effect_Allele[trait,i])/2
      all1=round(all1*alleleEffectMultiplCoeff[trait])
      all2=((target_mean[trait]*effect_Locus[trait,i])+effect_Allele[trait,i])/2
      all2=round(all2*alleleEffectMultiplCoeff[trait])
    } else {
      all1=0
      all2=0
    }
    
    if(oneTraitMaturation == TRUE & trait %in% c(1:4)) {
      stringLocusEffect=paste(stringLocusEffect,"[",(counter - nbLocus[trait]*(trait-1)),",",all1,",",all2,"]",sep="")
    } else {
      if(oneTraitSmoltification == TRUE & trait %in% c(5:6)) {
        stringLocusEffect=paste(stringLocusEffect,"[",(counter - nbLocus[trait]*(trait-2)),",",all1,",",all2,"]",sep="")
      } else {
        if(oneTraitGrowthPotential == TRUE & trait %in% c(7:8)) { #al-14.11.23
          stringLocusEffect=paste(stringLocusEffect,"[",(counter - nbLocus[trait]*(trait-3)),",",all1,",",all2,"]",sep="")
        } else {
          stringLocusEffect=paste(stringLocusEffect,"[",(counter),",",all1,",",all2,"]",sep="")
        }
      }
    }
    if (i<nbLocus[trait]) stringLocusEffect=paste(stringLocusEffect,";",sep="")
  }
  stringLocusEffect=paste(stringLocusEffect,"}\t{}\t{}\t"
                          ,heritability[trait]
                          ,"\t",sep="")
  if (totEnvirVar[trait]==0) {
    stringLocusEffect=paste0(stringLocusEffect,"0\t0")
  } else {
    stringLocusEffect=paste0(stringLocusEffect,
                             totEnvirVar[trait]#round(totEnvirVar[trait]*alleleEffectMultiplCoeff[trait])
                             ,"\t0") #interEnv=1 #al-mb-14.09.23
  }
  cat (stringLocusEffect,"\n")
}



cat("\n\n//--------INDIVIDUALS---------// \n")
cat("\n//id	female	age_days	nuclear	mCytoplasmic	pCytoplasmic	mId	pId	creationDate	riverId	reachId	birthriverId	birthreachId	atSea	seaRegionId	weight	forkLength	fat	state	lastNumberOfSeaWinters	durationInRiverAsParr\n")

data <- read.csv(file=paste0("initIndividuals_ABC",row,".inv"))#,sep="\t",quote="\\'")
data[] <- lapply(data, function(x) gsub("\"", "", x))
for (i in 1:nrow(data)){
  cat(data[i,], sep="\t")#,row.names = FALSE)
  cat("\n")
}
cat("\n")

sink()

file.copy(paste0("inventory_ABC",row,".txt"),paste0(capsis_workingdir,"/inventory_ABC",row,".txt"),overwrite = TRUE)
