####--------SIMULATION parameters---------####
# Initial date, e.g. 91 = 1 April (if not a leap year)
initialYear = 1990 #1995
initialDoy = 267#91 # 1st day of autumn / juveniles are created at day 266

#SUMMER_START_DOY = 91; // April 1st
#SUMMER_END_DOY = 274; // September 30th


####--------LIFE HISTORY parameters---------####
# fixed time windows for process
precociousMaturationWindow = c(91,150)
seawardMigrationDecisionWindow = c(274,274)
seawardEffectiveMigrationWindow = c(75,147) #al-11.09.23 #c(80,140) # mb - 16.05.2023
# Maturation window at sea (47 days starting Oct 1st):
riverwardMigrationDecisionWindow = c(274,321) # to check see Trehin et aL. 2020
reproductionWindow = c(320,5)#c(322,5) #c(362,362)
fishingWindow = c(1,319) #c(90,274)


## REPRODUCTION
# Species parameters for Date of reproduction
sp_rateDateRepro = 53.10522 #(doy) #3.8275 (ndays) #3.598 (Nivelle Mat)
# on average 38 days after November 1st (Ibasam v1)
sp_shapeDateRepro = 346.9242 #(doy) #26.06669 (ndays) #38.24 (Nivelle Mat)


# Species parameters for number of male partners
sp_spawning_nAnadromousMaleMean = 2 #3
sp_spawning_nprecociousMaleMean = 5 #10

# Species parameters for reproduction
# aFert: parameter adjusting competitive advantage due to weight among males
sp_aFert = 0.5
# fat loss after reproduction (female: loose only their eggs; anad males: 59% of FmID; preco males: average reserve for 1+ in november)
sp_Loss_Fat_Female = matrix_param[row,"LossFatFem"] #1.5 #1
sp_Loss_Fat_AnadromousMale = 200 #matrix_param[row,"LossFatMal"] #200 #73
sp_Loss_Fat_PrecociousMale = matrix_param[row,"LossFatPreco"] #4 #3
# mortality after reproduction
sp_ReproMortalityFemale = 0.99
sp_ReproMortalityMale = 0.999


#species parameters number of eggs produced by female
sp_WtoNegg_a = 0.86
sp_WtoNegg_b = 1.63
#species parameters mean egg weight produced by female
sp_Min_egg_W_pop = 0.06
sp_Max_egg_W_pop = 0.2
sp_WtoWegg_a = 0.168
sp_WtoWegg_c = 5.68
sp_CVEmergenceWeight = 0.034 # #3.4% of variance around mean W of eggs for one female (Fleming Jonsson Gross Lamber 1996)


## EMERGENCE
#species parameters temperature effect eggs survival
sp_aRt = 0.00019346
sp_bRt = 5161.93
sp_cRt = 0.608211
sp_dRt = 19.055

#species parameters flow effect eggs survival
sp_coeffCriticalFlowSurvival_Inf = 0.1
sp_coeffCriticalFlowSurvival_Sup = 7
sp_betaNoiseReddSurvival = 0.01
sp_betaReddFlowSurvival = 100

# species parameter for emergence: accumulated temperature units (ATUs), i.e. degree days
#sp_emergenceTemperatureThreshold = 800 # DDemerge_min = 880 / DDemerge_min = 920
sp_DDemergeMin = 880 #(al+mb 04-2021)
sp_DDemergeMax = 920

#species parameters density effect eggs survival
# TO CHECK: adjusted on Scorff data (al+mb 07-2020)
# number of fry / m2
sp_densityFryMax = 1.7 #1 #3.21
sp_alphaBH = 0.9 #1



## GROWTH
#species parameters temperature effect river growth
sp_dr = 0.5 #(al+mb 04-2021)
sp_Tlr = 6.0
sp_Tur = 24.5
sp_gr = 0.208
sp_b = 0.31

#species parameters activity effect river growth
sp_activityState_summer = matrix_param[row,"activitySummer"] #0.95 #1
sp_activityState_smolt1 = matrix_param[row,"activitySmolt1"] #0.65 #0.725
sp_activityState_smoltN = matrix_param[row,"activitySmoltN"] #0.7 #0.188
sp_activityState_winter = matrix_param[row,"activityWinter"] #0.1 #0.2 #0.1

#species parameter density effect river growth
sp_betaDens = 142.7
sp_exponentEffectiveDensity = 2

#species parameter flow effect river growth
sp_coeffCriticalFlowGrowth = 0.2

#species parameters allocation process growth
sp_pPercFm = 0.12
sp_pPercFsd = 0.01
sp_matPercF_females = 0.18
sp_matPercF_males = 0.04
sp_lwa_parr = 3.804
sp_lwa_anadromous = 3.82568
sp_lwb_parr = 0.32
sp_lwb_anadromous = 0.333779

#species parameters sea growth
sp_Kg = 3.057074E-3
sp_Wmax = 8500

#species parameters for growth-survival trade-off
#In river
sp_maxRIV = 5 # al+mb 04-2021
sp_kappaRIV = 0.001 # al+mb 04-2021
sp_sigRIV = 3.7 # al+mb 04-2021 
#In sea
sp_maxSEA = -1 #al-14.11.23 -so pGsea != pGriver #50
sp_kappaSEA = 0.001 # al+mb 04-2021 
sp_sigSEA = 100 # al+mb 04-2021 



## SURVIVAL
#Parameters for daily survival in river (state dependent)
sp_Sp0 = matrix_param[row,"sp0"] #0.98912 # #0.9841606*1.0025
sp_Sp1M = matrix_param[row,"sp1M"] #0.9863295*1.0025
sp_Sp1S = matrix_param[row,"sp1S"] #0.9998 #0.9967923*1.002
sp_Sp1 = matrix_param[row,"sp1"] #0.995 #0.9962 #0.9914398*1.002
sp_Spn = matrix_param[row,"spn"] #0.9992 #0.9998 #0.99775*1.002
sp_SpnM = matrix_param[row,"spnM"] #0.997 #0.9911798*1.002
sp_SpOSW = 0.9999713
sp_SpMSW = 0.9998677

#Parameters for daily survival in sea (size dependent)
sp_RickA = 2.533333
sp_RickB = -0.524


## MATURATION
#species nDays window and projection for maturation process
#In river (precocious maturation)
sp_nDaysWindowRiverMaturation = 61
sp_nDaysProjectionRiverMaturation = 122
#In sea
sp_nDaysWindowSeaMaturation = 47
sp_nDaysProjectionSeaMaturation = 135



## MIGRATION
#species smoltification reaction norm population parameters
#sp_alphaS = 0.15
#sp_Lmid = 89.03333
# Species parameters for Date of seaward migration (by age)
sp_rateDateSeawardMigration_smolt0 = 8.475996 #(doy) #2.79 (ndays) #al-11.09.23 #3.445 # mb - 16.05.2023 # on average 38 days after mid-March 
sp_rateDateSeawardMigration_smolt1 = 8.694488 #(doy) #2.33 (ndays) #al-11.09.23 #3.445 # mb - 16.05.2023 # on average 38 days after mid-March 
sp_rateDateSeawardMigration_smolt2 = 8.694488 #(doy) #2.33 (ndays) #al-11.09.23 #3.445 # mb - 16.05.2023 # on average 38 days after mid-March 
sp_shapeDateSeawardMigration_smolt0 = 114.8266 #(doy) #39.05 (ndays) #al-11.09.23 #35.28 # mb - 16.05.2023
sp_shapeDateSeawardMigration_smolt1 = 105.683 #(doy) #29.66 (ndays) #al-11.09.23 #35.28 # mb - 16.05.2023
sp_shapeDateSeawardMigration_smolt2 = 105.683 #(doy) #29.66 (ndays) #al-11.09.23 #35.28 # mb - 16.05.2023


#species migration time (from sea to river mouth)
#sp_riverWardMigrationTime_1seaWinter = 254
#sp_riverWardMigrationTime_multipleSeaWinters = 134
sp_rateDateRiverwardMigration_OSW = 5.058201
sp_rateDateRiverwardMigration_MSW = 2.731925
sp_shapeDateRiverwardMigration_OSW = 226.0293
sp_shapeDateRiverwardMigration_MSW = 161.4942

#species dispersal kernel parameters
sp_muKernel = 0
sp_betaKernel = 28
