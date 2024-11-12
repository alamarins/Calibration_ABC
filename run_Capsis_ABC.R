setwd("/scratch/project_2008731/alamarins/trunk")

#sh setmem.sh 2048 #increase memory allocated to Capsis

# A command file for IbasamScript - fc+al - April 2023

########## MODEL INPUT #############

configFileName=paste0("config_ABC",row,".txt") #"config_Scorff.txt"
inventoryFileName = paste0("inventory_ABC",row,".txt") #"inventory_Scorff_oneTraitMaturation_heri1_matThrMaleParr-0.6.txt" #"inventory_Scorff_oneTraitMaturation_heri1_gG-CV0.15.txt" #"inventory_Scorff_oneTraitMaturation_heri1.txt"
riverEnvironmentFileName = "riverEnvironment_Scorff_control_23reaches.txt"
seaEnvironmentFileName = "seaEnvironment.txt" #"climat_sea.txt"

numberOfYears = 32 #27
memoDoys = "267"  #"75, 319"

exportAllFish = "false"
exportSummary = "true"

exportSeawardMigration = "false"
exportRiverwardMigration = "false"
exportCapture = "false"
exportSimulationBreeders = "false"

exportGenotypes = "false"

folder <- paste0("data/ibasam/CALIBRATION-ABC/command",row,".txt") #"data/ibasam/calibration_Scorff/command.txt"

########## RUN SIMULATIONS #############
#nSIMUL=1
scenario_name=paste0("calibration_ABC_row",row) #"adjusted+matThr-0.6"
sim=1
#for (sim in 1:nSIMUL) {
  
  #1/create command file
  sink(folder)
  
  cat("\n\n#--------COMMAND FILE SCORFF - al - Sept 2023---------#")
  cat(paste(
    "\nsimulationName = ",scenario_name,"_sim_", sim
    ,"\n"
    ,"\nconfigFileName = ", configFileName
    ,"\ninventoryFileName = ", inventoryFileName
    ,"\nriverEnvironmentFileName = ", riverEnvironmentFileName
    ,"\nseaEnvironmentFileName = ", seaEnvironmentFileName
    ,"\n"
    ,"\nnumberOfYears = ", numberOfYears
    ,"\nmemoDoys = ", memoDoys
    ,"\n"
    ,"\nexportAllFish = ", exportAllFish
    ,"\nexportSummary = ", exportSummary
    ,"\nexportGenotypes = ", exportGenotypes
    ,"\nexportSeawardMigration = ", exportSeawardMigration
    ,"\nexportRiverwardMigration = ", exportRiverwardMigration
    ,"\nexportCapture = ", exportCapture
    ,"\nexportSimulationBreeders = ", exportSimulationBreeders
    , sep=""));
  
  sink()
  
  #2/run capsis
  #cmd<-"sh capsis.sh -p script ibasam.script.IbasamScript data/ibasam/IbasamScript_Example/command.txt"
  cmd<-paste0("sh capsis.sh -p script ibasam.script.IbasamScript ", folder)
  system(cmd, wait=TRUE)
#}


