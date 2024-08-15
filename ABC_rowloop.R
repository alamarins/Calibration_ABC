#4. need to run model for the n rows of this matrix X
#need to create config AND inventory files for each row
args <- commandArgs(TRUE)
print(args)
if (length(args)==0){
  stop("No arguments provided.")
}

row = as.numeric(args[1]) + 0 #because Slurm do not accept ID values above 1000
if (is.na(row)) {
  stop("Argument must be a numeric value.")
}

#Define the working directory and the Capsis directory
project_workingdir <- "/scratch/project_2008731/alamarins/CALIBRATION-ABC"
capsis_workingdir <- "/scratch/project_2008731/alamarins/trunk/data/ibasam/CALIBRATION-ABC"
load(paste0(project_workingdir, "/calibration_ABC_matrix_param.RData"))

#Generate the config and inventory files for the parameters combination
setwd(project_workingdir)
source("InitParameters-ABC/Generate_InitFiles_ABC.R")
  
#Run the model
setwd(project_workingdir)
source("run_Capsis_ABC.R")  
  
#Remove config and inventory files
file.remove(paste0(project_workingdir,"/InitParameters-ABC/config_ABC",row,".txt"))
file.remove(paste0(project_workingdir,"/InitParameters-ABC/inventory_ABC",row,".txt"))
file.remove(paste0(project_workingdir,"/InitParameters-ABC/initIndividuals_ABC",row,".inv"))
file.remove(paste0(capsis_workingdir,"/config_ABC",row,".txt"))
file.remove(paste0(capsis_workingdir,"/inventory_ABC",row,".txt"))
file.remove(paste0(capsis_workingdir,"/command",row,".txt"))
  
#}
