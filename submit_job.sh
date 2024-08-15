#!/bin/bash

#SBATCH --job-name=calibration-ABC                    # Job name
#SBATCH --account=project_2008731
#SBATCH --begin=09:25:00                          # beginning time
#SBATCH --array=1-10                          # Array range
##SBATCH --array=729,970                       # Explicit array items
#SBATCH --time=48:00:00                       # Time limit hrs:min:sec
##SBATCH --output=output_%j_%a.txt                    # Standard output and error log (%A for job ID, %a for array index)
##SBATCH --error=errors_%j_%a.txt
#SBATCH --partition=small
#SBATCH --cpus-per-task=1                     # Number of CPU cores per task
#SBATCH --mem-per-cpu=4G                      # Memory per processor
#SBATCH --mail-type=BEGIN #uncomment to enable mail

# Load r-env
module load r-env

# Set memory using setmem.sh script (in the Capsis4 folder)
cd /scratch/project_2008731/alamarins/trunk
sh setmem.sh 2048 

# Run the R script with a different argument for each job array task (in the working directory)
Rscript /scratch/project_2008731/alamarins/CALIBRATION-ABC/ABC_rowloop.R $SLURM_ARRAY_TASK_ID
