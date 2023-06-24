#!/bin/bash

# Example of running R script with a job array

#SBATCH -A huber                # group to which you belong
#SBATCH -N 1                        # number of nodes
#SBATCH -n 3                        # number of cores
#SBATCH --mem 2G                    # memory pool for all cores
#SBATCH -t 2-2:00                   # runtime limit (D-HH:MM:SS)
#SBATCH -o simulation/out/high_dim_sim_out-%j.out
#SBATCH -e simulation/error_out/high_dim_sim_er-%j.err          # STDERR
#SBATCH --mail-type=END,FAIL        # notifications for job done & fail
#SBATCH --mail-user=daniel.fridljand@embl.de # send-to address
# Load software
module load R

# Run R script
Rscript simulation/20230619_run_high_dim_sim.R 