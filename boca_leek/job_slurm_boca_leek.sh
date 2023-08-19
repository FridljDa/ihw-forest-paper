#!/bin/bash

# Example of running R script with a job array

#SBATCH --job-name=bocaleek.sim.job
#SBATCH -A huber                # group to which you belong
#SBATCH -N 1                        # number of nodes
#SBATCH -n 3                        # number of cores
#SBATCH --mem 10G                    # memory pool for all cores
#SBATCH -t 3-2:00                   # runtime limit (D-HH:MM:SS)
#SBATCH -o boca_leek/out/boca_leek_out-%j.out
#SBATCH -e boca_leek/error_out/boca_leek_er-%j.err          # STDERR
#SBATCH --mail-type=END,FAIL        # notifications for job done & fail
#SBATCH --mail-user=daniel.fridljand@embl.de # send-to address
# Load software
module load R

# Retrieve the command-line argument
split_index=$1
num_splits=$2


# Run R script
Rscript boca_leek/20230725_boca_leek_analysis_script.R $split_index $num_splits