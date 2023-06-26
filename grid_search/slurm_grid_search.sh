#!/bin/bash

# Example of running R script with a job array

#SBATCH -A huber                # group to which you belong
#SBATCH -N 1                        # number of nodes
#SBATCH -n 3                        # number of cores
#SBATCH --mem 10G                    # memory pool for all cores
#SBATCH -t 2-2:00                   # runtime limit (D-HH:MM:SS)
#SBATCH -o grid_search/out/grid_search_out-%j.out
#SBATCH -e grid_search/error_out/grid_search_er-%j.err          # STDERR
#SBATCH --mail-type=END,FAIL        # notifications for job done & fail
#SBATCH --mail-user=daniel.fridljand@embl.de # send-to address
# Load software
module load R

# Run R script
Rscript grid_search/20230626_grid_search.R 