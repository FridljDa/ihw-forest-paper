#!/bin/bash

#SBATCH -A huber                # group to which you belong
#SBATCH -N 1                        # number of nodes
#SBATCH -n 1                        # number of cores
#SBATCH --mem 2G                    # memory pool for all cores
#SBATCH -t 0-2:00                   # runtime limit (D-HH:MM:SS)
#SBATCH -o hello-%j.out
#SBATCH -e hello_er-%j.err          # STDERR
#SBATCH --mail-type=END,FAIL        # notifications for job done & fail
#SBATCH --mail-user=daniel.fridljand@embl.de # send-to address

module purge
module load R
module load pandoc 

Rscript --vanilla TestRmd.R