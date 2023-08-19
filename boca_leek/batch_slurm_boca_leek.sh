#!/bin/bash

#SBATCH --job-name=flexible.sim.batch
#SBATCH -A huber                # group to which you belong
#SBATCH -t 0-0:01                   # runtime limit (D-HH:MM:SS)
#SBATCH -o bocaleek/out/high_dim_sim_out_batch-%j.out
#SBATCH -e bocaleek/error_out/high_dim_sim_out_batch-%j.err       # STDERR
#SBATCH --mail-type=All        # notifications for job done & fail
#SBATCH --mail-user=daniel.fridljand@embl.de # send-to address

# Number of times to submit the job
num_splits=20

# Loop to submit the job multiple times
for ((i=1; i<=num_splits; i++))
do
    sbatch boca_leek/job_slurm_boca_leek.sh $i $num_splits
done