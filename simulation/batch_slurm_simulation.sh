#!/bin/bash

#SBATCH --job-name=high.dim.sim.batch
#SBATCH -A huber                # group to which you belong
#SBATCH -t 0-0:01                   # runtime limit (D-HH:MM:SS)
#SBATCH -o simulation/out/high_dim_sim_out_batch-%j.out
#SBATCH -e simulation/error_out/high_dim_sim_out_batch-%j.err       # STDERR
#SBATCH --mail-type=All        # notifications for job done & fail
#SBATCH --mail-user=daniel.fridljand@embl.de # send-to address

# Number of times to submit the job
num_jobs=1

# Loop to submit the job multiple times
for ((i=1; i<=$num_jobs; i++))
do
    sbatch simulation/job_slurm_simulation.sh $i
done