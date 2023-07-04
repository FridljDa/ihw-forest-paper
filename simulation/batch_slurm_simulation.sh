#!/bin/bash

#SBATCH --job-name=high.dim.sim.batch
#SBATCH -A huber                # group to which you belong
#SBATCH -t 0-0:01                   # runtime limit (D-HH:MM:SS)
#SBATCH -o grid_search/out/grid_search_out_batch-%j.out
#SBATCH -e grid_search/error_out/grid_search_er_batch-%j.err          # STDERR
#SBATCH --mail-type=END,FAIL        # notifications for job done & fail
#SBATCH --mail-user=daniel.fridljand@embl.de # send-to address

# Number of times to submit the job
num_jobs=2

# Loop to submit the job multiple times
for ((i=1; i<=$num_jobs; i++))
do
    sbatch simulation/job_slurm_simulation.sh $i
done