#!/bin/bash

# Number of times to submit the job
num_jobs=2

# Loop to submit the job multiple times
for ((i=1; i<=$num_jobs; i++))
do
    sbatch simulation/job_slurm_simulation.sh $i
done