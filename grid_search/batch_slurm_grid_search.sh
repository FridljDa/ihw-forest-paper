#!/bin/bash

# Number of times to submit the job
num_jobs=5
num_splits=5

# Loop to submit the job multiple times
for ((i=1; i<=$num_jobs; i++))
do
    for ((split_index=1; split_index<=$num_splits; split_index++))
    do
        echo "$i $split_index"
        sbatch grid_search/job_slurm_grid_search.sh $i $num_splits $split_index
    done
done

