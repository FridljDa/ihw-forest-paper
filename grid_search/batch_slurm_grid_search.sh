#!/bin/bash

#SBATCH -A huber                # group to which you belong
#SBATCH -t 0-0:01                   # runtime limit (D-HH:MM:SS)
#SBATCH -o grid_search/out/grid_search_out_batch-%j.out
#SBATCH -e grid_search/error_out/grid_search_er_batch-%j.err          # STDERR
#SBATCH --mail-type=END,FAIL        # notifications for job done & fail
#SBATCH --mail-user=daniel.fridljand@embl.de # send-to address

# Number of times to submit the job
num_jobs=20 #monte carlo replicates *5
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

