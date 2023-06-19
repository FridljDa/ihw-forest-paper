#!/bin/bash
for i in `seq 1 2`;
do
    bsub -M 10000 -n 8 -R "span[hosts=1]" -u daniel.fridljand@gmail.com  slurm/hello_world.R $i
done