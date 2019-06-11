#!/bin/bash
#
# request Bourne shell as shell for job
#$ -S /bin/bash

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/shared/R_Library/3.5/ld_libs_dir
export LD_LIBRARY_PATH 

#$ -N jobs
#$ -l slot_type=highmem

#$ -cwd
#$ -o log/log$TASK_ID.r
#$ -e log/err$TASK_ID.r

#$ -t 1-500

# $ -m be
# $ -M kwathen@its.jnj.com

cat BuildMe.R |  R --vanilla -q


exit 0;
