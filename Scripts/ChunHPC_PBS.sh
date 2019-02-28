#!/bin/bash

#PBS -l ncpus=12
#PBS -l walltime=45:00:00
#PBS -l mem=32GB
#PBS -N RainChange
#PBS -P RDS-FAE-HPWC-RW
#PBS -m ae
#PBS -M willem.vervoort@sydney.edu.au
# #PBS -q defaultQ

module load R/3.4.0

R --vanilla </project/RDS-FAE-HPWC-RW/Script/HPC_gridtrendmodel.R> Shell_output.out
