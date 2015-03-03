#!/bin/sh
#$ -wd /usr2/postdoc/crolli/CARCA/Growth-Models/Annual/Scripts/
#$ -j y
#$ -S /bin/bash
#$ -V
#$ -m e
#$ -M crollinson@gmail.com
#$ -l h_rt=24:00:00
#$ -N ACRU_Ann
#cd /usr2/postdoc/crolli/CARCA/Growth-Models/Annual/Scripts/
R CMD BATCH ACRU_Full_AnnClim.R ACRU_Ann.log