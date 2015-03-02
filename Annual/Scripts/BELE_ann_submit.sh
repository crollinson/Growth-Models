#!/bin/sh
#$ -wd ~/CARCA/Growth-Models/Annual/
#$ -j y
#$ -S /bin/bash
#$ -V
#$ -m e
#$ -M crollinson@gmail.com
#$ -l h_rt=24:00:00
#$ -N BELE_Ann
#cd ~/CARCA/Growth-Models/Annual/
R CMD BATCH BELE_Full_AnnClim.R