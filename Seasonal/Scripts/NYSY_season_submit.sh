#!/bin/sh
#$ -wd /usr2/postdoc/crolli/CARCA/Growth-Models/Seasonal/Scripts/
#$ -j y
#$ -S /bin/bash
#$ -V
#$ -m e
#$ -M crollinson@gmail.com
#$ -l h_rt=72:00:00
#$ -N NYSY_Seas
#cd /usr2/postdoc/crolli/CARCA/Growth-Models/Seasonal/Scripts/
R CMD BATCH NYSY_Full_SeasonClim.R NYSY_Season.log