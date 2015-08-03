####################################################################
# Appendix A figures: Species Site & Size Distributions
# Modified from original scripts: 
#  -- GrowthModels/Original/SizeGraphing.R; 
#   -- Establishment\ Modeling/1_Establishment0_ReleaseDetection.R
####################################################################
rm(list=ls())
setwd("..")

# -------------------------
# Load Libraries & some custom ggplot themes
# -------------------------
library(ggplot2)
library(grid)
library(car)

# dir.ms <- "~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Growth Manuscript/Ecology Final"
dir.ms <- "~/Dropbox/CARCA/Growth Manuscript/Ecology Final"

species.colors <- c("purple", "blue", "green3", "orange", "red")


theme.ecology <- 	theme(axis.line=element_line(color="black", size=0.5), 
                		  panel.grid.major=element_blank(), 
                		  panel.grid.minor= element_blank(), 
                		  panel.border= element_blank(), 
                		  panel.background= element_blank()) +
                    theme(axis.text.x=element_text(color="black", size=rel(1)),
                          axis.text.y=element_text(color="black", size=rel(1)), 
                          axis.title.x=element_text(size=rel(1)),  
                          axis.title.y=element_text(size=rel(1))) +
                    theme(legend.text=element_text(size=rel(1)),
                    	  legend.key=element_blank())

# -------------------------



# -------------------------------------------------------------
# Size Distributions
# -------------------------------------------------------------
# Loading in the raw tree ring data
model.data <- read.csv("Inputs/TargetSpecies_AllSites_Ann_1990-2011.csv")
summary(model.data)

trees <- read.csv("../RawInputs/TreeData.csv", na.strings=c("", "*"))
summary(trees)

trees.model <- trees[trees$TreeID %in% unique(model.data$TreeID),]
summary(trees.model)
# -------------------------------------------------------------

# -------------------------------------------------------------
# 
# -------------------------------------------------------------
