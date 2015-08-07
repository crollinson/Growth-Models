####################################################################
# Appendix B figures: Individual Model Parameters
# Modified from original script: ParameterComparisons.R
####################################################################
rm(list=ls())
setwd("..")

# -------------------------
# Load Libraries & some custom ggplot themes
# -------------------------
library(ggplot2)
library(grid)
library(car)

dir.ms <- "~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Growth Manuscript/Ecology Submission 4"
# dir.ms <- "~/Dropbox/CARCA/Growth Manuscript/Ecology Submission 4"

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
# Reading in the data & formatting it
# -------------------------------------------------------------
params.ann <- read.csv("Inputs/SpeciesModelParameters_Annual.csv")
params.ann$Category <- as.factor(substr(params.ann$Parameter, 1, 1))
levels(params.ann$Category) <- c("Size", "Competition", "Max Growth", "Precipitation", "Model Std Dev", "Temperature")
summary(params.ann)

ann.bounds <- aggregate(params.ann[c("Upper.Bound", "Lower.Bound")], by=list(params.ann$Parameter, params.ann$Category), FUN=mean)
names(ann.bounds)[1:2] <- c("Parameter", "Category")
summary(ann.bounds)

ann.bounds2 <- stack(ann.bounds[,3:4])
names(ann.bounds2) <- c("Value", "Bound")
ann.bounds2$Parameter <- ann.bounds$Parameter
ann.bounds2$Category <- ann.bounds$Category
summary(ann.bounds2)


params.seas <- read.csv("Inputs/SpeciesModelParameters_Season.csv")
params.seas$Parameter.ID <- params.seas$Parameter
params.seas$Parameter <- as.factor(substr(params.seas$Parameter,1,2))
params.seas$Season <- as.factor(substr(params.seas$Parameter.ID, 3, 4))
params.seas$Season <- as.factor(ifelse(params.seas$Season==c("", "1", "ax"), "Year", paste(params.seas$Season)))
levels(params.seas$Season) <- c("Year", "pSummer", "pFall", "Winter", "Spring", "Summer", "Fall", "Year")
params.seas$Category <- as.factor(substr(params.seas$Parameter, 1, 1))
levels(params.seas$Category) <- c("Size", "Competition", "Max Growth", "Precipitation", "Model Std Dev", "Temperature")
summary(params.seas)

seas.bounds <- aggregate(params.seas[c("Upper.Bound", "Lower.Bound")], by=list(params.seas$Parameter, params.seas$Category, params.seas$Season), FUN=mean)
names(seas.bounds)[1:3] <- c("Parameter", "Category", "Season")
summary(seas.bounds)

seas.bounds2 <- stack(seas.bounds[,4:5])
names(seas.bounds2) <- c("Value", "Bound")
seas.bounds2$Parameter <- seas.bounds$Parameter
seas.bounds2$Category <- seas.bounds$Category
seas.bounds2$Season <- seas.bounds$Season
summary(seas.bounds2)

# -------------------------------------------------------------

# -------------------------------------------------------------
# Looking at the parameters & bounds graphically
# -------------------------------------------------------------
# Annual, All Parameters
png(file.path(dir.ms, "Figure_B1_Parameters_Annual_All.png"), width=8, height=6, units="in", pointsize=10, res=600)
ggplot(data=params.ann) + facet_wrap(Category~Parameter, scales="free") +
	geom_pointrange(aes(x=Parameter, y=MLE, ymin=Lower.Lim, ymax=Upper.Lim, color=Species, shape=Species), size=1.25, position=position_jitter(width=0.1)) +
	geom_blank(data=ann.bounds2, aes(x=Parameter, y=Value)) +
	scale_color_manual(values=species.colors) +
	scale_shape_manual(values=c(9, 10, 11, 8, 13)) +
	scale_y_continuous(name="Maximum Likelihood Estimate") +
	theme.ecology +
	theme(axis.title.x=element_blank(),
		  axis.text.x=element_blank(),
		  axis.ticks.x=element_blank())
dev.off()

# Seasonal, Non-Climate
png(file.path(dir.ms, "Figure_B2_Parameters_Season_Non-Climate.png"), width=8, height=6, units="in", pointsize=10, res=600)
ggplot(data=params.seas[params.seas$Season=="Year",]) + facet_wrap(Category~Parameter, scales="free") +
	geom_pointrange(aes(x=Parameter, y=MLE, ymin=Lower.Lim, ymax=Upper.Lim, color=Species, shape=Species), size=1.25, position=position_jitter(width=0.1)) +
	geom_blank(data=seas.bounds2[!(seas.bounds2$Category=="Temperature" | seas.bounds2$Category=="Precipitation"),], aes(x=Parameter, y=Value)) +
	scale_y_continuous(name="Maximum Likelihood Estimate") +
	scale_color_manual(values=species.colors) +
	scale_shape_manual(values=c(9, 10, 11, 8, 13)) +
	theme.ecology +
	theme(axis.title.x=element_blank(),
		  axis.text.x=element_blank(),
		  axis.ticks.x=element_blank())
dev.off()

# Seasonal, Temperature
png(file.path(dir.ms, "Figure_B3_Parameters_Season_Temperature.png"), width=8, height=6, units="in", pointsize=10, res=600)
ggplot(data=params.seas[params.seas$Category=="Temperature",]) +
	facet_wrap(Parameter~Season, scales="free", ncol=6) +
	geom_pointrange(aes(x=Parameter, y=MLE, ymin=Lower.Lim, ymax=Upper.Lim, color=Species, shape=Species), size=1.25, position=position_jitter(width=0.1)) +
	geom_blank(data=seas.bounds2[seas.bounds2$Category=="Temperature",], aes(x=Parameter, y=Value)) +
	scale_y_continuous(name="Maximum Likelihood Estimate") +
	scale_color_manual(values=species.colors) +
	scale_shape_manual(values=c(9, 10, 11, 8, 13)) +
	theme.ecology +
	theme(axis.title.x=element_blank(),
		  axis.text.x=element_blank(),
		  axis.ticks.x=element_blank())
dev.off()

# Seasonal, Precipitation
png(file.path(dir.ms, "Figure_B4_Parameters_Season_Precipitation.png"), width=8, height=6, units="in", pointsize=10, res=600)
ggplot(data=params.seas[params.seas$Category=="Precipitation" & !(params.seas$Season=="Year"),]) +
	facet_wrap(Parameter~Season, scales="free", ncol=6) +
	geom_pointrange(aes(x=Parameter, y=MLE, ymin=Lower.Lim, ymax=Upper.Lim, color=Species, shape=Species), size=1.25, position=position_jitter(width=0.1)) +
	geom_blank(data=seas.bounds2[seas.bounds2$Category=="Precipitation" & !(seas.bounds2$Season=="Year"),], aes(x=Parameter, y=Value)) +
	scale_y_continuous(name="Maximum Likelihood Estimate") +
	scale_color_manual(values=species.colors) +
	scale_shape_manual(values=c(9, 10, 11, 8, 13)) +
	theme.ecology +
	theme(axis.title.x=element_blank(),
		  axis.text.x=element_blank(),
		  axis.ticks.x=element_blank())
dev.off()
# -------------------------------------------------------------

# -------------------------------------------------------------
# Making Tables
# -------------------------------------------------------------
# -------------------------------------------------------------
