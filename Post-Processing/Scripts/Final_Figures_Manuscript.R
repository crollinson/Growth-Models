####################################################################
# Making the final figures for Ecology for Rollinson et al 2015
# All pre-figure processing & original figures is in
#  -- Species_Comparisons_FullModel_Ann.R (Annual Scalars, Fig. 1)
#  -- Species_Comparisons_FullModel_Seasonal.R (Seasonal Scalars, Fig. 2, 3)
#  -- Species_Comparisons_FullModel_Ann_BAI.R (Interactions, Fig. 4, 5)
####################################################################
rm(list=ls())
# load("Species_Comparisons_FullModel_Ann.RData")
setwd("..")
source("Scripts/GrowthEffect_Functions.R")
source("Scripts/GrowthModel_Functions.R")

library(ggplot2)
library(grid)

# dir.ms <- "~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Growth Manuscript/Ecology Final"
dir.ms <- "~/Dropbox/CARCA/Growth Manuscript/Ecology Submission 4"

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


####################################################################
# Annual Scalars (Figure 1)
# Data Processing Script: Species_Comparisons_FullModel_Ann.R 
####################################################################
load("Inputs/Scalars_Annual_Bootstrapped.Rdata")
summary(scalars.annual)


aut.ann <- ggplot(data=scalars.annual[scalars.annual$Factor=="Size",]) +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species, linetype=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,100), breaks=seq(0, 100, 25)) +
	scale_x_continuous(name=expression(paste(Basal~Area~~(cm^2)))) + 
	theme(legend.position=c(0.8,0.5),
		  legend.key.height=unit(2, "lines")) +
	theme(plot.margin=unit(c(1,0,1,0), "lines")) +
	theme.ecology

comp.ann <- ggplot(data=scalars.annual[scalars.annual$Factor=="Competition",]) +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species, linetype=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_y_continuous(name="", limits=c(0,100), breaks=seq(0, 100, 25)) +
	scale_x_continuous(name="Relative Basal Area") + 
	theme(legend.position=c(0.8,0.5)) +
	guides(fill=F, color=F, linetype=F) +
	theme(plot.margin=unit(c(1,0,1.75,0), "lines")) +
	theme.ecology +
	theme(axis.title.y=element_blank(),
	axis.text.y=element_blank())

temp.ann <- ggplot(data=scalars.annual[scalars.annual$Factor=="Temperature",]) +
	geom_ribbon(aes(x=X-273, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X-273, y=MLE*100, color=Species, linetype=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,100), breaks=seq(0, 100, 25)) +
	scale_x_continuous(name=expression(paste("Annual Temperature ("^"o","C)"))) + 
	theme(legend.position=c(0.8,0.5)) +
	guides(fill=F, color=F, linetype=F) +
	theme(plot.margin=unit(c(0,0,1,0), "lines")) +
	theme.ecology

precip.ann <- ggplot(data=scalars.annual[scalars.annual$Factor=="Precipitation",]) +
	geom_ribbon(aes(x=X-273, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X-273, y=MLE*100, color=Species, linetype=Species), size=1.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	scale_y_continuous(name="", limits=c(0,100), breaks=seq(0, 100, 25)) +
	scale_x_continuous(name=expression(paste("Annual Precipitation (mm yr"^"-1",")"))) + 
	theme(legend.position=c(0.8,0.5)) +
	guides(fill=F, color=F, linetype=F) +
	theme(plot.margin=unit(c(0,0,0.75,0), "lines")) +
	theme.ecology +
	theme(axis.title.y=element_blank(),
	axis.text.y=element_blank())

png(file.path(dir.ms, "Figure_1_Scalars_Ann.png"), width=6, height=6, units="in", pointsize=6, res=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
print(aut.ann,    vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(comp.ann,   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(temp.ann,   vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(precip.ann, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()

####################################################################
# Seasonal Scalars (Figures 2, 3)
# Data Processing Script: Species_Comparisons_FullModel_Seasonal.R
####################################################################
load("Inputs/Scalars_Seasonal_Bootstrapped.Rdata")

png(file.path(dir.ms, "Figure_2_Scalars_Season_Temp.png"), width=8, height=6, units="in", pointsize=12, res=600)
ggplot(data=temp.stack) + facet_grid(Year~Season, scales="free_x") +
	geom_ribbon(aes(x=x.temp-273, ymin=Min*100, ymax=Max*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.temp-273, y=MLE*100, color=Species, linetype=Species), size=1.) +
	geom_vline(aes(xintercept=temp.min-273), linetype="dashed", color="black") + 
	geom_vline(aes(xintercept=temp.max-273), linetype="dashed", color="black") + 
	geom_ribbon(aes(x=Ribbon.min-273, ymin=0, ymax=100),fill="gray50", alpha=0.5) +
	geom_ribbon(aes(x=Ribbon.max-273, ymin=0, ymax=100),fill="gray50", alpha=0.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# scale_x_continuous(name="Total Season Precipitation (mm)", breaks=c(0, 50, 100, 150, 200)) + 
	scale_x_continuous(name=expression(paste("Season Temperature ("^"o","C)"))) + 
	scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,105), breaks=seq(0, 100, 25), expand=c(0,0)) +
	theme(legend.position=c(0.1, 0.75)) +
	theme.ecology
dev.off()

png(file.path(dir.ms, "Figure_3_Scalars_Season_Precip.png"), width=8, height=6, units="in", pointsize=12, res=600)
ggplot(data=precip.stack) + facet_grid(Year~Season, scales="free_x") +
	geom_ribbon(aes(x=x.precip, ymin=Min*100, ymax=Max*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=x.precip, y=MLE*100, color=Species, linetype=Species), size=1.) +
	geom_vline(aes(xintercept=precip.min), linetype="dashed", color="black") + 
	geom_vline(aes(xintercept=precip.max), linetype="dashed", color="black") + 
	geom_ribbon(aes(x=Ribbon.min, ymin=0, ymax=100),fill="gray50", alpha=0.5) +
	geom_ribbon(aes(x=Ribbon.max, ymin=0, ymax=100),fill="gray50", alpha=0.5) +
	scale_color_manual(values=species.colors) + scale_fill_manual(values=species.colors) +
	# scale_x_continuous(name="Total Season Precipitation (mm)", breaks=c(0, 50, 100, 150, 200)) + 
	scale_x_continuous(expression(paste("Season Precipitation (mm yr"^"-1",")")), breaks=c(25, 75, 125, 175)) + 
	scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,105), breaks=seq(0, 100, 25), expand=c(0,0)) +
	theme(legend.position=c(0.1, 0.75)) +
	theme.ecology
dev.off()

####################################################################
# Interactions Figures (Figures 4, 5)
# Data Processing Script: Species_Comparisons_FullModel_Ann_BAI.R
####################################################################
source("Scripts/facetAdjust.R")

# Temp-Precip Interaction
load(file="Inputs/Interactions_Bootstrapped_Temp_Precip.Rdata")

png(file.path(dir.ms, "Figure_4_Interactions_Temp_Precip.png"), width=4, height=4, units="in", pointsize=6, res=600)
facetAdjust(
ggplot(data=temp.stack) + facet_wrap(~ Species) +
	geom_ribbon(aes(x=x.temp-273.15, ymin=Min, ymax=Max, fill=Precip), alpha=0.3) +
	geom_line(aes(x=x.temp-273.15, y=MLE, color=Precip, linetype=Precip), size=1) +
	scale_color_manual(values=c("black", "gray30", "gray60")) + 
	scale_fill_manual(values=c("black", "gray30", "gray60")) +
	scale_x_continuous(name=expression(paste("Mean Annual Temperature ("^"o","C)"))) + 
	ylab(expression(paste(Basal~Area~Increment~~(mm^2~yr^-1)))) +
	labs(fill="Precipitation", color="Precipitation", linetype="Precipitation") +
	theme.ecology+
	theme(legend.position=c(0.85,0.25), 
	      legend.key.height=unit(2, "line")),
pos="down")
dev.off()


# Temp-Size-Competition Interaction
load(file="Inputs/Interactions_Bootstrapped_Temp_Size_Comp.Rdata")

png(file.path(dir.ms, "Figure_5_Interactions_Temp_Size_Comp.png"), width=6, height=5, units="in", pointsize=12, res=600)
ggplot(data=temp.comp.stack) + facet_wrap(~ Scenario , nrow=1) +
	geom_ribbon(aes(x=x.temp-273.15, ymin=Min, ymax=Max, fill=Species), alpha=0.2) +
	geom_line(aes(x=x.temp-273.15, y=MLE, color=Species, linetype=Species), size=1) +
	scale_color_manual(values=species.colors) + 
	scale_fill_manual(values= species.colors) +
	scale_x_continuous(name=expression(paste("Mean Annual Temperature ("^"o","C)")), breaks=c(6,9,12)) + 
	ylab(expression(paste(Basal~Area~Increment~~(mm^2~yr^-1)))) +
	theme.ecology +
	theme(legend.position=c(0.15,0.75), 
	      legend.key.height=unit(1.25, "line")) +
	labs(fill="Species", color="Species", linetype="Species")
dev.off()