####################################################################
# Appendix C Figures: Distributions of predictor data plotted along size response curves for each species
# Modified from original script: Species_Comparisons_Appendix1_Distributions.R
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
# -------------------------


# -------------------------
# Load data
# -------------------------
# Load already bootstrapped runs
load("Inputs/Scalars_Annual_Bootstrapped.Rdata") # loads object "scalars.annual"
# change Temp to Celcius
summary(scalars.annual)

# Loading in the raw tree ring data
model.data <- read.csv("Inputs/TargetSpecies_AllSites_Ann_1990-2011.csv")
# model.data$BA.tree.cm2 <- model.data$BA.tree/100

# # Subsetting the data to make our lives easier
# species <- c("ACRU", "QURU", "QUPR", "BELE", "NYSY")
# model.data <- model.data[(model.data$Spp %in% species) & model.data$Year>=1990 & model.data$Year<=2011,]

# Translating model.data into something that more closely represents the scalars.annual to 
#   make figures a lot easier
data.x <- stack(model.data[,c("BA.tree.cm2", "RelBA", "Tavg", "Precip.PRISM.sum")])[,c(2,1)]
names(data.x)  <- c("Factor", "X")
data.x$Factor  <- recode(data.x$Factor, "'BA.tree.cm2'='Size'; 'RelBA'='Competition'; 'Tavg'='Temperature'; 'Precip.PRISM.sum'='Precipitation'")
data.x$Species <- model.data$Spp
data.x$Site    <- model.data$Site 
data.x$PlotID  <- model.data$PlotID
data.x$Trans   <- model.data$Trans
data.x$TreeID  <- model.data$TreeID
data.x$Year    <- model.data$Year
summary(data.x)
# -------------------------

##################################################################################
# Species data distributions
##################################################################################

# Making a table to draw from; 
# Note: this is basically Table 3
species.df <- data.frame(Species=unique(scalars.annual$Species), Color=species.colors)
for(s in unique(species.df$Species)){
	species.df[species.df$Species==s, "n"] <- length(data.x[data.x$Species==s & data.x$Factor=="Size", "X"])
	species.df[species.df$Species==s, "n.tree"] <- length(unique(data.x[data.x$Species==s & data.x$Factor=="Size", "TreeID"]))
}
species.df

for(i in 1:nrow(species.df)){
	s = species.df[i,"Species"]
	color.spp <- species.df[species.df$Species==s, "Color"]
	n.spp <- species.df[species.df$Species==s, "n"]

	plot.size <- ggplot(data=scalars.annual[scalars.annual$Species==s & scalars.annual$Factor=="Size",]) + facet_wrap(~Factor, scales="free_x") +
	geom_histogram(data=data.x[data.x$Species==s & data.x$Factor=="Size",], aes(x=X, weight=(1/n.spp)*100), color="gray50") +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species), size=2) +
	scale_color_manual(values=paste(color.spp)) + scale_fill_manual(values=paste(color.spp)) +
	scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,110), breaks=seq(0, 100, 25), expand=c(0,0)) +
	scale_x_continuous(name=expression(paste(Basal~Area~~(cm^2))), limits=c(min(data.x[data.x$Factor=="Size", "X"]), max(data.x[data.x$Factor=="Size", "X"]))) + 
	guides(fill=F, color=F) +
	theme(plot.margin=unit(c(1,0,1,0), "lines")) +
	theme.ecology

	plot.comp <- ggplot(data=scalars.annual[scalars.annual$Species==s & scalars.annual$Factor=="Competition",]) + facet_wrap(~Factor, scales="free_x") +
	geom_histogram(data=data.x[data.x$Species==s & data.x$Factor=="Competition",], aes(x=X, weight=(1/n.spp)*100), color="gray50") +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species), size=2) +
	scale_color_manual(values=paste(color.spp)) + scale_fill_manual(values=paste(color.spp)) +
	scale_y_continuous(name="", limits=c(0,110), breaks=seq(0, 100, 25), expand=c(0,0)) +
	scale_x_continuous(name="Relative Basal Area", limits=c(min(data.x[data.x$Factor=="Competition", "X"]), max(data.x[data.x$Factor=="Competition", "X"]))) + 
	guides(fill=F, color=F) +
	theme(plot.margin=unit(c(1,1.5,1.75,0), "lines")) +
	theme.ecology +
	theme(axis.title.y=element_blank(),
		  axis.text.y=element_blank())

	plot.temp <- ggplot(data=scalars.annual[scalars.annual$Species==s & scalars.annual$Factor=="Temperature",]) + facet_wrap(~Factor, scales="free_x") +
	geom_histogram(data=data.x[data.x$Species==s & data.x$Factor=="Temperature",], aes(x=X, weight=(1/n.spp)*100), color="gray50") +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species), size=2) +
	scale_color_manual(values=paste(color.spp)) + scale_fill_manual(values=paste(color.spp)) +
	scale_y_continuous(name="Growth Rate (% Max)", limits=c(0,110), breaks=seq(0, 100, 25), expand=c(0,0)) +
	scale_x_continuous(name=expression(paste("Annual Temperature ("^"o","C)")), limits=c(min(data.x[data.x$Factor=="Temperature", "X"]), max(data.x[data.x$Factor=="Temperature", "X"]))) + 
	guides(fill=F, color=F) +
	theme(plot.margin=unit(c(0,0,1,0), "lines")) +
	theme.ecology

	plot.precip <- ggplot(data=scalars.annual[scalars.annual$Species==s & scalars.annual$Factor=="Precipitation",]) + facet_wrap(~Factor, scales="free_x") +
	geom_histogram(data=data.x[data.x$Species==s & data.x$Factor=="Precipitation",], aes(x=X, weight=(1/n.spp)*100), color="gray50") +
	geom_ribbon(aes(x=X, ymin=CI.low*100, ymax=CI.hi*100, fill=Species), alpha=0.3) +
	geom_line(aes(x=X, y=MLE*100, color=Species), size=2) +
	scale_color_manual(values=paste(color.spp)) + scale_fill_manual(values=paste(color.spp)) +
	scale_y_continuous(name="", limits=c(0,110), breaks=seq(0, 100, 25), expand=c(0,0)) +
	scale_x_continuous(name=expression(paste("Annual Precipitation (mm yr"^"-1",")")), limits=c(min(data.x[data.x$Factor=="Precipitation", "X"]), max(data.x[data.x$Factor=="Precipitation", "X"]))) + 
	guides(fill=F, color=F) +
	theme(plot.margin=unit(c(0,1.5,0.75,0), "lines")) +
	theme.ecology +
	theme(axis.title.y=element_blank(),
		  axis.text.y=element_blank())

png(file.path(dir.ms, paste0("Figure_C",i,"_",s, "_Scalars_Ann.png")), width=6, height=6, units="in", pointsize=6, res=600)
pushViewport(viewport(layout=grid.layout(2,2)))
print(plot.size,    vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot.comp,   vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot.temp,   vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot.precip, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()	
}
