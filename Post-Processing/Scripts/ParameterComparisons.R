####################################################################
# Comparing individual growth model parameters across species
####################################################################


# -------------------------------------------------------------
# Setting directories & reading in libraries
# -------------------------------------------------------------
setwd("..")
library(ggplot2)
library(grid)
library(car)
# -------------------------------------------------------------

# -------------------------------------------------------------
# Some ggplot themes that I use a lot
# -------------------------------------------------------------
q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))

q.blank2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1)), axis.text.y=element_text(color="black", size=rel(1)), axis.title.x=element_text(face="bold", size=rel(1)),  axis.title.y=element_text(face="bold", size=rel(1)))


large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines"))
# -------------------------------------------------------------


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
pdf("Figures/Parameters_Annual_All.pdf", width=10, height=7.5)
ggplot(data=params.ann) + facet_wrap(Category~Parameter, scales="free") +
	geom_pointrange(aes(x=Parameter, y=MLE, ymin=Lower.Lim, ymax=Upper.Lim, color=Species), size=1.25, position=position_jitter(width=0.1)) +
	scale_color_manual(values=c("purple3", "blue3", "green3", "orange3", "red3")) +
	geom_blank(data=ann.bounds2, aes(x=Parameter, y=Value)) +
	theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank())
dev.off()

# Seasonal, Non-Climate
pdf("Figures/Parameters_Season_Non-Climate.pdf", width=10, height=7.5)
ggplot(data=params.seas[params.seas$Season=="Year",]) + facet_wrap(Category~Parameter, scales="free") +
	geom_pointrange(aes(x=Parameter, y=MLE, ymin=Lower.Lim, ymax=Upper.Lim, color=Species), size=1.25, position=position_jitter(width=0.1)) +
	scale_color_manual(values=c("purple3", "blue3", "green3", "orange3", "red3")) +
	geom_blank(data=seas.bounds2[!(seas.bounds2$Category=="Temperature" | seas.bounds2$Category=="Precipitation"),], aes(x=Parameter, y=Value)) +
	theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank())
dev.off()

# Seasonal, Temperature
pdf("Figures/Parameters_Season_Temperature.pdf", width=10, height=7.5)
ggplot(data=params.seas[params.seas$Category=="Temperature",]) +
	ggtitle("Temperature Parameters")+
	facet_wrap(Parameter~Season, scales="free", ncol=6) +
	geom_pointrange(aes(x=Parameter, y=MLE, ymin=Lower.Lim, ymax=Upper.Lim, color=Species), size=1.25, position=position_jitter(width=0.1)) +
	scale_color_manual(values=c("purple3", "blue3", "green3", "orange3", "red3")) +
	geom_blank(data=seas.bounds2[seas.bounds2$Category=="Temperature",], aes(x=Parameter, y=Value)) +
	theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank())
dev.off()

# Seasonal, Precipitation
pdf("Figures/Parameters_Season_Precipitation.pdf", width=10, height=7.5)
ggplot(data=params.seas[params.seas$Category=="Precipitation" & !(params.seas$Season=="Year"),]) +
	ggtitle("Precipitation Parameters") +
	facet_wrap(Parameter~Season, scales="free", ncol=6) +
	geom_pointrange(aes(x=Parameter, y=MLE, ymin=Lower.Lim, ymax=Upper.Lim, color=Species), size=1.25, position=position_jitter(width=0.1)) +
	scale_color_manual(values=c("purple3", "blue3", "green3", "orange3", "red3")) +
	geom_blank(data=seas.bounds2[seas.bounds2$Category=="Precipitation" & !(seas.bounds2$Season=="Year"),], aes(x=Parameter, y=Value)) +
	theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank())
dev.off()
# -------------------------------------------------------------

# -------------------------------------------------------------
# Making Tables
# -------------------------------------------------------------
# -------------------------------------------------------------
