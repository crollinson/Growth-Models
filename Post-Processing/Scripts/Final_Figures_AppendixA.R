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

dir.ms <- "~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Growth Manuscript/Ecology Submission 4"
# dir.ms <- "~/Dropbox/CARCA/Growth Manuscript/Ecology Final"

species.colors <- c("purple", "blue", "green3", "orange", "red")
species.colors2 <- c("purple", "blue", "green3", "gray50", "orange", "red")

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
# Loading Data
# -------------------------------------------------------------
# -------------
# Loading in the tree ring data that was used in (Annual) growth models
# -------------
dat.acru <- read.csv("../Annual/Inputs/ACRU_AllSites_1990-2011.csv")
dat.bele <- read.csv("../Annual/Inputs/BELE_AllSites_1990-2011.csv")
dat.nysy <- read.csv("../Annual/Inputs/NYSY_AllSites_1990-2011.csv")
dat.qupr <- read.csv("../Annual/Inputs/QUPR_AllSites_1990-2011.csv")
dat.quru <- read.csv("../Annual/Inputs/QURU_AllSites_1990-2011.csv")

model.data <- rbind(dat.acru, dat.bele, dat.nysy, dat.qupr, dat.quru)
summary(model.data)
# -------------

# -------------
# Loading data for all trees & rings (to get stand-level stats)
# -------------
ring.data <- read.csv("../RawInputs/CARCA_CoreData_Climate_Annual.csv")
summary(ring.data)

trees <- read.csv("../RawInputs/TreeData.csv", na.strings=c("", "*"))
summary(trees)

plot.data <- read.csv("../RawInputs/PlotData.csv")
summary(plot.data)
# -------------

# -------------
# Doing some stats & subsetting
# -------------
# Find the year a plot was sampled; only doing for plots we have ring data for
for(p in unique(ring.data$PlotID)){
	yr.end <- nchar(paste(plot.data[plot.data$plotID==p, "date.sample"]))
	trees[trees$PlotID==p, "Year.Sampled"] <- as.numeric(substr(plot.data[plot.data$plotID==p, "date.sample"],yr.end-3, yr.end))
}
summary(trees)


# Find age
for(t in unique(ring.data$TreeID)){
	trees[trees$TreeID==t, "Age"] <- trees[trees$TreeID==t, "Year.Sampled"]-min(ring.data[!is.na(ring.data$BAI) & ring.data$TreeID==t, "Year"], na.rm=T)
}
summary(trees)

# Subset just the trees used in modeling
trees.model <- trees[trees$TreeID %in% unique(model.data$TreeID),]
summary(trees.model)

# Finding all trees present in the plots we sampled & Groupign them by species
target.spp <- c("ACRU", "BELE", "NYSY", "QUPR", "QURU")
trees.sampled <- trees[!is.na(trees$Year.Sampled),]
trees.sampled$Spp.Graph <- as.factor(ifelse(trees.sampled$Spp %in% target.spp, paste(trees.sampled$Spp), "Other"))
trees.sampled$Spp.Graph <- recode(trees.sampled$Spp.Graph, "'ACRU'='1'; 'BELE'='2'; 'NYSY'='3'; 'QUPR'='4'; 'QURU'='5'; 'Other'='6'")
levels(trees.sampled$Spp.Graph) <- c("ACRU", "BELE", "NYSY", "QUPR", "QURU", "Other")
trees.sampled$Canopy.code <- recode(trees.sampled$Canopy, "''='NA'; '*'='NA'; 'U'='1'; 'I'='2'; 'C'='3'; 'D'='4'")
levels(trees.sampled$Canopy.code) <- c("Understory", "Intermediate", "Co-dominant", "Dominant", NA)
summary(trees.sampled)
# -------------
# -------------------------------------------------------------

# -------------------------------------------------------------
# Stats & plots
# -------------------------------------------------------------
# Finding mean tree age
mean(trees$Age, na.rm=T); sd(trees$Age, na.rm=T)

# -------------
# Size distributions for appendix
# -------------
plot.dbh <- ggplot(trees.sampled[trees.sampled$Live=="LIVE" & !is.na(trees.sampled$DBH),]) +
				geom_histogram(aes(x=DBH, fill=Spp.Graph), binwidth=5) +
				scale_fill_manual(values=c(species.colors, "gray50")) +
				theme.ecology +
				labs(fill="Species", x="DBH (cm)") +
				guides(fill=F) 
plot.canopy <- ggplot(trees.sampled[trees.sampled$Live=="LIVE" & !is.na(trees.sampled$Canopy.code),]) +
				geom_histogram(aes(x=Canopy.code, fill=Spp.Graph), binwidth=5) +
				scale_fill_manual(values=c(species.colors, "gray50")) +
				theme.ecology +
				labs(fill="Species", x=("Canopy Class")) +
				theme(legend.position=c(0.9, 0.7), legend.key.height=unit(1.5, "lines"))

png(file.path(dir.ms, paste0("Figure_A1_Size_Distributions.png")), width=4, height=6, units="in", pointsize=10, res=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
print(plot.dbh,    vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot.canopy,   vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()
# -------------

# -------------
# Disturbance Graph -- See "Establishment Modeling/1_Establishment0_ReleaseDetection.R"
# -------------
# -------------
# -------------------------------------------------------------
