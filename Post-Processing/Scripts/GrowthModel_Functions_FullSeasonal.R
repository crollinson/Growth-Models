# ----------------------------------------------------
# Full Seasonal Growth Model, BAI
# ----------------------------------------------------
full.model.seasonal.bai <- function(species, var, SIZE, RS, BA.PLOT, TEMP, PRECIP, FLOW, out, param.est, param.distrib, n){
	full.temp <- data.frame(array(dim=c(length(var),1)))
	row.names(full.temp) <- var

	for(p in unique(param.est$Parameter)){	
		assign(paste(p, "mle", sep="."), param.est[param.est$Species==species & param.est$Parameter==p, "MLE"])
		}
	

	# aa
	for(i in 1:n){ #aa
		aa.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="aa", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE=SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP=PRECIP,
									FLOW=FLOW,
									aa=aa.samp, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# ab
	for(i in (1*n+1):(2*n)){ #ab
		ab.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="ab", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP=PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.samp,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# ca
	for(i in (2*n+1):(3*n)){ #ca
		ca.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="ca", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.samp, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}		
	# cb
	for(i in (3*n+1):(4*n)){ #cb
		cb.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="cb", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.samp, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# cc
	for(i in (4*n+1):(5*n)){ #cc
		cc.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="cc", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.samp, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# cd
	for(i in (5*n+1):(6*n)){ #cd
		cd.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="cd", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.samp, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# ce
	for(i in (6*n+1):(7*n)){ #ce
		ce.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="ce", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.samp, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# cf
	for(i in (7*n+1):(8*n)){ #cf
		cf.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="cd", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.samp, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# cg
	for(i in (8*n+1):(9*n)){ #cg
		cg.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="cg", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.samp, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.samp,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}

	# -------------------------------------------------------------------------------------
	# Seasonal Component gets complicated
	# -------------------------------------------------------------------------------------
	# ta
	for(i in (9*n+1):(10*n)){ #ta
		ta1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="ta1", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.samp, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# tb
	for(i in (10*n+1):(11*n)){ #tb
		tb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="tb1", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.samp,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# -------------------------------------------------------------------------------------
	# pa
	for(i in (11*n+1):(12*n)){ #pa
		pa1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="pa1", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.samp, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# pb
	for(i in (12*n+1):(13*n)){ #pb
		pb1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="pb1", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.samp, pc1=pc1.mle,
									gmax=gmax.mle)
		}
	# pc
	for(i in (13*n+1):(14*n)){ #pc
		pc1.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="pc1", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.samp,
									gmax=gmax.mle)
		}
	# -------------------------------------------------------------------------------------
	# gmax
	for(i in (14*n+1):(15*n)){ #gmax
		gmax.samp <- as.numeric(sample(param.distrib[param.distrib$Species==species & param.distrib$Parameter=="gmax", 3:ncol(param.distrib)], size=1, replace=T))
		full.temp[,i] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP= PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.samp)
		}

		#############

	out[,paste(species, "VAR", sep=".")] <- var
	out[,paste(species, "MLE", sep=".")] <- full.model.seasonal(SIZE= SIZE,
									RS=RS, 
									BA.PLOT=BA.PLOT,
									TEMP=TEMP,
									PRECIP=PRECIP,
									FLOW=FLOW,
									aa=aa.mle, ab=ab.mle,
									ca=ca.mle, cb=cb.mle, cc=cc.mle, cd=cd.mle, ce=ce.mle, cf=cf.mle, cg=cg.mle,
									ta1=ta1.mle, tb1=tb1.mle,
									pa1=pa1.mle, pb1=pb1.mle, pc1=pc1.mle,
									gmax=gmax.mle)
	
	#---------------------------------------------
	ci <- apply(full.temp, 1, FUN=quantile, c(0.025, 0.975),na.rm=T)
	range  <- apply(full.temp, 1, FUN=range, na.rm=T)

	out[,paste(species, "CI.low", sep=".")] <- ci[1,]
	out[,paste(species, "CI.high", sep=".")] <- ci[2,]
	out[,paste(species, "Min", sep=".")] <- range[1,]
	out[,paste(species, "Max", sep=".")] <- range[2,]
	
	return(out)
	#---------------------------------------------
	}
# ----------------------------------------------------
