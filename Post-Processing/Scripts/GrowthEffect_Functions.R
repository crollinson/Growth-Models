####################################################################
# Full Model: BAI ~ Gmax*Autogenic*Competition*Climate

autogenic.effect <- function(SIZE,aa,ab) 
	{ (1 - aa*exp(-ab*SIZE))  }

size.effect <- function(RS,ca,cb,cc) 
	{ ca*exp(-cb*(RS^cc)) }

ba.effect <- function(BA.PLOT,ce,cf,cg) 
	{ ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg)) }

comp.effect <- function(RS,BA.PLOT,ca,cb,cc,cd,ce,cf,cg)
	{ size.effect <- ca*exp(-cb*(RS^cc))
      plot.effect <- (ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg)))
      comp.resp <- exp(-size.effect*(plot.effect^cd)) 
      return(comp.resp)
      }

temp.effect <- function(TEMP,ta1,tb1)
	{ (exp(-0.5*(((TEMP)-ta1)/tb1)^2)) }

precip.effect <- function(PRECIP,FLOW,pa1,pb1,pc1)
	{ (exp(-0.5*(((PRECIP + pc1*FLOW*PRECIP)-pa1)/pb1)^2)) }



# ----------------------------------------------------
# Full Growth Model
# ----------------------------------------------------
full.model.annual <-function(SIZE,RS,BA.PLOT,TEMP,PRECIP,FLOW,aa,ab,ca,cb,cc,cd,ce,cf,cg,ta1,tb1,pa1,pb1,pc1,gmax)	{ 
	  aut <- (1 - aa*exp(-ab*SIZE))  
	  size <- ca*exp(-cb*(RS^cc))
	  plot <- ce/1000 + ( (1-ce/1000)/(1+(BA.PLOT/cf)^cg))
	  comp <- exp(-size*(plot^cd))
	  temp <- exp(-0.5*(((TEMP)-ta1)/tb1)^2)
	  precip <- exp(-0.5*(((PRECIP+pc1*FLOW*PRECIP)-pa1)/pb1)^2)
	  
	  gmax*aut*comp*temp*precip
	}
# ----------------------------------------------------
