vertex.dH <-
function(g,matrix,mlps=0,logB=exp(1),parall=FALSE,preschedule=FALSE,cores=1,verbose=FALSE) {
	## calculate uncertainty before projection
	v_H.before = c()
	for (r in 1:nrow(matrix)) {
		t <- c()	
		for (c in 1:ncol(matrix)) {
			if (matrix[r,c]==1) {t <- c(t,paste(matrix[,c],collapse=""))}
		}	
		tfreq <- table(t) / length(t)
		v_H.before <- c(v_H.before,-sum(tfreq*log(tfreq,base=logB)))
	}
	for (v in 1:length(V(g))) {
		V(g)[v-1]$H.before = v_H.before[[v]]
	}
	## calculate uncertainty after projection
	if (parall){	## use parall lapply provided my multicore package
		if (verbose) cat(' in parallel mode\n')
		Hafter <- unlist(mclapply(1:length(V(g)),get.Hafter.vertex,g,mlps,logB,mc.preschedule=preschedule,mc.cores=cores))
	}
	else{		## regular sequential calculation
		if (verbose) cat(' in sequential mode\n')
		Hafter <- unlist(lapply(1:length(V(g)),get.Hafter.vertex,g,mlps,logB))
	}
	for (k in 1:length(V(g))) {
		g <- set.vertex.attribute(g,'H.after',index=k-1,Hafter[k])
	}
	## get H.delta, Change in uncertainty
	V(g)$H.delta <- V(g)$H.after - V(g)$H.before
	## return
	return(g)
}

