collapse.bigzList <-
function(l){
	s <- as.bigz(0)
	for (i in 1:length(l)) s <- sum.bigz(s,as.bigz(l[[i]]))
	return(s)
}

