number.subcliques <-
function(n,mode='clique') {
	if (mode=='clique'){
		return(sub.bigz(pow.bigz(2,as.character(n)),add.bigz(as.character(n),1)))
	}
	if (mode=='vertex'){
		if (n>0){
			return(sub.bigz(pow.bigz(2,sub.bigz(as.character(n),1)),1))
		}
		else{
			return(as.bigz(0))
		}
	}
	if (mode=='edge'){
		return(pow.bigz(2,sub.bigz(as.character(n),2)))
	}
}

