redundant.cliques <-
function(new, checkSet,mode='vertex'){
	## checking for redundant clique counts
	if (length(checkSet)==0){			## if checkSet is empty no cliques are redundant
		return(as.bigz(0))
	}
	cliqs <- as.bigz(number.subcliques(length(new),mode=mode))	## number of cliques present in new
	rc <- as.bigz(0)
	intersections = set()				## all true intersections
	intersection_sizes = c()			## keeping track of size of intersections
	intersections_rc <- c()
	## Sets to check for insection
	for (item in checkSet){				## compare new set to vector of other sets
		actIntersect <- set_intersection(item,new)
		if (length(actIntersect)>0){		## if intersection is non-empty
			if (set_is_subset(new,item)){		## if new is subset of any one of the others, all subcliques are redundant
				## new Set is true subset, terminate redundat_cliques, return number of cliques
				return(cliqs)
			}
			else{
				## new set is no true subset, proceed to compare to other intersections
				if (length(intersections)==0){
					intersections <- set(actIntersect)
				}
				else{
					## remove formerly inserted intersections that are true subsets of new one
					r <- set()
					for (entry in intersections) if (set_is_proper_subset(entry,actIntersect)) r <- r | set(entry)
					intersections <- intersections - r
					intersections <- set_union(set(actIntersect),intersections)
				}
				## done checking current intersection
			}
		}
		
	}
	intersections_valid <- set()
	if (length(intersections)>0){
		## determine intersection sizes and recursively their number of redundant cliques
		for (entry in intersections){
			## checking intersection
			if (length(intersection_sizes)>0){
				intersection_sizes <- c(intersection_sizes,as.bigz(as.character(length(entry))))
			}
			else{
				## first intersection size added
				intersection_sizes <- as.bigz(as.character(length(entry)))
			}
			## starting recursion
			tmp_rc <- redundant.cliques(entry,intersections_valid)
	

			## determine number of redundant cliques in intersection
			if (length(intersections_rc)>0){
				## adding number of redundant cliques
				intersections_rc <- c(intersections_rc,tmp_rc)
			}
			else{
				## first number of redundant cliques added
				intersections_rc <- tmp_rc
			}
			## adding intersection to set of valid intersections
			intersections_valid <- set_union(intersections_valid,set(entry))
		}
	}
	## if no intersections were found with item from checkSet
	if (length(intersections_valid)==0){
		## terminate function and return 0
		return(as.bigz(0))
	}
	else{
		## valid intersections found, calculate total number of redundant cliques
		for (i in 1:length(intersection_sizes)){
			## number of non-redundant cliques in intersection
			tmp <- sub.bigz(number.subcliques(intersection_sizes[i],mode=mode),intersections_rc[i])
			## adding to total number of redundant cliques for given set
			rc <- sum.bigz(rc,tmp)
		}
		## returning result and terminating instance
		return(rc)
	}
}

