source('logloss.r')
source('normalize_AB.r')
optcuts <- function(response,truth) {
	response1 <- response
	classes <- length(unique(c(truth))) #also can be specified as: length(unique(truth)) or length(unique(response))
	initial_cuts <- c(0.4,0.5,0.6)

	optimize_cuts <- function(initial_cuts,response,truth,freedom,resolution) {
        	for (i in 2:classes) {
	                	LL_min <- 2
        	        span <- seq(initial_cuts[i]-freedom,initial_cuts[i]+freedom,by=resolution)
                	for (j in span) {
                        	if ( j > initial_cuts[i-1] ) {
                                	response[which(response<j)] <- normalize_AB(response[which(response<j)],0,0.5)
                                	response[which(response>=j)] <- normalize_AB(response[which(response>=j)],0.5,1)
	                                LL <- logloss(truth,response)
					print(LL)
        	                        if (LL < LL_min) {
                	                        LL_min <- LL
                        	                j_min <- j
                                	}
					response <- response1
	                        }
        	        }
                	initial_cuts[i] <- j_min
	        }
        	return(initial_cuts)
	}

	#iterations for optimum cuts.
	for (i in 1:20) {
		cat('.')
        	temp <- initial_cuts
		resolution <- min(diff(initial_cuts)) / 50
	        freedom <- min(diff(initial_cuts)) - resolution
        	initial_cuts <- optimize_cuts(initial_cuts,response,truth,freedom,resolution)
	        if (min(abs(temp - initial_cuts)) < (resolution/10)  )
        	        break
	}
	cat('\n')
        response[which(response<j_min)] <- normalize_AB(response[which(response<j_min)],0,0.5)
        response[which(response>=j_min)] <- normalize_AB(response[which(response>=j_min)],0.5,1)
	LL <- logloss(truth,response)
	output <- list(LL=LL,cuts=initial_cuts,response=response)
	return(output)
}
