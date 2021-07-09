#' @title Calculate t-value
#' 
#' @import foreach
#' @import doParallel
#' @import parallel
#'
#' @export
tcalFast <- function(obj, rep=100, method="V2", ...) {
    no_cores <- parallel::detectCores() - 1    
    no_cores <- ifelse(no_cores == 0, 1, no_cores)    
    ## FIXME    
    if (is.na(no_cores)) { no_cores <- 4 }    
    doParallel::registerDoParallel(no_cores)

    ret <- NA
    if (method=="V2") {
	ret <- median(unlist(foreach(i=1:rep) %dopar% median(bootMCFast(obj, 10000, "HC3", "Liu1988")$tVal)))
    } else if (method=="V1") {
	ret <- median(unlist(foreach(i=1:rep) %dopar% median(bootPlainFast(obj, 10000, "HC3", "Liu1988")$tVal)))
    } else {
	stop("Unknown method!")
    }

    doParallel::stopImplicitCluster()

    return(ret)
}


#' @title Calculate t-value, fast implementation
#' 
#' @description data must adhere to the form y~x
#' 
#' @import foreach
#' @import doParallel
#' @import parallel
#'
#' @export
tcalFast2 <- function(obj, rep=100, method="V2", res="HC3", eps="Liu1988", ...) {
    no_cores <- parallel::detectCores() - 1    
    no_cores <- ifelse(no_cores == 0, 1, no_cores)    
    ## FIXME    
    if (is.na(no_cores)) { no_cores <- 4 }    
    doParallel::registerDoParallel(no_cores)

    ret <- NA
    if (method=="V2") {
	ret <- median(unlist(foreach(i=1:rep) %dopar% median(bootMCFast2(obj, 10000, res, eps)$tVal)))
    } else if (method=="V1") {
	ret <- median(unlist(foreach(i=1:rep) %dopar% median(bootPlainFast2(obj, 10000, res, eps)$tVal)))
    } else {
	stop("Unknown method!")
    }

    doParallel::stopImplicitCluster()

    return(ret)
}
