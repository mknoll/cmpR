#' @title Calculate p-value
#' 
#' @import foreach
#' @import doParallel
#' @import parallel
#'
#' @export
pcal <- function(obj, rep=100, method="V2", ...) {
    no_cores <- parallel::detectCores() - 1    
    no_cores <- ifelse(no_cores == 0, 1, no_cores)    
    ## FIXME    
    if (is.na(no_cores)) { no_cores <- 4 }    
    doParallel::registerDoParallel(no_cores)

    ret <- NA
    if (method=="V2") {
	ret <- median(unlist(foreach(i=1:rep) %dopar% median(bootMC(obj, 10000, "HC3", "Liu1988")$tVal)))
    } else if (method=="V1") {
	ret <- median(unlist(foreach(i=1:rep) %dopar% median(bootPlain(obj, 10000, "HC3", "Liu1988")$tVal)))
    } else {
	stop("Unknown method!")
    }

    doParallel::stopImplicitCluster()

    return(ret)
}
