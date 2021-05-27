#' @title ABC
bootNP <- function(obj, B=999, typeRes="HC2", typeEps="Liu1988") {
    data <- obj@data
    frm <- obj@formula

    n <- length(data[,1]) #number of datapoints
    
    weights <- matrix( rexp(n * B, 1) , ncol = n, byrow = TRUE)    
    weights <- weights / rowSums(weights)    
 
    fit <- lm(frm, data=data)

    tr <- numeric(B)
    tr[] <- NA
    for (i in 1:(B-1)) {
	if (i %% 10 == 0) { cat(paste0("\r  ", round(i/B*100), "%    ")) }
	data$RES <- sample(multE(resTransf(fit, type=typeRes), type=typeEps), replace=T)
	if (any(is.na(data$RES))) {
	    next
	}

	data$PRED <- predict(fit, newdata=data)+data$RES

	bak <- data
	bak$VAL <- data$PRED 
	wval <<- weights[i,] ###very strange bug in R! FIXME

	tmp <- lm(frm, data=bak, weights=wval)

	if (summary(tmp)$coef[2,3] == 0 || abs(summary(tmp)$coef[2,1])> 1000) {
	    ## 
	} else {
	    data$VAL <- data$PRED 
	    fit <- lm(frm, data=data,weights=wval)
	    tr[i] <- summary(fit)$coef[2,3]
	}
    }

    return(tr)
}
