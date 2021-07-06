#' @title Variant 1
bootPlainFast <- function(obj, B=1000,
		      typeRes="HC3", typeEps="Liu1988",
		      trunc=1000) {
    frm <- obj@formula
    data <- obj@data

    fit <- lm(frm, data=data)

    tr <- list()
    tr[[length(tr)+1]] <- summary(fit)$coef[2,3]

    res <- list()
    for (i in 2:B) {
	if (i %% 10 == 0) { 
	    cat (paste0("\r ", round(i/B*100),"%  ")) 
	}

	tryCatch({
	    uhat <- multE(resTransf(fit, type=typeRes), type=typeEps)

	    tmp <- data
	    prd <- predict(fit, newdata=data)
	    tmp[,obj@depVar] <- prd+sample(uhat, replace=T) 
	    fitTmp <- lm(frm, data=tmp)

	    perfectFit <- F
	    tryCatch({summary(fitTmp)$r.squared}, warning=function(e) perfectFit <- T )

	    if (!any(is.na(prd)) && !any(is.infinite(prd)) && !any(is.na(uhat)) && !perfectFit) {

		if (length(res) > 0 &&  (
					 any(uhat > trunc*max(res[[1]])) || 
					     any(abs(data$RES) < 1/trunc*abs(min(res[[1]])))
					 )) {
		    break
		}

		data <- tmp
		fit <- fitTmp

		## TODO: check if covar is selected 
		tr[[length(tr)+1]] <- summary(fit)$coef[2,3]

		res[[length(res)+1]] <- uhat
	    }
	}, error=function(e) { print(e)  })
    }

    tr <- unlist(tr)
    trFull <- unlist(tr)
    stopW <- length(res)

    return(list(tVal=tr, stopW=stopW, stopFact=trunc, res=res, tValFull=trFull))
}
