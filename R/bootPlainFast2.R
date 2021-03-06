#' @title Variant 1
#' 
#' @description Variables must be names y and x!
#' 
#' @import Rfast
bootPlainFast2 <- function(obj, B=1000,
		      typeRes="HC3", typeEps="Liu1988",
		      trunc=1000) {
    frm <- obj@formula
    frmS <- as.formula(paste("~1", all.vars(frm)[-1], sep="+"))

    data <- obj@data
    X <- model.matrix(frmS, data=data)

    tr <- list()
    #t-value for init    
    fit <- lmfit(X, data[,obj@depVar])        
    sX <- solve(t(X) %*% X) /(length(X[,1])-length(X[1,]))  
    hv <- diag(X %*% solve(t(X) %*% X) %*% t(X))
    vc <- sX * sum(fit$residuals^2)  
    #vc <- solve(t(X) %*% X) * sum(fit$residuals^2)/(length(X[,1])-length(X[1,]))    
    tval <- fit$be[,1] / sqrt(diag(vc))    
    tr[[length(tr)+1]] <- tval[2] #summary(fit)$coef[2,3]

    res <- list()
    stopW <- B
    for (i in 2:B) {
	if (i %% 10 == 0) { 
	    cat (paste0("\r ", round(i/B*100),"%  ")) 
	}

	tryCatch({
	    uhat <- multE(resTransf2(fit$residuals, fit$be, hv, type=typeRes), type=typeEps)    

	    tmp <- data

	    prd <- X %*% as.matrix(fit$be) 
	    tmp[,obj@depVar] <- prd+sample(uhat, replace=T) 
	    fitTmp <- lmfit(X, tmp[,obj@depVar])

	    if (!any(is.na(prd)) && !any(is.infinite(prd)) && !any(is.na(uhat))) {
		if (length(res) > 0 && (
					any(uhat > trunc*max(res[[1]])) ||
					    any(abs(uhat) < 1/trunc*abs(min(res[[1]])))

					)) {
		    stopW <- i
		    break
		}

		data <- tmp
		fit <- fitTmp

		## TODO: check if covar is selected 
		#vc <- solve(t(X) %*% X) * sum(fit$residuals^2)/(length(X[,1])-length(X[1,]))
		vc <- sX * sum(fit$residuals^2)
		tval <- fit$be[,1] / sqrt(diag(vc))

		tr[[length(tr)+1]] <- tval[2]

		res[[length(res)+1]] <- uhat
	    }
	}, error=function(e) {  })
    }

    tr <- unlist(tr)
    trFull <- unlist(tr)

    return(list(tVal=tr, stopW=stopW, stopFact=trunc, res=res, tValFull=trFull))
}
