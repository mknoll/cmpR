bootMCFast2 <- function(obj, B=1000, typeRes="HC3", typeEps="Liu1988", trunc=1000) {
    frm <- obj@formula
    frmS <- as.formula(paste("~1", all.vars(frm)[-1], sep="+"))

    data <- obj@data
    X <- model.matrix(frmS, data=data)

    ### reference
    fit <- lmfit(X, data[,obj@depVar])
    #vc <- solve(t(X) %*% X) * sum(fit$residuals^2)/(length(X[,1])-length(X[1,]))
    sX <- solve(t(X) %*% X)/(length(X[,1])-length(X[1,]))
    hv <-diag(X %*% solve(t(X) %*% X) %*% t(X))
    vc <- sX * sum(fit$residuals^2)
    tval <- fit$be[,1] / sqrt(diag(vc))

    chain <- array(dim=c(B+1, 2))    
    chain[1,] <- fit$be[,1] #c(summary(fit)$coef[1,1], summary(fit)$coef[2,1])    
    tr <- numeric(B)    
    #tr[1] <- summary(fit)$coef[2,3]    
    tr[1] <- tval[2] #summary(fit)$coef[2,3]    
    ac <- 0    
    res <- list()

    origD <- data
    stopW <- B
    for (i in 1:(length(chain[,1])-1)) {    
	if (i %% 10 == 0) {    
	    cat (paste0("\r ", round(i/B*100),"%  "))    
	}    

	proposal <- proposalfunction(chain[i,])

	probab <- exp(posterior(proposal, data[,obj@covar], data[,obj@depVar]) - posterior(chain[i,], data[,obj@covar], data[,obj@depVar]))

	if (!is.na(probab) && runif(1) < probab) {
	    #data$RES <- sample(multE(resTransf(fit, type=typeRes), type=typeEps), replace=T)    
	    data$RES <- sample(multE(resTransf2(fit$residuals, fit$be, hv, type=typeRes), type=typeEps), replace=T)    
	    if (any(is.na(data$RES))) {
		next
	    }

	    ### truncate
	    if (length(res) > 0 && any(data$RES > trunc*max(res[[1]]))) {
		tr <- tr[1:(i-1)]
		stopW <- i
		break
	    }

	    chain[i+1,] = proposal 
	    ac <- ac+1

	    prd <- X %*% as.matrix(fit$be)
	    data[,obj@depVar] <- prd+data$RES
	    #data[,obj@depVar] <- predict(fit, newdata=data)+data$RES

	    #fit <- lm(frm, data=data)
	    fit <- lmfit(X, data[,obj@depVar])
	} else{
	    chain[i+1,] = chain[i,]
	}
	
	#add either updated (accept) or old t-value
	## TODO: check if correct covar is selected
	vc <- solve(t(X) %*% X) * sum(fit$residuals^2)/(length(X[,1])-length(X[1,]))
	vc <- sX* sum(fit$residuals^2)
	tval <- fit$be[,1] / sqrt(diag(vc))

	#tr[i] <- summary(fit)$coef[2,3]
	tr[i] <- tval[2]

	res[[length(res)+1]]<- data$RES

    }

    ## truncate    
    tr <- unlist(tr)    
    trFull <- tr
    #stopW <- length(res)

    return(list(tVal=tr, stopW=stopW, chain=chain, accRatio=ac/B, res=res, tValFull=trFull))
}


proposalfunction <- function(param, sd=c(0.5,1)) {    
    return(rnorm(2, mean=param[1:2], sd=sd))    
}    
    
likelihood <- function(param, x, y) {    
    #pred <- param[1] +df$GRP*param[2]     
    #sum(dnorm(df$VAL-pred, log=T))    
    pred <- param[1] +x*param[2]     
    sum(dnorm(y-pred, log=T))    
}    
posterior <- function(param,x,y) {    
    likelihood(param, x,y)+prior(param)    
}    
prior <- function(param) {    
    beta0_prior <- param[1]    
    beta1_prior <- param[2]    
    
    b0_prior <- dunif(beta0_prior, min=-2, max=2, log=T)    
    b1_prior <- dnorm(beta1_prior, sd=2, log=T)    
    
    
    return(b0_prior+b1_prior)    
}    

