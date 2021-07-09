resTransf <- function(fit, type="HC3") {  
    if (type == "HC0") {
	fit$residuals
    } else if (type == "HC1") {
	n <- length(fit$residuals)
	k <- length(summary(fit)$coef[,1]) 
	fit$residuals * (n/(n-k))^0.5
    } else if (type == "HC2") {
	fit$residuals/(1-hatvalues(fit))^0.5
    } else if( type == "HC3") {
	fit$residuals/( 1-hatvalues(fit)) 
    } else if (type == "HC4") {
	n <- length(fit$residuals)
	p <- length(summary(fit)$coef[,1]) 
	res <- fit$residuals
	hv <- hatvalues(fit)
	vc <- n*hv/p
	delta <- sapply(vc, function(x) min(4,x))
	res/(1-hv)^delta 
    } else if (type == "HC4m") {
	n <- length(fit$residuals)
	p <- length(summary(fit)$coef[,1]) 
	res <- fit$residuals
	hv <- hatvalues(fit)
	vc <- n*hv/p
	gamma1 <- 1.0 #suggested value
	v1 <- sapply(gamma1, function(x) min(x,vc))
	gamma2 <- 1.5 #suggested value
	v2 <- sapply(gamma2, function(x) min(x,vc))
        delta <- v1+v2
	res/(1-hv)^delta
    } else if (type == "HC5") {
	n <- length(fit$residuals)
	p <- length(summary(fit)$coef[,1]) 
	res <- fit$residuals
	hv <- hatvalues(fit)
	k <- 0.7 #suggested value
	vc <- n*hv/p
	vc2 <- n*k*max(hv)/p 
	vc2_2 <- sapply(vc2, function(x) max(4, x))
	delta <- sapply(vc, function(x) min(vc, vc2_2))
	res/((1-hv)^delta)^0.5
    } else {
	stop("Unknown type!")
    }
}

resTransf2 <- function(res, X, hv, type="HC3") {  
    if (type == "HC0") {
	res
    } else if (type == "HC1") {
	n <- length(res)
	k <- length(X[1,]) 
	res * (n/(n-k))^0.5
    } else if (type == "HC2") {
	res/(1-hv)^0.5
    } else if( type == "HC3") {
	res/( 1-hv) 
    } else if (type == "HC4") {
	n <- length(X[,1])
	p <- length(X[1,])
	vc <- n*hv/p
	delta <- sapply(vc, function(x) min(4,x))
	res/(1-hv)^delta 
    } else if (type == "HC4m") {
	n <- length(X[,1])
	p <- length(X[1,])
	vc <- n*hv/p
	gamma1 <- 1.0 #suggested value
	v1 <- sapply(gamma1, function(x) min(x,vc))
	gamma2 <- 1.5 #suggested value
	v2 <- sapply(gamma2, function(x) min(x,vc))
        delta <- v1+v2
	res/(1-hv)^delta
    } else if (type == "HC5") {
	n <- length(X[,1])
	p <- length(X[1,])
	k <- 0.7 #suggested value
	vc <- n*hv/p
	vc2 <- n*k*max(hv)/p 
	vc2_2 <- sapply(vc2, function(x) max(4, x))
	delta <- sapply(vc, function(x) min(vc, vc2_2))
	res/((1-hv)^delta)^0.5
    } else {
	stop(paste("Unknown type: ", type))
    }
}


multE <- function(x, type="Liu1988") { 
    if (type == "Liu1988") {
	fact <- ifelse(runif(1) > 0.5, -1, 1) 
    } else if (type == "Mammen1993") {
	val <- c(-(sqrt(5)-1)/2, (sqrt(5)+1)/2)
	prob <- (sqrt(5)+1)/(2*sqrt(5))
	fact <- sample(val, 1, prob=c(prob, 1-prob))
    } else {
	stop(paste("Unknown type: ", type))
    }

    x*fact
}
