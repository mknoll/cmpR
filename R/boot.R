resTransf <- function(fit, type="HC3") {  
    if (type == "HC0") {
	fit$residuals
    } else if (type == "HC1") {
	n <- length(fit$residuals)
	#k <- 2 #FIXME
	k <- length(summary(fit)$coef[,1]) 
	fit$residuals * (n/(n-k))^0.5
    } else if (type == "HC2") {
	fit$residuals/(1-hatvalues(fit))^0.5
    } else if( type == "HC3") {
	fit$residuals/( 1-hatvalues(fit)) 
    } else {
	stop("Unknown type!")
    }
}

resTransf2 <- function(res, X, hv, type="HC3") {  
    if (type == "HC0") {
	res
    } else if (type == "HC1") {
	n <- length(res)
	#k <- 2 #FIXME
	k <- length(X[1,]) 
	res * (n/(n-k))^0.5
    } else if (type == "HC2") {
	#hv <-diag(X %*% solve(t(X) %*% X) %*% t(X))
	res/(1-hv)^0.5
    } else if( type == "HC3") {
	#hv <-diag(X %*% solve(t(X) %*% X) %*% t(X))
	res/( 1-hv) 
    } else {
	stop("Unknown type!")
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
	stop("Unknown type!")
    }

    x*fact
}
