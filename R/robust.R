#' @title Robust tests
#' 
#' @import MASS
#' @import sfsmisc
#' @import robustbase
#' @import lmtest
#' @import sandwich
#' 
#' @export
robust <- function(obj) {
    coll <-list()

    frm <- obj@formula

    ## standard
    fit <- lm(frm, data=obj@data)
    coll[[length(coll)+1]] <- list(type="stats::lm", fit=summary(fit)$coefficients)

    res <-coeftest(fit, vcov = vcovHC(fit, "HC0"))    # robust; HC0 
    coll[[length(coll)+1]] <- list(type="HC0", fit=res)

    res <-coeftest(fit, vcov = vcovHC(fit, "HC1"))    # robust; HC1
    coll[[length(coll)+1]] <- list(type="HC1", fit=res)

    res <-coeftest(fit, vcov = vcovHC(fit, "HC2"))    # robust; HC2
    coll[[length(coll)+1]] <- list(type="HC2", fit=res)

    res <-coeftest(fit, vcov = vcovHC(fit, "HC3"))    # robust; HC3
    coll[[length(coll)+1]] <- list(type="HC3", fit=res)

    res <-coeftest(fit, vcov = vcovHC(fit, "HC4"))    
    coll[[length(coll)+1]] <- list(type="HC4", fit=res)

    res <-coeftest(fit, vcov = vcovHC(fit, "HC4m"))    
    coll[[length(coll)+1]] <- list(type="HC4m", fit=res)

    res <-coeftest(fit, vcov = vcovHC(fit, "HC5"))    
    coll[[length(coll)+1]] <- list(type="HC5", fit=res)

    val <- NA
    tryCatch({
	fit <- lmrob(frm, data=obj@data)
	val <- summary(fit)$coefficients
    }, error=function(e) {})
    coll[[length(coll)+1]] <- list(type="robustbase::lmrob", fit=val)

    fit <- rlm(frm, data=obj@data)
    #coll[[length(coll)+1]] <- list(type="MASS::rlm", fit=summary(fit))
    ## TODO: check var paramaeter

    val <-NA 
    tryCatch({
	fit <- f.robftest(fit) 
	val <- fit$p.value
    }, error=function(e) { } )
    coll[[length(coll)+1]] <- list(type="sfsmisc::f.robftest", fit=val)


    return(coll)
}
