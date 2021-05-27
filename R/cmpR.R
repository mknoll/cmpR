#' A S4 class representing a cmpR analysis
#'
#' @name cmpR-class
#' @exportClass cmpR
cmpR <- setClass("cmpR",
		 slots=c(data="data.frame",
			 formula="formula",
			 depVar="character",
			 covar="character"))


#' @title Constructor cmpR
#'
#' @description Constructor for new instance
#' 
#' @param .Object cmpR object
#' @param data data.frame with data to be evalauted
setMethod("initialize", "cmpR",
	  function(.Object,
		   data=data.frame,
		   formula=formula) {
	      .Object@data <- data
	      .Object@formula <- formula
	      tmp <- strsplit(as.character(formula), "~",fixed=T)
	      .Object@depVar <- tmp[[2]][1]
	      .Object@covar <- trimws(strsplit(trimws(tmp[[3]][1]), "+",fixed=T)[[1]][1])
	      .Object
	  })
