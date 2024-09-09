setClass("anoint.fit",
	representation(
		K = "numeric",
		responsiveness = "list",
		tests = "list",
		pvalues = "list",
		fits = "list"
	)
)


anointfit.table <- 	function(x,...){
		test.table <- cbind(unlist(x@tests))
		row.names(test.table) <- c(
			"OBO",
			"OBO (adj.)",
			"UIM",
			"PIM (exact)",
			"PIM (approx)",
			"PIM/OBO (adj.)",
			"PIM/UIM"
		)
		colnames(test.table) <- "Global null rejected"

test.table
}
	
	
setMethod("print","anoint.fit",function(x,...) print(anointfit.table(x)))

setGeneric("show", function(object) {})
setMethod("show","anoint.fit",function(object) print(anointfit.table(object)))

anointfit.summary <- function(object, 
                              type=c("all", "obo","uim","pim.exact","pim.approx"),
                              ...) {
	
  type = match.arg(type)
  
  test.table <- anointfit.table(object)
	
	rowN = switch(type,
	             "all" = c(1, 2, 3, 4, 5, 6, 7), 
	             "obo" = c(1,2), 
	             "uim" = c(3), 
	             "pim.exact" = c(4), 
	             "pim.approx" = c(5))
	
	rowP = switch(type,
	              "all" = c(1, 2, 3, 4),
	              "obo" = c(1),
	              "uim" = c(2),
	              "pim.exact" = c(3),
	              "pim.approx" = c(4))
	
	pvalues <- cbind(unlist(object@pvalues))
	
	row.names(pvalues) <- c(
		"OBO (max)",
		"UIM",
		"PIM (exact)",
		"PIM (approx)"
	)
	
	colnames(pvalues) <- "Global LRT (p-value)"

	print(test.table[rowN,,drop=F])
	cat("\n")
	print(pvalues[rowP,,drop=F])
	
	cat("\n")
	if (type %in% c("uim", "obo")) {
	  print(summary(object@fits[[type]]$fit[[1]]))
	} else if (type %in% c("pim.exact", "pim.approx")) {
	  print(pim.summary(object@fits[[type]]))
	}
	
	invisible(list(tests=object@tests,pvalues=object@pvalues))	
}

setMethod("summary","anoint.fit",anointfit.summary)

anointfit.fit <- function(object,type=c("obo","uim","pim.exact","pim.approx")){
	object@fits[type]
}

setGeneric("fits",function(object,...)standardGeneric("fits"))

setMethod("fits","anoint.fit",anointfit.fit)
