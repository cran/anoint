setClass("anoint",
		representation(
			formula = "formula.anoint",
			trt.index = "numeric",
			data = "data.frame",
			select = "logical"			
))


anoint <- function(formula,data,family="binomial",select=NULL,nfolds=10,type.measure="deviance",keep.vars=NULL,na.action=na.omit,...){
  data <- na.action(data[,all.vars(formula)]) # APPLY NA.ACTION
	f.anoint <- anoint.formula(formula,family=family)
	trt.index <- which(names(data)==f.anoint@trt)
          
	if(!is.null(select)){
		if(select=="stepAIC"){
			message("Performing selection procedure for prognostic model...\n")
			update.formula <- select.stepAIC(f.anoint,trt.index,data,family,...)
		}
		else if(select=="glmnet"){
		  message("Performing selection procedure for prognostic model...\n")
			update.formula <- select.glmnet(object=f.anoint,index=trt.index,data=data,family=family,nfolds=nfolds,type.measure=type.measure,keep=keep.vars,...)
		}		
		else{
			stop("Selection method not recognized.")
		}
		if(length(grep("trt",update.formula))>0)
			update.formula <- update.formula[-length(update.formula)]
		update.formula <- paste(update.formula,sep="",collapse="+")
		formula <- paste(as.character(formula)[2],"~",
		paste("(",update.formula,")*",f.anoint@trt,sep="",collapse=""),collapse="")
		message("Selected MIM:\n")
		formula <- formula(formula)
		message(formula,showEnv=FALSE)
		}
	
	new("anoint",
		formula = anoint.formula(formula,family=family),
		trt.index = trt.index,
		data = data,
		select = !is.null(select)
	)
}

setMethod("print","anoint",definition=
	function(x,...) print(x@formula)
)

setGeneric("show", function(object) {})
setMethod("show","anoint",
	function(object) print(object@formula)
)

setMethod("summary","anoint",
	function(object,...){
		
		candidates <- all.vars(object@formula@prognostic)
		response.index <- cbind(model.frame(object@formula@prognostic,object@data)[,1])
		response.index <- ncol(response.index)
		candidates <- candidates[-(1:response.index)]
		
	  cat(paste("MIM object with",length(candidates),"candidate response factors\n"))
	  cat(paste("Family:",object@formula@family,"\n"))
	  cat("Candidates:\t",paste(candidates,collapse=", "),"\n")
	
		print(object)
		
		formula.list <- list(
			onebyone = object@formula@uni,
			uim = object@formula@formula
		)
		
		invisible(formula.list)
		}
)
