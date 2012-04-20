select.glmnet <- function(object,index,data,family,nfolds=10,type.measure="deviance",...){
	
     	control <- data[data[,index]==0,]
		f <- object@prognostic
		X <- model.matrix(f,control)
		Y <- model.frame(f,control)[,1]

		if(family=="coxph"){

			if(ncol(Y)>2){
				Y<- cbind(time=Y[,2],status=Y[,3])
			}
			else{
				Y <- cbind(time=Y[,1],status=Y[,2])
			}
		}
		
			arg.list <- list(...)
			arg.list$x <- X
			arg.list$y <- Y
			arg.list$family <- ifelse(family=="coxph","cox",family)
			arg.list$nfolds <- nfolds
			arg.list$type.measure <- type.measure
			
			if(any(names(arg.list)=="penalty.factor")){
				K <- ncol(X)
				if(length(arg.list$penalty.factor)<K){
					warning("Penalty factor must include intercept. Fixing...")
					arg.list$penalty.factor <- c(1,arg.list$penalty.factor)
				}
			}

			# fit <- cv.glmnet(       
            					  # x=X,
            					  # y=response,...,
            					  # family="cox",
          				     	  # nfolds=nfolds,        
             				 	  # type.measure=type.measure
             			 # )
		# }
		# else{
			# X <- model.matrix(f,control)
			# Y <- model.frame(f,control)[,1]
		
			# fit <- cv.glmnet(       
            					  # x=X,
            					  # y=Y,...,
            					  # family=family,
          				     	  # nfolds=nfolds,        
             				 	  # type.measure=type.measure
             			 # )
		
		# }

	 fit <- do.call("cv.glmnet", arg.list)
  	 result <- coef(fit$glmnet.fit,s=fit$lambda.min)
	 selected <- row.names(result)[as.numeric(result)!=0]
	 
	if(length(grep("Intercept",selected))!=0)
		selected <- selected[-grep("Intercept",selected)]

	if(length(selected)==0)
		stop("No prognostic factors selected.")
		
selected
}
	
