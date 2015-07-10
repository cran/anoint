\name{anoint}
\alias{anoint}
\alias{anoint-class}

\title{Create analysis of interactions object}

\description{

Prepares formula and data to be used in methods with the analysis of interactions class.

}

\usage{
anoint(formula,data,family="binomial",select=NULL,nfolds=10,
  type.measure="deviance",keep.vars=NULL,na.action=na.omit,...)
}

\arguments{
  \item{formula}{analysis of interaction formula for \code{glm} or \code{coxph}, see details}
  \item{data}{data.frame containing the variables of \code{formula}}
  \item{family}{character specifying family of \code{glm} or character "coxph" if \code{coxph} model is fit}
  \item{select}{character for type of selection to perform, either "\code{stepAIC}" or "\code{glmnet}".} 
  \item{nfolds}{number of folds used in cross-validation to find lasso penalty parameter when \code{select} is set to \code{TRUE}. Used only when \code{select} is \code{glmnet}. See \link{cv.glmnet}}
  \item{type.measure}{loss to use for cross-validation. Used only when \code{select} is \code{glmnet}. See \link{cv.glmnet}}
  \item{keep.vars}{vector of names of variables to retain if selection procedure is used. Used only when \code{select} is \code{glmnet}.}
  \item{na.action}{function, \link{na.action} to perform for handling observations with missing variables among variables in formula. Default is \code{na.omit}}
  \item{...}{additional arguments passed to \code{cv.glmnet} when \code{select} is \code{glmnet} or \code{stepAIC} when \code{select} is \code{stepAIC}.}
}

\value{
Returns instance of \code{anoint} class.
}

\details{
To test proportional multiple interactions between treatment variable indicator \code{z} (binary, 0 or 1) and variables \code{a}, \code{b}, with response \code{y} of a GLM model, \code{formula} must be \code{y~(a+b)*z}. If a Cox model with event time \code{time} and event indicator \code{event}, \code{formula} is \code{Surv(time,event)~(a+b)*z}.

Factors should not be included as \code{a} or \code{b} because this could change how the reference group is represented in the model. Separate 0/1 dummy variables must be supplied by the user. 

When \code{select} is \code{glmnet} a Lasso method (\code{cv.glmnet}) is used to select prognostic factors using 10-fold cross-validation with the control data only. If \code{select} is set to \link{stepAIC} a stepwise selection procedure is used with specifications based on arguments passed to \code{...}.
}

\author{Stephanie Kovalchik <s.a.kovalchik@gmail.com>}

\seealso{\link{data.anoint},\link{cv.glmnet}}

\references{
Follmann DA, Proschan MA. A multivariate test of interaction for use in clinical trials. \emph{Biometrics} 1999; 55(4):1151-1155
}

\examples{

# NO INTERACTION CONDITION, LOGISTIC MODEL

null.interaction <- data.anoint(
                             alpha = c(log(.5),log(.5*.75)),
                             beta = log(c(1.5,2)),
                             gamma = rep(1,2),
                             mean = c(0,0),
                             vcov = diag(2),
                             type="survival", n = 500
                             )

head(null.interaction)

object <- anoint(Surv(y, event)~(V1+V2)*trt,data=null.interaction,family="coxph")

object

summary(object)

# NO INTERACTION CONDITION, WITH PROGNOSTIC SELECTION

null.interaction <- data.anoint(
                             alpha = c(log(.2/.8),log(.2*.75/(1-.2*.75))),
                             beta = c(1.5,2,0,0),
                             gamma = rep(1,4),
                             mean = rep(0,4),
                             vcov = diag(4),
                             type="binomial", n = 500
                             )

head(null.interaction)

object <- anoint(y~(V1+V2+V3+V4)*trt,data=null.interaction,select="glmnet")

summary(object)

# FORCE V1, V2 INTO THE MODEL; INTERCEPT IS ALWAYS THE FIRST TERM OF MODEL
object <- anoint(y~(V1+V2+V3+V4)*trt,data=null.interaction,
			select="glmnet",keep.vars=c("V1","V2"))

summary(object)

# SELECTION WITH STEPWISE SELECTION AND AIC CRITERION
object <- anoint(y~(V1+V2+V3+V4)*trt,data=null.interaction,
			select="stepAIC")

summary(object)

}
