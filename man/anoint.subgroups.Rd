\name{anoint.subgroups}
\alias{anoint.subgroups}

\title{Perform one-by-one subgroup analyses}

\description{

Computes all interaction effects one variable at a time.

}

\usage{
anoint.subgroups(formula,trt,data,family="binomial",na.action=na.omit,fwer=0.05,...)
}

\arguments{
  \item{formula}{formula for covariate model as given in \code{glm} or \code{coxph}, i.e. \code{y~x1+x2}}
  \item{trt}{character name of treatment assignment indicator}
  \item{data}{data.frame containing the variables of \code{formula} and \code{trt}}
  \item{family}{character specifying family of \code{glm} or character "coxph" if \code{coxph} model is fit}
  \item{na.action}{function, \link{na.action} to perform for handling observations with missing variables among variables in formula. Default is \code{na.omit}}
  \item{fwer}{numeric value for the desired familywise error rate, should be between 0 and 1.}
  \item{...}{additional arguments passed to \code{glm} or \code{coxph}}
}

\value{
Returns a list with 

\describe{
	\item{subset}{indicator of the covariates included in the fitted model}
	\item{interaction}{value of the of treatment-covariate interaction effect (using model with treatment-covariate product term)}
	\item{LRT}{value of likelihood ratio test of treatment-covariate interaction}
        \item{lower}{lower endpoints of 95 percent confidence interval for interaction parameter}
	\item{upper}{upper endpoints of 95 percent confidence interval for interaction parameter}
	\item{pvalue}{pvalue for 1-df chi-squared test}
	\item{include.exclude.matrix}{matrix of same rows as covariates and columns as covariates with logical entries indicating which covariates (columns) were include in the fitted model (row)}
	\item{covariates}{vector of covariate names as in formula}
	\item{reject}{indicator of rejected hypotheses using a Bonferroni multiple testing correction such that familywise error is controlled at level \code{fwer}}.
	}
}


\author{Stephanie Kovalchik <s.a.kovalchik@gmail.com>}

\examples{

set.seed(11903)

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

anoint.subgroups(Surv(y, event)~V1+V2,trt="trt",data=null.interaction,family="coxph")


# PROPORTIONAL INTERACTION WITH THREE COVARIATES AND BINARY OUTCOME

pim.interaction <- data.anoint(
			     n = 5000,
                             alpha = c(log(.2/.8),log(.2*.75/(1-.2*.75))),
                             beta = rep(log(.8),3),
                             gamma = rep(1.5,3),
                             mean = c(0,0,0),
                             vcov = diag(3),
                             type="binomial"
                             )

anoint.subgroups(y~V1+V2+V3,trt="trt",data=pim.interaction,family="binomial")
}
