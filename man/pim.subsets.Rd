\name{pim.subsets}
\alias{pim.subsets}

\title{Perform all subsets proportional interactions modeling}

\description{

Computes all possible proportional interactions model among \code{p} covariates.

}

\usage{
pim.subsets(formula,trt,data,family="binomial",na.action=na.omit,fwer=0.05,...)
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
	\item{interaction}{value of the interaction effect of the proportional interaction model, see details}
	\item{LRT}{value of likelihood ratio test of proportional interaction}
        \item{lower}{lower endpoints of 95 percent confidence interval for interaction parameter}
	\item{upper}{upper endpoints of 95 percent confidence interval for interaction parameter}
	\item{pvalue}{pvalue for 1-df chi-squared test}
	\item{include.exclude.matrix}{matrix of same rows as subsets and columns as covariates with logical entries indicating which covariates (columns) were include in which subset model (row)}
	\item{covariates}{vector of covariate names as in formula}
	\item{reject}{indicator of rejected hypotheses using a multiple testing correction such that familywise error is controlled at level \code{fwer}}.
	}
}

\details{
Under the proportional interaction model the \code{coef} of the main covariate effects in the control arm are multiplied by the \code{interaction} effect to get the covariate effects for the treatment group.
}

\author{Stephanie Kovalchik <s.a.kovalchik@gmail.com>}


\references{
Follmann DA, Proschan MA. A multivariate test of interaction for use in clinical trials. \emph{Biometrics} 1999; 55(4):1151-1155
}

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

pim.subsets(Surv(y, event)~V1+V2,trt="trt",data=null.interaction,family="coxph")


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

pim.subsets(y~V1+V2+V3,trt="trt",data=pim.interaction,family="binomial")
}
