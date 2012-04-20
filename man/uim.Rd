\name{uim}
\alias{uim}

\title{Perform unrestricted multiple treatment-covariate interaction regression}

\description{

Fits GLM of Cox regression analysis for model with all pairwise interactions between treatment and covariates specified in \code{anoint} object.
}

\usage{
uim(object,...)
}

\arguments{
  \item{object}{object of class \code{anoint}}
  \item{...}{other arguments passed to \code{glm} or \code{coxph}}
}

\value{
List of model fit of \code{glm} or \code{coxph} class (\code{fit}), likelihood-ratio test of global test of treatment-covariate interaction (\code{LRT}), and LRT p-value for global test of interaction (\code{pvalue}).
}


\author{Stephanie Kovalchik <s.a.kovalchik@gmail.com>}

\seealso{\link{glm},\link{coxph},\link{anoint}}

\examples{
data(simsolvd)

obj <- anoint(Surv(ttodthorchfhosp,censor==0)~(age+beat+lvef)*trt,
				data = simsolvd, family = "coxph")

fit <- uim(obj)

fit 

summary(fit$fit)
}
