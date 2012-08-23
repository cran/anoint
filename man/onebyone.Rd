\name{obo}
\alias{obo}

\title{Perform one-by-one (OBO) estimates of treatment-covariate interaction}

\description{

Fits GLM of Cox regression models with a treatment-covariate interaction separately for each covariate specified in \code{anoint} object.
}

\usage{
obo(anoint,...)
}

\arguments{
  \item{anoint}{object of class \code{anoint}}
  \item{...}{other arguments passed to \code{glm} or \code{coxph}}
}

\value{
A list of model fits (\code{fit}), interaction likelihood-ratio statistics (\code{LRT}), and pvalues per interaction test (\code{pvalue}).
}


\author{Stephanie Kovalchik \email{s.a.kovalchik@gmail.com}}

\seealso{\link{anoint}}

\examples{
data(simsolvd)

obj <- anoint(Surv(ttodthorchfhosp,censor==0)~(age+beat+lvef)*trt,
				data = simsolvd, family = "coxph")

fits <- obo(obj)

fits

lapply(fits$fit, summary)
}
