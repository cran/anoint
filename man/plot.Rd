\name{plot}
\docType{methods}
\alias{plot}
\alias{plot,anoint-method}

\title{Prognostic response plot (PR-plot) for \code{anoint} class.}

\description{
Computes the prognostic score (baseline risk) based on the covariates of \code{anoint} or a supplied set of predictions. Risk scores are binned into ten groups based on risk deciles and a treatment effect (and confidence interval) is estimated in each group. This is compared to the overall treatment effect which is indicated by the shaded region.
}

\section{Methods}{
\describe{

 \item{plot}{\code{signature(object = "anoint",predict=NULL,fun=exp,...)}:
Prognostic response plot.
  }

 }
}

\details{
Additional arguments are passed to \code{glm} or \code{coxph}.
}

\author{S. Kovalchik \email{s.a.kovalchik@gmail.com}}


\keyword{methods}

\examples{
set.seed(11903)

# BINOMIAL EVENT DATA WITH 4 NORMAL PROGNOSTIC FACTORS
pim.interaction <- data.anoint(
                             alpha = c(log(.2/.8),log(.2*.75/(1-.2*.75))),
                             beta = log(c(1.5,1.1,2,1.3)),
                             gamma = rep(1.5,4),
                             mean = rep(0,4),
                             vcov = diag(4),
                             type="binomial", n = 500
                             )

object <- anoint(y~(V1+V2+V3+V4)*trt,data=pim.interaction)

plot(object,bty="n",las=1)

# PLOT TREATMENT EFFECT ON LINEAR PREDICTOR SCALE
plot(object,fun=function(x)x,bty="n",las=1,ylab="treatment effect (linear predictor)")

}
