\name{anoint.fit}
\docType{class}
\alias{anoint.fit}
\alias{anoint.fit-class}

\title{Fits and global tests of analysis of interaction models}

\description{

Fits one-by-one (OBO), unrestricted (UIM), and proportional interaction (PIM) regression models to investigate multiple treatment response factors in a parallel-group clinical trial.

}


\section{Objects from the Class}{
Objects can be created by calls of the form \code{anoint.fit(object, level = .05, interval=c(.5,3))}
}

\arguments{
  \item{object}{object of \code{anoint} class}
  \item{level}{significance level for global interaction tests}
  \item{interval}{interval of possible values for responsiveness parameter of PIM}
}

\section{Slots}{

\describe{
\item{\code{K}}{number of prognostic factors}
\item{\code{responsiveness}}{list with exact and approximate estimates of PIM responsiveness parameter}
\item{\code{tests}}{list of global interaction test results}
\item{\code{pvalues}}{list of pvalues on which test rejections are based}
\item{\code{fits}}{list of fitted models for each \code{anoint} method}
}

Components of \code{tests} are the results of the global tests of interaction:

\describe{
\item{\code{obo.reject}}{Result of unadjusted one-by-one global test of interaction. Null is no effect modification for K subgroups, the alternative is at least one K is an effect modifier.}
\item{\code{obo.adjust}}{Same as \code{obo.reject} but with Bonferroni-correction for K comparisons}
\item{\code{uim.reject}}{Result of UIM global test of interaction. Null is no effect modification for K subgroups, the alternative is at least one K is an effect modifier.}
\item{\code{pim.exact.reject}}{Result of PIM exact global test of interaction. Null is no \emph{proportional} effect modification (theta responsiveness parameter = 1) against the alternative that the treatment responsiveness parameter theta is not equal to 1.}
\item{\code{pim.approx.reject}}{Same as \code{pim.exact.reject} but using approximate method.}
\item{\code{pim.obo}}{Two-stage global test. First stage tests PIM using an exact method at \code{level/2} significance. If not rejected, the second stage is a test of adjusted OBO with a second-stage global \code{level/2} significance.}
\item{\code{pim.uim}}{Same as \code{pim.obo} but with UIM at the second stage.}
}

Components of \code{pvalues} on which the global tests are based:
\describe{
\item{\code{obo.p}}{p-value for the maximum LRT of the one-by-one testing}
\item{\code{uim.p}}{p-value for the global LRT of any interaction base on UIM}
\item{\code{pim.exact.p}}{p-value for the test of proportional interaction using the PIM exact method}
\item{\code{pim.approx.p}}{p-value for the test of proportional interaction using the PIM approximate method}
 }

Components of \code{fits} are the models underlying the global interaction tests:
\describe{
	\item{\code{obo}}{Univariate interaction regression models of each subgroup.}
	\item{\code{uim}}{Full regression model with all pairwise treatment-covariate interactionns}
	\item{\code{pim.exact}}{Proportional interactions model with exact fit}
	\item{\code{pim.approx}}{Proportional interactions model with asymptotic approximate estimation}
}

}


\section{Methods}{

  \describe{

\item{show}{\code{signature(object = "anoint.fit")}:
Display table of results of global test of interaction.
  }

\item{print}{\code{signature(x = "anoint.fit",...)}:
Display table of results of global test of interaction.
  }

\item{summary}{\code{signature(object = "anoint.fit",...)}:
Display results of global test of interaction and p-values. Returns list with \code{tests} and \code{pvalues}.
  }

\item{fits}{\code{signature(object = "anoint.fit",type=c("obo","uim","pim.exact","pim.approx")}:
Extracts the specified fitted object from a \code{anoint.fit}.
  }
 }
}

\details{
The global tests for the presence of treatment response factors (treatment-covariate interaction) are one-stage or two-stage likelihood ratio tests.

The fitted multiple interaction models include: one-by-one univariate interaction models (OBO), a full unrestricted model with all pairwise treatment-covariate interactions (UIM), and a proportional interactions model (PIM) fit with an exact or asymptotic approximate estimate for the likelihood ratio test and responsiveness parameter, \code{theta}.
}

\author{S. Kovalchik \email{s.a.kovalchik@gmail.com}}

\seealso{\link{anoint},\link{anoint-class},\link{obo},\link{uim},\link{pim}}


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

object <- anoint(Surv(y, event)~(V1+V2)*trt,data=null.interaction,family="coxph")

fit <- anoint.fit(object)

summary(fit)

fits(fit,type="obo")
}
