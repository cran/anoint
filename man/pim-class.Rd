\name{pim-class}
\docType{class}
\alias{pim-class}

\title{Class "pim"}

\description{
Class for proportional interactions model (PIM).
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pim", ...)}. 
}

\section{Slots}{
  \describe{
      \item{\code{formula}:}{object of code{anoint} class}
      \item{\code{coef}:}{list of baseline (alpha), control prognostic effects (beta), and responsiveness parameter (theta)}
      \item{\code{exact}:}{logical indicator whether \code{exact} fit used}
      \item{\code{LRT}:}{likelihood ratio test statistic of responsiveness parameter}
      \item{\code{interval}:}{numeric vector supplied to \code{optimize} if \code{exact} fit is used}
      \item{\code{boot.pim}:}{matrix of bootstrap estimates of PIM coefficients}
      \item{\code{vcov}:}{matrix of bootstrap variance-covariance}
 }
}

\section{Methods}{

  \describe{

\item{show}{\code{signature(object = "pim")}:
Display point estimates of \code{pim} object.
  }

\item{print}{\code{signature(x = "pim",...)}:
Display point estimates of \code{pim} object.
  }

\item{summary}{\code{signature(object = "pim",...)}:
List of estimates and convergence information.
  }

\item{coef}{\code{signature(object = "pim")}:
Extractor for MLEs. 
  }

\item{confint}{\code{signature(object = "pim",parm,level=0.95,...)}:
Computes confidence interval for specified parameters and level. Intervals are based on bootstrap resamples of PIM model. 
  }
  

\item{predict}{\code{signature(object = "pim")}:
Returns linear predictor for each subject.
  }

  \item{vcov}{\code{signature(object = "pim")}:
Extractor for variance-covariance matrix which is estimated with bootstrap resampling.
  }
 }
}

\author{Stephanie Kovalchik \email{s.a.kovalchik@gmail.com}}

\seealso{\link{pim}}

\keyword{classes}
