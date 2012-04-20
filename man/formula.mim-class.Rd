\name{formula.anoint-class}
\docType{class}
\alias{formula.anoint-class}

\title{Class "formula.anoint"}

\description{
Formula class for \code{anoint} objects
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("formula.anoint", ...)}. 
}

\section{Slots}{
  \describe{
      \item{\code{formula}:}{formula supplied to \code{anoint}}
      \item{\code{uni}:}{list of all univariate interaction model formulas}
      \item{\code{prognostic}:}{formula of subgroup main effects}
      \item{\code{prognostic.trt}:}{formula of subgroup and treatment main effects model (no interactions)}
      \item{\code{trt}:}{character string of treatment variable}
      \item{\code{family}:}{character string describing model family}
 }
}

\section{Methods}{

  \describe{

\item{show}{\code{signature(object = "formula.anoint")}:
Display main formula of \code{formula.anoint} object.
  }

\item{print}{\code{signature(x = "formula.anoint",...)}:
Display main formula of \code{formula.anoint} object.
  }

\item{update}{\code{signature(object = "formula.anoint",...)}:
Updates formula with same syntax as supplied to \code{anoint} function.
  }
 }
}

\author{Stephanie Kovalchik \email{s.a.kovalchik@gmail.com}}

\seealso{\link{anoint}}

\keyword{classes}
