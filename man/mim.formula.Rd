\name{anoint.formula}
\alias{anoint.formula}

\title{Create a \code{formula.anoint} object}

\description{

Create formula for use with \code{anoint}.

}

\usage{
anoint.formula(formula=y~(a+b)*trt,family="binomial")
}

\arguments{
  \item{formula}{formula specifying analysis of interaction model}
  \item{family}{string indicating model family, should be one of \code{glm} families or \code{coxph}}
}

\value{
Returns instance of \code{formula.anoint} class.
}

\details{
The \code{formula} should have all subgroups enclosed in parantheses and the treatment variable as an interaction, as shown in default.
}

\author{Stephanie Kovalchik <s.a.kovalchik@gmail.com>}

\seealso{\link{formula.anoint-class},\link{anoint}}

\examples{

anoint.formula()

}
