\name{forest}
\alias{forest}

\title{Construct a forestplot from a \code{anoint} object}

\description{

Constructs a forestplot displaying the treatment effect within subgroups for each set of specified categorical variables. Provides a visual comparison of subgroup treatment effect to overall treatment effect, without adjustment for confounding factors or multiplicity.

Includes unadjusted p-values for each subgroup's treatment-covariate interaction test for glm or Cox regression models.
}

\usage{

forest(object,terms=NULL,x.axis=NULL,labels=NULL,fun=exp,...)
}

\arguments{
  \item{object}{object of \code{anoint} class}
  \item{terms}{numeric index indicating which terms in prognostic model to include in plot}
  \item{x.axis}{Points for tick marks of the bottom axis}
  \item{labels}{Matrix of labels of the same rows as \code{onebyone}, columns printed from left to right}
  \item{fun}{functional transformation applied to treatment effects and confidence intervals}
  \item{...}{additional arguments passed to \code{foresplot} (see details)}
}

\details{

Additional arguments that can be specified include:

\describe{
  \item{pch.size}{one number of a vector of two elements indicating the (min, max) for the relative plotting symbols. These reflect the relative precision of the symbol estimates}
  \item{x.axis}{Points for tick marks of the bottom axis}
  \item{header}{Vector of names for the columns of \code{labels}}
  \item{main}{String of the title of plot}
}

If no values are specifed for the \code{x.axis}, eight values over the range of the CIs is used. 

If no values are specified for \code{labels}, the row names are used.

}

\value{
A labeled forestplot of subgroup treatment effects.
}


\author{Stephanie Kovalchik <s.a.kovalchik@gmail.com>}

\examples{
data(simsolvd)

obj <- anoint(Surv(ttodthorchfhosp, censor==0)~(nyha+cardratio+current)*trt,data=simsolvd,
       		       family="coxph")

forest(obj)

grid.newpage()

forest(obj,x.axis=seq(0.7,1.5,by=.2))


}
