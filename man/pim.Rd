\name{pim}
\alias{pim}

\title{Fit proportional interaction model}

\description{

Fits a proportional interactions model from parallel-group clinical trial.

}

\usage{
pim(object,exact=TRUE,interval=c(-3,3),n.boot=NULL,...)
}

\arguments{
  \item{object}{object of class \code{anoint}}
  \item{exact}{logical indicator whether \code{exact}  or approximate fit is performed}
  \item{interval}{interval passed to \code{optimize} if \code{exact} is \code{TRUE}}
  \item{n.boot}{number of bootstrap resamples for variance calculations}
  \item{...}{other arguments passed to \code{optimize} if \code{exact} is \code{TRUE}}
}

\value{
Returns instance of \code{pim} class.
}

\details{
When \code{exact} is \code{FALSE} the method of Follmann and Proschan (1999) is used to estimate the PIM coefficients and perform a likelihood-ratio test on the responsiveness parameter \code{theta}.

If \code{exact} method is specified, then \code{optimize} is used to maximize the profile-likelihood alternating between fixing \code{theta} and fixing all other PIM parameters. The arguments \code{interval} and additional arguments to \code{...} control the optimization with respect to \code{theta}.

When \code{n.boot} is \code{NULL} no bootstrap resamples are taken. In this case, when using the \code{exact} method the variance-covariance for the main effects is based on the model likelihood treating the responsiveness parameter as fixed. To include uncertainty measures for the responsiveness parameter, bootstrap resampling can be used. For the approximate method, only the bootstrap resampling variance is provided for the \code{vcov} and \code{confint} methods, which is invoked by specifying a positive integer number of samples or \code{n.boot}. 
}

\author{Stephanie Kovalchik <s.a.kovalchik@gmail.com>}

\references{
Follmann DA, Proschan MA. A multivariate test of interaction for use in clinical trials. \emph{Biometrics} 1999; 55(4):1151-1155
}

\seealso{\link{pim-class}}

\examples{

set.seed(1115)

pim.interaction <- data.anoint(
                             alpha = c(log(.2/.8),log(.2*.75/(1-.2*.75))),
                             beta = log(c(1.25,1.5)),
                             gamma = rep(1.2,2),
                             mean = c(0,0),
                             vcov = diag(2),
                             type="binomial", n = 500
                             )


object <- anoint(y~(V1+V2)*trt,data=pim.interaction,family="binomial")

object

fit <- pim(object)

fit

summary(fit)

# EXAMPLE WITH BOOTSTRAP 
fit <- pim(object, n=50)
summary(fit)
confint(fit)
}

