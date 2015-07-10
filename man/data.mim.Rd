\name{data.anoint}
\alias{data.anoint}

\title{Generate a clinical trial data set under a specified multiple interaction model}

\description{

  Returns a simulated equal-allocation, parallel-group clinical trial with possibly multiple interactions among prognostic factors.
  }

\usage{
data.anoint(alpha, beta, gamma, mean, vcov, n=100, event=.8,
type = c("binomial","survival"))
}

\arguments{
  \item{alpha}{vector specifying control and treatment group intercept}
  \item{beta}{vector specifying prognostic factor main effects}
  \item{gamma}{vector specifying modification of prognostic effects in the presence of treatment}
  \item{mean}{vector of covariate mean to be supplied to \code{mvrnorm} of the \code{MASS} package.}
  \item{vcov}{matrix of variance-covariance matrix of prognostic covariates to be supplied to \code{mvrnorm}}
  \item{n}{number of subjects in each treatment arm}
  \item{event}{proportion of observed events when \code{type} is "survival"}
  \item{type}{string indicating type of response variable}
}

\value{
Data frame with \code{y}, \code{trt}, and \code{V1},...,\code{VK} prognostic factors. The data frame also has \code{event} with a time-to-event reponse, which is an indicator for an observed event.
}

\details{
When \code{type} is "survival", the parameters specify the log-rate for an exponentially distributed random variable. Censored times are non-informatively right-censored. When \code{type} is "binomial", the parameters specify a log-odds model.
}

\author{S. Kovalchik \email{s.a.kovalchik@gmail.com}}


\examples{

null.interaction <- data.anoint(
                             alpha = c(log(.2/.8),log(.2*.75/(1-.2*.75))),
                             beta = c(1.5,2),
                             gamma = rep(1,2),
                             mean = c(0,0),
                             vcov = diag(2),
                             type="binomial"
                             )

fit <- glm(y~(.)*trt,data=null.interaction,family="binomial")

summary(fit)


null.interaction <- data.anoint(
                             alpha = c(log(.5),log(.5*.75)),
                             beta = c(1.5,2),
                             gamma = rep(1,2),
                             mean = c(0,0),
                             vcov = diag(2),
                             type="survival",
                             event = .7
                             )

fit <- coxph(Surv(y, event)~(.)*trt,data=null.interaction)

summary(fit)
}
