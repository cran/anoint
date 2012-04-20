\name{simsolvd}
\alias{simsolvd}
\docType{data}
\title{Simulated SOLVD-Trial data set}
\description{
A simulated clinical trial based on the design of the Studies of Left Ventricular Dysfunction Trial (SOLVD-T), a placebo-controlled trial of the angiotensin-converting-enzyme inhibitor enalapril for patients with congestive heart failure.
}

\usage{data(simsolvd)}
\format{
  A data frame with 2569 observations on the following 12 variables.
  \describe{
    \item{trt}{indicator for enalapril group}
    \item{age}{age at baseline (centered and scaled)}
    \item{beat}{pulse at baseline (centered and scaled)}
    \item{lymphocyte}{lymphocyte count at baseline (centered and scaled)}
    \item{lvef}{left ventricular ejection fraction at baseline (centered and scaled)}
    \item{noise}{simulated vector of random uniform variables} 
   \item{nyha}{indicator whether New York Heart Association score greater than 2}
    \item{cardratio}{indicator whether cardiothoracic ratio is greater than 0.5}
    \item{creatinine}{creatinine at baseline (centered and scaled)}
    \item{sodium}{sodium at baseline (centered and scaled)}
    \item{ttodthorchfhosp}{time to death or hospitalization in days}
    \item{censor}{indicator whether censored (1) or an event (0)}
    \item{current}{indicator whether current smoker}
  }
}
\source{
  Simulated data set based on the clinical study reported by:
  Yusuf, S. et al. (1991). Effect of Enalapril on Survival in Patients with Reduced Left-Ventricular Ejection Fractions and Congestive-Heart-Failure. \emph{NEJM} 325:293-302.
}

\examples{

data(simsolvd)

obj <- anoint(Surv(ttodthorchfhosp,censor==0)~(age+beat+lvef)*trt,
				data = simsolvd, family = "coxph")

uim(obj)$fit

}

\keyword{datasets}
