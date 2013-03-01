\name{forest.subsets}
\alias{forest.subsets}

\title{Subsets forest plot for proportional interactions models}

\description{

Plot of interaction effects for all possible proportional interactions models.
}

\usage{
forest.subsets(object, index = 1:(min(length(object$interaction), 
    30)), labels = NULL, exclude.fill = "white", include.fill = "grey30", 
    signif.fill = "red", percent.inner = 0.9, xlimits = NULL, 
    legend = TRUE, subgroup.text = NULL, subgroup.axis = NULL, 
    subgroup.title = "Included Covariates", 
    effects.text = NULL, effects.axis = NULL, confint = TRUE, 
    segments.gpar = NULL, subgroup = FALSE)
}

\arguments{

 \item{object}{result of \code{pim.subsets}}
 \item{index}{vector indicating which subset models to include in plot, maximum of 30 of the best subsets if not specified.}
 \item{labels}{vector of names for subgroups. If \code{NULL}, covariates of \code{pim.subsets} is used.}
 \item{exclude.fill}{color for grid squares of excluded covariates}
 \item{include.fill}{color for grid squares of included covariates}
 \item{signif.fill}{color for plot circles indicating multiplicity-corrected significance}
 \item{percent.inner}{percentage of graphic device window for plot region}
 \item{xlimits}{vector of two elements indicating minimum and maximum value for effects plot. Values and confidence intervals outside \code{xlimits} will be clipped.}
 \item{legend}{logical value indicating whether legend for significant values should be included}
 \item{subgroup.text}{\code{gpar} list for modifying title of subgroup grid}
 \item{subgroup.axis}{\code{gpar} list for modifying text of subgroup grid labels}
 \item{subgroup.title}{character for title over inclusion/exclusion grid} 				  
 \item{effects.text}{\code{gpar} list for modifying title of effects plot}
 \item{effects.axis}{\code{gpar} list for modifying text of effects plot axis}
 \item{confint}{logical indicating whether to include 95 percent confidence intervals on effects plot}
 \item{segments.gpar}{\code{gpar} list for rendering of confidence interval segments}
 \item{subgroup}{logical indicator of whether fitted object is the result of \code{anoint.subgroups}}

}

\value{
Returns a plot of the results of all subsets of proportional interactions models. On the lefthand side we plot a grid describing the subsets models. This is a grid showing the included and exclude covariates of each proportional interactions model. Each row corresponds to a particular model. Colored squares in each row indicate the covariates given a proportional interaction effect, while unfilled (\code{exclude.fill}) indicate covariates left out of the model. The righthand side shows the interaction effect estimates (effects) for the corresponding subset model. 
}

\details{
The significance level is the multiplicity corrected criterion with \code{fwer} control as specified by \code{pim.subsets}.
}

\author{Stephanie Kovalchik <s.a.kovalchik@gmail.com>}

\seealso{\link{pim.subsets}}

\examples{

set.seed(11903)

# NO INTERACTION CONDITION, LOGISTIC MODEL
# SUPPOSE 5 HYPOTHESIZED EFFECT MODIFIERS

null.interaction <- data.anoint(
                             alpha = c(log(.5),log(.5*.75)),
                             beta = log(rep(1.5,5)),
                             gamma = rep(1,5),
                             mean = rep(0,5),
                             vcov = diag(5),
                             type="survival", n = 500
                             )

head(null.interaction)

fit <- pim.subsets(Surv(y, event)~V1+V2+V3+V4+V5,trt="trt",
		data=null.interaction,family="coxph")

forest.subsets(fit)
}
