\name{MGF}
\alias{MGF}
\title{Modified generalized F-test}
\usage{
MGF(data,group,pred,rept)
}
\description{
This function performs modified generalized F-test.
}
\examples{
library(doex)
MGF(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
  \item{pred}{Estimators to be used for the group comparison:"tb" for Tukey's Bisquare estimators, "otb" for one-step version of Tukey's Bisquare estimators, "aw" for Andrew's Wave estimators, "oaw" for one-step version of Andrew's Wave estimators, "huber" for Huber's M-estimators and "hubers" for Huber's second proposal.}
  \item{rept}{The loop size to perform the test.}
}
\value{
  \item{pvalue}{the p-value of the modified generalized F-test}
}
\author{Mustafa CAVUS}
\references{
Cavus, M., Yazici, B. and Sezer, A. (2017) Modified tests for comparison of group means under heteroskedasticity and non-normality caused by outlier(s), Hacettepe Journal of Mathematics and Statistics, 46 (3), 492-510.
}
