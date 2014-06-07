\name{aml}
\alias{aml}
\title{
Remission Times for Acute Myelogenous Leukaemia
}
\description{
The \code{aml} data frame has 23 rows and 3 columns.


A clinical trial to evaluate the efficacy of maintenance chemotherapy for
acute myelogenous leukaemia was 
conducted by Embury et al. (1977) at Stanford University.  After reaching a
stage of remission through treatment by chemotherapy, patients were randomized
into two groups. The first group received maintenance chemotherapy and the 
second group did not.  The aim of the study was to see if maintenance 
chemotherapy increased the length of the remission.  The data here formed a
preliminary analysis which was conducted in October 1974.
}
\usage{
aml
}
\format{
This data frame contains the following columns:


\describe{
\item{\code{time}}{
The length of the complete remission (in weeks).
}
\item{\code{cens}}{
An indicator of right censoring.  1 indicates that the patient had a relapse
and so \code{time} is the length of the remission. 0 indicates that the patient
had left the study or was still in remission in October 1974, that is the
length of remission is right-censored.
}
\item{\code{group}}{
The group into which the patient was randomized.  Group 1 received
maintenance chemotherapy, group 2 did not.
}}}
\source{
The data were obtained from


Miller, R.G. (1981) \emph{Survival Analysis}. John Wiley.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.


Embury, S.H, Elias, L., Heller, P.H., Hood, C.E., Greenberg, P.L. and
Schrier, S.L. (1977) Remission maintenance therapy in  acute myelogenous
leukaemia. \emph{Western Journal of Medicine}, \bold{126}, 267-272.
}
\note{
  Package \pkg{survival} also has a dataset \code{aml}. It is the same
  data with different names and with \code{group} replaced by a factor
  \code{x}.
}
\keyword{datasets}
% Converted by Sd2Rd version 1.15.
