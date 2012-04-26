\name{breslow}
\alias{breslow}
\title{
Smoking Deaths Among Doctors
}
\description{
The \code{breslow} data frame has 10 rows and 5 columns.


In 1961 Doll and Hill sent out a questionnaire to all men on the British 
Medical Register enquiring about their smoking habits. Almost 70\% of 
such men replied.  Death certificates were obtained for medical practitioners
and causes of death were assigned on the basis of these certificates.  The
\code{breslow} data set contains the person-years of observations and deaths from
coronary artery disease accumulated during the first ten years of the study.
}
\usage{
breslow
}
\format{
This data frame contains the following columns:


\describe{
\item{\code{age}}{
The mid-point of the 10 year age-group for the doctors.
}
\item{\code{smoke}}{
An indicator of whether the doctors smoked (1) or not (0).
}
\item{\code{n}}{
The number of person-years in the category.
}
\item{\code{y}}{
The number of deaths attributed to coronary artery disease.
}
\item{\code{ns}}{
The number of smoker years in the category (\code{smoke*n}).
}}}
\source{
The data were obtained from


Breslow, N.E. (1985) Cohort Analysis in Epidemiology. In 
\emph{A Celebration of Statistics} 
A.C. Atkinson and S.E. Fienberg (editors), 109--143.
Springer-Verlag.
}
\references{
Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.


Doll, R. and Hill, A.B. (1966) Mortality of British doctors in relation to 
smoking: Observations on coronary thrombosis. 
\emph{National Cancer Institute Monograph}, \bold{19}, 205-268.
}
\keyword{datasets}
% Converted by Sd2Rd version 1.15.
