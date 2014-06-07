% file MASS/man/Pima.tr.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Pima.tr}
\alias{Pima.tr}
\alias{Pima.tr2}
\alias{Pima.te}
\title{
  Diabetes in Pima Indian Women
}
\description{
  A population of women who were at least 21 years old, of Pima Indian heritage
  and living near Phoenix, Arizona, was tested for diabetes
  according to World Health Organization criteria.  The data
  were collected by the US National Institute of Diabetes and Digestive and
  Kidney Diseases.  We used the 532 complete records after dropping the
  (mainly missing) data on serum insulin.
}
\usage{
Pima.tr
Pima.tr2
Pima.te
}
\format{
  These data frames contains the following columns:
  \describe{
    \item{\code{npreg}}{
      number of pregnancies.
    }
    \item{\code{glu}}{
      plasma glucose concentration in an oral glucose tolerance test.
    }
    \item{\code{bp}}{
      diastolic blood pressure (mm Hg).
    }
    \item{\code{skin}}{
      triceps skin fold thickness (mm).
    }
    \item{\code{bmi}}{
      body mass index (weight in kg/(height in m)\eqn{^2}{\^2}).
    }
    \item{\code{ped}}{
      diabetes pedigree function.
    }
    \item{\code{age}}{
      age in years.
    }
    \item{\code{type}}{
      \code{Yes} or \code{No}, for diabetic according to WHO criteria.
    }
  }
}
\details{
  The training set \code{Pima.tr} contains a randomly selected set of 200
  subjects, and \code{Pima.te} contains the remaining 332 subjects.
  \code{Pima.tr2} contains \code{Pima.tr} plus 100 subjects with
  missing values in the explanatory variables.
}
\source{
  Smith, J. W., Everhart, J. E., Dickson, W. C., Knowler, W. C.
  and Johannes, R. S. (1988)
  Using the ADAP learning algorithm to forecast the onset of
  \emph{diabetes mellitus}.
  In \emph{Proceedings of the Symposium on Computer Applications in
    Medical Care (Washington, 1988),} ed. R. A. Greenes,
  pp. 261--265. Los Alamitos, CA: IEEE Computer Society Press.

  Ripley, B.D. (1996)
  \emph{Pattern Recognition and Neural Networks.}
  Cambridge: Cambridge University Press.
}
\keyword{datasets}
