\name{NBC}
\alias{NBC}
\title{Naive Bayes Classifier}
\description{
  A Naive Bayes Classifier is a model of a categorical response
  variable with the primary assumption that the predictors are
  independent of eachother.
}
\usage{
NBC(data, ..., outputs = result)
NBCMake(predictors, response, histogram.width.factor = 500,
        number.bins = 10)
}
\arguments{
  \item{data}{
    an object of class \code{"\link{data}"}.
  }

  \item{...}{
    arguments passed onto \code{KMeansMake}
  }

  \item{outputs}{
    the name of the result. If not length 1, an error is thrown.
  }

  \item{predictors}{
    \code{\link{expressions}} to use as predictors in the model.
  }

  \item{response}{
    a categorical attribute of \code{data} to be used as the response in
    the model.
  }

  \code{range}{
    the number of standard deviations of the predictors to model
    for. See \sQuote{details} for more information.
  }

  \code{bins}{
    the number of bins in each histogram. See \sQuote{details} for more
    information.
  }
}
\details{
  In the tradtional Naive Bayes Classifier, posterior probabilities for
  numerical predictor variables are calculated by using the training
  data to a model for each predictor per classification of the
  response and then applying the appropriate PDF. However, this
  introduces additional parameters, such as the specification of the
  underlying distributions, and also increases the running
  time. Instead, the underlying model and associated PDF are
  approximated by constructing a histogram during the training, which is
  then used to estimate the posterior probability by examining how often
  the training data fell within the same bin as the predictor. The
  trade-off for the above benefits is a slight loss of accuracy as well
  as increased memory usage.

  The exact shape of the histograms is specified by \code{range} and
  \code{bins}. Due to the presence of outliers, it may be unrealistic to
  construct a histogram for the whole of the data; instead, the interval
  of data that the histogram is constructed on is \eqn{[\mu - n \sigma,
  \mu + n \sigma]}, where \code{range} is denoted by \var{n}. The number
  of sub-intervals to use in the histogram is given by \code{bins}. In
  the event that this interval exceeds the actual range of the data in
  either direction, the appropriate endpoint is restricted to that of
  the data.
}
\section{AUTO}{
  \code{AUTO} is not allowed for any argument.
}
\value{
  An object of class \code{"\link{data}"} with a single attribute.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\examples{
}