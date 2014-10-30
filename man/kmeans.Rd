\name{KMeans}
\alias{kmeans.data}
\alias{KMeans}
\title{\var{k}-means clustering}
\concept{clustering}
\description{
  \code{KMeans} is used to perform \var{k}-means clustering, with the
  \var{k}-kmeans++ algorithm supported for picking initial clusters.
}
\usage{
kmeans.data(x, ...)

KMeans(data, ..., inputs = AUTO, outputs = resul)

KMeansMake(centers, p = 2, m = 1, max.iteration = 25,
           algorithm = "kMeansPP", epsilon = .0001,
           sample.size = 10000, convergence = "relative",
           normalized = FALSE, debug = FALSE)
}
\arguments{
  \item{x, data}{
    an object of class \code{"\link{data}"}.
  }

  \item{...}{
    arguments passed onto \code{KMeansMake}
  }

  \item{inputs}{
    which attributes of \code{data} to perform the GLA on.
  }

  \item{outputs}{
    the name of the result. If not length 1, an error is thrown.
  }

  \item{centers}{
    either an integer specifying \var{k} or a \emph{matrix} specifying
    the initial centers. In the latter case, each row should specify a
    point with one column per input.
  }

  \item{p}{
    the parameter used for the \var{p}-Minkowski distance metric. The
    default is 2, the Euclidean metric. The only other commonly used
    value is 1, which specifies the Manhattan distance.
  }

  \item{m}{
    the parameter used for fuzzy clustering. In the default case of 1,
    no fuzziness is used. Otherwise, the value should be strictly
    larger than 1.
  }

  \item{max.iteration}{
    the maximum iteration allowed, after which the algorithm stops,
    regardless of whether convergence conditions have not been reached.
  }

  \item{algorithm}{
    which algorithm should be used to select the initial centers, either
    \code{"k-means++"} or \code{"standard"}.
  }

  \item{sample.size}{
    the size of the sample to be used for k-means++.
  }

  \item{convergence}{
    the type of convergence to be tested for, either \code{"relative"}
    or \code{"absolute"}.
  }

  \item{epsilon}{
    a numeric value greater than 0 used to test for convergence.
  }
}
\details{
  For the purposes of consistency with the \code{stats} library
  implementation of \code{kmeans}, an S3 method version of \code{kmeans}
  was created, with the default simply referring to the built-in
  version; \code{kmeans.data} merely passes the call to \code{KMeans}
  and should not be used within other GLAs such as
  \code{\link{GroupBy}}.

  \code{centers} should be a matrix regardless of \var{k} or the number
  of attributes used; i.e., even though a vector could provide the
  necessary information if either was 1, a matrix should still be used
  as some decisions within the function are based on class.
}
\section{Algorithm}{
  First, the initial centers for the clusters are set-up via the
  following:

  \subsection{Initialization}{
    In the case that the initial cluster centers are not provided by the
    user, there are 2 methods by which they are randomly generated.

    The original algorithm, specified by \code{"standard"}, is to simply
    randomly pick \var{k} points from the data, performed using reservoir
    sampling. This option tends to need more iterations for convergence
    and also has a higher likelihood for sub-optimal results.

    The other option, \code{"k-means++"}, also uses random sampling. It
    differs by picking centers sequentially with the probability of
    being chosen as the next center being weighted by the squared
    distance to the nearest center among those already picked. The
    running time of this process would then be \eqn{O(kn)} which is
    infeasible for large data. Instead, a sample of \code{sample.size}
    many points is intially picked and used in place of the whole
    set of data.
  }

  After the initial centers are picked, the iterative step is applied
  until either convergence has been reached or \code{max.iteration}
  many iterations have been performed. If the initial centers were not
  supplied, then one iteration is spent on initialization, which is
  also counted against \code{max.iteration}.

  Lloyd's algorithm is employed. During each pass over the data, the
  sum of the points in each cluster is aggregated, as well as the
  total number of points per cluster. Afterwards, the new cluster
  ceneter is simply set to the average of the points within that
  cluster for the current iteration.

  Lastly, convergence is checked. Originally, Lloyd's Algorithm was run
  until the cluster centers were constant. However, this necessitates
  recording the cluster for each point, requiring \eqn{O(n)} space.
  Instead, there is merely a check that the cluster centers have
  not moved more than \code{epsilon} from their position after the
  previous iteration. If relative convergence was specified, the
  distance for a given center is first divided by its previous positive
  before taking the modulus to compute the distance.
}
\section{AUTO}{
  In the case of \code{inputs = AUTO}, all attributes of the data are
  used.
}
\value{
  An object of class \code{"\link{data}"} with a single attribute.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\examples{
}





