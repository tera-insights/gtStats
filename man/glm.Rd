\name{GLM}
\alias{glm.data}
\alias{GLM}
\title{Fitting Large Scale Generalized Linear Models}
\description{
  \code{gtGlm} is used to fit generalized linear models, specified by
  giving a symbolic description of the linear predictor and a
  description of the error distribution. The syntax is modeled after
  that of \code{\link{glm}}.

  For more information regarding the helper functions, see the
  description of \code{Make} and \code{Argument} functions in the
  general documentation on the Grokit system.
}
\usage{
glm.data(data, ..., outputs = result, force.frame = FALSE)

GLM(data, ..., model = NULL, outputs = result)

GLMMake(formula, family = gaussian, weights = NULL, start = NULL,
        eta.start = NULL, mu.start = NULL, offset = NULL,
        maxit = 25, epsilon = 1e-8, trace = FALSE, debug = FALSE,
        convergence = "relative", model = TRUE,  ...)
}
\arguments{
  \item{data}{
    an object of class \code{"\link{data}"}.
  }
  \item{formula}{an object of class \code{\link{formula}}. The
    specification of formula differs slightly from the stats library
    \code{glm} (see \sQuote{Details} for specifications).
  }
  \item{family}{
    the expected error distribution of the data and the link
    function. This  argument can be a character string naming a family
    (e.g. \code{\sQuote{gaussian}}), a family function
    (e.g. \code{gaussian}), or a list that is the result of a call
    to a family funtion (e.g. \code{gaussian(link = \sQuote{inverse})}).
    See \code{\link{family}} for further information.
  }
  \item{weights}{
    an optional  description of \sQuote{prior weights} to
    be used. Unlike \code{glm}, this should be NULL or an
    \code{\link[=expressions]{expression}}.
  }
  \item{start}{
    an optional vector that specifies starting values for the
    parameters in the linear predictor.
  }
  \item{etastart}{
    an optional specification of starting values for the linear
    predictor. The required format is equivalent to that of
    \code{weights}. If \code{start} is non-\code{NULL}, this argument is
    unused.
  }
  \item{mustart}{
    an optional specification of starting values for the
    predicted means. The required format is equivalent to that of
    \code{weights}. If either \code{start} or \code{etastart} is
    non-NULL, this argument is unused.
  }
  \item{offset}{
    an optional specification of a known component to be
    added to the linear predictor during fitting. The required
    format is equivalent to that of \code{weights}.
  }
  \item{maxit}{
    the maximal number of IWLS iterations to be used. In
    \code{glm}, this argument is specified within the control
    argument.
  }
  \item{epsilon}{
    the maximum absolute change of parameters allowed for
    convergence. See section \sQuote{Algorithm} for more information.
  }
  \item{trace}{
    logical indicating whether every iteration of the
    coefficient vector should be returned.
  }
  \item{debug}{
    logical indicating whether sample calculations should be
    outputed to the terminal.
  }
  \item{convergence}{
    the type of convergence to be tested for, either \code{"relative"}
    or \code{"absolute"}.
  }
}
\details{
  A typical formula has the form \code{response ~ terms} where both
  \code{response} and \code{terms} are \code{\link{expressions}} with
  the additional function \code{\link{I}} allowed. Additionally,
  \code{terms} is allowed the additional binary operator \code{:}. See
  the \sQuote{Details} of \code{\link{glm}} for more information about
  \code{\link{I}}, \code{:}, \code{+}, and \code{*}.

  Unlike \code{glm}, a binomial model is specified exactly the same as
  other models. To specify number of trials, simply include them in the
  \code{weights}, which is easily accomplished because \code{weights} is
  allowed to be a mathematical expression. Furthermore, the response
  must be the proportion of successes.

  For example, let \code{S} denote a vector of number of successes;
  \code{F}, a vector of number of failures; \code{W}, a vector of
  weights. In the implementation of \code{glm}, this could be called as
  \code{glm(cbind(S, F) ~ [formula], family = binomial, weights =
  W)}. To form an equivalent model using \code{gtGlm}, use
  \code{gtGlm(S/(S + F) ~ [formula], family = binomial, weights = (F +
    S) * W)}. Here, \code{[formula]} represents an arbitrary formula of
  covariates.

  %% In the formula, redundant terms are automatically parsed out such that
  %% the largest order terms remain. This occurs only to ease parsing and
  %% does not affect any statistical results; however, the output labelling
  %% may differ from that of \code{glm}. It should be noted that the
  %% intercept term is treated as redundant if any single term is a factor
  %% or an interaction of only factors.

  %% For example, let \code{V1} and \code{V2} be covariates that are
  %% factors each with two categories - \code{"yes"} and
  %% \code{"no"}. Calling \code{glm(y ~ V1 + V1:V2)} would return a
  %% gaussian model that has coefficients represented double with names:
  %% \code{(Intercept) V1yes  V1no:V2yes V1yes:V2yes} Meanwhile, \code{glm(y ~
  %%   V1:V2)} would produce the names: \deqn{ \text{(Intercept)} \t
  %%   \text{V1no:V2no} \t \text{V1yes:V2no} \t \text{V1no:V2yes} \t
  %%   \text{V1yes:V2yes}} Although there appear to be more paramters,
  %% \code{V1yes:V2yes} will have a value of \code{NA}.

  %% By contrast, both \code{gtGlm(y ~ V1 + V1:V2)} and \code{gtGlm(y ~
  %% V1:V2)} will produce coefficient vectors with names:
  %% \deqn{\text{V1no:V2no} \t \text{V1no:V2yes} \t \text{V1yes:V2no} \t
  %% \text{V1yes:V2yes}} Although the coefficients produced will appear to
  %% be different from that of \code{glm}, this is because the lack of an
  %% intercept and a first order term. See \sQuote{examples} for a fully
  %% fleshed example.
}

\section{Algorithm}{
  As the algorithm for maximimizing the log likelihood is different from
  that of \code{\link{glm}}, a brief outline is given:

  1. \eqn{\eta_i = \textbf{x}_i \cdot \textbf{\beta}_j} - The linear predictor
  is the dot product of the input vector (which is formed from the
  formula and the data) and the current iteration of the coefficient
  vector.

  2. \eqn{\hat{\mu}_i = g^{-1}\left(\eta_i\right)} - the predict mean is
  the inverse link function of the linear predictor.

  3. \eqn{z_i = \eta_i + \left(y_i - \hat{\mu}_i \right) \cdot
    \frac{d\eta_i}{d \mu_i}} - The working dependent variable is
  computed.

  4. \eqn{w_i = \frac{p_i}{\text{var}\left(\mu_i\right) \cdot
  \left(\frac{d\eta_i}{d \mu_i}\right)^2}} - the iterative weight
  calculated from the prior weights (the \code{weights} argument) and
  the variance function specified by the family.

  5. \eqn{\textbf{X}^\textbf{T} \textbf{WX} \stackrel{+}{=} w_i \cdot
  \textbf{x}_i^\textbf{T} \textbf{x}} - The weights matrix is updated
  for that item.

  6. \eqn{\textbf{X}^\textbf{T} \textbf{Wz} \stackrel{+}{=} w_i \cdot
    z_i \cdot \textbf{x}_i} - The response matrix is updated for that
  item.

  7. \eqn{\boldsymbol \beta_{j+1} = \left( \textbf{X}^\textbf{T} \textbf{WX}
  \right)^{-1} \cdot \textbf{X}^\textbf{T} \textbf{Wz}} - the next
  iteration of the coefficient vector is computed. Note: \eqn{( \cdot
  )^{-1}} denotes the Moore-Penrose psuedoinverse, not the standard
  matrix inverse.

  8.  If \eqn{\max\limits_i \left|\beta_j[i] - \beta_{j+1}[i]\right| <
    \epsilon}, then the IWLS has converged and deviance is
  calculated. Otherwise, another iteration is performed.
}

\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
