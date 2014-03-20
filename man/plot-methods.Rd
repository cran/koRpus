\docType{methods}
\name{plot}
\alias{plot}
\alias{plot,kRp.tagged,missing-method}
\title{Plot method for objects of class kRp.tagged}
\usage{
plot(x, y, ...)

\S4method{plot}{kRp.tagged,missing}(x, what = "wclass", ...)
}
\arguments{
  \item{x}{An object of class \code{kRp.tagged}}

  \item{what}{Character string, valid options are:
  \describe{ \item{\code{"wclass"}:}{Barplot of
  distribution of word classes}
  \item{\code{"letters"}:}{Line plot of distribution of
  word length in letters} }}

  \item{y}{the y coordinates of points in the plot,
  \emph{optional} if \code{x} is an appropriate structure.}

  \item{...}{Arguments to be passed to methods, such as
  \link{graphical parameters} (see \code{\link{par}}).
  Many methods will accept the following arguments:
  \describe{ \item{\code{type}}{what type of plot should be
  drawn.  Possible types are \itemize{ \item \code{"p"} for
  \bold{p}oints, \item \code{"l"} for \bold{l}ines, \item
  \code{"b"} for \bold{b}oth, \item \code{"c"} for the
  lines part alone of \code{"b"}, \item \code{"o"} for both
  \sQuote{\bold{o}verplotted}, \item \code{"h"} for
  \sQuote{\bold{h}istogram} like (or \sQuote{high-density})
  vertical lines, \item \code{"s"} for stair \bold{s}teps,
  \item \code{"S"} for other \bold{s}teps, see
  \sQuote{Details} below, \item \code{"n"} for no plotting.
  } All other \code{type}s give a warning or an error;
  using, e.g., \code{type = "punkte"} being equivalent to
  \code{type = "p"} for S compatibility.  Note that some
  methods, e.g. \code{\link{plot.factor}}, do not accept
  this.  } \item{\code{main}}{an overall title for the
  plot: see \code{\link{title}}.} \item{\code{sub}}{a sub
  title for the plot: see \code{\link{title}}.}
  \item{\code{xlab}}{a title for the x axis: see
  \code{\link{title}}.} \item{\code{ylab}}{a title for the
  y axis: see \code{\link{title}}.} \item{\code{asp}}{the
  \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.} }
  }
}
\description{
Plot method for S4 objects of class
\code{\link[koRpus]{kRp.tagged-class}}, plots the
frequencies of tagged word classes.
}
\examples{
\dontrun{
tagged.results <- treetag("~/my.data/sample_text.txt", treetagger="manual", lang="en",
   TT.options=list(path="~/bin/treetagger", preset="en"))
plot(tagged.results)
}
}
\seealso{
\code{\link[koRpus]{kRp.tagged-class}}
}
\keyword{methods}
\keyword{plot}

