\name{get.kRp.env}
\alias{get.kRp.env}
\title{Get koRpus session environment}
\usage{
  get.kRp.env(TT.cmd = FALSE, lang = FALSE,
    TT.options = FALSE)
}
\arguments{
  \item{TT.cmd}{Logical, whether the set tagger command
  should be returned.}

  \item{lang}{Logical, whether the set language should be
  returned.}

  \item{TT.options}{Logical, whether the set TT.options for
  \code{treetag} should be returned.}
}
\value{
  A character string or list, possibly including:
  \item{TT.cmd}{Path information for the TreeTagger
  command} \item{lang}{The specified language}
  \item{TT.options}{A list with options for \code{treetag}}
  If the desired property is not set at all, the function
  will fail with an error message.
}
\description{
  The function \code{get.kRp.env} returns information on
  your session environment regarding the koRpus package,
  e.g. A function to get information on your koRpus
  environment
}
\details{
  where your local TreeTagger installation resides, if it
  was set before using
  \code{\link[koRpus:set.kRp.env]{set.kRp.env}}.
}
\examples{
\dontrun{
set.kRp.env(TT.cmd="~/bin/treetagger/cmd/tree-tagger-german", lang="de")
get.kRp.env(TT.cmd=TRUE)
}
}
\seealso{
  \code{\link[koRpus:set.kRp.env]{set.kRp.env}}
}
\keyword{misc}

