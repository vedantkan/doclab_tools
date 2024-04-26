\name{crsr_func}
\alias{crsr_func}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get thr CRSR
%%  ~~function to do ... ~~
}
\description{
Function to get all the CRSR data for each patient
}
\usage{
crsr_func(data, date = "test_datetime", time = "test_datetime", type = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
 The redcap database form which you want the CRSR data
}

  \item{date}{
 Mention the name of the date column as in the databse. Default = "test_datetime"
}

  \item{time}{
 Mention the name of the time column as in the databse. Default = "test_datetime"
}

  \item{type}{
 Choose form raw/yesno/all. Default = "raw".
 raw returns the crsr total value at each timepoint,
 yesno returns binarized crsr=yes or crsr=no for each patient,
 all return both raw and yes/no binarized values.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
