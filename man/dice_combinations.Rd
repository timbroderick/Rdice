\name{dice.combinations}
\alias{dice.combinations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dice combinations}
\description{
Calculates all possible combinations as result of rolling a set of dice. Similar permutations are identified under the same combination and counted as many times as many occurrencies.
}
\usage{
dice.combinations(faces, dice, rolls, weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{faces}{ The number of faces the dice have; if unspecified, it defaults to 6.}
  \item{dice}{ The number of dice to roll; if unspecified, it defaults to 1.}
  \item{rolls}{ The number of times to roll the die; if unspeciefid, it defaults to 1.}
  \item{weights}{ A vector of probability weights to assign to each face of the die; if unspecified, it defaults to a fair die with weights \eqn{1/N}. If specified, its lenght must obviously be equals to the number of faces and all the single weights must sum up to 1.}
}
\details{
The function returns an object of class \emph{diceRoll}, namely a list whose values are themselves \code{data.table} objects, in turn, so that one can directly apply any \code{data.table} function thereupon.
}
\value{
  \item{values}{ Bla bla}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Gennaro Tedesco
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
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
