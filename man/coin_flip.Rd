\name{coin.flip}
\alias{coin.flip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coin flip
}
\description{
Simulates a coin flip.
}
\usage{
coin.flip(flips = ..., weights = c(0.5, 0.5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flips}{ The number of flips. If missing, it defaults to 100.}
  \item{weights}{ A vector of probability weights to assign to each face of the coin; if unspecified, it defaults to a fair coin with equally likely faces. If specified, its lenght must obviously be a vector of length two whose values sum up to 1.}
}
\details{
The function is a particular case of \code{\link{dice.roll}}, namely a roll with 2 faces and 1 die.
}
\value{
  \item{values}{ A table containing the frequencies for each of the two occurrences (head and tail) after the specified number of flips.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Gennaro Tedesco: \email{gennarotedesco@gmail.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Examples
coin.flip(100, weights = c(0.5, 0.5))
