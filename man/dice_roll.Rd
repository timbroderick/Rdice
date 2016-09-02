\name{dice.roll}
\alias{dice.roll}
\title{Dice simulator}
\description{Simulates rolling of a set of dice.}
\usage{
dice.roll(faces = ..., dice = ..., rolls = ..., weights = ...)
}
\arguments{
  \item{faces}{ The number of faces the dice have; if unspecified, it
            defaults to 6.}
  \item{dice}{ The number of dice to roll; if unspecified, it
              defaults to 1.}
  \item{rolls}{ The number of times to roll the die; if unspeciefid, it
            defaults to 1.}
  \item{weights}{ A vector of probability weights to assign to each
                  face of the die; if unspecified, it defaults to a fair
                  die with weights \eqn{1/N}. If specified, its lenght
                  must obviously be = \emph{faces} and all the
                  single weights must sum up to 1.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 \item{results}{ The numerical results rolled.}
 \item{frequencies}{ A table containing each single occurrency (permutation)
                      of results and the corresponding frequencies.}
 \item{sums_freq}{ A table containing the frequencies of the sums of the
                  values obtained in each single roll by all the dice.
                  A cumulative sum is provided too.}
 \item{exp_value_sum}{ The expectation value of the sum of the values obtained.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Gennaro Tedesco: \email{gennarotedesco@gmail.com}
}
\note{
The case face = 2 corresponds to the coin flip.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Provide examples
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

