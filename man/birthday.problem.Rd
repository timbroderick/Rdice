\name{birthday.problem}
\alias{birthday.problem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Birthday problem
}
\description{
What is the probability that N different persons have the same birthdays?
}
\usage{
birthday.problem(days, people, gap, repetitions)
}
\arguments{
  \item{days}{The total number N of possible choices per each occurrency; namely, for K occurrencies the total sample space is \eqn{N^K}. It defaults to the number of days in a regular year, i. e. 365.}
  \item{people}{The number of different cases whose coincidences we want to look at. In the example at hand the number of different people whose birthdays one wants to happen.}
  \item{gap}{The allowed gap between any two different cases. It defaults to 0, namely birthdays happening on the same day.}
  \item{repetitions}{The number of times one wants to repeat the experiment to derive the statistics.}
}
\details{
The function also provides the theoretical result, namely the theoretical probability that K cases can occurr simultaneously in the total sample space.
}
\value{
  \item{experiment}{The frequency of success cases from the experiment, after the selected amount of repetitions.}
  \item{theory}{The theoretical probability value of K cases occurring simultaneously in the total sample space.}
}

\examples{
## Simultaneous birthdays
birthday.problem(people = 23, repetitions = 20000)
## Birthdays one day apart
birthday.problem(people = 23, gap = 1, repetitions = 20000)
}

