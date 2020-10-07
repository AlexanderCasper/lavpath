\name{nameorder}
\alias{nameorder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
nameorder()
}
\description{
Shows the right order to specify your coordinate-vectors for the 'pathfinder()' function.
Input is your fitted lavaan-model.
}
\usage{
nameorder(model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
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
Alexander Casper
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(lavaan)

model <- 'Y2 ~ Y1
            Y3 ~ Y2+Y1
            Y3 ~ Y1'
fit_1 <- sem(model, data=data)

nameorder(fit_1) ## Output: [1]Y1, [2]Y2, [3]Y3

xfactor <- c(x_Y1,x_Y2,x_Y3)
yfactor <- c(y_Y1,y_Y2,y_Y3)
}

