\name{pathfinder2}
\alias{pathfinder2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pathfinder2()}
\description{
Visualize the pathmodel of your lavaan-objekt.

The minimum input is your fitted model.

This function is semi interactive. You have to define the coordinates of the variables in a pop-up textbox.

The plotsize is 10*10, so your coordinates should be between 0 and 11.

}
\usage{
pathfinder( model,
            estplace = 0, distance = 0.2, aheadlength = 0.2, aheadangle = 20,
            box1 = "black",  box2 = "lightgrey", textcol = "black", acol = "black",
            estcol ="black", alength = 0.6, lwd = 2,  cex = 15, cex2 = 1, family = "",
            font=2, font2 = 1, font3 = 1, pch = 22 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

### Basic Imput: ###

model       = lavaan-objekt



### Font and Style: ###

cex2        = textsize

family      = style (arial/...)

font        = font (big/italic/..)

font2       = style of R2 (big/italic/..)

font3       = font of estimates (big/italic/..)



### Arrows: ###

alength     = factor for arrowlenght (0 = in the box/ 1 = very short..)

lwd         = arrowsize

aheadlenght = size of arrowhead

aheadangle  = angle of arrowhead-lines



### Boxes: ###

cex         = size if the box

pch         = symboltype (22 = Box)



### Estimates: ###

estplace    = shift the estimates out of the middle (0 = middle)

distance    = distance of estimate and arrow






### colours ###

box1        = outlines of symbols

box2        = backgroundcolour of boxes

textcol     = color of variablenames

acol        = color of arrows

estcol      = colour of estimates

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

data("pathdata")

library(lavaan)

mod <- "Acting ~ Attention + Reputation
Attention ~ Reputation"


mod.fit <- sem(mod, pathdata)


pathfinder2(mod.fit)

}
