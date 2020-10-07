\name{pathfinder}
\alias{pathfinder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pathfinder()}
\description{
Visualize the pathmodel of your lavaan-objekt.
The minimum input is your fitted model and two vectors, which contain the wished x/y coordinates of the variables.
The first value of the vector which is used for 'xfactor' is the x-value of your first variable.
Same for the vector for 'yfactor'.
The plotsize is 10*10, so your coordinates should be between 0 and 11.
The two vectors must have the same number of values as there are variables.
In other words, every variable needs an x and an y value.

The order of coordinates has to be the alphabetic order of your variables.
You can easy find out the order by useing the 'nameorder()' function
}
\usage{
pathfinder( model, xfactor, yfactor,
            estplace = 0, distance = 0.2, aheadlength = 0.2, aheadangle = 20,
            box1 = "black",  box2 = "lightgrey", textcol = "black", acol = "black",
            estcol ="black", alength = 0.6, lwd = 2,  cex = 15, cex2 = 1, family = "",
            font=2, font2 = 1, font3 = 1, pch = 22 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

### Basic Imput: ###

model       = lavaan-objekt

xfactor     = x coordinate in order to "nameorder(model)"

yfactor     = y coordinate in order to "nameorder(model)"




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

#### 1) Example ####

nameorder(model)    # shows the right order of variables to specify coordinates
xvector <- c(1,5,9) # x coordinates. Beginns with the first variable which you get with "nameorder"
yvector <- c(4,8,4) # y coordinates. Same.
pathfinder(fit_1,xfactor = xvector,yfactor = yvvector) # model is your fitted lavaan model.

## The first variable is places at x(1) and y(4),...


#### 2) Example ####

library(lavaan)

model <- 'Y2 ~ Y1
            Y3 ~ Y2+Y1
            Y3 ~ Y1'
fit_1 <- sem(model, data=data)

nameorder(fit_1) ## Output: [1]Y1, [2]Y2, [3]Y3

xfactor <- c(x_Y1,x_Y2,x_Y3)
yfactor <- c(y_Y1,y_Y2,y_Y3)

pathfinder(fit_1,xfactor = xfactor,yfactor = yfactor)
# = pathfinder(fit_1,xfactor,yfactor)
}
