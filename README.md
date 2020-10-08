# What does lavpath? How to use it?

## Initial Situation
You calculated a path model with lavaan. Typically you used the funktion lavaan::sem(). The easiest example would be a single mediatiator model.  
Now you have all your parameters and you could visualize it with MS PowerPoint, but that is laborious and error-prone.  
The solution can be this package. With the function lavpath::pathfinder() you can visualize your path model in an easy way.  

## Example

A high "spatial distance to environmental disasters" is associated with lower "environmentally conscious action". 
This can be explained by the fact that people are less feared by disasters which are far away. Actually, "fear" is responsible for the effect on "environmentally conscious action".

We measured that stuff in some way. Now we have data. 
We use lavaan to calculate relevant parameters:


    library(lavaan)

    model <- 'Fear ~ Distance
              Action ~ Fear + Distance'

    fited_model <- sem(model, data=data)
    
    summary(fited_model)

To visualize this model with lavpath, you just need the fitted model and coordinates for Fear, Distance and Action. 
Coordinates define where the variable will be printed in a 10x10-XY-coordinate system.

    library(lavpath)

With the nameorder() function you get the order of the coordinates.

    nameorder(fit_1)
    
    Output: [1] Action [2] Distance [3] Fear

In the pathfinder() function you add the fitted model, a vector with the x-coordinates and a vector with the y-coordinates (pay attention to the right order). 
The plot has the size 10x10. "Action" should be more right [x=9/y=5], "Distance" should be more left [x=1/y=5]. "Fear" somewhere in the middle but a little bit higher please[x=5/y=7].


    pathfinder(fit_1, c(9,1,5), c(5,5,7))

That's it. A good size to print the plot is 1000x1000.



