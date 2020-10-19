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

In the pathfinder1() function you add the fitted model, and an object with the x/y-coordinates. 
The plot has the size 10x10. "Action" should be more right [x=9/y=5], "Distance" should be more left [x=1/y=5]. "Fear" somewhere in the middle but a little bit higher please[x=5/y=7].

    object <- " Action <- 9/5
                Distance <- 1/5
                Fear <- 5/7
    pathfinder1(fit_1, object)


In the pathfinder2() function you only add the fitted model. The function is semi interactive. 

    pathfinder2(fit_1)


In the pathfinder3() function you only add the fitted model. The function is full interactive. 

    pathfinder3(fit_1)


I recomend pathfinder3().

That's it. A good size to print the plot is 1000x1000.



