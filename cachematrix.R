## R Programming Assignment 2

## Matrix inverses are widely used in science, although
## finding the inverse is a computationally expensive 
## operation (http://en.wikipedia.org/wiki/Invertible_matrix).
## In order to avoid multiple evaluations of the same matrix
## inverse, one can simply cache the result and use it
## if the initial matrix has remained unchanged.
## The goal of this project is to create a function (cacheINV),
## which would be computing the inverse if it was not yet
## computed and caching it or retrieveing the inverse matrix
## from the cache.

## In order to achieve this goal, a helper function needed:
##                       makeMatrix
## will create a special "matrix" which is really a list 
## containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of inverse
## get the value of inverse
## (just like in the example problem)

makeMatrix <- function(x = matrix()) {

        # set to NULL when makeMatrix is called
        inv <- NULL
        
        # set function: sets the matrix
        set <- function(y) {
                x <<- y 
                inv <<- NULL # check out the assignment operator here:
                # http://stat.ethz.ch/R-manual/R-patched/library/base/html/assignOps.html
        }
        
        # get function: gets the matrix
        get <- function() x
        
        # setinverse function: sets the inverse 'by hand'
        setinverse <- function(inverse) inv <<- inverse
        
        # getinverse function: gets the inverse
        getinverse <- function() inv
        
        # Return these as a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	
}

##        cacheINV
## function follows the example format as well.
## It will return an inverse of the input matrix

cacheINV <- function(x, ...) {
        
        # Was the inverse computed already?
        inv <- x$getinverse()  
        
        # If the inverse matrix is not all-zeroes matrix,
        # the answer is yes!
        if(!is.null(inv)) {
                # In that case we just return cached inverse		
                message("Getting cached matrix")
                return(inv)
        }
        
        # If the answer is 'no',       
        data <- x$get()         # we should retrieve the initial matrix,
        inv <- solve(data, ...) # invert it
        x$setinverse(inv)       # and cache the outcome.
        # Note that this will change the list created in makeMatrix,
        # so the next time x$getinverse() will not be a null matrix.
        
        inv # Return the inverse
}

##                Let us TEST the code.

## In order to avoid possible 'good numbers', we will generate an initial
## matrix as a matrix of random elements:

## > n=3
## > x<-matrix(runif(n*n), ncol=n) 
## > x
##           [,1]      [,2]      [,3]
## [1,] 0.9351385 0.1828613 0.1617958
## [2,] 0.6203263 0.5859340 0.5296237
## [3,] 0.7268867 0.2416970 0.3212066

## (In other cases the elements mighht be different)

## Next, let us prepare a list of functions M using makeMatrix()
## > M <- makeMatrix(x)

## And calculate the inverse of the initial matrix:
## > invM <- cacheINV(M)
## > invM
##           [,1]       [,2]        [,3]
## [1,]  1.320040 -0.4304724  0.04486633
## [2,]  4.072644  4.0077765 -8.65969192
## [3,] -6.051762 -2.0415614  9.52785297

## Is this the correct answer?
## > invM == solve(x)
##      [,1] [,2] [,3]
## [1,] TRUE TRUE TRUE
## [2,] TRUE TRUE TRUE
## [3,] TRUE TRUE TRUE

## Now let us compute the inverse again:
## > invM <- cacheINV(M)
## Getting cached matrix
## > invM == solve(x)
##      [,1] [,2] [,3]
## [1,] TRUE TRUE TRUE
## [2,] TRUE TRUE TRUE
## [3,] TRUE TRUE TRUE

## As we can see, the programm has simply fetched
## the cached matrix.
##    TESTING is SUCCESSFULL
