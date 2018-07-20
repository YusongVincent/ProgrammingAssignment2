## R functions that are able to cache potentially time-consuming computations

## The first function creates a special "vector",
## which is really a list containing functions

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y){
                x<<-y
                inver<<-NULL
        }
        get <- function(){
                x
        }
        set_inverse <- function(y){
                inver <<- y
        }
        get_inverse <- function(){
                inver
        }
        list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## The following function calculates the inverse of matrix

cacheSolve <- function(x, ...) {
        inver <- x$get_inverse()
        if(!is.null(x$get_inverse())){
                print("getting cached data")
                return(inver)
        }
        data <- x$get()
        y <- solve(data,...)
        x$set_inverse(y)
        y
        ## Return a matrix that is the inverse of 'the matrix
}
