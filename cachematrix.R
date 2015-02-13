## Put comments here that give an overall description of what your
## functions do

## return a list with 4 functions: get's and set's for the matrix and its inverse

makeCacheMatrix<- function(x = matrix()) {
        #m initialized as null. contains the matrix inverse
        m <- NULL
        #function to set the matrix to the variable
        set <- function(y) {
                #assign to variable x in the above environment the value of y
                x <<- y
                #assign to variable m in the above environment null. y is a new matrix and hasn't been solved
                m <<- NULL
        }
        #function to get the matrix
        get <- function() x
        #function to solve the matrix. assign to m the result of solve made by cachesolve function
        setsolve <- function(solve) m <<- solve
        #function that returns the matrix inverse
        getsolve <- function() m
        #return a list with the 4 functions as attributes of a given variable
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## check if a given makeCacheMatrix has already an inverse. it will calculate it if not or return the cached inverse

cacheSolve<- function(x, ...) {
        #set m as the result the solve of the matrix in x
        m <- x$getsolve()
        #m will be null if the matrix in x or
        #different of null in case the computation already be done
        if(!is.null(m)) {
                #just returns m after print a message
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        #the matrix in x will be solved
        m <- solve(data, ...)
        #the matrix inverse will be assigned in x
        x$setsolve(m)
        #return the matrix inverse
        m
}




