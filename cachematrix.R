## Put comments here that give an overall description of what your
## functions do

## This function makes a list of functions that are used in calculating the inverse 
## of a given matrix. Input is matrix by default

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ### Create a closure function called set that sets the value of the matrix 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ### Create a function get that gets the value of the matrix
        get <- function() x
        ### Create function setinv that saves the matrix inverse value of matrix x 
        ### if inverse is solved
        setinv <- function(solve) m <<- solve
        ### create a function getinv for finding a value of an inverse matrix 
        ### if the matrix inverse for that matrix has been solved
        getinv <- function() m
        ### create a list from the closure functions defined above 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the inverse matrix of a given matrix. If the matrix is not 
## already inversed, it calculates the matrix inverse by using the solve function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ### Check if m already has a value
        m <- x$getinv()
        ### If m can be found, inform the user
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ### Use function get to get the data related to x 
        data <- x$get()
        ### Use solve to calculate the inverse matrix of data and save it to variable
        ### m
        m <- solve(data)
        ### Save m to the list x
        x$setinv(m)
        ### Return the inverse matrix
        m
}
