## Put comments here that give an overall description of what your functions do
# The 2 functions will create a matrix and retrieve its inverse from cache or solve for the inverse if it DNE 

## Write a short comment describing this function
# The following function creates a matrix, and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function
# The following function will retrieve the inverse from the cache or solve for the matrix inverse if not found 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}

# Example usage of the functions

print(matrix1 <- matrix(1:4, 2, 2))
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

inv <- makeCacheMatrix(matrix1)
cacheSolve(inv)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
