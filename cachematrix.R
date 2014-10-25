## The function makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse
## It first "sets" the the matrix
## then "gets" the the matrix
## then sets the inverse of the matrix
## lastly, "gets" the inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x<<- y
          inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv<<- inverse
        get_inverse <- function() inv
        list(set=set,get=get,
             set_inverse=set_inverse,
             get_inverse=get_inverse)
}


##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv<- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  inv 
  }
