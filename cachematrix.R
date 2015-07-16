## makeCacheMatrix return a LIST variable of four closed functions 'set','get','setmean','getmean'.
## It stores original variable x and its inverse
## A matrix is passed to this function as a variable
##cacheSolve function calls this special Variable, checks if inverse is already calculated, if yes, returns
## the inverse from above special variable. If not, calculates the same using 'solve' R function and returns the inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL			##Defined two variables 'x' and 'inv' at parent function level
	
	set <- function(y) {
            x <<- y		##'set' closed function call sets values of 'x' and 'inv' in parent function
      	inv <<- NULL
      }
      
	get <- function() x	##'get' closed function call retrieves value of 'x' from its parent function
      
	setInverse <- function(inverse) inv <<- inverse		##'setInverse' closed function call sets value of 'inv' in its parent function
	
      getInverse <- function() inv	##'getInverse' closed function call retrieves value of 'inv' from its parent function
        
	list(set = set, get = get,	
             setInverse = setInverse,
             getInverse = getInverse)	## parent function 'makeVector' returns a LIST variable which consists of four closed functions

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
      if(!is.null(inv)) {
      	message("getting cached data")
            return(inv)
	}
      
	data <- x$get()
      inv <- solve(data, ...)
      
	x$setInverse(inv)
      
	inv
}
