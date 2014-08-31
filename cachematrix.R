## Put comments here that give an overall description of what your
## functions do



## This function receives matrix as an input and saves it into set() sub function. 
## Then it returns itself, its content and its functions as a list, which then can be called as its subventions to check the value, set and retrieve its cached inverse matrix if it already had been calculated and cached.

makeCacheMatrix <- function(x = matrix()) {

		 m <- NULL
		 set <- function(y) {
			x <<- y
			m <<- NULL
		 }
		 get <- function() x
		 setinv <- function(inv) m <<- inv
		 getinv <- function() m
		 list(set = set, get = get,
			   setinv = setinv,
			   getinv = getinv)

}


## this function receives that list as an input and calls its getinv() content to see if inverse had been already calculated. Accordingly it returns either a cached value or calculates a new one, caches it and returns. 

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
		
		m <- x$getinv()
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		}
		data <- x$get()
		m <- solve(data, ...)  ## this is essentially the only line that needed to be changed in this assignment to achieve required results. 
		x$setinv(m)
		m
		
}
