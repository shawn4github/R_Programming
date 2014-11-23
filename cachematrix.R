## Create a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix function - Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## Construct a special "matrix" object, can cache its inverse

	inverseX <- NULL
	## Set inverseX to empty

	set <- function(y) {
                x <<- y
                inverseX <<- NULL
        }
	## Modify existing matrix, and initially clean cache to empty 	

	get <- function() x					
	## Returns original matrix
        
	setinverse <- function(solve) inverseX <<- solve
	## compute inverse matrix, assign to inverseX, and store it to cache
        
	getinverse <- function() inverseX			
	## Returns matrix inverse
        
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve function - computes, caches, and returns matrix inverse.

cacheSolve <- function(x, ...) {		
        ## Return a matrix that is the inverse of 'x'
	
	inverseX <- x$getinverse()		
	## Assign matrix inverse to inverseX
        
	if(!is.null(inverseX)) {
                message("getting cached data")
                return(inverseX)		
        }
        ## If inverseX is not empty, then get it from cache, and return it

	data <- x$get()				
	## Otherwise, get original matrix and assign to data
        
	inverseX <- solve(data, ...)		
        ## Using solve() to compute inverse matrix and assign to inverseX
	
	x$setinverse(inverseX)
        ## Store inverseX into cache

	inverseX
	## Return inverseX
}
