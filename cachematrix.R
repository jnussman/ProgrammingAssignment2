##These functions will create a matrix & then cache the inverse of the matrix for future use.
##Especially useful with a large matrix whose inverse will be referenced multiple times

##This function is similar to the mean example and will create & cache a matrix.
##Also provides the infrastructure to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, 
		 get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the pre-defined matrix & caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
