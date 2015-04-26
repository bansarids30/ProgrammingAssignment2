##makeCacheMatrix creates list containing
##1. set value of square matrix
##2. get value of square matrix
##3. set the value of inverse of square matrix
##4. get the value of inverse of square matrix

makeCacheMatrix <- function(x = matrix()) { 	        ##Defining square matrix
	m <- NULL					##Inverse matrix
        set <- function(y) {
                x <<- y					##Saving matrix as x
                m <<- NULL				##Setting inverse to null
        }
        get <- function() x				##Return original value of x
        setInverse <- function(inverse) m <<- inverse   #assigning inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cachemean function calculates Inverse of a square matrix if the inverse
## of matrix has not been calculated before

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)){				##if m is not null,then
		message("getting cached data")	        ##returning cached data
		return(m)					
	}
	data <- x$get()
	m <- solve(data,...)				##Using solve function 
							##to calculate Inverse
	x$setInverse(m)					##setting inverse
	m

}

##m<-matrix(c(1,4,9,0,-3,2,2,7,8),3,3)
##m1 = makeCacheMatrix(m)
##cacheSolve(m1)
