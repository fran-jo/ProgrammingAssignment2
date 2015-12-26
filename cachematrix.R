## Put comments here that give an overall description of what your
## functions do
## own description to understand the use of makeCacheMatrix and cacheMatrix
## > A<- matrix(c(-1, -2, 1, 1), 2,2)
## > Ap<- makeCacheMatrix(A)
## > cacheMatrix(Ap) 
## > # first, it will calculate the inverse matrix; second, it will get the message and the cache matrix, because Ap is allocated in a know memory position
## > cacheSolve(makeCacheMatrix(matrix(c(1,2,-1,-1),2,2)))
## > # in this case, invoking makeCacheMatrix will create an object in a differen memory position, so cacheSolve will never get the message neither the cache matrix.

## Write a short comment describing this function

## this function adds get/set functionality for a cache matrix. Stores the calculated matrix in a known memory position, being a cache matrix when the inverse is calculated
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set= set, get= get, setinv= setinv, getinv= getinv)
}

## Write a short comment describing this function

## function will retrieve the calculated inverse matrix from the memory postion. Otherwise, it calculates the inverse matrix and store it in a memory position.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## check if the matrix is in memory
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        ## calculates the inverse of the matrix and stores it in memory
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
