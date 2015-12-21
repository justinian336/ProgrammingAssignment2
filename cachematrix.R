## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix accepts a matrix as an argument and defines four methods that can be used for caching and retreiving the matrix and its inverse:
# set() saves the values of the matrix and its inverse in the cache.
# get() returns the function that is stored in the cache.
# setinv() replaces the value of the inverse in the cache with the one provided as an argument.
# getinv() retrieves the value of the inverse stored in the cache.
#makeCacheMatrix returns a list with the four functions.

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes as an argument a list created with makeCacheMatrix() and returs and stores the inverse of the matrix provided to makeCacheMatrix().

cacheSolve <- function(x, ...) {
        # First, obtain the value in the cache.
        inv <- x$getinv()
        # If there is a value stored for the inverse, retrieve that value and return it:
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # Otherwise, obtain the matrix stored in the cache...
        data <- x$get()
        # Obtain its inverse...
        inv <- solve(data, ...)
        # Cache the inverse...
        x$setinv(inv)
        # Show the inverse.
        inv
}

#The following makes a comparison between the running time of finding the inverse of a large matrix for different strategies:
#First, generate a large matrix
x<-matrix(runif(1000000),nrow=1000,ncol=1000)

#(1) Measure the time it takes to cache the inverse for the first time:
print("Time to cache the matrix and its inverse")
t0<-proc.time()
y<-makeCacheMatrix(x)
z<-cacheSolve(y)
print((proc.time()-t0)[3])

#(2) Now the time it takes to obtain the inverse by solve() alone:
print("Time to find the inverse with solve()")
t1<-proc.time()
solve(x)
print((proc.time()-t1)[3])

#(3) The time it takes to obtain the inverse of a matrix that is already in the cache (basically just retreiving the inverse, since it has already been obtained in step (1)
print("Time to retrieve the inverse from the cache")
t2<-proc.time()
y$getinv()
print((proc.time()-t2)[3])

# The difference in runtimes between getting the inverse and caching it on one hand, and simply obtaining it by solve() is not significant.
# The second time that the inverse of the same matrix is obtained, the gain in efficiency is large, since obtaining the inverse of a matrix
# that has been cached is equal to simply retreiving its value.