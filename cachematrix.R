## In order to avoid a costly computational effort when we have to perform
## matrix inverse compute, lexical scoping is a good option to implement, so
## in this case there are two function to take advantage of it because the
## lexical scoping let us simplify computations.
## Remember: when a value of a varaible hasn't been found in the same 
## enviroment that a function is created, then it will be search in the 
## parent environment.
## Besides, the cache is usfull to avoid repeating the same compute because
## it will be stored in memory and accesible whe is neded.


## The makeCacheMatrix function returns a list of functions defined by name.
## Setters and getters are defined and will be available for the next function.
## Take into account the last comment about the searching of a value because at
## the begining of the function there is a initialization and then there is a
## set of varibles in the parent environment. The <<- operator let us assign a
## value in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function needs makeCacheMatrix function as formal.
## Determinant of a matrix is calculated and considered in an if control
## structured previously to use solve()
## Basically, the function gets the getinverse() and then asks if the inverse
## of a matrix is cached. If is not, the solve function is needed to compute
## the inverse and finally use a setter.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached-matrix data")
                return(inv)
        }
        data <- x$get()
        if (det(data) != 0 ){
                inv <- solve(data, ...)
        }
        
        else print("Matrix is Singular")
        
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
