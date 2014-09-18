## Code written by Ranjit Karmarkar as part of the Assignment-2 submission
## the function calculates an inverse of a given matrix and saves into the cashe
## so that for the following user attempt, previously saved values are returned
## instead of recalculating the inverse of that matrix
## This function creates a matrix object to do the following:
## 1: set metrix value
## 2: get metrix value
## 3: set inverse values
## 4: get inverse values

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## define cashe
        set <- function(y) {
                x <<- y  ## assign input metrix (y) to a variable (x) in the parent env.
                m <<- NULL  ## reset min the parent environment
        }
        get <- function() x ## return variable x
        setinverse <- function(inverse) m <<- inverse ## set matrix inverse in cashe 
        getinverse <- function() m  ## return cashe inverse of variable x
        list(set = set, get = get,
             setinverse = setnverse,
             getinverse = getinverse)

}


## The follow code is a function to calculate inverse of a metrix created above
## The function code follows below steps:
## check if an inverse is already in cashe
## if it exists, get the inverse value from cashe and skip calculations
## else calculate inverse of the given matrix and set the calculated values in cashe
## via the above setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message ("getting cashe data as calculation already exists")
                return(m) 
        }
        
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
