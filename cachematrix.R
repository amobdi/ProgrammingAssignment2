## 2 functions: first one takes a matrix and calculates its inverse and caches it 
## second ond takes the matrix and tries to retrieve its cached inverse


## function takes a matrix and caches its inverse 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function(y){
        x <<- y ## set the matrix 
        i <<- NULL
        
    }
    get <- function() x ## get the matrix 
    
    setinv <- function(solve) i <<- solve  ## set i to solve(x)
    getinv <- function() i   ## get inverse 
    list(set = set, get = get, setinv = setinv, getinv= getinv) ## list of functions implemented above

}


## function that takes a matrix and try to retrieve its cached inverse and if doesn't exist it calculates it and caches it

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.na(i)){    #if the inverse is present and cached 
        message("getting cached data")
        return(i)
        
    }
    # if not 
    data <- x$get()   # get matrix x
    i <- solve(data,...) # calculate solve(x)  -> inverse
    x$setinv(i)  # cache inverse of the matrix 
    i
        ## Return a matrix that is the inverse of 'x'
}
