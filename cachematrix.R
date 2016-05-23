## makeCacheMatrix creates a list of arguments which are passed to the cacheSolve function,
## and the cache with the solution to the matrix x. The set function commits the matrix to the environment, 
## the get function defined here is used to obtain the matrix in the cacheSolve function. 
## The setmatrix function defined here is used to solve the matrix in cacheSolve, and the 
## getmatrix function caches this solution.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}



## The function cacheSolve accepts the matrix (x), and the list of arguments created by makeCacheMatrix.
## First, casheSolve checks whether the solution to matrix x already exists in the cache created by makeCacheMatrix. 
## If so, the solution (m) is retrieved. If the solution does not exist in the cache, it is computed,
## and uses the setmean function (defined in makeCacheMatrix) to store the solution in the cache.

cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
        
}


