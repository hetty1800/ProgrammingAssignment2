##Caching and calculate the Inverse of a Matrix.

## Creates a special "matrix" that can be cached.
makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
    ## Set the value of the matrix.
        set<-function(y){
                x<<-y
                s<<-NULL
        }
    ## Get the value of the matrix.
        get<-function() x
    ## Set the value of the inverse of the matrix.
        setsolve<-function(solve) s <<-solve
    ## Get the value of the inverse of the matrix.
        getsolve<-function() s
        list(set=set,get=get,
             setsolve=setsolve,
             getsolve=getsolve)
}

## Calculate the inverse of the matrix, using functions set aboved;
cacheSolve <- function(x, ...) {
    ## Set the value of the inverse of 'x' .
        s<-x$getsolve()
    ## Only return the value of the inverse of 'x' when it is not NULL.
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
    ## Get the value of matrix 'x'.
        data<-x$get()
    ## Calculate and set the inverse of 'x'.
        s<-solve(data,...)
    ## Get and return the inverse of 'x'.
        x$setsolve(s)
        s
}
