makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Aquí se guardará la inversa en caché (empieza vacía)
  
  # Función para cambiar la matriz
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Si la matriz cambia, limpiamos la caché anterior
  }
  
  # Función para obtener la matriz original
  get <- function() x
  
  # Función para guardar la inversa calculada en la caché
  setinverse <- function(inverse) inv <<- inverse
  
  # Función para obtener la inversa guardada
  getinverse <- function() inv
  
  # Devolvemos una lista con las 4 funciones para poder usarlas con el signo $
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Intenta obtener la inversa guardada en el objeto 'x'
  inv <- x$getinverse()
  
  ## Si ya estaba calculada (no es NULL), la devuelve inmediatamente
  if(!is.null(inv)) {
    message("getting cached data") # Mensaje indicador
    return(inv)
  }
  
  ## Si no estaba en caché, obtenemos la matriz original
  data <- x$get()
  
  ## Calculamos la inversa usando solve()
  inv <- solve(data, ...)
  
  ## Guardamos el resultado en la caché para futuras consultas
  x$setinverse(inv)
  
  ## Devolvemos la matriz inversa
  inv
}
