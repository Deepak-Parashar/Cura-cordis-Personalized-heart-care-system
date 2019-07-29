chiSquare <- function(training_set , c1 , x){
  a = length(levels(c1))
  b = length(levels(dataset$num))
  
  l = 0

  
  Observed = c()
  
  
  for(i in levels(training_set$num)){
    for(j in levels(c1)){
      l = l + 1
      Observed[l] = nrow(subset(training_set ,  Sex== j & num == i))
    }
  }
  
  mat = matrix(Observed, nrow = 5, byrow = T)
  
  df = (a - 1) * (b - 1)
  
  sumCol = c()
  sumRow = c()
  
  
  for( v in 1:b){
    sumRow[v] = sum(mat[v,])
  }
  
  for( u in 1:a){
    sumCol[u] = sum(mat[,u])
  }
  
  
  d = nrow(training_set)
  
  Expected = c()
  
  i1 = 0
  
  
  for(i in 1:b){
    for(j in 1:a){
      i1 = i1 + 1
      Expected[i1] = (sumRow[i] *  sumCol[j])/d
    }
  }
  
  mat1 = matrix(Expected, nrow = 5, byrow = T)
  
  v1 = Observed - Expected
  
  v2 = v1 * v1
  
  v3 = v2/Expected
  
  chiSquare = sum(v3)
}