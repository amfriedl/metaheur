#Definição dos dados a serem apresentados à rede
dados <- matrix(
  c(0,1,1,
    0,2,1,
    1,1,1,
    1,2,1,
    1,3,1,
    2,2,1,
    2,3,1,
    3,2,1,
    4,1,1,
    4,3,1,
    2,0,-1,
    2,1,-1,
    3,0,-1,
    3,1,-1,
    3,3,-1,
    4,0,-1,
    4,2,-1,
    5,0,-1,
    5,1,-1,
    5,2,-1,
    5,3,-1,
    0,3,1),
  nrow = 22, ncol = 3, byrow = TRUE
)

theta <- 0

x <- dados[,1:2]

#padronizando os dados entre -1 e 1
minx <- min(x)
maxx<- max(x)
for(i in 1:22){
  for(j in 1:2){
    x[i,j] <- 2 * ( x[i,j] - minx ) / ( maxx - minx ) - 1
  }
}

#inicializando e atualizando os pesos para cada par de pontos
w <- matrix(0,2,1)
d <- dados[,3]

for(i in 1:22){
  
  w[1,1] <- w[1,1] + x[i,1]*d[i]
  w[2,1] <- w[2,1] + x[i,2]*d[i]
  theta <- theta + d[i]
  
}

#cálculo de y*
y_estrela <- x%*%w + theta

#se y*>=0, então y = 1. Caso contrário, y = -1
y <- rep(0,22)

for(i in 1:22){
  if(y_estrela[i] >= 0){
    y[i] <- 1
  }else{
    y[i] <- -1
  }
}
