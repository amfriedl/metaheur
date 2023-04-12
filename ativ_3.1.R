library(matlib)
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
    0,3,1,
    2,0,0,
    2,1,0,
    3,0,0,
    3,1,0,
    3,3,0,
    4,0,0,
    4,2,0,
    5,0,0,
    5,1,0,
    5,2,0,
    5,3,0),
  nrow = 22, ncol = 3, byrow = TRUE
)

#definição dos centros
u <- matrix(
  c(
    1,2,
    4,0,
    5,3
  ),
  nrow = 3, ncol = 2, byrow = TRUE
)

#definição dos parâmetros
e <- exp(1)
sigma <- 5
theta <- 1

#matriz das distâncias dos pontos aos centros, e matriz G
dists <- matrix(0,22,3)
G <- matrix(0,22,4)
G[,4] <- theta

#preenchimento da matriz G
for(j in 1:22){
  
  x1 <- dados[j,1]
  x2 <- dados[j,2]
  
  for(i in 1:3){
    
    dists[j,i] <- (x1-u[i,1])^2+(x2-u[i,2])^2
    G[j,i] <- e^{(-1*dists[j,i])/(2*sigma)}
    
}
}

#cálculo dos pesos
w <- inv(t(G)%*%G)%*%t(G)%*%dados[,3]

#cálculo das saídas
y <- matrix(0,22,1)

for(i in 1:22){
  y[i,1] <- G[i,]%*%w
}

#cáclulo do erro quadrático
erro <- 0
for(i in 1:22){
  erro <- erro + 0.5*(y[i,1]-dados[i,3])^2
}
