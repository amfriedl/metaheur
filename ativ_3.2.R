library(matlib)
#Definição dos dados a serem apresentados à rede
dados <- matrix(
  c(0,1,0,
    1,2,0,
    0,3,1,
    2,3,1,
    1,3,1),
  nrow = 5, ncol = 3, byrow = TRUE
)

#definição dos centros
u <- matrix(
  c(
    0,0,
    1,1,
    3,3
  ),
  nrow = 3, ncol = 2, byrow = TRUE
)

#definição dos parâmetros
e <- exp(1)
sigma <- 5
theta <- 1

dists <- matrix(0,5,3)
G <- matrix(0,5,4)
G[,4] <- theta

#cálculo das distâncias dos pontos aos centros
for(j in 1:5){
  
  x1 <- dados[j,1]
  x2 <- dados[j,2]
  
  for(i in 1:3){
    
    dists[j,i] <- (x1-u[i,1])^2+(x2-u[i,2])^2
    G[j,i] <- e^{(-1*dists[j,i])/(2*sigma^2)}
    
}
}

#cálculo dos pesos
w <- inv(t(G)%*%G)%*%t(G)%*%dados[,3]

#cálculo das saídas
y <- matrix(0,5,1)

for(i in 1:5){
  y[i,1] <- G[i,]%*%w
}

#cálculo do erro quadrático
erro <- 0
for(i in 1:5){
  erro <- erro + 0.5*(y[i,1]-dados[i,3])^2
}
