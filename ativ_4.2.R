library(matlib)

#definição de constantes
e <- exp(1)
alpha <- 0.5
R <- 2.6

#dados a serem apresentados
cidades <- matrix(
  c(16.47,96.1,
    16.47,94.44,
    20.09,92.54,
    22.39,93.37,
    25.23,97.24,
    22,96.05,
    20.47,97.02,
    17.2,96.29,
    16.3,97.38,
    14.05,98.12,
    16.53,97.38,
    21.52,95.59,
    19.41,97.13,
    20.09,94.55
  ),
  nrow = 14, ncol = 2, byrow = TRUE
)

#pesos equidistantes para obtenção da matriz lambda
pesos_x <- seq(13,26,length.out = 6)
pesos_y <- seq(90,103,length.out = 6)

pesos <-matrix(0, nrow = 20, ncol = 2)

pesos[1:6,1] <- pesos_x
pesos[1:6,2] <- 90
pesos[7:11,1] <-26
pesos[7:11,2] <- pesos_y[2:6]
pesos[12:16,1] <- rev(pesos_x[1:5])
pesos[12:16,2] <- 103
pesos[17:20,1] <- 13
pesos[17:20,2] <- rev(pesos_y[2:5])

distancias <- matrix(0,20,20)

for(i in 1:20){
  for(j in 1:20){
    distancias[i,j] <- dist(rbind(pesos[i,],pesos[j,]))/2.6
  }
}

#cálculo e normalização da matriz lambda de vizinhanças gaussianas
Lambda <- matrix(0,20,20)

for(i in 1:20){
  for(j in 1:20){
    Lambda[i,j] <- e^(-distancias[i,j]^2)/2*R^2
  }
}

#divisão para que a diag. principal fique 1
Lambda <- Lambda/Lambda[1,1]



#pesos a serem usados no mapa
pesos_x <- seq(14,25.5,length.out = 6)
pesos_y <- seq(92.5, 98.2, length.out = 6)

pesos <-matrix(0, nrow = 20, ncol = 2)

pesos[1:6,1] <- pesos_x
pesos[1:6,2] <- 92.5
pesos[7:11,1] <-25.5
pesos[7:11,2] <- pesos_y[2:6]
pesos[12:16,1] <- rev(pesos_x[1:5])
pesos[12:16,2] <- 98.2
pesos[17:20,1] <- 14
pesos[17:20,2] <- rev(pesos_y[2:5])

#imagem da iteração zero
jpeg(file = "E:/DADOS_MESTRADO/PCV/myplot_0.jpg")
plot(cidades[,1],cidades[,2],xlim=c(13.5,26),ylim=c(92,99),pch = 19,col = "blue", main = "Iteração 0")
points(pesos[,1],pesos[,2],pch=19, col = "red")
for(i in 1:19){
  segments(pesos[i,1],pesos[i,2],pesos[i+1,1],pesos[i+1,2])
}
segments(pesos[20,1],pesos[20,2],pesos[1,1],pesos[1,2])
dev.off()

#variáveis de parada
dif_pesos <- 1
iter <- 1

#--------ITERAÇÃO---------
#critérios de parada: máximo de iterações e mudança dos pesos a cada iteração
while(iter <= 50  & dif_pesos > 0.01){
  
pesos_ant <- pesos

#randomizando a ordem das cidades a serem apresentadas
rand_cidades <- cidades[sample(nrow(cidades)),]

#cálculo da distância de cada neurônio ao ponto
dists <- matrix(0,20,1)

  for(i in 1:14){
    for(j in 1:20){
      dists[j,1] <- sum((rand_cidades[i,]-pesos[j,])^2)
    }
    
    min <- arrayInd(which.min(dists), dim(dists))[1,1] #determinação do neurônio vencedor
    
    for(k in 1:20){
      for(l in 1:2){
        pesos[k,l] <- pesos[k,l] + alpha*Lambda[k,min]*(rand_cidades[i,l]-pesos[k,l]) #atualização dos pesos
      }
    }
  }

  alpha <- 0.9*alpha #atualização da taxa de aprendizagem

  #criação da imagem de cada iteração
  mypath <- file.path("E:","DADOS_MESTRADO","PCV",paste("myplot_", iter, ".jpg", sep = ""))
  jpeg(file = mypath)
  title = paste("Iteração", iter)
  plot(cidades[,1],cidades[,2],xlim=c(13.5,26),ylim=c(92,99),pch = 19,col = "blue", main = title)
  points(pesos[,1],pesos[,2],pch=19, col = "red")
  for(i in 1:19){
    segments(pesos[i,1],pesos[i,2],pesos[i+1,1],pesos[i+1,2])
  }
  dev.off()
  
  #critérios de parada
  iter <- iter + 1
  dif_pesos <- max(abs(pesos - pesos_ant))
}
