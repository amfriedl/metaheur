#Definição dos dados a serem apresentados à rede
dados <- matrix(
  c(
    -3,-10,
    -2.5,-6,
    -2,-3.6,
    -1.5,-2.1,
    -1,-1.2,
    -0.5,-0.5,
    0,0,
    0.5,0.52,
    1,1.18,
    1.5,2,
    2,3.6,
    2.5,6.05,
    3,10.02
    
  ),
  nrow = 13, ncol = 2, byrow = TRUE
)
#normalização dos dados 
dados[,2]<-dados[,2]/max(dados[,2])

#inicialização de variáveis
max_iter <- 1000
iter <- 0
erro <- 1
erro_ant <- 0
dif_erro <- 1

#para cálculo do erro quadrático
y_erro_iteracao <- matrix(0,13,1)
d_erro_iteracao <- matrix(dados[,2],nrow = 13)

#pesos aleatórios
v <- matrix(rnorm(2),nrow = 2)
w <- matrix(rnorm(2),nrow = 2)

#bias aleatórios
theta_a <- matrix(rnorm(2),ncol = 1)
theta_b <- rnorm(1)

#taxa de aprendizagem
alpha <- 0.9

#iteração
while(erro > 0.01 & iter < max_iter & abs(dif_erro) > 0.0001){
for(i in 1:13){

x<-matrix(dados[i,1],nrow = 1, ncol = 1, byrow = TRUE)

d<- dados[i,2]

z_estrela <- v%*%x+theta_a

z <- tanh(z_estrela)

y_estrela <- t(w)%*%z+theta_b

y <- tanh(y_estrela)

#atualização de bias e pesos da camada de saída
delta_theta_b <- drop(alpha*(d-y)*(1-y^2)) #drop() para declarar como
                                           #valor escalar
delta_w <- delta_theta_b*z

w <- w + delta_w

#atualização de bias e pesos da camada escondida
delta_theta_a <- matrix(
  c(delta_theta_b*w[1,1]*z[1,1]*(1-z[1,1]),
    delta_theta_b*w[2,1]*z[2,1]*(1-z[2,1])),
  nrow=2, byrow=TRUE
  )

theta_a <- theta_a + delta_theta_a

delta_v <- matrix(c(
  delta_theta_a[1,1]*x[1,1],
  delta_theta_a[2,1]*x[1,1]),
  ncol = 1, nrow = 2, byrow = TRUE)

v<-v + delta_v

#preenchimento do vetor para cálculo do erro quadrático
y_erro_iteracao[i,1] <- y
}

#cálculo do erro quadrático
erro <- 0

for(j in 1:13){
  erro <- erro + (d_erro_iteracao[j,1]-y_erro_iteracao[j,1])^2
}
erro <- 0.5*erro
dif_erro <- erro - erro_ant
erro_ant <-erro

#atualização da taxa de aprendizagem
alpha <- 0.9*alpha

iter <- iter + 1

print(erro)
}

#"desnormalizando" dados e saídas
y_normal <-y_erro_iteracao*10.02
d_normal <- d_erro_iteracao*10.02
erro_normal <- 0

for(k in 1:13){
erro_normal <- (d_normal[k,1] - y_normal[k,1])^2
}

erro_normal<- 0.5*erro_normal
