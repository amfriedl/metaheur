#dados de teste
dados <- matrix(
  c(
    0,0,1,
    1,0,-1,
    4.5,0.5,-1,
    3.5,1.5,1,
    4,2.5,1,
    1.5,1.5,1,
    2,0.5,-1,
    2.5,2.5,1
  ),
  nrow = 8, ncol = 3, byrow = TRUE
)

#inicialização de variáveis
max_iter <- 3
iter <- 0
erro <- 1

#para cálculo do erro quadrático
y_erro_iteracao <- matrix(0,8,1)
d_erro_iteracao <- matrix(dados[,3],nrow = 8)

#pesos aleatórios
v <- matrix(rnorm(6),nrow = 2)
w <- matrix(rnorm(3),nrow = 3)

#bias aleatórios
theta_a <- matrix(rnorm(3),ncol = 1)
theta_b <- rnorm(1)

#taxa de aprendizagem
alpha <- 0.5

#iteração
while(erro > 0.01 & iter < max_iter){
for(i in 1:8){

x<-matrix(dados[i,1:2],nrow = 2, ncol = 1, byrow = TRUE)

d<- dados[i,3]

z_estrela <- t(v)%*%x+theta_a

z <- sigmoid(z_estrela)

y_estrela <- t(w)%*%z+theta_b

y <- sigmoid(y_estrela)

#atualização de bias e pesos da camada de saída
delta_theta_b <- drop(alpha*(dados[1,3]-y)*y*(1-y)) #drop() para declarar como
                                                    #valor escalar
delta_w <- delta_theta_b*z

w <- w + delta_w

#atualização de bias e pesos da camada escondida
delta_theta_a <- matrix(
  c(delta_theta_b*w[1,1]*z[1,1]*(1-z[1,1]),
    delta_theta_b*w[2,1]*z[2,1]*(1-z[2,1]),
    delta_theta_b*w[3,1]*z[3,1]*(1-z[3,1])),
  nrow=3, byrow=TRUE
  )

theta_a <- theta_a + delta_theta_a

delta_v <- matrix(
  c(
      delta_theta_a[1,1]*x[1,1], delta_theta_a[2,1]*x[1,1], delta_theta_a[3,1]*x[1,1],
      delta_theta_a[1,1]*x[2,1], delta_theta_a[2,1]*x[2,1], delta_theta_a[3,1]*x[2,1]
  ),
  ncol = 3, nrow = 2, byrow = TRUE
)

v<-v + delta_v

#preenchimento do vetor para cálculo do erro quadrático
y_erro_iteracao[i,1] <- y
}

#cálculo do erro quadrático
erro <- 0

for(i in 1:8){
  erro <- erro + (d_erro_iteracao[i,1]-y_erro_iteracao[i,1])^2
}
erro <- 0.5*erro

#atualização da taxa de aprendizagem
alpha <- 0.9*alpha

iter <- iter + 1
}
