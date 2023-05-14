iter_max = 3
peso_max = 275
alpha = 0.5
beta = 0.5

#matriz de pesos
p = c(63,21,2,32,13,80,19,37,56,41,14,8,32,42,7)

#matriz de valores
v = c(13,2,20,10,7,14,7,2,2,4,16,17,17,3,21)
valor_max = sum(v)

#eta
eta = 1/(max(v)+1-v)

#tau (valores randômicos)
set.seed(1)
tau = runif(15,0.2,0.5)

#matriz para armazenar as melhores soluções de cada iteração
sol = matrix(0,iter_max,15)

#matriz de controle para os objetos que estão ou não na mochila
controle = 1-diag(15)

#definição de variáveis para as iterações
sol_temp = matrix(0,15,15)
probs = numeric(15)
somatorio = numeric(15)

eta_beta = eta^beta

iter = 1

#iteração
while(iter <= iter_max){

  #definir sol temporária para zero e calcular produtos tau^alpha*eta^beta
  sol_temp = matrix(0,15,15)
  tau_alpha = tau^alpha
  prod = t(t(tau_alpha*eta_beta))
  
for(i in 1:15){

  #coloca-se o objeto i na mochila
  peso_iter = peso_max-p[i]
  sol_temp[i,i] = 1
 
  controle = 1-diag(15)
  
  #atualiza-se a matriz de controle, indicando que o objeto i está na mochila
  controle[i,] = 0
  controle[,i] = 0
  
#enquanto há capacidade na mochila:
while(peso_iter>0){

  
  for(j in 1:15){
  
    somatorio[j]= controle[,j]%*%prod
    
  }
  
  #cálculo das probabilidades de cada objeto entrar na mochila
  probs = t(prod)/somatorio
  probs[is.infinite(probs)]<-0
  maior = which.max(probs)
  
  peso_iter = peso_iter - p[maior]
  
  #verificação se o objeto cabe na mochila
  if(peso_iter>0){
    sol_temp[i,maior]=1
    controle[maior,]=0
    controle[,maior]=0
  }else{break}
  
  }

}


#armazenando todos os valores das soluções obtidas na iteração:

L = sol_temp%*%t(t(v))

#definindo e armazenandoa melhor solução de cada iteração
melhor_sol = which.max(L)

sol[iter,] = sol_temp[melhor_sol,]


#atualizando os feromônios com base nos valores obtidos
delta_tau = 1/(valor_max - L)


for(i in 1:15){
  
  #a coluna i da matriz de soluções da iteração determina quais formigas escolheram o objeto i
  tau[i] = 0.5*tau[i] + t(delta_tau)%*%sol_temp[,i]
  
}

iter = iter + 1

}

#exibindo as soluções obtidas em cada iteração
for(i in 1:iter_max){
  texto1 = paste("A melhor solução na iteração", i, "foi:")
  print(texto1)
  print(sol[i,])
  texto2 = paste("Cujo peso é:", sol[i,]%*%p)
  print(texto2)
  texto3 = paste( "E valor:",sol[i,]%*%v)
  print(texto3)
}
