#definição dos parâmetros do problema
n = 20
peso_max = 510
peso_1 = 240
peso_2 = 190
peso_3 = 170

#matriz de pesos
p = c(20,25,14,46,39,57,58,47,38,30,53,57,38,53,58,48,14,6,40,10)

#matriz de valores
v = c(7,7,8,3,5,8,1,4,9,7,10,8,7,1,7,9,3,2,4,2)

#criação de matriz para soluções
sol_inic = matrix(0,3,n)

#parâmetros de parada
p_1 = p_2 = p_3 = 1000
p_tot = 3000

#repetir até encontrar uma solução factível
while(p_1 > peso_1 | p_2 > peso_2 | p_3 > peso_3 | p_tot > peso_max){
  
  for(i in 1:3){
  sol_inic[i,] <- as.integer(round(runif(n),0))
  }

#garantir que cada objeto esteja em apenas um compartimento
  for(i in 1:2){
    for(j in 1:n){
      if(sol_inic[i,j]==sol_inic[i+1,j]){ 
        sol_inic[i+1,j]=0
      }
    
      if(sol_inic[1,j]==sol_inic[3,j]){
        sol_inic[3,j]=0
      }
    }
  }

#atualização dos critérios de parada
  p_1 <- sol_inic[1,]%*%p
  p_2 <- sol_inic[2,]%*%p
  p_3 <- sol_inic[3,]%*%p
  p_tot <- p_1 + p_2 + p_3
}
