

library("CADFtest")
library("car")
library("lmtest")
library("sandwich")
library("vars")

#Pacote para testes de cointegração
library('urca')


ibc = read.csv2("C:/Users/isado/Downloads/ibc.csv")
ibc = ts(log(ibc[,2]), start=c(substring(ibc[1,1],1,4),substring(ibc[1,1],6,7)),frequency=12)

m1 = read.csv2('C:/Users/isado/Downloads/m1.csv')
m1 = ts(log(m1[,2]), start=c(substring(m1[1,1],1,4),substring(m1[1,1],6,7)),frequency=12)

igp = read.csv2('C:/Users/isado/Downloads/igp.csv')
igp = ts(log(igp[,2]), start=c(substring(igp[1,1],1,4),substring(igp[1,1],6,7)),frequency=12)

dados = cbind(ibc,m1,igp)
dados = window(dados, start = c(2003,1), end = c(2024,2))

#Ao rodar o procedimento sequencial, notamos que todas as séries são I(1),
#e que m1 e igp exibem drift (intercepto na primeira diferença)


###############################
#Procedimento de Engle Granger#
###############################

#Vamos testar se há uma relação de longo prazo entre as variáveis 

#Vamos considerar o caso em que igp deve participar da relação (caso ela exista), 
#visto o que esperamos da neutralidade do longo prazo e da teoria quantitativa

#procedimento passo a passo

#se todas as series forem I(1)
#Estimarr por MQO uma regress�o de Yt em Xt e um intercpto  ( igp deve participar da rela��o portanto sera o Yt ( variavel 
#dependente do modelo) dado o que esperamos da neutradiadade do longo prazo e da teoria qunatitativa )
#calcular os residuos
#fazer o teste da nula da raiz unitaria dos residuos com os valores criticos de Philips e Ouliars
# como escolher o caso na tabela de valores criticos:
# - se h� somente tendencia estocastica CASO 2 ( n-1)
# - se apresentam tendencia estocastica e ao menos uma s�rie em Xt apresenta tendencia deterministica em nivel CASO 3 (n-1)
# - se Yt apresenta tendencia linear -> combina��o trend-stacionary -> CASO 3 com (n+1)
# - se a rela�ao de cointegra��o tem m�dia zero -> sem intecpto -> CASO 1  (n-1)

teste = lm(igp~m1+ibc, data=dados)  # estimando a regress�o

res= ts(residuals(teste),start = c(2003,1),frequency=12) #calcula os res�duos do modelo de regress�o linear 

# Philips e Perron

# em vez de cobtrolar pela defasagem supor que os erros sao seriais, sem controlar por nada
ur.pp(res, type = c("Z-tau"))

#The value of the test statistic is: -4.6666 

# como a serie apresneta tendencia deterministica em nivel - CASO 3
# dado que n = 3
# vamos ver na tabela n - 1 = 2 para o caso 3 a 1%


#Olhando a tabela dos slides, valor crítico para n=3, com drift do lado direito (Caso 3)
#a 1% é: -4.36. Logo, como -4.66 < -4.36

#rejeitamos a nula de raiz unitaria 

# ha cointegra��o

# se nao rejeitarmos a nula n�o ha cointegra��o e as series sao  I(O)


#Para os testes de hipótese, sabemos que é curcial que (Delta m1 e Delta ibc) nao sejam
#correlacionados, contemporanea ou extemporaneamente, com o erro da relação de longo prazo

#garantir que as vari�veis explicativas (mudan�as nas vari�veis independentes) 
#n�o estejam correlacionadas com os res�duos do modelo

# portmanteau
#Vamos checar isso
est = cbind(diff(dados[,c("ibc","m1")]), res)
colnames(est) = c('d_ibc','d_m1', 'err')
est= est[-1,]
acf(est)

#Alguma evidência pontual de correlação, o que sugere que devemos tomar cuidado com a 
#inferência no vetor de cointegração. 

#Intervalos de confiança para os coeficientes da equaçào
coefci(teste, vcov. = vcovHAC)

#m1- contem 1 -> o teste da hipotese nula n vai ser rejeitado
#ibc- contem -1 

# n rejeito os valores copativeis pela teoria quantitiva

# encotramos intervalos compativies com a eq quantitativa 

# n � uma evidencia contra a neutralidade da moeda, nao somos capazes de rejeirar 

#Intervalos compatíveis com a equação quantitativa





