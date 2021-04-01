#importando arquivo
#BD <- read.csv( "C:/XXXX/Thiago Silva/XXXXXDataSciencexxxxx/XXXX/Admission_Predict.csv", sep = ",", encoding = "UTF-8",na.strings = "")
BD<-read.csv(file.choose())

#Excluindo variavel com indice de linhas
BD$Serial.No. <- NULL

library(caret)

#Criando o modelo
?trainControl

set.seed(1)
#Prevendo a chance de admissao com base em todas as outras variaveis
#utilizando o algoritmo glmnet (regressao linear)
#Utilizando cross validation (validacao cruzada) nas separacoes de treino e teste
#A validacao cruzada separa os dados de treino e teste em "k" grupos, onde sera gerado um modelo para cada separacao de grupo ou seja "k" vezes.
#Esta tecnica tem o intuito de entregar mais fidelidade a acuracia do modelo, mas exige um pouco mais de esforco computacional.
#Haja visto que geralmente o modelo e rodado apenas uma vez, mas desta forma o modelo sera rodada "K" vezes com o intuito de testar diferentes grupos de treino e teste.
#E indicado que o valor de K seja de 5 a 10. 
modelo<- train(Chance.of.Admit ~., data = BD, method= "glmnet", trControl= trainControl(method = 'cv', number = 5))


#modelo$resample = tabela , $Rsquared = valores dos R quadrados 
#Visualizar tabela com os K valores de Rquadrados
modelo$resample$Rsquared
#Resumo dos K valores
summary(modelo$resample$Rsquared)
#Medias dos K valores
mean(modelo$resample$Rsquared)
#Identificando melhores valores para alpha e lambda
modelo$bestTune
#Quantos valores de alpha e lambda foram testados?
#Por padrao a funcao train utilizou 3 valores para alpha e 3 valores para lambda, assim temos 9 testes.
#Visualizar valores testados 
modelo$results
#Visualizar valores testados para alpha e lambda 
modelo$results[,c(1,2,4)]


#-------------------------------------------------------------------------------
#Reconstruindo modelo com a funcao tuneLenght, quer dizer que o modelo fará no máximo "K" testes, ou seja "K" valores para alpha e "K" valores para lambda
#Eh normal que este modelo consuma mais memoria e demore um pouco mais para ser executado.
#A funcao metric e para avaliar o modelo com base no Rsquared , por padrao o modelo utiliza "RMSE".

set.seed(1)
modelo<- train(Chance.of.Admit ~., data = BD, method= "glmnet", tuneLenght=10, trControl= trainControl(method = 'cv', number = 5), metric = "Rsquared")


#modelo$resample = tabela , $Rsquared = valores dos R quadrados 
#Visualizar tabela com os K valores de Rquadrados
modelo$resample$Rsquared
#Resumo dos K valores
summary(modelo$resample$Rsquared)
#Medias dos K valores
mean(modelo$resample$Rsquared)
#Identificando melhores valores para alpha e lambda
modelo$bestTune
#Quantos valores de alpha e lambda foram testados?
#Por padrao a funcao train utilizou 3 valores para alpha e 3 valores para lambda, assim temos 9 testes.
#Visualizar valores testados 
modelo$results
#Visualizar valores testados para alpha e lambda 
modelo$results[,c(1,2,4)]

#-------------------------------------------------------------------------------
#Reconstruindo modelo definindo alpha e lambda manualmente
#Neste caso nao eh necessario usar a funcao metric uma vez que estamos passando os valores de alpha e lambda e nao sera feito nenhum tipo de comparacao,

set.seed(1)
modelo1<- train(Chance.of.Admit ~., data = BD, method= "glmnet", tuneGrid= data.frame(alpha=0.55, lambda=0.002487668), trControl= trainControl(method = 'cv', number = 5))

#modelo$resample = tabela , $Rsquared = valores dos R quadrados 
#Visualizar tabela com os K valores de Rquadrados
modelo1$resample$Rsquared
#Resumo dos K valores
summary(modelo1$resample$Rsquared)
#Medias dos K valores
mean(modelo1$resample$Rsquared)
#Identificando melhores valores para alpha e lambda
modelo1$bestTune
#Quantos valores de alpha e lambda foram testados?
#Por padrao a funcao train utilizou 3 valores para alpha e 3 valores para lambda, assim temos 9 testes.
#Visualizar valores testados 
modelo1$results
#Visualizar valores testados para alpha e lambda 
modelo1$results[,c(1,2,4)]


#-------------------------------------------------------------------------------
#Reconstruindo modelo com ajuste fino para encontrar uma melhor performance

#Visualizando resultados a partir dos valores alpha e lambda
View(modelo$results[,c(1,2,4)])
#Haja visto que as melhores performances apresentaram lambda com valores entre 0.002 a 0.009 criaremos outro modelo que realize mais testes neste intervalo
#O mesmo acontece para alpha com valores entre 0.5 a 0.9

#armazenando intervalos nas variaveis alpha e lambda
#Interpretacao: Selecione 20 valores no intervalo de 0.002 a 0.009 e armazene na variavel lambda
#Interpretacao: Selecione 5 valores no intervalo de 0.5 a 0.9 e armazene na variavel alpha
lambda<- seq(0.002, 0.009, length.out = 20)
alpha<- seq(0.5, 0.9, length.out = 5)

#Neste caso nao podemos criar um data frame com 5 linhas para alpha e 20 linhas para lambda
#entao utilizaremos a funcao expand.grid para contruir um data frame com todas as combicacoes possiveis (Neste caso 20*5 =100 combinacoes)
valores <- expand.grid(alpha= alpha, lambda= lambda)
#View(valores)

set.seed(1)
modelo<- train(Chance.of.Admit ~., data = BD, method= "glmnet", tuneGrid= valores, trControl= trainControl(method = 'cv', number = 5), metric= "Rsquared")

#modelo$resample = tabela , $Rsquared = valores dos R quadrados 
#Visualizar tabela com os K valores de Rquadrados
modelo$resample$Rsquared
#Resumo dos K valores
summary(modelo$resample$Rsquared)
#Medias dos K valores
mean(modelo$resample$Rsquared)
#Identificando melhores valores para alpha e lambda
modelo$bestTune
#Quantos valores de alpha e lambda foram testados?
#Por padrao a funcao train utilizou 3 valores para alpha e 3 valores para lambda, assim temos 9 testes.
#Visualizar valores testados 
modelo$results
#Visualizar valores testados para alpha e lambda 
modelo$results[,c(1,2,4)]

print(c(paste("Resultado do primeiro modelo", round(modelo1$resample$Rsquared)*100,2),"%"), paste("Resultado do segundo modelo", round(mean(modelo$resample$Rsquared)*100,2),"%"))

modelo1result <- mean(modelo1$resample$Rsquared)
modeloresult <- mean(modelo$resample$Rsquared)
modeloresult - modelo1result

#Apos o ajuste fino houve uma melhora de 4.835666e-05
