#importando arquivo
BD <- read.csv( "C:/XXXX/Thiago Silva/XXXXXDataSciencexxxxx/XXXX/Admission_Predict.csv", sep = ",", encoding = "UTF-8",na.strings = "")

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