
##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################


#Pacotes utilizados
pacotes <- c("plotly",
             "tidyverse",
             "tidymodels",
             "patchwork",
             "FactoMineR",
             "factoextra",
             "ggrepel",
             "fastDummies",
             "knitr",
             "kableExtra",
             "splines",
             "reshape2",
             "PerformanceAnalytics",
             "metan",
             "correlation",
             "see",
             "ggraph",
             "nortest","rgl",
             "car","olsrr",
             "jtools",
             "ggstance",
             "magick",
             "beepr",
             "Rcpp", 
             "lmtest",
             "caret", 
             "pROC",
             "ROCR",
             "nnet",
             "magick",
             "cowplot",
             "rpart",      # Biblioteca de árvores
             "rpart.plot", # Conjunto com Rpart, plota a parvore
             "gtools",     # funções auxiliares como quantcut,
             "Rmisc",      # carrega a função sumarySE para a descritiva
             "scales",     # importa paletas de cores
             'neuralnet',   # Pacote para fazer redes neurais.
             'viridis',
             'gamlss',
             'gamlss.add',
             "skimr",
             "mice",
             "lme4",
             "randomForest",
             "tree",
             "swiss",
             "GGally",
             "corrgram",
             "corrplot",
             "rpart.plot",
             "e1071",
             "PerformanceAnalytics",
             "xgboost",
             "magrittr",
             "dplyr",
             "Matrix",
             "caTools",
             "janitor", #Usada para a funçao clean_names que já limpa espaço por NA
             "funModeling",# Usada na def_status para padronizar e juntar os conjuntos
             "visdat",
             "mlbench"
             
             
)




if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


#Listar os arquivos do nosso project
list.files()



#Carregando a base de dados
#LEITURA DOS DADOS
meta <- read.csv(file = "metadata.csv", na.strings = c(""," ")) %>% clean_names()
train <- read.csv(file = "train.csv",na.strings = c(""," ")) %>% clean_names()
test <- read.csv(file = "test.csv",na.strings = c(""," ")) %>% clean_names()





#Padronizar variaveis com a media e moda


for ( i in 2:55){
  train[,i]  <- factor( train[,i] )
  train[i][is.na(train[i])] <- which.max(train[,i])
  train[i][train[i] == -999]  <- which.max(train[,i])
  train[i][train[i] == 999]  <- which.max(train[,i])
  
  test[,i]  <- factor( test[,i] )
  test[i][is.na(test[i])] <- which.max(train[,i])
  test[i][test[i] == -999]  <- which.max(test[,i])
  test[i][test[i] == 999]  <- which.max(test[,i])
  
}


for ( i in 56:67){
  train[,i][is.na(train[i])] <- mean(train[,i])
  train[i][train[i] == -999]  <- mean(train[,i])
  train[i][train[i] == 999]  <- mean(train[,i])
  train[i] <- scale(train[,i])
  
  test[,i][is.na(test[i])] <- mean(test[,i])
  test[i][test[i] == -999]  <- mean(test[,i])
  test[i][test[i] == 999]  <- mean(test[,i])
  test[i] <- scale(test[,i])
  
}




train$y  <- factor( train$y )
train$id  <- NULL
test$id  <- NULL



#Eliminar colunas desnecessárias

col_del <- c("var1","var2","var3","var4","var5","var6","var7",
             "var8","var10","var11","var12","var13",
             "var14","var16","var18",
             "var15","var17","var19","var20","var28",
             "var21","var32","var34","var35","var36",
             "var37",
             "var42",#revisar
             "var38","var40","var43","var44",
             "var45","var46","var47","var48",
             "var52",
             #"var54",
             "var67","var68")

train[col_del] <- NULL
test[col_del] <- NULL




#DUMMIES

train_dummies <- dummy_columns(.data = train,
                               select_columns = c("var9","var22","var23","var24","var25","var26",
                                                  "var27","var29", "var30","var31","var33","var39",
                                                  "var41","var49","var50","var51","var53","var54"
                               ),
                               remove_selected_columns = T,
                               remove_first_dummy = T)      



test_dummies <- dummy_columns(.data = test,
                              select_columns = c("var9","var22","var23","var24","var25","var26",
                                                 "var27","var29", "var30","var31","var33","var39",
                                                 "var41","var49","var50","var51","var53","var54"
                              ),
                              remove_selected_columns = T,
                              remove_first_dummy = T)  





treino_dummies <- dummy_columns(.data = train,
                               select_columns = c("var9","var22","var23","var24","var25","var26",
                                                  "var27","var29", "var30","var31","var33","var39",
                                                  "var41","var49","var50","var51","var53","var54"
                               ),
                               remove_selected_columns = T,
                               remove_first_dummy = T)      



teste_dummies <- dummy_columns(.data = test,
                              select_columns = c("var9","var22","var23","var24","var25","var26",
                                                 "var27","var29", "var30","var31","var33","var39",
                                                 "var41","var49","var50","var51","var53","var54"
                              ),
                              remove_selected_columns = T,
                              remove_first_dummy = T)  




#Data Partition2 - Exemplo1
set.seed(123)
indices <- sample(nrow(train),0.75*nrow(train),replace = FALSE)
treino <- train[indices,]
teste  <- train[-indices,]



indices <- sample(nrow(train_dummies),0.75*nrow(train_dummies),replace = FALSE)
treino_dummies <- train_dummies[indices,]
teste_dummies  <- train_dummies[-indices,]





###############ESTIMAÇÃO DE UM MODELO LOGÍSTICO BINÁRIO###############
modelo_contrata<- glm(formula = y ~ 
                   
                        
                      + var22_1 + var22_2 + var22_3 + var22_4 + var22_5#dummies
                      + var23_1 + var23_2 + var23_3 + var23_4 #dummies
                      + var24_1 + var24_2 
                      + var27_1#dummies 
                      + var25_1 + var25_2 + var25_3 + var25_4 #dummies
                      + var29_1 + var29_2 + var29_3 + var29_4 + var29_5#dummies
                      + var30_1 + var30_2 
                      + var31_1 #dummies 
               
                      + var33_1 + var33_2 + var33_3 + var33_4 + var33_5 + var33_6 #dummies
                      
                      + var50_1 
                      
                      + var53_1 + var53_2 + var53_3  #dummies
                      + var54_1 + var54_2 + var54_3  #dummies
                      
                      
                      
                      + var55 + var56 + var57 + var58  
                      + var59 + var60 + var61  
                      + var62 + var63 + var64
                      + var65 + var66
                      
                      
                  ,data = train_dummies, 
                  family = "binomial")





#Parâmetros do modelo
summary(modelo_contrata)


#Extração do valor de Log-Likelihood (LL)
logLik(modelo_contrata)





# Adicionando os valores previstos de probabilidade da base de dados
train_dummies$phat <- modelo_contrata$fitted.values #recorrencia a partir das caracteristiscas....



#Matriz de confusão para cutoff = 0.5 (função confusionMatrix do pacote caret)
confusionMatrix(table(predict(modelo_contrata, type = "response") >= 0.5,
                      train_dummies$y == 1)[2:1, 2:1])


#Visualizando os principais indicadores desta matriz de confusão
data.frame(Sensitividade = confusionMatrix(table(predict(modelo_contrata,
                                                         type = "response") >= 0.2,
                                                 train_dummies$y == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(modelo_contrata,
                                                          type = "response") >= 0.2,
                                                  train_dummies$y == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(modelo_contrata,
                                                    type = "response") >= 0.2,
                                            train_dummies$y == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)





#função prediction do pacote ROCR
predicoes <- ROCR::prediction(predictions = modelo_contrata$fitted.values, 
                              labels = train_dummies$y) 


#função performance do pacote ROCR
dados_curva_roc <- performance(predicoes, measure = "sens") 



#Desejamos os dados da sensitividade e de especificidade. Então, devemos
#digitar os seguintes códigos::

sensitividade <- (performance(predicoes, measure = "sens"))@y.values[[1]] 

especificidade <- (performance(predicoes, measure = "spec"))@y.values[[1]]


#Extraindo os cutoffs:
cutoffs <- dados_curva_roc@x.values[[1]] 



#frame que contém os vetores mencionados.

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)





#Visualizando o novo data frame dados_plotagem
#IMPORTANTE!!!! MOSTRA PARA CADA CUTOFF de sensibilidade e especificado o valor a ser implantado
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)




#O ENCONTRO DE X E Y NÃO QUER DIZER QUE VAI AUMENTAR O VALOR DE ACURACIA DO MODELO !!!!!!!!
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())



#CONSTRUÇÃO DA CURVA RO


#função roc do pacote pROC
ROC <- roc(response = train_dummies$y, 
           predictor = modelo_contrata$fitted.values)



ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)






###############RAONDOM FOREST###############


modelFinal  <- randomForest(y ~ . 
                            -var30       
                            -var33                 
                            -var41             
                            -var49       
                            -var51   
                            ,
                            ntree=500,
                            mtry=3,#melhor quantidade de variaveis
                            data=train, 
                            importance = TRUE,
                            prOximity=TRUE,
                            na.action=na.roughfix)
modelFinal




#Checar importancia das varaeis
importance(modelFinal)

#Checar importancia das variaveis grafico
windows()
dev.new()
varImpPlot(modelFinal, sort = T)



#usando for para identificar a direita mtry para o model
a=c()
i=7
for ( i in 3:10){
  modelFinal  <- randomForest(y ~ .
                              -var30       
                              -var33                 
                              -var41             
                              -var49       
                              -var51   
                              , 
                          ntree=500,
                          mtry=i,
                          data=train, 
                          importance = TRUE,
                          prOximity=TRUE,
                          na.action=na.roughfix
                          )
  
  predValid <- predict(modelFinal, train, type ="class")
  a[i-2] =mean(predValid == train$y)
}

#Verificando a melhor quantidade de arvores e acuracia
a
plot(3:10,a)



#Modelo final depois de checar a melhor quantidade de variaveis x indice gini
modeloFinal  <- randomForest(y ~ . 
                            -var30       
                            -var33                 
                            -var41             
                            -var49       
                            -var51   
                            ,
                            ntree=500,
                            mtry=7,#melhor quantidade de variaveis
                            data=treino, 
                            importance = TRUE,
                            proximity=TRUE,
                            na.action=na.roughfix)
modeloFinal







#Plotar os erros
plot(modeloFinal$err.rate) # erro OOB


# MeanDecreaseAccuracy: permutação
importance(modeloFinal, type = 1)


# Quantas vezes cada variável explicativa foi utilizada na construção das árvores
varUsed(modeloFinal, count = T)



#Atribuindo as probablidades

p1 <- predict(modeloFinal, treino, type = "prob")
p2 <- predict(modeloFinal, teste, type = "prob")

treino$y = p1
teste$y = p2


write.csv(treino, file = "treino_randomForest.csv")
write.csv(teste, file = "teste_randomForest.csv")








#################xgboost##################################


treino.x = model.matrix(y ~ .,
                        data = treino)

teste.x = model.matrix(y ~.,
                       data = teste)





treino_dummies.x = model.matrix(y ~. ,
                                data = treino_dummies)

teste_dummies.x = model.matrix(y ~.,
                               data = teste_dummies)



X_train = data.matrix(treino_dummies[,-80])                  # independent variables for train
y_train = as.matrix(treino_dummies[,80])                                # dependent variables for train


X_test = data.matrix(teste_dummies[,-80])                    # independent variables for test
y_test =  as.matrix(teste_dummies[,,80])                                   # dependent variables for test



xgboost_train = xgb.DMatrix(data=X_train, label=y_train)



#xgboost
modelo.xgb = xgboost( data = data.matrix(treino_dummies),
                      label = as.numeric(as.character(treino_dummies$y)),
                      
                      stopping_metric = c("AUC"),
                      categorical_encoding = c("AUTO"),
                      ntrees = 500,
                      max_depth = 7,
                      min_rows = 10,
                      learn_rate = 0.2,
                      calibrate_model = FALSE,
                      max_bins = 256,
                      reg_lambda =1,
                      backend = c("cpu"),
                      training_frame = treino_dummies,
                      validation_frame = teste_dummies,
                      eta = 0.1,
                      nrounds = 1000,
                      #objective="binary:logistic",
                      set.seed=1502,
                      eval_metric ="auc",
                      verbose=1,
)



modelo.xgb10 = xgboost( data = xgboost_train,
                       
                       stopping_metric = c("AUC"),
                       categorical_encoding = c("AUTO"),
                       ntrees = 500,
                       max_depth = 7,
                       min_rows = 10,
                       learn_rate = 0.2,
                       calibrate_model = FALSE,
                       max_bins = 256,
                       reg_lambda =1,
                       backend = c("cpu"),
                       #training_frame = treino,
                       #validation_frame = teste,
                       #seed = seed,
                       
                       eta = 0.1,
                       nrounds = 100,
                       #objective="binary:logistic",
                       set.seed=1502,
                       eval_metric ="auc",
                       objective="binary:logistic",
                       
                       verbose=1,
)




#Informações sobre os modelos
summary(modelo.xgb)
summary(modelo.xgb10)




#transformação
prob_treino = xgb.DMatrix(data.matrix(treino_dummies), missing = NA)
prob_teste = xgb.DMatrix(data.matrix(teste_dummies), missing = NA)



#Atribuindo as probablidades

p1 <- predict(modelo.xgb, newdata=prob_treino, type = "prob")
p2 <- predict(modelo.xgb, newdata=prob_teste,  type = "prob" )

treino$y = p1
teste$y = p2


write.csv(treino, file = "treino_xboost.csv")
write.csv(teste, file = "teste_xboost.csv")
