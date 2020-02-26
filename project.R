#### Projeto Supervisionado - an�lise de origem de aquisi��o dos clientes 
library(readxl)
library(Hmisc)
library(car)
library(Rcmdr)
library(RcmdrMisc)
library(ggplot)
library(ggplot2)
library(readr)
library(base)
library(MASS)
library("statmod")

## Leitura dos dados
data = readRDS("C:\\Users\\leonardo\\Desktop\\FACULDADE\\2019\\2 SEMESTRE\\PROJETO SUPERVISIONADA\\pink.RDS")

### TRATAMENTO DOS DADOS ###
i = 1
j = 0
data$Origem <- data$`Origem/M�dia`
for (i in 1:148554){
  if (data$`Origem/M�dia`[i] == "google / cpc"){
    data$Origem[i] = 1
    j = j + 1
  }else if (data$`Origem/M�dia`[i] == "google / organic"){
    data$Origem[i] = 1
    j = j + 1 
  }else{
    data$Origem[i] = 0
  }
}
data$Origem = as.factor(data$Origem)
data$`Origem/M�dia` <- NULL


i = 1
data$Dispositivo <- data$`Categoria Dispositivo`
for (i in 1:148554){
  if (data$`Categoria Dispositivo`[i] == "desktop"){
    data$Dispositivo[i] = 1
  }
  else{
    data$Dispositivo[i] = 0
  }
}
data$Dispositivo = as.factor(data$Dispositivo)


i = 1
data$Tipo <- data$`Tipo de Usu�rio`
for (i in 1:148554){
  if (data$`Tipo de Usu�rio`[i] == "Returning Visitor"){
    data$Tipo[i] = 1
  }
  else{
    data$Tipo[i] = 0
  }
}
data$Tipo = as.factor(data$Tipo)

## dados filtrados e pronto ###
data_raw <- data
data <- as.data.frame(data_raw[,c(6,7,8,9,10)])


## Divis�o dos Dados
set.seed(100)

data <- na.omit(data)
summary(data)
ran <- sample(1:nrow(data), 0.7 * nrow(data))

treino <- data[ran,]
teste <- data[-ran,]

data_target_treino <- data[ran,3]
data_target_teste <- data[-ran,3]

##### PRIMEIRO MODELO - TODAS AS COVARI�VEIS ########
mod_todos <- glm(treino$Origem ~ . , data = treino, family = binomial)
summary(mod_todos)
anova(mod_todos, test = "Chisq")
library(car)
vif(mod_todos)
pred <- predict(mod_todos, fun = predict, index = 4, newdata = teste[,-4], type = 'response')
round(pred,5)
require(pROC)
install.packages("pROC")
predroc1 <- roc(teste$Origem, pred, percent = TRUE)
c1 <- coords(predroc1, 'best'); c1
par(mfrow= c(1,1))
plot(predroc1,asp = 0.5, main = "Curva ROC - Modelo MLG Comum", ylab = "Sensibilidade (%)", xlab ="1 - Especificidade (%)")

OR1=exp(mod_todos$coefficients);OR1
ICbeta1=confint.default(mod_todos,level=0.95);ICbeta1
ICOR1=exp(ICbeta1);ICOR1
round((cbind(OR1, ICOR1)),3)
par(mfrow = c(2,2))
plot(mod_todos)

plot(fitted(mod_todos), qresiduals (mod_todos))
qqnorm(qresiduals (mod_todos), distribution = qnorm)

### Tabela de confus�o e medidas de performance preditiva.
class1 <- ifelse(pred > 0.7463401, 1, 0)
tabcruz <- table(class1, teste$Origem)
tabcruz
ac <- sum(diag(tabcruz))/nrow(teste); ac ### Acur�cia.
s <- tabcruz[2,2]/(tabcruz[1,2] + tabcruz[2,2]); s ### Sensibilidade.
e <- tabcruz[1,1]/(tabcruz[1,1] + tabcruz[2,1]); e ### Especificidade

## modelo com stepwise
step.model <- stepAIC(mod_todos,direction = 'both', trace = FALSE)
summary(step.model)
anova(step.model, test = "Chisq")
vif(step.model)
pred2 <- predict(step.model, newdata = treino)
round(pred2,5)
require(pROC)
install.packages("pROC")
predroc2 <- roc(treino$Origem, pred2, percent = TRUE)
c2 <- coords(predroc2, 'best'); c2
par(mfrow= c(1,1))
plot(predroc1)

OR2=exp(mod_sig$coefficients);OR2
ICbeta2=confint.default(mod_sig,level=0.95);ICbeta2
ICOR2=exp(ICbeta2);ICOR2
round((cbind(OR2, ICOR2)),3)
par(mfrow = c(2,2))
plot(mod_sig)



### modelo quadr�tico
mod_sig <- glm(treino$Origem ~ .^2,data = treino, family = binomial)
summary(mod_sig)
anova(mod_sig, test = "Chisq")
vif(mod_sig)
pred2 <- predict(mod_sig, newdata = treino)
round(pred2,5)
require(pROC)
install.packages("pROC")
predroc2 <- roc(treino$Origem, pred2, percent = TRUE)
c1 <- coords(predroc2, 'best'); c1
par(mfrow= c(1,1))
plot(predroc2,asp = 0.5, main = "Curva ROC - Modelo MLG Comum", ylab = "Sensibilidade (%)", xlab ="1 - Especificidade (%)")


OR2=exp(mod_sig$coefficients);OR2
ICbeta2=confint.default(mod_sig,level=0.95);ICbeta2
ICOR2=exp(ICbeta2);ICOR2
round((cbind(OR2, ICOR2)),3)
par(mfrow = c(2,2))
plot(mod_sig)


#### KNN ###
library(class)
pr <- class::knn(treino,teste,cl=data_target_treino,k=2, prob = TRUE, use.all = FALSE)
tab <- table(pr,data_target_teste)
tab

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
prob <- accuracy(tab)

### Calculando para a ROC curve ### 
prob <- attr(pr, "prob")
prob <- 2*ifelse(pr == "-1", 1-prob, prob) - 1
install.packages("ROCR")
library(ROCR)
y <- as.vector(pr, mode = "numeric")
y_teste <- as.vector(data_target_teste, mode = "numeric")
pred_knn<-prediction(y,y_teste)
pred_knn <- performance(pred_knn, "tpr", "fpr")
par(mfrow = c(1,1))
plot(pred_knn, avg= "threshold", lwd=3)


## Plot de acur�cia pelo k ##
i=1
k.optm=1
for (i in 1:20){
  knn.mod <- knn(train=treino, test = teste, cl=data_target_treino, k=i)
  k.optm[i] <- 100 * sum(data_target_teste == knn.mod)/NROW(data_target_teste)
  k=i
  cat(k,'=',k.optm[i],'')
}
ggplot(k.optm, type="b", xlab="K",ylab="Acur�cia", scale_x_continuous(breaks = round(seq(min(k.optm), max(k.optm), by = 0.5),1)),scale_y_continuous(breaks = round(seq(min(dat$y), max(dat$y), by = 0.5),1)))

## k escolhido - 3 
pr <- knn(treino,teste,cl=data_target_treino,k=3)
tab <- table(pr,data_target_teste)
tab

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


