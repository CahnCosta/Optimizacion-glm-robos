
#### 1. libraries and clean ####
rm(list=ls())
library(rpart)
library(rpart.plot)
library(ggplot2)
library(lubridate)
library(dplyr)

#### 2. read data ####
df_train <- read.csv('data/df_train.csv')
rownames(df_train) <- df_train$X
df_test <- read.csv('data/df_test.csv')
rownames(df_test) <- df_test$X

summary(df_train)

#### 3. treat data ####

#separo hora y fecha
df_train$Date <- as.POSIXlt(df_train$Date)
df_test$Date <- as.POSIXlt(df_test$Date)
df_train$Month <- as.numeric(format(df_train$Date,format="%m"))
df_train$Weekday <- weekdays(df_train$Date)
df_train$Hour <- as.numeric(format(df_train$Date,format="%H"))
df_train$Date <- as.Date(df_train$Date)

df_test$Month <- as.numeric(format(df_test$Date,format="%m"))
df_test$Weekday <- weekdays(df_test$Date)
df_test$Hour <- as.numeric(format(df_test$Date,format="%H"))
df_test$Date <- as.Date(df_test$Date)



df_train$Target <- as.factor(df_train$Target)
df_train$f1 <- as.factor(df_train$f1)
df_train$f2 <- as.factor(df_train$f2)
df_test$f1 <- as.factor(df_test$f1)
df_test$f2 <- as.factor(df_test$f2)

# verifies NAs in Ammount and mark them
df_train$AmmountNA <- F
df_train$AmmountNA[is.na(df_train$Ammount)] <- T

df_test$AmmountNA <- F
df_test$AmmountNA[is.na(df_test$Ammount)] <- T

#quiero visualizar la distribucion geografica a ver si puedo reconocer barrios o zonas
ggplot(df_test, aes(x = LONG, y = LAT)) + geom_point()
#aprox 7 barrios, probar luego con más o menos

#emprolijo los dfs para poder hacerles un rbind()

df_train$aux <- 1
df_test$aux <- 0
df_test$Target <- NA

df_tot <- rbind(df_train, df_test)

#Armo n clusters de ubicacion
n <- 5
XY_tot <- data.frame(df_tot$LONG)
XY_tot$LAT <- df_tot$LAT 
A <- kmeans(XY_tot, n)
XY_tot$Cluster <- A$cluster
df_tot$Cluster <- as.factor(XY_tot$Cluster)
#es horrible armar las columnas del cluster en los df asi pero no se por que no funciona otra cosa
summary(df_tot)

#los vuelo a separar
df_train <- df_tot %>% filter(aux==1)
df_test <- df_tot %>% filter(aux==0)

ggplot(df_test, aes(x = LONG, y = LAT, color = Cluster)) + geom_point()

#saco columnas que no necesito mas
df_train$aux <- NULL
df_test$aux <- NULL

#cuts the largest numbers in Ammount. within the 95 quartile
thres <- quantile(df_train$Ammount, 0.8,na.rm=T)
ids_to_mark <- df_train$Ammount > thres
df_train$Ammount[ids_to_mark] <- thres
df_train$AmmountEdited <- F
df_train$AmmountEdited[ids_to_mark] <- T

# marks the observations with Ammount edited
ids_to_mark <- df_test$Ammount > thres
df_test$Ammount[ids_to_mark] <- thres
df_test$AmmountEdited <- F
df_test$AmmountEdited[ids_to_mark] <- T


rownames(df_train) <- df_train$X
rownames(df_test) <- df_test$X
df_train$X <- NULL
df_test$X <- NULL

#### 4. fit tree ####
loss_matr <- matrix(c(0, 1, 2, 0), nrow = 2)
fit <- rpart(Target~., data=df_train,cp=0.0005,parms = list(loss = loss_matr))
rpart.plot(fit)

#### 5. use the model to predict ####
out <- predict(fit, newdata = df_test, type='class')
df_out <- data.frame(id=rownames(df_test), prediction=out) #id and prediction must be the col.names
write.csv(df_out,file='data/out.csv',row.names =F)

#score con este script: 1869!!!!


