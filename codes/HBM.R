# LIBRERIAS 
library('StanHeaders')
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("dplyr")
library('caret')

#PREPRARCION DATA: LIMPIEZA Y ESCALAMIENTO
data <- read.csv("DF_SYNTH2.csv")
rut<-data[data$Semana==10,]$rut_seller
data<-as.data.frame(data[(data$rut_seller %in% rut),])
rownames(data) <- 1:nrow(data)
borrar<-c('plata','index_y', 'fecha','X')
data2<-data[ ,!(names(data) %in% borrar)]
data2$suma[data2$suma==0]<-0.1

features<-as.matrix(cbind(data2[4],data2[6:90]))
features<-as.data.frame(features)

data3<-cbind(data2$suma, data2$Semana, features[c('plata2','precio_desv','precio_median','precio_min','precio_75','SKU','tramo_6.0','tramo_9.0','G02','G06','G07','G09','G13','G16','G18','G21','G22','G0801','G0802','G1604','G1711','G1901','G2104','Otra.cat','X464100.0','X471990.0','X477399.0','X702000.0','X731001.0')])
colnames(data3)<-c('suma','Semana','plata2','precio_desv','precio_median','precio_min','precio_75','SKU','tramo_6.0','tramo_9.0','G02','G06','G07','G09','G13','G16','G18','G21','G22','G0801','G0802','G1604','G1711','G1901','G2104','Otra.cat','X464100.0','X471990.0','X477399.0','X702000.0','X731001.0')


y1= data3['suma'][data3$Semana==1,]
y2= data3['suma'][data3$Semana==2,]
y3= data3['suma'][data3$Semana==3,]
y4= data3['suma'][data3$Semana==4,]
plata2_<-data3['plata2'][data3$Semana==2,]
plata3<-data3['plata2'][data3$Semana==3,]
plata4<-data3['plata2'][data3$Semana==4,]
data4<-cbind(y1,y2,y3,y4,data3[data3$Semana==1,], plata2_,plata3,plata4)
borrar<-c('plata2','Semana','suma')
data4<-data4[ ,!(names(data4) %in% borrar)]
rownames(data4)<-1:803

# ESCALADO LOG Y EXP
estand<-cbind(log(data4[,1:9]+1),data4[,10:33],log(data4[,33:35]+1))
data4<-as.data.frame(estand)
#parar, ir a cv
#separacion train-test
set.seed(30)  
data4$seller<-1:803
particion <- createDataPartition(data4$seller, p = 0.8, list = FALSE)
train <- data4[particion, ]
#train<-cbind(train['seller'],train[,1:4],scale(train[,5:12]))
test <- data4[-particion, ]
#test<-cbind(test['seller'],test[,1:4],scale(test[,5:12]))

# write.csv(train,'train_sellers.csv')
# write.csv(test,'test_sellers.csv')


#### TODAS LAS SEMANAS ####
# SET TRAIN
Z_new=train[,5:35]
Z_prev=train[,5:35]
S=643
S_new=643
D=31
W=4
X_prev=train[,1:4]
X_new=train[,1:4]
model=stan_model('train_test.stan')
prediccion_train<-c()
real_train<-c()
data3$seller<-rep(1:803,each=10)

y_trains <- subset(data3, data3$seller %in% rownames(train))
y_tests<-subset(data3, data3$seller %in% rownames(train))
for (j in 5:10){
  print(j)
  y_train= na.omit(y_trains['suma'][y_trains$Semana==j,])
  data=list(S=S,S_new=S_new,D=D,W=W,X_prev=X_prev,X_new=X_new,Z_prev=Z_prev,Z_new=Z_new,y_train=y_train)
  fit_data<- sampling(model,data,iter=1000,chains=2) 
  samples_y_new <- as.data.frame(extract(fit_data)$y_new)
  y_new <- as.data.frame(apply(samples_y_new, 2, mean))
  colnames(y_new)<-'pred'
  prediccion_train<-c(prediccion_train,y_new$pred)
  real_train<-c(real_train,y_tests$suma[y_trains$Semana==j])
}

#SET TESTEO
Z_new=test[,5:35]
Z_prev=train[,5:35]
S=643
S_new=160
D=31
W=4
X_prev=train[,1:4]
X_new=test[,1:4]
model=stan_model('train_test.stan')
prediccion_test<-c()
real_test<-c()
data3$seller<-rep(1:803,each=10)
y_trains <- subset(data3, data3$seller %in% rownames(train))
y_tests<-subset(data3, data3$seller %in% rownames(test))

for (j in 5:10){
  print(j)
  y_train= na.omit(y_trains['suma'][y_trains$Semana==j,])
  data=list(S=S,S_new=S_new,D=D,W=W,X_prev=X_prev,X_new=X_new,Z_prev=Z_prev,Z_new=Z_new,y_train=y_train)
  fit_data<- sampling(model,data,iter=1000,chains=2) 
  samples_y_new <- as.data.frame(extract(fit_data)$y_new)
  y_new <- as.data.frame(apply(samples_y_new, 2, mean))
  colnames(y_new)<-'pred'
  prediccion_test<-c(prediccion_test,y_new$pred)
  real_test<-c(real_test,y_tests$suma[y_tests$Semana==j])
}


#### Evaluacion ####

#SET TRAAIN
prediccion_train[prediccion_train<0]=0
df_total_train<-as.data.frame(cbind(prediccion_train,real_train))
df_total_train$index<-c(rep(1:643, 6))
df_total2_trainSemana<-rep(5:10,each=643)

pred=df_total_train[,1]
y=df_total_train[,2]
resid=y-pred
n=length(y)
sigma=sd(resid)
sm_lg=exp((1/n)* sum(resid^2)/(2*sigma^2))-((n-1)/(2*n))

df_total_train['prediccion_train']= pred*sm_lg
df_total_train


#SET TEST
prediccion_test[prediccion_test<0]=0
df_total_test<-as.data.frame(cbind(prediccion_test,real_test))
df_total_test$index<-c(rep(1:160, 6))
df_total_test$Semana<-rep(5:10,each=160)

pred=df_total_test[,1]
y=df_total_test[,2]
resid=y-pred
n=length(y)
sigma=sd(resid)
sm_lg=exp((1/n)* sum(resid^2)/(2*sigma^2))-((n-1)/(2*n))

df_total_test['prediccion_test']= pred*sm_lg
df_total_test

library(MLmetrics)
MAPE_test<-c()
WMAPE_test<-c()
RMSE_test<-c()
MAPE_train<-c()
WMAPE_train<-c()
RMSE_train<-c()
df_train<-df_total_train
df_test<-df_total_test

for (i in 1:643){
  PRED=df_train[df_train$index==i,'prediccion_train']
  REAL=df_train[df_train$index==i,'real_train']
  
  MAPE_train<-c(MAPE_train, MAPE(PRED,REAL)*100)
  WMAPE_train<-c(WMAPE_train,sum(abs(REAL-PRED))/sum(REAL)*100)
  RMSE_train<-c(RMSE_train,RMSE(PRED,REAL))
}
for (i in 1:160){
  PRED=df_test[df_test$index==i,'prediccion_test']
  REAL=df_test[df_test$index==i,'real_test']
  
  MAPE_test<-c(MAPE_test, MAPE(PRED,REAL)*100)
  WMAPE_test<-c(WMAPE_test,sum(abs(REAL-PRED))/sum(REAL)*100)
  RMSE_test<-c(RMSE_test,RMSE(PRED,REAL))
}

df_errores_train<-as.data.frame(cbind(MAPE_train, WMAPE_train, RMSE_train))
df_errores_test<-as.data.frame(cbind(MAPE_test, WMAPE_test, RMSE_test))

#Visualización  errores
summary(df_errores_train) 
summary(df_errores_test) 


