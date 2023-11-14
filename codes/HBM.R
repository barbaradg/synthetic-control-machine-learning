# LIBRERIAS 
library('StanHeaders')
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("dplyr")
library('caret')

#PREPRARCION DATA: LIMPIEZA Y ESCALAMIENTO
data <- read.csv("DF_SYNTH2_km.csv")
rut<-data[data$Semana==10,]$rut_seller
data<-as.data.frame(data[(data$rut_seller %in% rut),])
rownames(data) <- 1:nrow(data)
borrar<-c('plata','index_y', 'fecha','X')
data2<-data[ ,!(names(data) %in% borrar)]
data2$suma[data2$suma==0]<-0.1

features<-as.matrix(cbind(data2[4],data2[6:90]))
features<-as.data.frame(features)

# borrar2<-c('precio_median','G10', 'G03')
# features<-features[ ,!(names(features) %in% borrar2)]
# data3<-cbind(data2$suma, data2$Semana, features)
# colnames(data3)<-c('suma','Semana',colnames(data3[,3:50]))
data3<-cbind(data2$suma, data2$Semana, features[c('plata2','precio_desv','precio_median','precio_min','precio_75','SKU','tramo_6.0','tramo_9.0','G02','G06','G07','G09','G13','G16','G18','G21','G22','G0801','G0802','G1604','G1711','G1901','G2104','Otra.cat','X464100.0','X471990.0','X477399.0','X702000.0','X731001.0','cluster')])
colnames(data3)<-c('suma','Semana','plata2','precio_desv','precio_median','precio_min','precio_75','SKU','tramo_6.0','tramo_9.0','G02','G06','G07','G09','G13','G16','G18','G21','G22','G0801','G0802','G1604','G1711','G1901','G2104','Otra.cat','X464100.0','X471990.0','X477399.0','X702000.0','X731001.0','cluster')


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
#estandarizando
# estand<-as.matrix(cbind(data4[,1:4],data4[,6:13]))
# estand<- scales::rescale(estand, to=c(0,1))
#normalizando
# estand<-scale(cbind(data4[,1:4],data4[,6:13]))
# data4<-as.data.frame(estand)
#LOG Y EXP
estand<-cbind(log(data4[,1:9]+1),data4[,10:33],log(data4[,33:35]+1))
data4<-as.data.frame(estand)
#parar, ir a cv
#separacion train-test
set.seed(30)   #seed(9), seed(30)
data4$seller<-1:803
particion <- createDataPartition(data4$seller, p = 0.8, list = FALSE)
train <- data4[particion, ]
#train<-cbind(train['seller'],train[,1:4],scale(train[,5:12]))
test <- data4[-particion, ]
#test<-cbind(test['seller'],test[,1:4],scale(test[,5:12]))

# write.csv(train,'train_sellers50.csv')
# write.csv(test,'test_sellers50.csv')


summary(train[,1:4])
summary(test[,1:4])

Z_new=test[,5:37]
Z_prev=train[,5:37]
S=643
S_new=160
D=32
W=4
X_prev=train[,1:4]
X_new=test[,1:4]
model=stan_model('train_test.stan')
#

#### SOLO SEMANA 5 ####
y_train=train[,1]
data=list(S=S,S_new=S_new,D=D,W=W,X_prev=X_prev,X_new=X_new,Z_prev=Z_prev,Z_new=Z_new,y_train=y_train)
fit_data<- sampling(model,data,iter=1000,chains=2)


print(fit_data, pars = "gamma_b",
      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
      digits_summary = 2, include = TRUE)
print(fit_data, pars = "mu_bb",
      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
      digits_summary = 2, include = TRUE)
print(fit_data, pars = "beta",
      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
      digits_summary = 2, include = TRUE)
print(fit_data, pars = "beta_new",
      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
      digits_summary = 2, include = TRUE)
print(fit_data, pars = "mu_a",
      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
      digits_summary = 2, include = TRUE)
print(fit_data, pars = "sigma",
      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
      digits_summary = 2, include = TRUE)

print(fit_data, pars = "y_new",
      probs = c(0.025, 0.25, 0.5, 0.75, 0.975), include = TRUE)

samples_y_new <- as.data.frame(extract(fit_data)$y_new)
y_new <- as.data.frame(apply(samples_y_new, 2, mean))

#### TODAS LAS SEMANAS ####
# SET TRAIN
Z_new=train[,5:36]
Z_prev=train[,5:36]
S=643
S_new=643
D=32
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
Z_new=test[,5:36]
Z_prev=train[,5:36]
S=643
S_new=160
D=32
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


#

####CROSS VALIDATION####
cv<-as.data.frame(rep(1:5, each=160))
cv<-rbind(cv,5,5,5)
data4['CV']<-cv
model=stan_model('train_test.stan')
prediccion<-c()
real<-c()
D=8
W=4


for (i in 1:5){
  if (i<5){
    S=643 
    S_new=160 
    train=data4[data4$CV!=i,]
    test=data4[data4$CV==i,]
    Z_prev<- na.omit(train[,6:13])
    X_prev=train[,2:5] #vtas semanas de entrenamiento
    X_new=test[,2:5]
    Z_new<- na.omit(test[,6:13])
    y_trains <- subset(data3, data3$seller %in% rownames(train))
    y_tests<-subset(data3, data3$seller %in% rownames(test))
    for (j in 1:10){
      print(j)
      y_train= na.omit(y_trains['suma'][y_trains$Semana==j,])
      data=list(S=S,S_new=S_new,D=D,W=W,X_prev=X_prev,X_new=X_new,Z_prev=Z_prev,Z_new=Z_new,y_train=y_train)
      fit_data<- sampling(model,data,iter=1000,chains=2) #rhat tiene que estar en 1
      samples_y_new <- as.data.frame(extract(fit_data)$y_new)
      y_new <- as.data.frame(apply(samples_y_new, 2, mean))
      colnames(y_new)<-'pred'
      prediccion<-c(prediccion,y_new$pred)
      real<-c(real,y_tests$suma[y_tests$Semana==j])
    }
  }
  else{
    S=640 
    S_new=163 
    train=data4[data4$CV!=i,]
    test=data4[data4$CV==i,]
    Z_prev<- na.omit(train[,6:13])
    X_prev=train[,2:5] #vtas semanas de entrenamiento
    X_new=test[,2:5]
    Z_new<- na.omit(test[,6:13])
    y_trains <- subset(data3, data3$seller %in% rownames(train))
    y_tests<-subset(data3, data3$seller %in% rownames(test))
    for (j in 1:10){
      y_train= na.omit(y_trains['suma'][y_trains$Semana==j,])
      data=list(S=S,S_new=S_new,D=D,W=W,X_prev=X_prev,X_new=X_new,Z_prev=Z_prev,Z_new=Z_new,y_train=y_train)
      fit_data<- sampling(model,data,iter=1000,chains=2) 
      samples_y_new <- as.data.frame(extract(fit_data)$y_new)
      y_new <- as.data.frame(apply(samples_y_new, 2, mean))
      colnames(y_new)<-'pred'
      prediccion<-c(prediccion,y_new$pred)
      real<-c(real,y_tests$suma[y_tests$Semana==j])
    }
  }
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
write.csv(df_errores_test, 'HBM.csv')

df_sel3<-df_total_test[df_total_test$index==110,]

options(
  repr.plot.width = 8,  # Ancho deseado en pulgadas
  repr.plot.height = 4  # Alto deseado en pulgadas
)
ggplot(df_sel3, aes(x = Semana)) +
  geom_line(aes(y = real_test, color = "Data Real"), size=1) +
  geom_line(aes(y = prediccion_test, color = "Predicciones"), size=1) +
  labs(x = "Semana", y = "Ventas") +
  scale_color_manual(values = c("Data Real" = "blue", "Predicciones" = "red")) +
  ggtitle('Predicciones seller 110')




summary(df_errores_train2) # train normal
summary(df_errores_test2) #test normal

summary(df_errores_train3) #train normal cv
summary(df_errores_test3) #test normal cv

boxplot(df_errores_test$MAPE_test,ylim=c(0,2000))
boxplot(df_errores_test2$MAPE_test,ylim=c(0,2000))

## cv
prediccion[prediccion<0]=0

df_total3<-as.data.frame(cbind(prediccion,real))
df_total3$index<-c(rep(1:160, 10),rep(161:320,10),rep(321:480,10),rep(481:640,10),rep(641:803,10))
df_total3$Semana<-c(rep(1:10,each=160,4),rep(1:10,each=163))

library(MLmetrics)
MAPE_test<-c()
WMAPE_test<-c()
RMSE_test<-c()
MAPE_train<-c()
WMAPE_train<-c()
RMSE_train<-c()
df_train<-df_total3[df_total3$Semana<=4,]
df_test<-df_total3[df_total3$Semana>4,]

for (i in 1:803){
  PRED=df_train[df_train$index==i,'prediccion']
  REAL=df_train[df_train$index==i,'real']
  
  MAPE_train<-c(MAPE_train, MAPE(PRED,REAL)*100)
  WMAPE_train<-c(WMAPE_train,sum(abs(REAL-PRED))/sum(REAL)*100)
  RMSE_train<-c(RMSE_train,RMSE(PRED,REAL))
}
for (i in 1:803){
  PRED=df_test[df_test$index==i,'prediccion']
  REAL=df_test[df_test$index==i,'real']
  
  MAPE_test<-c(MAPE_test, MAPE(PRED,REAL)*100)
  WMAPE_test<-c(WMAPE_test,sum(abs(REAL-PRED))/sum(REAL)*100)
  RMSE_test<-c(RMSE_test,RMSE(PRED,REAL))
}
df_errores_train3<-as.data.frame(cbind(MAPE_train, WMAPE_train, RMSE_train))
df_errores_test3<-as.data.frame(cbind(MAPE_test, WMAPE_test, RMSE_test))
summary(df_errores_train3)
summary(df_errores_test3)

summary(df_errores_test2)






#errores por grupo de testeo (CV)
df_errores$cv<-data3$CV[data2['Semana']==1]
summary(df_errores[df_errores$cv==1,])
summary(df_errores[df_errores$cv==2,])
summary(df_errores[df_errores$cv==3,])
summary(df_errores[df_errores$cv==4,])
summary(df_errores[df_errores$cv==5,])

df_total$Semana<-rep(5:10,160)
colnames(df_total)<-c('prediccion','real','Seller','Semana')





#### EVALUACION ####

library(MLmetrics)
y_test5=data2['suma'][data2$Semana==5,]
y_test5=y_test5[641:803]
df_predict<- pred5
df_test<-c(y_test5)

df_total<-data.frame(value=1:978)
df_total$index<- rep(1:163,times=6)

df_total<- cbind(df_total$index,df_test,df_predict)
colnames(df_total)<-c('index','test','predict')
df_total<-as.data.frame(df_total)

df_total<-cbind(df_predict,df_test)
df_total$index<-1:163
colnames(df_total)<-c('predict','test','index')
MAPE_total<-c()
WMAPE_total<-c()
RMSE_total<-c()


for (i in 1:163){
  PRED=df_total[df_total$index==i,'predict']
  REAL=df_total[df_total$index==i,'test']
  
  MAPE_total<-c(MAPE_total, MAPE(PRED,REAL)*100)
  WMAPE_total<-c(WMAPE_total,sum(abs(REAL-PRED))/sum(REAL)*100)
  RMSE_total<-c(RMSE_total,RMSE(PRED,REAL))
}

boxplot(df_total[,1:2], ylim=c(0,200))






#borrando outliers
# q1 <- quantile(data2$suma, 0.25)
# q3 <- quantile(data2$suma, 0.75)
# iqr <- q3 - q1
# umbral_superior <- q3 + 1.5 * iqr
# umbral_inferior <- q1 - 1.5 * iqr
# data3$suma[data3$suma > umbral_superior | data3$suma < umbral_inferior] <- mean(data3$suma)



rownames(X_new)<-1:160
#gama_b
gama<-as.data.frame(extract(fit_data)$gamma_b)
gama <- as.data.frame(apply(gama, 2, mean))
colnames(gama)<-'promedio gamma_b'
col1<-gama[1:8,]
col2<-gama[9:16,]
col3<-gama[17:24,]
col4<-gama[26:32,]
gama_b<-as.data.frame(cbind(col1,col2,col3,col4))
colnames(gama_b)<-c('Semana 1','Semana 2','Semana 3', 'Semana 4')
gama_b
rownames(Z_new)<-1:160

#beta
BETA<-as.data.frame(extract(fit_data)$beta_new)
BETA<-as.data.frame(apply(BETA, 2, mean))

traceplot(fit_data, pars='lp__', include = TRUE,inc_warmup = TRUE)
?traceplot
