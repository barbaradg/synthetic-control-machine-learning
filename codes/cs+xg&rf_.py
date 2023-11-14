# -*- coding: utf-8 -*-
"""CS+XG&RF_.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1n34aN5mAMputF0tWcEmizfy4VjVq8WgA
"""
# -*- coding: utf-8 -*-
"""cs+xgb2_.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1n34aN5mAMputF0tWcEmizfy4VjVq8WgA
"""

#librerias
import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
import seaborn as sns 
from dateutil.relativedelta import relativedelta
from datetime import datetime, date
from tqdm import tqdm
import warnings
from sklearn.metrics import mean_absolute_percentage_error, mean_squared_error
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import GridSearchCV
from sklearn.preprocessing import StandardScaler
from scipy.stats import randint, uniform


warnings.filterwarnings("ignore")
tqdm.pandas()
def WMAPE2 (true,predicted):
    WMAPE2=np.round((np.sum((np.abs(true-predicted)))/np.sum(true))*100,3)
    return (WMAPE2)


#Creacion matrices X_train, y_train, X_test e y_test
df=pd.read_csv('DF_SYNTH2.csv') # dataframe con todos los sellers y variables
train_sellers=pd.read_csv('train_sellers.csv') #dataframe con solo los sellers seleccionados de entrenamiento
test_sellers=pd.read_csv('test_sellers.csv') #dataframe con solo los sellers seleccionados de testeo
df=df.iloc[:,1:94]
df=df[df['Semana']<=10] #se selecciona hasta la semana 10 de los sellers en falabella.com
df=df.drop(['plata'],axis=1)

#Escalamos variables numericas con logaritmo, despues de predecir se saca exponencial y se aplica factor de correccion de sesgo
df_escaled=df
df_escaled=df_escaled.set_index('index')
df_escaled=df_escaled.drop('rut_seller', axis=1)
df_escaled['suma']=df_escaled['suma'].replace(0,0.1)
df_escaled.iloc[:,3:13]=np.log(df_escaled.iloc[:,3:13]+1)
df_escaled['plata2']=np.log(df_escaled['plata2']+1)
df_escaled['suma']=np.log(df_escaled['suma']+1)

df_escaled=pd.concat([df_escaled.iloc[:,0:11],df_escaled.iloc[:,11:94]], axis=1)

#Se seleccionan variables más influyentes obtenidas del análisis con LASSO
df_escaled=df_escaled[['suma','Semana','plata2','precio_desv','precio_median','precio_min','precio_75','SKU','tramo_6.0','tramo_9.0','G02','G06','G07','G09','G13','G16','G18','G21','G22','G0801','G0802','G1604','G1711','G1901','G2104','Otra cat','464100.0','471990.0','477399.0','702000.0','731001.0','cluster']]

#Se separan las matrices en el periodo donde se busca predecir (Semana 4)
df_escaled_ent=df_escaled[df_escaled['Semana']<=4]
df_escaled_test=df_escaled[df_escaled['Semana']>4]

#Valores reales para las semanas a predecir
sem5=df['suma'][df['Semana']==5].reset_index().drop('index',axis=1)
sem6=df['suma'][df['Semana']==6].reset_index().drop('index',axis=1)
sem7=df['suma'][df['Semana']==7].reset_index().drop('index',axis=1)
sem8=df['suma'][df['Semana']==8].reset_index().drop('index',axis=1)
sem9=df['suma'][df['Semana']==9].reset_index().drop('index',axis=1)
sem10=df['suma'][df['Semana']==10].reset_index().drop('index',axis=1)
index=list(range(1,804))
df_test=pd.concat([sem5,sem6,sem7,sem8,sem9,sem10], axis=1)
df_test['index']=index
df_test.columns = ['Semana 5','Semana 6','Semana 7','Semana 8', 'Semana 9','Semana 10','index']

#Separacion sets
X_train=df_escaled_ent[df_escaled_ent.index.isin(train_sellers.seller)]
y_train=df_escaled_test[df_escaled_test.index.isin(train_sellers.seller)]

X_test=df_escaled_ent[df_escaled_ent.index.isin(test_sellers.seller)]
y_test=df_escaled_test[df_escaled_test.index.isin(test_sellers.seller)]

X_train=X_train.reset_index()
y_train=y_train.reset_index()
X_test=X_test.reset_index()
y_test=y_test.reset_index()

#--------------------------------------------------------------------------------------------------------------------------------#

#Transformando a formato 'control sintético'
features = X_train.drop(['Semana','index'],axis=1).columns
X_train_inv = (X_train
            .pivot(index='index', columns="Semana")[features]
            .T)
features= y_train.drop(['Semana','index'],axis=1).columns
y_train_inv = (y_train
            .pivot(index='index', columns="Semana")[features]
            .T)
features = X_test.drop(['Semana','index'],axis=1).columns
X_test_inv = (X_test
            .pivot(index='index', columns="Semana")[features]
            .T)
features= y_test.drop(['Semana','index'],axis=1).columns
y_test_inv = (y_test
            .pivot(index='index', columns="Semana")[features]
            .T)

#Sacando duplicados
X_train_inv=X_train_inv.reset_index()
vtas=X_train_inv.iloc[0:8,:]
resto=X_train_inv.iloc[8:348,:]
X_train_inv=pd.concat([vtas,resto[resto['Semana']==1]], axis=0)
X_train_inv=X_train_inv.set_index(['level_0','Semana'])

y_train_inv=y_train_inv.reset_index()
vtas=y_train_inv.iloc[0:12,:]
resto=y_train_inv.iloc[12:348,:]
y_train_inv=pd.concat([vtas,resto[resto['Semana']==5]], axis=0)
y_train_inv=y_train_inv.set_index(['level_0','Semana'])

X_test_inv=X_test_inv.reset_index()
vtas=X_test_inv.iloc[0:8,:]
resto=X_test_inv.iloc[8:348,:]
X_test_inv=pd.concat([vtas,resto[resto['Semana']==1]], axis=0)
X_test_inv=X_test_inv.set_index(['level_0','Semana'])

y_test_inv=y_test_inv.reset_index()
vtas=y_test_inv.iloc[0:12,:]
resto=y_test_inv.iloc[12:348,:]
y_test_inv=pd.concat([vtas,resto[resto['Semana']==5]], axis=0)
y_test_inv=y_test_inv.set_index(['level_0','Semana'])

#---- Hiperparametros XGBOOST----#
#Descomentar para encontrar los hiperparametros de XGBOOST, se recomienda comentar y correr todo de nuevo una vez encontrados
#df_escaled=df_escaled.reset_index()
# df=df.iloc[:,1:90]
# X_train=pd.concat([train_sellers.iloc[:,1:5],train_sellers.iloc[:,11:14],df_escaled[df_escaled.index.isin(train_sellers.index)].drop('index', axis=1)], axis=1)
# y_train=df_test[df_test.index.isin(train_sellers.index)].drop('index', axis=1)
# X_test=pd.concat([test_sellers.iloc[:,1:5],test_sellers.iloc[:,11:14],df_escaled[df_escaled.index.isin(test_sellers.index)].drop('index', axis=1)], axis=1)
# y_test=df_test[df_test.index.isin(test_sellers.index)].drop('index', axis=1)
# scaler = StandardScaler()
# X_train_norm = pd.DataFrame(
#     scaler.fit_transform(X_train),
#     columns = X_train.columns
# )
# X_test_norm = pd.DataFrame(
#     scaler.fit_transform(X_test),
#     columns = X_test.columns
# )
# #Escalando
# X_train_esc=pd.concat([np.log(X_train[['y1','y2','y3','y4','precio_median','SKU','precio_min','precio_75','plata2_','plata3','plata4']]+1),X_train[['tramo_6.0','tramo_9.0','G02','G06','G07','G09','G13','G16','G18','G21','G22','G0801','G0802','G1604','G1711','G1901','G2104','Otra cat','464100.0','471990.0','477399.0','702000.0','731001.0']]], axis=1)
# X_test_esc=pd.concat([np.log(X_test[['y1','y2','y3','y4','precio_median','SKU','precio_min','precio_75','plata2_','plata3','plata4']]+1),X_test[['tramo_6.0','tramo_9.0','G02','G06','G07','G09','G13','G16','G18','G21','G22','G0801','G0802','G1604','G1711','G1901','G2104','Otra cat','464100.0','471990.0','477399.0','702000.0','731001.0']]], axis=1)

#Metodo RandomizedSearchCV demora menor que GridSearchCV, si bien no es muy completo da indicios de los parametros a probar en GridSearchCV
# parameters = {
#     'n_estimators': randint(100, 1000),
#     'learning_rate': uniform(0.01, 0.1),
#     'max_depth': randint(3, 6),
#     'min_child_weight': randint(1, 5),
#     'subsample': uniform(0.5, 0.5),
#     'gamma': uniform(0, 0.5),
#     'colsample_bytree': uniform(0.5, 0.5)
# }
# model = xgb.XGBRegressor()

# grid_lr =RandomizedSearchCV(estimator=model, param_distributions=parameters, n_iter=10,cv=3,refit=True,n_jobs=-1)
# grid_lr.fit(X_train, y_train)
#print(grid_lr.best_estimator_)

#Método GridSearchCV 
# parameters = {
#     'n_estimators': [100, 300, 500, 600],
#     'learning_rate': [0.001,0.01,0.05],
#     'max_depth': [3,4,5,6],
#     'min_child_weight': [1,3,5]
# }
# model=xgb.XGBRegressor()
# grid_lr = GridSearchCV(estimator=model, param_grid=parameters, refit=True,n_jobs=-1)
# tqdm(grid_lr.fit(X_train, y_train))
# print(grid_lr.best_params_)
# reg=grid_lr.best_estimator_

#Parametros encontrados
reg = xgb.XGBRegressor(n_estimators=200, learning_rate=0.02, max_depth=4, min_child_weight=5, gamma=0.4,subsample=0.5, colsample_bytree=0.6)

#Predicciones set de sellers de entrenamiento 
reg.fit(X_train_inv.T,y_train_inv.T)
df_prueba_train=pd.DataFrame()
df_prueba_train['pred_train']=(np.exp(reg.predict(X_train_inv.T))-1).flatten() #se saca exponencial por el escalado con log antes
indec=[]
#Dado que este modelo predice TODAS las características, hacemos esto para quedarnos con la variable de interes-> Ventas semana 5,6,7,8,9 y 10
for i in range(643):
  for j in range(1,42):
    indec.append(j+4)
sel=[]
for i in range(1,644):
  for j in range(1,42):
    sel.append(i)
df_prueba_train['indec']=indec
df_prueba_train['sel']=sel
df_prueba_train=df_prueba_train[df_prueba_train['indec']<=10]
df_prueba_train=df_prueba_train.sort_values(['sel','indec'], ascending=[True,True])
df_prueba_train=df_prueba_train.reset_index().drop('index', axis=1)


#Predicciones set de testeo
reg.fit(X_train_inv.T,y_train_inv.T)
df_prueba_test=pd.DataFrame()
df_prueba_test['pred_test']=(np.exp(reg.predict(X_test_inv.T))-1).flatten()
indec=[]
for i in range(160):
  for j in range(1,42):
    indec.append(j+4)
sel=[]
for i in range(1,161):
  for j in range(1,42):
    sel.append(i)
df_prueba_test['indec']=indec
df_prueba_test['sel']=sel
df_prueba_test=df_prueba_test[df_prueba_test['indec']<=10]
df_prueba_test=df_prueba_test.sort_values(['sel','indec'], ascending=[True,True])
df_prueba_test=df_prueba_test.reset_index().drop('index', axis=1)

REAL_train=pd.DataFrame()
df_testt=df_escaled[df_escaled['Semana']>4]
REAL_train['real']=np.exp(df_testt['suma'][df_testt['index'].isin(train_sellers.seller)])-1
REAL_train=REAL_train.reset_index().drop('index', axis=1)
REAL_test=pd.DataFrame()
REAL_test['real']=np.exp(df_testt['suma'][df_testt['index'].isin(test_sellers.seller)])-1
REAL_test=REAL_test.reset_index().drop('index', axis=1)

df_total_train=pd.concat([df_prueba_train,REAL_train], axis=1)
df_total_test=pd.concat([df_prueba_test,REAL_test], axis=1)

#Multiplicamos por factor de correccion Duan 
pred=df_total_train['pred_train']
y=df_total_train['real']
resid=y-pred
n=len(y)
sigma=np.std(resid)
sm_lg=np.exp((1/n)*sum(resid**2)/(2*sigma**2))-((n-1)/(2*n))
df_total_train['pred_train']= pred*sm_lg

pred=df_total_test['pred_test']
y=df_total_test['real']
resid=y-pred
n=len(y)
sigma=np.std(resid)
sm_lg=np.exp((1/n)*sum(resid**2)/(2*sigma**2))-((n-1)/(2*n))
df_total_test['pred_test']= pred*sm_lg

df_total_train=df_total_train.set_index('sel')
df_total_test=df_total_test.set_index('sel')

#Borramos esos valores 0 y reemplazamos por 0.1 para que los indices de error no se vean perjudicados por estos
df_total_train[df_total_train['real']==0]=0.1
df_total_train[df_total_train['pred_train']<=0]=0
df_total_test[df_total_test['real']==0]=0.1
df_total_test[df_total_test['pred_test']<=0]=0

ent=df_total_train
test=df_total_test
MAPE_ENT=[]
WMAPE_ENT=[]
RMSE_ENT=[]
MAPE_TEST=[]
WMAPE_TEST=[]
RMSE_TEST=[]

for i in np.unique(ent.index):
      pred=np.array(ent.loc[i,'pred_train'])
      real_ent=np.array(ent.loc[i,'real'])
      MAPE_ENT.append(np.round(mean_absolute_percentage_error(real_ent, pred)*100,3))
      WMAPE_ENT.append(WMAPE2(real_ent,pred))
      RMSE_ENT.append(np.sqrt(mean_squared_error(real_ent, pred)))

for i in np.unique(test.index):
      pred=np.array(test.loc[i,'pred_test'])
      real_test=np.array(test.loc[i,'real'])
      MAPE_TEST.append(np.round(mean_absolute_percentage_error(real_test, pred)*100,3))
      WMAPE_TEST.append(WMAPE2(np.array(real_test),(pred)))
      RMSE_TEST.append(np.sqrt(mean_squared_error(real_test, pred)))

BOXPLOTS4E=pd.DataFrame()
BOXPLOTS4E['MAPE_E']=MAPE_ENT
BOXPLOTS4E['WMAPE_E']=WMAPE_ENT
BOXPLOTS4E['RMSE_E']=RMSE_ENT
BOXPLOTS4E['MODELO']='CS+XGBOOST'
BOXPLOTS4T=pd.DataFrame()
BOXPLOTS4T['MAPE_T']=MAPE_TEST
BOXPLOTS4T['WMAPE_T']=WMAPE_TEST
BOXPLOTS4T['RMSE_T']=RMSE_TEST
BOXPLOTS4T['MODELO']='CS+XGBOOST'

print(np.round(BOXPLOTS4E.groupby('MODELO').agg(MAPE_E=('MAPE_E','mean'), WMAPE_E=('WMAPE_E','mean'), RMSE_E=('RMSE_E','mean')),2))
print(np.round(BOXPLOTS4T.groupby('MODELO').agg(MAPE_T=('MAPE_T','mean'), WMAPE_E=('WMAPE_T','mean'), RMSE_T=('RMSE_T','mean')),2))
"""# RANFOM FOREST"""

from sklearn.ensemble import RandomForestRegressor
params = {
    'n_estimators': [100, 300, 500, 600],
    'max_depth': [3, 5, 7 ],
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 2, 4],
    'max_features': ['auto', 'sqrt'],
    'bootstrap': [True, False]
}

model = RandomForestRegressor()

# grid_lr =RandomizedSearchCV(estimator=model, param_distributions=params, n_iter=10,cv=3,refit=True,n_jobs=-1)
# grid_lr.fit(X_train, y_train)
# grid_lr.best_estimator_.predict(X_test)
# reg=grid_lr.best_estimator_

# grid_lr = GridSearchCV(estimator=model, param_grid=params, refit=True,n_jobs=-1)
# tqdm(grid_lr.fit(X_train_esc, y_train))
# print(grid_lr.best_estimator_)

"""## modelo"""
# mejores hiperparametros encontrados
reg=RandomForestRegressor(n_estimators= 160, min_samples_split= 5,min_samples_leaf=3,max_features='sqrt', max_depth=7, bootstrap=False)

reg.fit(X_train_inv.T,y_train_inv.T)
df_prueba_train=pd.DataFrame()
df_prueba_train['pred_train']=(np.exp(reg.predict(X_train_inv.T))-1).flatten()
indec=[]
for i in range(643):
  for j in range(1,42):
    indec.append(j+4)
sel=[]
for i in range(1,644):
  for j in range(1,42):
    sel.append(i)
df_prueba_train['indec']=indec
df_prueba_train['sel']=sel
df_prueba_train=df_prueba_train[df_prueba_train['indec']<=10]
df_prueba_train=df_prueba_train.sort_values(['sel','indec'], ascending=[True,True])
df_prueba_train=df_prueba_train.reset_index().drop('index', axis=1)

#Testeo
# reg=grid_lr.best_estimator_
reg.fit(X_train_inv.T,y_train_inv.T)
df_prueba_test=pd.DataFrame()
df_prueba_test['pred_test']=(np.exp(reg.predict(X_test_inv.T))-1).flatten()
indec=[]
for i in range(160):
  for j in range(1,42):
    indec.append(j+4)
sel=[]
for i in range(1,161):
  for j in range(1,42):
    sel.append(i)
df_prueba_test['indec']=indec
df_prueba_test['sel']=sel
df_prueba_test=df_prueba_test[df_prueba_test['indec']<=10]
df_prueba_test=df_prueba_test.sort_values(['sel','indec'], ascending=[True,True])
df_prueba_test=df_prueba_test.reset_index().drop('index', axis=1)

REAL_train=pd.DataFrame()
# df_testt=df[df['Semana']>4]
REAL_train['real']=np.exp(df_testt['suma'][df_testt['index'].isin(train_sellers.seller)])-1
REAL_train=REAL_train.reset_index().drop('index', axis=1)

REAL_test=pd.DataFrame()
REAL_test['real']=np.exp(df_testt['suma'][df_testt['index'].isin(test_sellers.seller)])-1
REAL_test=REAL_test.reset_index().drop('index', axis=1)

df_total_train=pd.concat([df_prueba_train,REAL_train], axis=1)
df_total_test=pd.concat([df_prueba_test,REAL_test], axis=1)

pred=df_total_train['pred_train']
y=df_total_train['real']
resid=y-pred
n=len(y)
sigma=np.std(resid)
sm_lg=np.exp((1/n)*sum(resid**2)/(2*sigma**2))-((n-1)/(2*n))
df_total_train['pred_train']= pred*sm_lg

pred=df_total_test['pred_test']
y=df_total_test['real']
resid=y-pred
n=len(y)
sigma=np.std(resid)
sm_lg=np.exp((1/n)*sum(resid**2)/(2*sigma**2))-((n-1)/(2*n))
df_total_test['pred_test']= pred*sm_lg

df_total_train=df_total_train.set_index('sel')
df_total_test=df_total_test.set_index('sel')

df_total_train[df_total_train['real']==0]=0.1
df_total_train[df_total_train['pred_train']<=0]=0
df_total_test[df_total_test['real']==0]=0.1
df_total_test[df_total_test['pred_test']<=0]=0

ent=df_total_train
test=df_total_test
MAPE_ENT=[]
WMAPE_ENT=[]
RMSE_ENT=[]
MAPE_TEST=[]
WMAPE_TEST=[]
RMSE_TEST=[]

for i in np.unique(ent.index):
      pred=np.array(ent.loc[i,'pred_train'])
      real_ent=np.array(ent.loc[i,'real'])
      MAPE_ENT.append(np.round(mean_absolute_percentage_error(real_ent, pred)*100,3))
      WMAPE_ENT.append(WMAPE2(real_ent,pred))
      RMSE_ENT.append(np.sqrt(mean_squared_error(real_ent, pred)))

for i in np.unique(test.index):
      pred=np.array(test.loc[i,'pred_test'])
      real_test=np.array(test.loc[i,'real'])
      MAPE_TEST.append(np.round(mean_absolute_percentage_error(real_test, pred)*100,3))
      WMAPE_TEST.append(WMAPE2(np.array(real_test),(pred)))
      RMSE_TEST.append(np.sqrt(mean_squared_error(real_test, pred)))

BOXPLOTS5E=pd.DataFrame()
BOXPLOTS5E['MAPE_E']=MAPE_ENT
BOXPLOTS5E['WMAPE_E']=WMAPE_ENT
BOXPLOTS5E['RMSE_E']=RMSE_ENT
BOXPLOTS5E['MODELO']='CS+RF'
BOXPLOTS5T=pd.DataFrame()
BOXPLOTS5T['MAPE_T']=MAPE_TEST
BOXPLOTS5T['WMAPE_T']=WMAPE_TEST
BOXPLOTS5T['RMSE_T']=RMSE_TEST
BOXPLOTS5T['MODELO']='CS+RF'

print(np.round(BOXPLOTS5E.groupby('MODELO').agg(MAPE_E=('MAPE_E','mean'), WMAPE_E=('WMAPE_E','mean'), RMSE_E=('RMSE_E','mean')),2))
print(np.round(BOXPLOTS5T.groupby('MODELO').agg(MAPE_T=('MAPE_T','mean'), WMAPE_T=('WMAPE_T','mean'), RMSE_T=('RMSE_T','mean')),2))
