# -*- coding: utf-8 -*-
"""modelos_ML.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1S0dDmLZkC3Y42vsS06JZ1lcyI6pznayj
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
from sklearn.preprocessing import StandardScaler
warnings.filterwarnings("ignore")
tqdm.pandas()
#LGBM
import lightgbm as lgb
import xgboost as xgb
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import GridSearchCV

from scipy.stats import randint, uniform
def WMAPE2 (true,predicted):
    WMAPE2=np.round((np.sum((np.abs(true-predicted)))/np.sum(true))*100,3)
    return (WMAPE2)

#Preparando dataframes
df=pd.read_csv('DF_SYNTH2.csv')
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
train_sellers=pd.read_csv('train_sellers.csv')
train_sellers=train_sellers.iloc[:,1:15]
test_sellers=pd.read_csv('test_sellers.csv')
test_sellers=test_sellers.iloc[:,1:15]

X_train=pd.concat([train_sellers.iloc[:,0:4],train_sellers.iloc[:,10:13],df[df.index.isin(train_sellers.index)].drop('index', axis=1)], axis=1)
y_train=df_test[df_test.index.isin(train_sellers.index)].drop('index', axis=1)
X_test=pd.concat([test_sellers.iloc[:,0:4],test_sellers.iloc[:,10:13],df[df.index.isin(test_sellers.index)].drop('index', axis=1)], axis=1)
y_test=df_test[df_test.index.isin(test_sellers.index)].drop('index', axis=1)

#Normalizando
from sklearn.preprocessing import StandardScaler
scaler = StandardScaler()
X_train_norm = pd.DataFrame(
    scaler.fit_transform(X_train),
    columns = X_train.columns
)
X_test_norm = pd.DataFrame(
    scaler.fit_transform(X_test),
    columns = X_test.columns
)

#Escalando
X_train_esc=pd.concat([np.log(X_train[['y1','y2','y3','y4','precio_median','SKU','precio_min','precio_75','plata2_','plata3','plata4']]+1),X_train[['tramo_6.0','tramo_9.0','G02','G06','G07','G09','G13','G16','G18','G21','G22','G0801','G0802','G1604','G1711','G1901','G2104','Otra cat','464100.0','471990.0','477399.0','702000.0','731001.0','cluster']]], axis=1)
X_test_esc=pd.concat([np.log(X_test[['y1','y2','y3','y4','precio_median','SKU','precio_min','precio_75','plata2_','plata3','plata4']]+1),X_test[['tramo_6.0','tramo_9.0','G02','G06','G07','G09','G13','G16','G18','G21','G22','G0801','G0802','G1604','G1711','G1901','G2104','Otra cat','464100.0','471990.0','477399.0','702000.0','731001.0','cluster']]], axis=1)

"""# XGBOOST

## Modelos
"""



# Definir los hiperparámetros a buscar
parameters = {
    'n_estimators': [100, 300, 500, 600],
    'learning_rate': [0.001,0.01,0.05],
    'max_depth': [3,4,5,6],
    'min_child_weight': [1,3,5]
}
# Crear el modelo de XGBoost
model=xgb.XGBRegressor()
#model = xgb.XGBRegressor(n_estimators=700, learning_rate=0.01, max_depth=4, min_child_weight=1, gamma=0.1)
#grid_lr =RandomizedSearchCV(model, parameters, n_iter=10, cv=3, scoring='neg_mean_squared_error', random_state=42)

# grid_lr = GridSearchCV(estimator=model, param_grid=parameters, refit=True,n_jobs=-1)
# tqdm(grid_lr.fit(X_train_esc, y_train))
# print(grid_lr.best_params_)
#learning_rate=0.01, max_depth=3, min_child_weight=1, n_estimators=100

#model=grid_lr.best_estimator_

model = xgb.XGBRegressor(n_estimators=100, learning_rate=0.01, max_depth=3, min_child_weight=5, gamma=0.1)

#Se hara un tipo de prediccion directo, es decir 6 modelos para cada semana a predecir#

#Modelo 1
pred1=[]
y_train1=np.log(y_train['Semana 5']+1)
model.fit(X_train_esc, y_train1)
pred1.append(model.predict(X_train_esc))

#Modelo 2
pred2=[]
y_train2=np.log(y_train['Semana 6']+1)
model.fit(X_train_esc, y_train2)
pred2.append(model.predict(X_train_esc))

#Modelo 3
pred3=[]
y_train3=np.log(y_train['Semana 7']+1)
model.fit(X_train_esc, y_train3)
pred3.append(model.predict(X_train_esc))

#Modelo 4
pred4=[]
y_train4=np.log(y_train['Semana 8']+1)
model.fit(X_train_esc, y_train4)
pred4.append(model.predict(X_train_esc))

#Modelo 5
pred5=[]
y_train4=np.log(y_train['Semana 9']+1)
model.fit(X_train_esc, y_train4)
pred4.append(model.predict(X_train_esc))

#Modelo 6
pred6=[]
y_train6=np.log(y_train['Semana 10']+1)
model.fit(X_train_esc, y_train6)
pred6.append(model.predict(X_train_esc))

"""### Testeo"""

#Modelo 5
pred7=[]
y_train7=np.log(y_train['Semana 5']+1)
model.fit(X_train_esc, y_train7)
pred7.append(model.predict(X_test_esc))

#Modelo 6
pred8=[]
y_train8=np.log(y_train['Semana 6']+1)
model.fit(X_train_esc, y_train8)
pred8.append(model.predict(X_test_esc))

#Modelo 7
pred9=[]
y_train9=np.log(y_train['Semana 7']+1)
model.fit(X_train_esc, y_train9)
pred9.append(model.predict(X_test_esc))

#Modelo 8
pred10=[]
y_train10=np.log(y_train['Semana 8']+1)
model.fit(X_train_esc, y_train10)
pred10.append(model.predict(X_test_esc))

#Modelo 9
pred11=[]
y_train11=np.log(y_train['Semana 9']+1)
model.fit(X_train_esc, y_train11)
pred11.append(model.predict(X_test_esc))

#Modelo 10
pred12=[]
y_train12=np.log(y_train['Semana 10']+1)
model.fit(X_train_esc, y_train12)
pred12.append(model.predict(X_test_esc))


semana=[]
for i in range(5,11):
  for j in range(643):
    semana.append(i)
ind=[]
for i in range(6):
  for j in range(1,644):
    ind.append(j)

df_pred_train=pd.DataFrame()
df_pred_train['pred_train']=np.array(pred1+pred2+pred3+pred4+pred5+pred6).flatten()
df_pred_train['pred_train']=np.exp(df_pred_train['pred_train'])-1
df_pred_train['Semana']=semana
df_pred_train['indice']=ind
df_pred_train=df_pred_train.sort_values(['indice','Semana'], ascending=[True,True])
df_pred_train=df_pred_train.reset_index().drop('index', axis=1)
df_pred_train

semana=[]
for i in range(5,11):
  for j in range(160):
    semana.append(i)
ind=[]
for i in range(6):
  for j in range(1,161):
    ind.append(j)

df_pred_test=pd.DataFrame()
df_pred_test['pred_test']=np.array(pred7+pred8+pred9+pred10+pred11+pred12).flatten()
df_pred_test['pred_test']=np.exp(df_pred_test['pred_test'])-1
df_pred_test['Semana']=semana
df_pred_test['indice']=ind
df_pred_test=df_pred_test.sort_values(['indice','Semana'], ascending=[True,True])
df_pred_test=df_pred_test.reset_index().drop('index', axis=1)
df_pred_test

REAL_train=pd.DataFrame()
df_testt=df[df['Semana']>4]

REAL_train['real']=df_testt['suma'][df_testt['index'].isin(train_sellers.seller)]
REAL_train=REAL_train.reset_index().drop('index', axis=1)

REAL_test=pd.DataFrame()
REAL_test['real']=df_testt['suma'][df_testt['index'].isin(test_sellers.seller)]
REAL_test=REAL_test.reset_index().drop('index', axis=1)

df_total_train=pd.concat([df_pred_train,REAL_train], axis=1)
df_total_test=pd.concat([df_pred_test,REAL_test], axis=1)

#factor corrección train
pred=df_total_train['pred_train']
y=df_total_train['real']
resid=y-pred
n=len(y)
sigma=np.std(resid)
sm_lg=np.exp((1/n)*sum(resid**2)/(2*sigma**2))-((n-1)/(2*n))
df_total_train['pred_train']= pred*sm_lg

#factor corrección test
pred=df_total_test['pred_test']
y=df_total_test['real']
resid=y-pred
n=len(y)
sigma=np.std(resid)
sm_lg=np.exp((1/n)*sum(resid**2)/(2*sigma**2))-((n-1)/(2*n))
df_total_test['pred_test']= pred*sm_lg

df_total_train=df_total_train.set_index('indice')
df_total_test=df_total_test.set_index('indice')

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

BOXPLOTS1E=pd.DataFrame()
BOXPLOTS1E['MAPE_E']=MAPE_ENT
BOXPLOTS1E['WMAPE_E']=WMAPE_ENT
BOXPLOTS1E['RMSE_E']=RMSE_ENT
BOXPLOTS1E['MODELO']='XGBOOST'
BOXPLOTS1T=pd.DataFrame()
BOXPLOTS1T['MAPE_T']=MAPE_TEST
BOXPLOTS1T['WMAPE_T']=WMAPE_TEST
BOXPLOTS1T['RMSE_T']=RMSE_TEST
BOXPLOTS1T['MODELO']='XGBOOST'

"""## Resultado"""

print(np.round(BOXPLOTS1E.groupby('MODELO').agg(MAPE_E=('MAPE_E','mean'), WMAPE_E=('WMAPE_E','mean'),RMSE_E=('RMSE_E','mean')),2))
print(np.round(BOXPLOTS1T.groupby('MODELO').agg(MAPE_T=('MAPE_T','mean'), WMAPE_T=('WMAPE_T','mean'),RMSE_T=('RMSE_T','mean')),2))

"""# LIGHTGBM
#Correr nuevamente la preparacion de las matrices
## Modelos
"""

import lightgbm as lgb
from sklearn.model_selection import RandomizedSearchCV

from scipy.stats import randint, uniform

# Definir los hiperparámetros a buscar
params = {
    'max_depth': [3, 4, 5,6],
    'learning_rate': [0.1, 0.01, 0.001],
    'n_estimators': [100, 300, 500],
    'reg_alpha': [ 0.1, 0.5],
    'reg_lambda': [ 0.1, 0.5],
    'subsample': [0.8, 1.0],
    'colsample_bytree': [0.8, 1.0]
}


# Crear el modelo LightGBM
model=lgb.LGBMRegressor()
#Hiperparametros
# grid_lr =RandomizedSearchCV(model, param_distributions=params, n_iter=10, cv=3, scoring='neg_mean_squared_error', random_state=42)
# grid_lr.fit(X_train_esc, y_train)

# print(grid_lr.best_params_)

# grid_lr = GridSearchCV(estimator=model, param_grid=params, refit=True,n_jobs=-1)
# tqdm(grid_lr.fit(X_train_esc, y_train['Semana 5']))
# print(grid_lr.best_params_)

model = lgb.LGBMRegressor(colsample_bytree= 0.8, learning_rate= 0.01, max_depth= 5, n_estimators= 100, reg_alpha= 0.1, reg_lambda= 0.5, subsample= 0.8)

"""### Entrenamiento"""

#Modelo 1
pred1=[]
y_train1=np.log(y_train['Semana 5']+1)
model.fit(X_train_esc, y_train1)
pred1.append(model.predict(X_train_esc))

#Modelo 2
pred2=[]
y_train2=np.log(y_train['Semana 6']+1)
model.fit(X_train_esc, y_train2)
pred2.append(model.predict(X_train_esc))

#Modelo 3
pred3=[]
y_train3=np.log(y_train['Semana 7']+1)
model.fit(X_train_esc, y_train3)
pred3.append(model.predict(X_train_esc))

#Modelo 4
pred4=[]
y_train4=np.log(y_train['Semana 8']+1)
model.fit(X_train_esc, y_train4)
pred4.append(model.predict(X_train_esc))

#Modelo 5
pred5=[]
y_train4=np.log(y_train['Semana 9']+1)
model.fit(X_train_esc, y_train4)
pred4.append(model.predict(X_train_esc))

#Modelo 6
pred6=[]
y_train6=np.log(y_train['Semana 10']+1)
model.fit(X_train_esc, y_train6)
pred6.append(model.predict(X_train_esc))

"""### Testeo"""

#Modelo 5
pred7=[]
y_train7=np.log(y_train['Semana 5']+1)
model.fit(X_train_esc, y_train7)
pred7.append(model.predict(X_test_esc))

#Modelo 6
pred8=[]
y_train8=np.log(y_train['Semana 6']+1)
model.fit(X_train_esc, y_train8)
pred8.append(model.predict(X_test_esc))

#Modelo 7
pred9=[]
y_train9=np.log(y_train['Semana 7']+1)
model.fit(X_train_esc, y_train9)
pred9.append(model.predict(X_test_esc))

#Modelo 8
pred10=[]
y_train10=np.log(y_train['Semana 8']+1)
model.fit(X_train_esc, y_train10)
pred10.append(model.predict(X_test_esc))

#Modelo 9
pred11=[]
y_train11=np.log(y_train['Semana 9']+1)
model.fit(X_train_esc, y_train11)
pred11.append(model.predict(X_test_esc))

#Modelo 10
pred12=[]
y_train12=np.log(y_train['Semana 10']+1)
model.fit(X_train_esc, y_train12)
pred12.append(model.predict(X_test_esc))

"""### Factor corrección"""

semana=[]
for i in range(5,11):
  for j in range(643):
    semana.append(i)
ind=[]
for i in range(6):
  for j in range(1,644):
    ind.append(j)

df_pred_train=pd.DataFrame()
df_pred_train['pred_train']=np.array(pred1+pred2+pred3+pred4+pred5+pred6).flatten()
df_pred_train['pred_train']=np.exp(df_pred_train['pred_train'])-1
df_pred_train['Semana']=semana
df_pred_train['indice']=ind
df_pred_train=df_pred_train.sort_values(['indice','Semana'], ascending=[True,True])
df_pred_train=df_pred_train.reset_index().drop('index', axis=1)
df_pred_train

semana=[]
for i in range(5,11):
  for j in range(160):
    semana.append(i)
ind=[]
for i in range(6):
  for j in range(1,161):
    ind.append(j)

df_pred_test=pd.DataFrame()
df_pred_test['pred_test']=np.array(pred7+pred8+pred9+pred10+pred11+pred12).flatten()
df_pred_test['pred_test']=np.exp(df_pred_test['pred_test'])-1
df_pred_test['Semana']=semana
df_pred_test['indice']=ind
df_pred_test=df_pred_test.sort_values(['indice','Semana'], ascending=[True,True])
df_pred_test=df_pred_test.reset_index().drop('index', axis=1)
df_pred_test

REAL_train=pd.DataFrame()
df_testt=df[df['Semana']>4]

REAL_train['real']=df_testt['suma'][df_testt['index'].isin(train_sellers.seller)]
REAL_train=REAL_train.reset_index().drop('index', axis=1)

REAL_test=pd.DataFrame()
REAL_test['real']=df_testt['suma'][df_testt['index'].isin(test_sellers.seller)]
REAL_test=REAL_test.reset_index().drop('index', axis=1)

df_total_train=pd.concat([df_pred_train,REAL_train], axis=1)
df_total_test=pd.concat([df_pred_test,REAL_test], axis=1)

#factor corrección train
pred=df_total_train['pred_train']
y=df_total_train['real']
resid=y-pred
n=len(y)
sigma=np.std(resid)
sm_lg=np.exp((1/n)*sum(resid**2)/(2*sigma**2))-((n-1)/(2*n))
df_total_train['pred_train']= pred*sm_lg

#factor corrección test
pred=df_total_test['pred_test']
y=df_total_test['real']
resid=y-pred
n=len(y)
sigma=np.std(resid)
sm_lg=np.exp((1/n)*sum(resid**2)/(2*sigma**2))-((n-1)/(2*n))
df_total_test['pred_test']= pred*sm_lg

df_total_train=df_total_train.set_index('indice')
df_total_test=df_total_test.set_index('indice')

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

BOXPLOTS2E=pd.DataFrame()
BOXPLOTS2E['MAPE_E']=MAPE_ENT
BOXPLOTS2E['WMAPE_E']=WMAPE_ENT
BOXPLOTS2E['RMSE_E']=RMSE_ENT
BOXPLOTS2E['MODELO']='XGBOOST'
BOXPLOTS2T=pd.DataFrame()
BOXPLOTS2T['MAPE_T']=MAPE_TEST
BOXPLOTS2T['WMAPE_T']=WMAPE_TEST
BOXPLOTS2T['RMSE_T']=RMSE_TEST
BOXPLOTS2T['MODELO']='LIGHTGBM'

"""## Resultados"""

print(np.round(BOXPLOTS2E.groupby('MODELO').agg(MAPE_E=('MAPE_E','mean'), WMAPE_E=('WMAPE_E','mean'),RMSE_E=('RMSE_E','mean')),2))
print(np.round(BOXPLOTS2T.groupby('MODELO').agg(MAPE_T=('MAPE_T','mean'), WMAPE_T=('WMAPE_T','mean'),RMSE_T=('RMSE_T','mean')),2))

"""# RANDOMFOREST
#Correr nuevamente parte de preparacion de dataframe
## Modelos
"""

from sklearn.model_selection import RandomizedSearchCV
params = {
    'n_estimators': [100, 300, 500],
    'max_depth': [3, 5, 10, None],
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 2, 4],
    'max_features': ['auto', 'sqrt'],
    'bootstrap': [True, False]
}

# Crear el modelo de Random Forest
model = RandomForestRegressor()

# grid_lr = GridSearchCV(estimator=model, param_grid=params, refit=True,n_jobs=-1)
# tqdm(grid_lr.fit(X_train_esc, y_train))
# print(grid_lr.best_params_)

# grid_lr =RandomizedSearchCV(model, param_distributions=params, n_iter=10, cv=3, scoring='neg_mean_squared_error', random_state=42)
# grid_lr.fit(X_train_esc, y_train)

model = RandomForestRegressor(n_estimators=100, max_depth=5, max_features='sqrt', bootstrap=False, min_samples_split=10,min_samples_leaf=4)

"""### Entrenamiento"""

#Modelo 1
pred1=[]
y_train1=np.log(y_train['Semana 5']+1)
model.fit(X_train_esc, y_train1)
pred1.append(model.predict(X_train_esc))

#Modelo 2
pred2=[]
y_train2=np.log(y_train['Semana 6']+1)
model.fit(X_train_esc, y_train2)
pred2.append(model.predict(X_train_esc))

#Modelo 3
pred3=[]
y_train3=np.log(y_train['Semana 7']+1)
model.fit(X_train_esc, y_train3)
pred3.append(model.predict(X_train_esc))

#Modelo 4
pred4=[]
y_train4=np.log(y_train['Semana 8']+1)
model.fit(X_train_esc, y_train4)
pred4.append(model.predict(X_train_esc))

#Modelo 5
pred5=[]
y_train4=np.log(y_train['Semana 9']+1)
model.fit(X_train_esc, y_train4)
pred4.append(model.predict(X_train_esc))

#Modelo 6
pred6=[]
y_train6=np.log(y_train['Semana 10']+1)
model.fit(X_train_esc, y_train6)
pred6.append(model.predict(X_train_esc))

"""### Testeo"""

#Modelo 5
pred7=[]
y_train7=np.log(y_train['Semana 5']+1)
model.fit(X_train_esc, y_train7)
pred7.append(model.predict(X_test_esc))

#Modelo 6
pred8=[]
y_train8=np.log(y_train['Semana 6']+1)
model.fit(X_train_esc, y_train8)
pred8.append(model.predict(X_test_esc))

#Modelo 7
pred9=[]
y_train9=np.log(y_train['Semana 7']+1)
model.fit(X_train_esc, y_train9)
pred9.append(model.predict(X_test_esc))

#Modelo 8
pred10=[]
y_train10=np.log(y_train['Semana 8']+1)
model.fit(X_train_esc, y_train10)
pred10.append(model.predict(X_test_esc))

#Modelo 9
pred11=[]
y_train11=np.log(y_train['Semana 9']+1)
model.fit(X_train_esc, y_train11)
pred11.append(model.predict(X_test_esc))

#Modelo 10
pred12=[]
y_train12=np.log(y_train['Semana 10']+1)
model.fit(X_train_esc, y_train12)
pred12.append(model.predict(X_test_esc))

"""### Factor corrección"""

semana=[]
for i in range(5,11):
  for j in range(643):
    semana.append(i)
ind=[]
for i in range(6):
  for j in range(1,644):
    ind.append(j)

df_pred_train=pd.DataFrame()
df_pred_train['pred_train']=np.array(pred1+pred2+pred3+pred4+pred5+pred6).flatten()
df_pred_train['pred_train']=np.exp(df_pred_train['pred_train'])-1
df_pred_train['Semana']=semana
df_pred_train['indice']=ind
df_pred_train=df_pred_train.sort_values(['indice','Semana'], ascending=[True,True])
df_pred_train=df_pred_train.reset_index().drop('index', axis=1)
df_pred_train

semana=[]
for i in range(5,11):
  for j in range(160):
    semana.append(i)
ind=[]
for i in range(6):
  for j in range(1,161):
    ind.append(j)

df_pred_test=pd.DataFrame()
df_pred_test['pred_test']=np.array(pred7+pred8+pred9+pred10+pred11+pred12).flatten()
df_pred_test['pred_test']=np.exp(df_pred_test['pred_test'])-1
df_pred_test['Semana']=semana
df_pred_test['indice']=ind
df_pred_test=df_pred_test.sort_values(['indice','Semana'], ascending=[True,True])
df_pred_test=df_pred_test.reset_index().drop('index', axis=1)
df_pred_test

REAL_train=pd.DataFrame()
df_testt=df[df['Semana']>4]

REAL_train['real']=df_testt['suma'][df_testt['index'].isin(train_sellers.seller)]
REAL_train=REAL_train.reset_index().drop('index', axis=1)

REAL_test=pd.DataFrame()
REAL_test['real']=df_testt['suma'][df_testt['index'].isin(test_sellers.seller)]
REAL_test=REAL_test.reset_index().drop('index', axis=1)

df_total_train=pd.concat([df_pred_train,REAL_train], axis=1)
df_total_test=pd.concat([df_pred_test,REAL_test], axis=1)

df_total_test

#factor corrección train
pred=df_total_train['pred_train']
y=df_total_train['real']
resid=y-pred
n=len(y)
sigma=np.std(resid)
sm_lg=np.exp((1/n)*sum(resid**2)/(2*sigma**2))-((n-1)/(2*n))
df_total_train['pred_train']= pred*sm_lg

#factor corrección test
pred=df_total_test['pred_test']
y=df_total_test['real']
resid=y-pred
n=len(y)
sigma=np.std(resid)
sm_lg=np.exp((1/n)*sum(resid**2)/(2*sigma**2))-((n-1)/(2*n))
df_total_test['pred_test']= pred*sm_lg

df_total_train=df_total_train.set_index('indice')
df_total_test=df_total_test.set_index('indice')

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

BOXPLOTS3E=pd.DataFrame()
BOXPLOTS3E['MAPE_E']=MAPE_ENT
BOXPLOTS3E['WMAPE_E']=WMAPE_ENT
BOXPLOTS3E['RMSE_E']=RMSE_ENT
BOXPLOTS3E['MODELO']='RANDOM FOREST'
BOXPLOTS3T=pd.DataFrame()
BOXPLOTS3T['MAPE_T']=MAPE_TEST
BOXPLOTS3T['WMAPE_T']=WMAPE_TEST
BOXPLOTS3T['RMSE_T']=RMSE_TEST
BOXPLOTS3T['MODELO']='RANDOM FOREST'

"""## Resultados"""

print(np.round(BOXPLOTS3E.groupby('MODELO').agg(MAPE_E=('MAPE_E','mean'), WMAPE_E=('WMAPE_E','mean'),RMSE_E=('RMSE_E','mean')),2))
print(np.round(BOXPLOTS3T.groupby('MODELO').agg(MAPE_T=('MAPE_T','mean'), WMAPE_T=('WMAPE_T','mean'),RMSE_T=('RMSE_T','mean')),2)) 

"""#BOXPLOTS"""

BOXPLOTS=pd.concat([BOXPLOTS1T,BOXPLOTS2T,BOXPLOTS3T], axis=0)
#grafico para poner todos los boxplots
import plotly.express as px

fig = px.box(BOXPLOTS, color='MODELO', title='Métricas modelos')
fig.update_traces(quartilemethod="inclusive")
fig.show()

df = px.data.tips()
fig = px.violin(BOXPLOTS, y="RMSE_T",x='MODELO',color='MODELO',violinmode='overlay', title='Gráfico violin ML')
fig.show()

# BOXPLOTS1T.to_csv('xg.csv')
# BOXPLOTS2T.to_csv('lgbm.csv')
# BOXPLOTS3T.to_csv('rf.csv')
