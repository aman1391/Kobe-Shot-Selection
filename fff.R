library(data.table)
library(xgboost)
data1<- fread("data.csv")
names(data1)
data1[,lon:=with(data1 , (data1$lon-mean(data1$lon))/sd(data1$lon))]
data1[,lat:=with(data1 , (data1$lat-mean(data1$lat))/sd(data1$lat))]
data1[,loc_x:=with(data1 , (data1$loc_x-mean(data1$loc_x))/sd(data1$loc_x))]
data1[,loc_y:=with(data1 , (data1$loc_y-mean(data1$loc_y))/sd(data1$loc_y))]
data1[,time:=with(data1 , (data1$minutes_remaining/60)+data1$seconds_remaining)]
data1[,time:=with(data1 , ((data1$time-min(data1$time)))/(max(data1$time)-min(data1$time)))]
data1[,shot_distance:=with(data1 , ((data1$shot_distance-min(data1$shot_distance)))/(max(data1$shot_distance)-min(data1$shot_distance)))]
data1[,minutes_remaining:=NULL]
data1[,seconds_remaining:=NULL]
data1[,game_event_id:=with(data1 , ((data1$game_event_id-min(data1$game_event_id)))/(max(data1$game_event_id)-min(data1$game_event_id)))]
data1[,game_id:=with(data1 , ((data1$game_id-min(data1$game_id)))/(max(data1$game_id)-min(data1$game_id)))]
data1$game_id<- NULL
data1$game_event_id<- NULL
data1[,game_date:=as.Date(game_date)]
library(lubridate)
data1[,month:=lubridate::month(game_date)]
data1[,year:=lubridate::year(game_date)]
data1[,wday:=lubridate::wday(game_date)]
data1[,game_date:=NULL]
data1[,period:=with(data1 , ((data1$period-min(data1$period)))/(max(data1$period)-min(data1$period)))]
data1[,team_id:=NULL]
data1[,team_name:=NULL]
data1$season<- as.factor(data1$season)
data1$season<- as.numeric(data1$season)
table(data1$season)
data1[,season:=with(data1 , ((data1$season-min(data1$season)))/(max(data1$season)-min(data1$season)))]
data1[,season:=NULL]
names(data1)
data1[matchup %like% "@", matchup := 'Away']
data1[matchup %like% "vs.", matchup := 'Home']
data1$playoffs<- as.character(data1$playoffs)
data1[,wday:=with(data1 , ((data1$wday-min(data1$wday)))/(max(data1$wday)-min(data1$wday)))]
data1[,year:=with(data1 , ((data1$year-min(data1$year)))/(max(data1$year)-min(data1$year)))]
data1[,month:=with(data1 , ((data1$month-min(data1$month)))/(max(data1$month)-min(data1$month)))]
names(data1)
data1[,shot_id:=NULL]
library(dummies)
df<- dummy.data.frame(data1 , names = c('action_type' , 'combined_shot_type' , 'shot_type' , 'shot_zone_area' ,'playoffs' , 'shot_zone_basic' , 'shot_zone_range' , 'matchup' , 'opponent') , sep='_')
X_train <- df[!is.na(df$shot_made_flag), ]
X_test <- df[is.na(df$shot_made_flag), ]
y<- X_train$shot_made_flag
X_train$shot_made_flag<- NULL
X_test$shot_made_flag<- NULL
model_xgb_cv <- xgb.cv(data=as.matrix(X_train), label=as.matrix(y), objective="binary:logistic", nfold=5, nrounds=180, eta=0.02, max_depth=8, subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="logloss")
library(xgboost)





model_xgb <- xgboost(data=as.matrix(X_train), label=as.matrix(y), objective="binary:logistic", nrounds=180, eta=0.02, max_depth=8, subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="logloss")


pred <- predict(model_xgb, as.matrix(X_test))
sample1<- fread("sample_submission.csv")
# submission
submit<- data.frame("shot_id"=sample1$shot_id , "shot_made_flag"=pred)
write.csv(submit , 'su8.csv' , row.names = F)
X_train$pred_lr<- train$pred_lr
X_test$pred_lr<- test$pred_lr
X_test$pred_lr<- NULL
X_train$pred_lr<- NULL
train<- f$train
test<- f$test
GLM(X_train,y,X_test,cv=5,seed=123,metric="logloss")->fit
  