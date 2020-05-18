#csv파일 불러오기
df = read.csv(file="C:/Users/kimna/Desktop/land/land_anal_test.csv")

#객체의 이름을 반환
names(df)
View(df)

#df벡터에서 -c(7,8)을 (전국, 서울소비심리지수) 제외한 나머지만
df = df[,-c(7,8)]
df_tr = df
names(df_tr)
View(df_tr)


#lag_k라는 함수 만듦
#lag_k()기능 : k만큼 lag을 줌 
lag_k = function (x,k) c(matrix(NA,k), x[1:(length(x)-k)])



#### rsqure를 최대화 시키는 법####
#data=0, row=30개, col=1개인 행렬
rsqmat = matrix(0,30,1)
rsqmat

#1부터 30까지 반복
for (i in 1:30){
  
  #경제지표 데이터 독립변수 선언★
  tr_x = df_tr[,3:29]
  
  #부동산가격 데이터 종속변수 선언★
  tr_y  = df_tr[,30:46]
  
  #j부터 tr_x의 열 수 까지 반복 
  for (j in 1:ncol(tr_x)){
    #tr_x에 1~30 Lag을 줌
    tr_x[,j] = lag_k(tr_x[,j], k = i)
  } 
  
  #cbind()함수 : 벡터를 묶어 행렬로 표현
  #경제지표데이터에 Time Lag을 설정하고 df에 저장
  df = cbind(tr_x, tr_y)
  
  #na.omit()함수 : NA가 들어있는 열 모두 삭제
  #즉 df2는 test를 위한 matrix..
  df2= na.omit(df)
  
  #리니어 리그레션,, 경제지표와 부동산 가격
  fit = lm(df2$전국~ ., data = df2[1:28])
  summ = summary(fit)
  
  #summary에서 나온 r square값을 rsqmat라는 행렬 변수에 1부터 30까지 저장
  rsqmat[i,] = summ$r.squared
  
  #rsquare가 최대일 때 출력
  print(max(rsqmat))
}

colors()
#시각화
#plot()함수 : 산점도 rsqmat를, type="l"선으로 표현, lwd는 선의 굵기, col은 색깔
plot(rsqmat, type = "l", lwd = 2, col="plum2")






## 6개월 lag 모델

#독립변수 선행지표들
tr_x = df_tr[,3:46]
#종속변수 부동산가격
tr_y  = df_tr[,30:46]

for (j in 1:ncol(tr_x)){
  tr_x[,j] = lag_k(tr_x[,j], k = 24)
} 
df = cbind(tr_x, tr_y)

nrow(df)

tr = df[1:100,]
ts = df[101:nrow(df),]

df2= na.omit(tr)
nrow(df2)
names(df2)
fit = lm(df2$전국~ ., data = df2[1:44])
summ = summary(fit)
print(summ$r.squared)

pred = predict(fit,ts)
plot(ts$전국, type = "l", ylim = c(50,120))
points(pred, type = "l", col ="blue")

lag_dat = function(tr,k){
  for (j in 1:ncol(tr)){
    tr[,j] = lag_k(tr[,j], k )
  } 
  return(tr)
}
#install.packages("glmnet")
library(glmnet)
library(rpart)
library(randomForest)
ret = lag_dat(df_tr[,3:46],36)
#fix(ret)
nrow(newdat)
newdat = cbind(ret,tr_y)
newdat =na.omit(newdat)
nrow(newdat)
newdat_tr = newdat[1:80,]
names(newdat_tr)
library(pls)
help(lm)
model = lm(newdat_tr$서울특별시~ ., data = newdat_tr[,1:44])
fit = stepAIC(model, direction = "both", trace = FALSE)
#fit = plsr(newdat_tr$서울특별시~ ., data = newdat_tr[,1:44])
### lasso
names(newdat_tr)
cv.lasso <- cv.glmnet(x=as.matrix(newdat_tr[,1:44]), y=as.matrix(newdat_tr$서울특별시), alpha=1)
pred = predict(cv.lasso,as.matrix(newdat_tr[,1:44]) )
####

pred = predict(fit,newdat_tr[,1:44])
names(newdat_tr)
plot(newdat_tr$서울특별시, type = "l", ylim = c(50,120), lwd = 3)
points(pred, type = "l", col ="blue",lwd =2)

ph2 = newdat[c(nrow(newdat_tr)+1):c(nrow(newdat)),1:44]
#fix(ph2)
nrow(ph2)
ncol(ph2)
pred2 = predict(fit,ph2)
pred2 = predict(cv.lasso,as.matrix(ph2[,1:44]))

plot(ph2$서울특별시, type = "o",lwd = 2, col = "red", xlim = c(0,100),ylim = c(80,110))
points(pred2, type = "o",lwd = 2)
#plot(df_tr$서울특별시, type = "o")
########## phase 2 test
pred3 = predict(fit,df_tr[171:194,2:46])

pred3 = predict(cv.lasso,as.matrix(df_tr[171:194,3:46])) ## lasso fit

plot(df_tr$서울특별시[121:nrow(df_tr)], type = "o", col = "blue", lwd = 2)
#points(pred3, type = "o", ylim = c(80,120), col = "red", lwd = 2)
plot(pred3, type = "o", ylim = c(100,110), col = "red", lwd = 2)






### diff()함수

#임의로 벡터 하나 만듦..
d = c(1,2,3,4,5,6,7,8,9,10)

#diff()함수 : 현시차와 직전 시차의 차이 값
dd = diff(d)
#dd 출력값은 111111111(1이 9개)
dd


#앞서 만든 lag_k()함수로 k=2로 lag을 준다
lag_k(d,k=2)
#d의 출력값 NA NA 1 2 3 4 5 6 7 8
#lenght는 그대로고 lag을 2준 형태
d