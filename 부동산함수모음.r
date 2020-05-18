## 집값  분석
#install.packages("dynlm")
library(dynlm)
library(MASS)
#df = read.csv("C:/Users/jihoo/Desktop/코스피_산업별_주가지수_20200404004101.csv")
# names(df)
# class(df)
# ncol(df)
# df_tr = t(df[,2:195])
# df_tr = as.data.frame(df_tr)
# 
# df2 = na.omit(df)
# cor(df2)
# 
# ncol(df_tr)
# nrow(df_tr)
# dim(df_tr)
# colnames(df_tr)[1:39]  = c(as.character(df[,1]))
# names(df_tr)
# 
# pc = princomp(df_tr)
# biplot(pc)
#write.csv(df_tr,"d:/Users/jihoo/Desktop/kospi_landprice.csv" )

df = read.csv("e:/새연구/land_price/land_anal.csv" )

#df = df_tr
#names(df_tr)
names(df)
df = df[,-c(7,8)]
df_tr = df
#plot(df$미국대출금리, df$서울특별시)

# #df2 = na.omit(df)
# cor(df2)
# cr = cor(df[,2:ncol(df)])
# write.csv(cr,"D:/새연구/land_price/corr_mat1.csv"  )
# fix(cr)

lag_k = function (x,k) c(matrix(NA,k), x[1:(length(x)-k)])
names(df_tr)

#### rsqure룰 최대화 시키는 법
rsqmat = matrix(0,30,1)
for (i in 1:30){
  
  tr_x = df_tr[,3:29]
  tr_y  = df_tr[,30:46]
  
  for (j in 1:ncol(tr_x)){
   tr_x[,j] = lag_k(tr_x[,j], k = i)
  } 

  df = cbind(tr_x, tr_y)
  df2= na.omit(df)
  fit = lm(df2$전국~ ., data = df2[1:28])
  summ = summary(fit)
  rsqmat[i,] = summ$r.squared
  print(max(rsqmat))
  }
plot(rsqmat, type = "l", lwd = 2)

## 6개월 lag 모델
tr_x = df_tr[,3:46]
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




### diff

d = c(1,2,3,4,5,6,7,8,9,10)
dd = diff(d)
dd
lag_k(d,k=2)
d