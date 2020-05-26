#csv파일 불러오기
df = read.csv(file="C:/Users/kimna/Desktop/land/land_anal0524.csv")

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

#경제지표 데이터 독립변수 선언★
tr_x = df_tr[,3:41]
names(tr_x)
#부동산가격 데이터 종속변수 선언★
tr_y  = df_tr[,42:58]
names(tr_y)

#코스피지수평균에 6개월 Lag을 줌
tr_x[,31] = lag_k(tr_x[,31], k = 6)
View(tr_x)


tr_y[,1] = lag_k(tr_y[,1], k = 12)
View(tr_y)
write.csv(tr_x,"C:/Users/kimna/Desktop/졸업작품/test0524_x.csv")
write.csv(tr_y,"C:/Users/kimna/Desktop/졸업작품/test0524_y.csv")
