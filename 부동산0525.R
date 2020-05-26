#csv파일 불러오기
df = read.csv(file="C:/Users/kimna/Desktop/land/test0525.csv")

#객체의 이름을 반환
names(df)
View(df)

df_tr = df


#lag_k라는 함수 만듦
#lag_k()기능 : k만큼 lag을 줌 
lag_k = function (x,k) c(matrix(NA,k), x[1:(length(x)-k)])

plot( df$원달러환율,c(NA,diff(df$전국)))
plot(diff(df$원달러환율), type = "l")




###diff(경제지표)와 집값 비교
plot(df$전국,c(NA,diff(df$소비자물가지수)))
plot(df$전국,c(NA,diff(df$X1인당.국민총소득.명목..원화..만원.)))
plot(df$전국,c(NA,diff(df$코스피지수평균)))
plot(df$전국,c(NA,diff(df$원달러환율)))
plot(df$전국,c(NA,diff(df$한국대출금리)))

#시계열정보도 출력하고싶은데 어떻게 해요 ㅠ
plot(df$코스피대비집값,c(NA,diff(df$원달러환율)))
plot(df$국민총소득대비집값,c(NA,diff(df$한국대출금리)))


###diff(집값)과 경제지표 비교
plot(df$소비자물가지수,c(NA,diff(df$전국)))

#이거 왜.. box plot으로 나올까요 ㅠ
plot(df$X1인당.국민총소득.명목..원화..만원.,c(NA,diff(df$전국)))
plot(df$코스피지수평균,c(NA,diff(df$전국)))
plot(df$원달러환율,c(NA,diff(df$전국)))
plot(df$한국대출금리,c(NA,diff(df$전국)))
plot(df$원달러환율,c(NA,diff(df$코스피대비집값)))
plot(df$한국대출금리,c(NA,diff(df$국민총소득대비집값)))

#독립변수선언
tr_x = df_tr[,3:35]
names(tr_x)
#종속변수선언
tr_y = df_tr[,36:57]

#한국대출금리에 12개월 lag
tr_x[,4] = lag_k(tr_x[,4], k = 12)
#코스피지수평균에 6개월 lag
tr_x[,30] = lag_k(tr_x[,30], k = 6)
View(tr_x)
plot(tr_x$한국대출금리,tr_x$코스피지수평균)

df = cbind(tr_x, tr_y)
df2= na.omit(df)
names(df2)
fit = lm(df2$전국~ ., data = df2[1:33])
summ = summary(fit)
print(summ)
#summary(fit)해석 하는법,,

write.csv(df,"C:/Users/kimna/Desktop/졸업작품/test0525_2.csv")




#diff변수 저장
df = read.csv(file="C:/Users/kimna/Desktop/land/test0525.csv")
diff전국 <- c(diff(df$전국))
diff코스피대비집값 <- c(diff(df$코스피대비집값))
diff국민총소득대비집값 <- c(diff(df$국민총소득대비집값))
diff국민총소득 <- c(diff(df$X1인당.국민총소득.명목..원화..만원.))
diff소비자물가지수 <- c(diff(df$소비자물가지수))
diff코스피지수평균 <- c(diff(df$코스피지수평균))
diff원달러환율 <- c(diff(df$원달러환율))
diff한국대출금리 <- c(diff(df$한국대출금리))
차이 <- cbind(diff전국,diff코스피대비집값,diff국민총소득대비집값,
            diff국민총소득, diff소비자물가지수, diff코스피지수평균,
            diff원달러환율, diff한국대출금리)
write.csv(차이,"C:/Users/kimna/Desktop/졸업작품/test0525_3.csv")
View(차이)
