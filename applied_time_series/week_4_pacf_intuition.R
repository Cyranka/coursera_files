rm(list = ls())


# Lake Huron lecture question ---------------------------------------------
par( mfrow=c(3,1) )
plot(LakeHuron)
acf(LakeHuron)
acf(LakeHuron, type="partial")

# Week 4 - Quiz PACF Intuition (1) ----------------------------------------
attach(attitude);
rcl = cbind(rating, complaints, learning);
round(cor(rcl),2)


# Week 4 - Quiz PACF Intuition (2) ----------------------------------------
rm(list = ls())
attach(attitude);
rating.hat = predict(lm( rating ~ learning))
complaints.hat = predict( lm( complaints~learning))

round(cor((rating - rating.hat),(complaints - complaints.hat)),2)

