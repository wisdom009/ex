#1
p= c(150,160,170,180,190)
s = c(176,179,182,181,185)
ps_f = data.frame(p,s)
colnames(ps_f) = c("pa", "son")
ps_f
p=mean(ps_f$pa) # 아빠의 평균키 170
s=mean(ps_f$son)# 아들의 편균 180.6
sxy = sum((ps_f$pa - p) * (ps_f$son-s))
sxx = sum((ps_f$pa - p)^2)
b1 = sxy/sxx  # 0.2
b0 = p - b1*s # 133.88

plot(pa ~ son, data=ps_f)
out=lm(pa ~ son, data=ps_f)
abline(out, col = "red", lwd =1)
anova(out) # 아버지의 키 165 일때 아들의 키를 나타내는 그래프는 180 정도이다


#2
x = c(100,200,300,400,500)
y = c(30, 70, 85, 140, 197)
xp = data.frame(x,y)
colnames(xp) = c("m","c")
m=mean(xp$m) #월급평균 300
c=mean(xp$c) #카드소비 104.4
sxy = sum((xp$m - m) * (xp$c - c))
sxx = sum((xp$m - m)^2)
b1 = sxy/sxx  # 0.404
b0 = m - b1*c # 257


plot(m ~ c, data=xp)
tt=lm(m ~ c, data=xp)
abline(tt, col = "red", lwd =1)
anova(tt) # 250이 그래프로 나타내는 값은 80만원때이다


#3
#mtcars에서 에서 배기량 disp 에가른 바력 hp 의 회기식
mc = mtcars[c("disp","hp")]
mc
dm=mean(mc$disp) #230
hm=mean(mc$hp) #146.6
sxy = sum((mc$disp - dm) * (mc$hp-hm))
sxx = sum((mc$disp-dm)^2)
b1= sxy / sxx # 0.434
b0 = dm - b1 * hm  #166.5
plot(disp ~ hp, data=mc)
car=lm(disp ~ hp, data=mc)
abline(car, col = "red", lwd =1)
anova(car)


#4
library(MASS)
library(leaps)
bs = as.data.frame(Boston[,c("medv","tax","rad","age")])
fit=lm(medv ~ tax+rad+age, data=bs)
summary(fit)
par(mfrow=c(2,2))
plot(bs)
par(mfrow=c(1,1))
fit2 = lm(medv ~ tax+rad ,data=bs) # 2차식
fit3 = lm(medv~ tax+rad+age,data=bs) # 3차식
step(fit,direction = "backward",
     scope = ~ tax+rad+age, data=bs)
step(fit,direction = "backward", scope = list(upper=fit,lower=fit3),
                     method = 'seqrep',nbest = 4)
subsets = regsubsets(medv ~ .,data=bs,
                     method = 'exhaustive',nbest = 4)
plot(subsets)
