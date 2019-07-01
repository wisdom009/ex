#6-1
#학교 50%가 대중교통 이용

n=100
x=50
p=0.5
x = rnorm(100,50,3)
m1=mean(x)
s1=sd(x)

t.test(x) 

#6-2
100
4/5
dd= rnorm(100,4/5,11)
mean(dd)
sd(dd)
t.test(dd)
t.test(dd,conf.level = 0.90)

# 6-3
ee=rnorm(1000, 430, 20)

t.test(ee)
prop.test(430, 1000, alternative="greater", correct=FALSE)
t.test(ee,conf.level = 0.90)

# 7-1
mu=1000
n=10
a = c(980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017)
mean(a)
sd(a)
t.test(a , mu = 1000)
p.value
h1

# 7-2
mu= 55
z= c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
ex=mean(z)
s=sd(z)
t.test(z, mu = 55)
t.t = (ex - h0) / (s / sqrt(n))
a = 0.05 
c.t = pt(1-a, df=n-1)
p.value = 1-pt(t.t, df=n-1)
t.test(z, mu = 55 , alternative = "greater")

#7-3
mu=8.1
ac =c(15.50, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0, 4.97)
mean(ac)
sd(ac)
t.test(ac, mu=8.1)



