

a=c(5,7,6,8,6,7,8,8,6,10)
b=c(6,8,9,11,13,12,10,8,9,10)
d=c(14,25,26,18,19,22,21,16,20,30)
q = c(a,b,d)
group = rep(1:3, each = n)
group_d = data.frame(q, group)
sapply(group_d, class)
group_d$group= factor(group_d$group)
qa = aov(q ~ group, data = group_d)
qa
summary(qa)
chisq.t = sum(q)
boxplot(q ~ group, main = "", xlab = "",ylab = "")
tapply(q, group, summary)

# a=c(5,7,6,8,6,7,8,8,6,10), b=c(6,8,9,11,13,12,10,8,9,10), d=c(14,25,26,18,19,22,21,16,20,30)
#h = data.frame(a,b,d), like = stack(h), oneway.test(values ~ ind, data = like, var.equal = T) 

b1 = c(15.5,14.3,16.3,13.5,15.7,16.4,14.7)
b2 = c(14.7,16.3,15.5,15.2,16.3,13.5,15.4)
b3 = c(15.5,13.2,16.5,15.7,15.3,15.2,14.8)

p = data.frame(b1,b2,b3)
ps = stack(p)

names(ps)=c("가격","시장")
ps
boxplot(ps$가격 ~ ps$시장, main = "", xlab = "",ylab = "")

#11-1
x= rnorm(80,0.15,10)
mean(x)
sd(x)
t.test(x, mu=15, alternative = "greater")



11-2

한갑=c(23,31,13,67)
반갑=c(21,48,23,92)
x=c(63,159,119,341)
계=c(107,238,155,500)

data = data.frame(한갑,반갑,x,계)
data



data1 = apply(data, c(1,2), sum)

round(prop.table(data1, margin = 2) * 100, 1)


a.n = margin.table(data1, margin = 1)
g.n = margin.table(data1, margin = 2)

a.p = a.n /margin.table(data1)
g.p = g.n /margin.table(data1)
expected = margin.table(data1) * (a.p %*% t (g.p))
addmargins(expected)
chisq.t = sum(o.e)
qchisq(0.95,df=2)
1-pchisq(chisq.t, df=2)
chisq.test(data1)
#X-squared = 12.827, df = 9, p-value = 0.1706 

