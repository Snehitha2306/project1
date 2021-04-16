diet = read.csv("D:/KCBVS MDS 2019/ct lab 3/stcp-Rdataset-Diet.csv",row.names=1)
diet$weight.loss = diet$initial.weight - diet$final.weight 
boxplot(weight.loss~diet.type,data=diet,col="light gray",ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="blue")

diet.fisher  = aov(weight.loss~diet.type,data=diet)
diet.welch   = oneway.test(weight.loss~diet.type,data=diet)
diet.kruskal = kruskal.test(weight.loss~diet.type,data=diet)

summary(diet.fisher)
print(diet.welch)
print(diet.kruskal)

model=lm(weight.loss~diet.type*gender,data=diet)
print(anova(model))
plot(model)

mean_group   = tapply(diet$weight.loss,diet$diet.type,mean)
median_group = tapply(diet$weight.loss,diet$diet.type,median)
print(mean_group)
print(median_group)

manwill<-wilcox.test(diet$initial.weight,diet$final.weight,data=diet)
print(manwill)

#s.corr<-cor.test(x=diet$weight.loss,y=diet$diet.type,method='spearman')
#print(s.corr)


a=SignTest(x=diet$initial.weight,mu=4)
wilcox.test(x=diet$initial.weight,mu=4,conf.int = TRUE)

b=SignTest(x=diet$initial.weight,mu=4,alternative="less")
wilcox.test(x=diet$initial.weight,mu=4,conf.int = TRUE,alternative = "less")

c=SignTest(x=diet$initial.weight,mu=4,alternative="greater")
wilcox.test(x=diet$initial.weight,mu=4,conf.int = TRUE,alternative = "greater")

print(a)
print(b)
print(c)

plot(TukeyHSD(diet.fisher))
sd=sd(diet$weight.loss, na.rm=TRUE) /  sqrt(length(diet$weight.loss[!is.na(diet$weight.loss)]))
t=t.test(weight.loss~diet.type,data=diet[diet$diet.type!="C",],var.equal = TRUE)
print(sd)
print(t)