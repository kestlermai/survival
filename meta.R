#install.packages("meta")
library(meta)

setwd("C:/Users/11146/Desktop/NAFLD2.0/metaforest")

meta <- read.delim("obesity.txt",check.names = FALSE)
#两分类变量metabin；连续资料metacont
metaresult <- metabin(event.e, n.e,event.c,n.c,data=meta,
                    sm="OR",
                    #method="MH",#Mantel-Haenszel、方差倒数Inverse、Peto
                    comb.random=FALSE)#不用随机效应模型进行效应合并，而是固定效应模型

metaresult <- metacont(n.e,mean.e,sd.e,n.c,mean.c,sd.c,
                       data = meta,
                       sm = "SMD",
                       byvar = subDiversity,
                       print.byvar=F,
                       studlab=paste(Study),
                       comb.fixed=F,comb.random=T)
summary(metaresult)

#影响性分析，即每个研究对总估计效应的影响大小
metainf(metaresult,pooled="random")#fixed

#森林图
settings.meta('revman5')#则可绘制出RevMan 5风格的图表
pdf("obesity.pdf", width=10,height=15)
p <- forest(metaresult,
       col.square = "black",#col.square表示森林图中方框的颜色；
       col.diamond = "black",#col.diamond表示森林图中菱形的颜色;
       col.diamond.lines = "black",#col.diamond.lines表示菱形外框的颜色；
       #leftcols = "Study",
       hetstat = TRUE)#hetatat=TRUE表示汇报异质性。
dev.off()

#发表偏倚的检验由于纳入的研究个数小于10个，在cochrane手册中不建议做test，只要求做一个漏斗图。
pdf("funnel-NAFLD-control.pdf", width=5,height=5)
p1 <- funnel(metaresult)
dev.off()
#当研究个数大于等于10个的时候，对于这种四格表资料不建议使用egger检验或begg检验，可以使用Peters检验
metabias(metaresult,method.bias="Egger")#有subgroup的话会报错

#使用trim and filled或者copas模型等进行校正
tf1 <- trimfill(metaresult, comb.fixed=TRUE)#没意义就用固定效应模型
summary(tf1)
funnel(tf1)

#敏感性分析
metainf(metaresult, pooled="fixed")#pooled="random"改用随机效应模型
forest(metainf(metaresult), comb.fixed=TRUE)

#meta回归
#回归需要取对数进行
reg_age<-metareg(metaresult,~age)#~age为协变量，多个协变量可以用+连在一起，~age+sex+country+.....
bubble(reg_age)


