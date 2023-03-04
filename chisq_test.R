setwd("C:/Users/11146/Desktop")

df <- read.table("1.txt",header = T , sep = "\t")


df1 <- table(df$年份,df$E00最近一个月内最近一次发生性行为时使用安全套)
chisq.test(df1)
df1_tab <- as.data.frame(df1)
head(df)


df <- read.table("1.txt",header = T , sep = "\t")
vars <- colnames(df)[2:16]

chisq_results <- data.frame(var = character(), chisq_value = numeric(), p_value = numeric())

for (i in vars) {
  tab <- table(df[[i]], df$E00最近一个月内最近一次发生性行为时使用安全套)
  chisq_result <- chisq.test(tab)
  p_val <- round(chisq_result$p.value, 3)
  if (p_val == 0) {
    p_val <- "<0.001"
  }
  chisq_results <- rbind(chisq_results, 
                         data.frame(var = i, chisq.value = round(chisq_result$statistic, 3), 
                                    p.value = p_val))
}

chisq_results <- as.data.frame(chisq_results)
write.csv(chisq_results, "chisq_results.csv", fileEncoding="GB18030", quote=F, row.names = T)
