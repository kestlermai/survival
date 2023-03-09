setwd("C:/Users/11146/Desktop")

library(tidyverse)
library(broom)
library(rms)
library(scales)

#线性回归
df <- read.table("Awareness.txt",row.names = 1, header = T, sep = "\t")
# 将变量转为因子
df <- data.frame(lapply(df,factor))
vars <- colnames(df)[1:11]

#如果是分类变量，需要进行因子转换,就不需要run上面factor了
df$sample.source <- factor(df$sample.source, levels = c(1,2,3), labels = c("戒毒所","社区","美沙酮门诊"))
df$sex <- factor(df$sex, levels = c(1,2), labels = c("男性","女性"))
df$age <- factor(df$age, levels = c(1,2,3,4), labels = c("11~","21~","31~",">41"))
df$marriage <- factor(df$marriage, levels = c(1,2), labels = c("未婚或丧偶","同居或已婚"))
df$ethnic <- factor(df$ethnic, levels = c(1,2,3), labels = c("汉族","壮族","其他"))
df$Education <- factor(df$Education, levels = c(1,2,3,4), labels = c("未接受教育","小学","初中","高中及以上"))
df$G01 <- factor(df$G01, levels = c(0,1), labels = c("否","是"))
df$G02 <- factor(df$G02, levels = c(0,1), labels = c("否","是"))
df$G03Peer.education <- factor(df$G03Peer.education, levels = c(0,1), labels = c("否","是"))
df$HIV <- factor(df$HIV, levels = c(0,1), labels = c("否","是"))
df$syphilis <- factor(df$syphilis, levels = c(0,1), labels = c("否","是"))
df$inject.group <- factor(df$inject.group, levels = c(0,1), labels = c("否","是"))
df$condom.group <- factor(df$condom.group, levels = c(0,1), labels = c("否","是"))
df$lv <- factor(df$lv, levels = c(0,1), labels = c("否","是"))


#结果输入成vector
result_list <- list()

# for循环进行单因素回归
for (i in vars) {
  model <- glm(paste0("condom.group ~", i), data = df, family = binomial())
  result <- tidy(model)
  OR <- round(exp(coef(model)),3)
  OR_ci <- round(confint(model) %>% exp(),3)
  P <- round(summary(model)$coefficients[2, 4],3)
  result_list[[i]] <- tibble(vars = i, term = result$term, OR = OR, 
                             OR_lower = OR_ci[1, 1], OR_upper = OR_ci[2, 1],
                             P_value = P)
}
# 合并结果
result_df <- bind_rows(result_list)

write.csv(result_df,"result_df.csv", quote=F, row.names = T)

# for循环进行单因素回归
for (i in vars) {
  model <- glm(paste0("condom.group ~", i), data = df, family = binomial())
  result <- tidy(model)
  OR <- round(exp(coef(model)),3)
  OR_ci <- round(exp(confint(model)),3)
  p <- round(summary(model)$coefficients[2, 4],3)
  result_list[[i]] <- tibble(var = i, result = list(result), OR = list(OR), OR_ci = list(OR_ci),p_value = p)
}

# 合并结果
model_result <- bind_rows(result_list) %>% 
  unnest(result, OR, OR_ci) %>% 
  select(var, term, estimate, std.error, statistic, p_value, OR, OR_ci)

write.csv(model_result,"model_result.csv", quote=F, row.names = T)


#map循环进行单因素回归
model = tibble(vars) %>% 
  mutate(model = map(df[vars], 
                     ~ glm(lv ~ .x, data = df, family = binomial()))) %>% 
  #用tidy函数提取模型结果转换成数据框
  mutate(result = map(model, tidy),
         OR = map(model, ~ exp(coef(.x))),
         OR_ci = map(model, ~ exp(confint(.x)))) %>% 
  #去掉中间结果，展开每个回归结果为单独变量
  select(-model) %>% 
  unnest(c(result, OR, OR_ci))

#整理表格
result_sin = model %>% 
  mutate(OR_ci %>% as_tibble()) %>% 
  select(-OR_ci) %>% 
  rename(ci5 = V1, ci95 = V2) %>% 
  mutate(across(term, ~ str_remove(.x, '.x'))) %>% 
  filter(if_all(term, ~ !.x=='(Intercept)')) %>% 
  mutate(`OR(95%CI)` = str_c(round(OR,2), ' (', round(ci5,2), '-', round(ci95,2), ')')) %>% 
  select(vars, term, `OR(95%CI)`, p.value, OR, ci5, ci95, ) %>% 
  mutate(p.value = pvalue(p.value))

write.csv(result_sin,"result_sin.csv", fileEncoding="GB18030",quote=F, row.names = T)

#提取单因素p<0.05进入多因素
mul_glm_model <- as.formula(paste0("lv ~",
                            paste0(result_sin$vars[result_sin$p.value<0.05],
                            collapse = "+")))

mul_glm <- glm(mul_glm_model,family = binomial(link = "logit"),
               data = df)

mul_glm_result <- summary(mul_glm)
mul_glm_result[["coefficients"]]

#计算OR值和OR值的95%CI
mul_estimate <- round(mul_glm_result$coefficients[,1],3)
mul_OR <- round(exp(mul_glm_result$coefficients[,1]),3)
mul_SE <- mul_glm_result$coefficients[,2]
mul_CI5 <- round(exp(mul_glm_result$coefficients[,1]- 1.96*mul_SE),3)
mul_CI95 <- round(exp(mul_glm_result$coefficients[,1]+ 1.96*mul_SE),3)
mul_CI <- paste0(mul_CI5,"-",mul_CI95)
mul_P <- round(mul_glm_result$coefficients[,4],4)

#整理
result_mul <- data.frame("estimate"=mul_estimate,"mul_OR"= mul_OR,"mul_CI"= mul_CI,"mul_P"= mul_P)[-1,]
result_mul$mul_P[result_mul$mul_P==0] <- "<0.001"

write.csv(result_mul,"result_mul.csv", fileEncoding="GB18030",quote=F, row.names = T)

