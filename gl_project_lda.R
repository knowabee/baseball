library(readxl)
library(MASS)

rm(list=ls())

gl_dataset = read_excel("gl_dataset.xlsx")
gl_dataset = gl_dataset[!(gl_dataset$WLT=="T"), ]
attach(gl_dataset)
WLT = as.factor(WLT)

set.seed(1)
train = sample(1:nrow(gl_dataset), nrow(gl_dataset)/2)
gl_dataset.test = gl_dataset[-train, ]
WLT.test = WLT[-train]
lda.fit = lda(WLT ~ AB + singles + doubles + triples + HR + SH + SF + HBP + BB + K + SB + CS + GIDP + CI, subset=train, prior=c(0.5, 0.5))
print(lda.fit)
plot(lda.fit)

lda.pred = predict(lda.fit, newdata=gl_dataset.test)
lda.class = lda.pred$class
lda.table = table(lda.class, WLT.test)
print(lda.table)
pred_accuracy = (lda.table[1, 1] + lda.table[2, 2])/(lda.table[1, 1] + lda.table[1, 2] + lda.table[2, 1] + lda.table[2, 2])
print(pred_accuracy)

detach(gl_dataset)