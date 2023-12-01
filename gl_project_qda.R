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
qda.fit = qda(WLT ~ AB + singles + doubles + triples + HR + SH + SF + HBP + BB + K + SB + CS + GIDP + CI, subset=train, prior=c(0.5, 0.5))
print(qda.fit)

qda.pred = predict(qda.fit, newdata=gl_dataset.test)
qda.class = qda.pred$class
qda.class = predict(qda.fit, newdata=gl_dataset.test)$class
qda.table = table(qda.class, WLT.test)
print(qda.table)
pred_accuracy = (qda.table[1, 1] + qda.table[2, 2])/(qda.table[1, 1] + qda.table[1, 2] + qda.table[2, 1] + qda.table[2, 2])
print(pred_accuracy)

detach(gl_dataset)