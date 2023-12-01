library(readxl)
library(tree)

rm(list=ls())

gl_dataset = read_excel("gl_dataset.xlsx")
attach(gl_dataset)
WLT = as.factor(WLT)

print("First tree: all predictors (including H and IBB)")
tree.H = tree(WLT ~ AB + H + singles + doubles + triples + HR + SH + SF + HBP + BB + IBB + K + SB + CS + GIDP + CI)
print(tree.H)
print(summary(tree.H))
plot(tree.H)
text(tree.H)

print("", quote=F)
print("", quote=F)

print("Second tree: no H, IBB included")
tree.obj = tree(WLT ~ AB + singles + doubles + triples + HR + SH + SF + HBP + BB + IBB + K + SB + CS + GIDP + CI)
print(tree.obj)
print(summary(tree.obj))
plot(tree.obj)
text(tree.obj)

print("", quote=F)
print("", quote=F)

print("Third tree: H included, no IBB")
tree_HnoIBB = tree(WLT ~ AB + H + singles + doubles + triples + HR + SH + SF + HBP + BB + K + SB + CS + GIDP + CI)
print(tree_HnoIBB)
print(summary(tree_HnoIBB))
plot(tree_HnoIBB)
text(tree_HnoIBB)

print("", quote=F)
print("", quote=F)

print("Fourth tree: no H or IBB")
tree_noIBB = tree(WLT ~ AB + singles + doubles + triples + HR + SH + SF + HBP + BB + K + SB + CS + GIDP + CI)
print(tree_noIBB)
print(summary(tree_noIBB))
plot(tree_noIBB)
text(tree_noIBB)

print("", quote=F)
print("", quote=F)
print("", quote=F)
print("", quote=F)
print("Test data evaluation with no H or IBB:")
set.seed(1)
train = sample(1:nrow(gl_dataset), nrow(gl_dataset)/2)
gl_dataset.test = gl_dataset[-train, ]
WLT.test = WLT[-train]
tree.split = tree(WLT ~ AB + singles + doubles + triples + HR + SH + SF + HBP + BB + K + SB + CS + GIDP + CI,
                  subset=train)
tree.pred = predict(tree.split, gl_dataset.test, type="class")
print(table(tree.pred, WLT.test))
print("Classification success rates:")
print("True wins:")
print(table(tree.pred, WLT.test)[3, 3] / (table(tree.pred, WLT.test)[1, 3] + table(tree.pred, WLT.test)[3, 3]))
print("True losses:")
print(table(tree.pred, WLT.test)[1, 1] / (table(tree.pred, WLT.test)[1, 1] + table(tree.pred, WLT.test)[3, 1]))
print("Rate that observations were classed as W (should be .5):")
print(sum(tree.pred == "W")/length(tree.pred))

print("", quote=F)
print("", quote=F)
print("Test data evaluation with H but no IBB:")
tree.split.H = tree(WLT ~ AB + H + singles + doubles + triples + HR + SH + SF + HBP + BB + K + SB + CS + GIDP + CI,
                  subset=train)
tree.pred.H = predict(tree.split.H, gl_dataset.test, type="class")
print(table(tree.pred.H, WLT.test))
print("Classification success rates:")
print("True wins:")
print(table(tree.pred.H, WLT.test)[3, 3] / (table(tree.pred.H, WLT.test)[1, 3] + table(tree.pred.H, WLT.test)[3, 3]))
print("True losses:")
print(table(tree.pred.H, WLT.test)[1, 1] / (table(tree.pred.H, WLT.test)[1, 1] + table(tree.pred.H, WLT.test)[3, 1]))
print("Rate that observations were classed as W (should be .5):")
print(sum(tree.pred.H == "W")/length(tree.pred.H))

detach(gl_dataset)