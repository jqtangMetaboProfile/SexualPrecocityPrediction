# install.packages('R6')
# install.packages('caret')
# install.packages('ROCR')
# install.packages('C50')
# install.packages('e1071')

# install.packages("drat")
# drat::addRepo("dmlc")
# cran <- getOption("repos")
# cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/GPU"
# options(repos = cran)
# install.packages("mxnet")

exp_path <- 'C:/Users/ML/Documents/TJQProjects/SexualPrecocityPrediction'
data_path <- paste(exp_path, 'local_data', sep = '/')
setwd(exp_path)
source('classes/SexualPrecocityClassification.R')


#---------------------------- Cross Validation --------------------------------------
sexual_precocity <- SexualPrecocityClassification$new(exp_path, data_path)
sexual_precocity$setUseless('NO','Bsae.LH','Base.FSH','Peak.LH',
                            'Peak.FSH', 'Peak.LH.FSH', 'X', 'X.1')
sexual_precocity$preprocess(filename = 'summary_table.csv', na.rm = T)
sexual_precocity$setLabel(label = 'Group')
sexual_precocity$normalize(method = 'z_score')

sexual_precocity$crossValidate(model_type = 'logistic', k = 10)
sexual_precocity$evaluate(indicators = list('global_auc', 'average_auc'))
cut_off <- quantile(sexual_precocity$predicted_value, 1-mean(sexual_precocity$real_value)) #0.616

sexual_precocity$crossValidate(model_type = 'c50', k = 10, trials = 20)
sexual_precocity$evaluate(indicators = list('global_auc', 'average_auc'))

sexual_precocity$crossValidate(model_type = 'svm', k = 10, cost = 1)
sexual_precocity$evaluate(indicator = list('global_auc', 'average_auc'))

sexual_precocity$crossValidate(model_type = 'mlp', k = 10)
sexual_precocity$evaluate(indicator = list('global_auc', 'average_auc'))


#---------------------------  Practical Application ----------------------------------
exp_path <- 'C:/Users/ML/Documents/TJQProjects/sexual_precocity'
data_path <- paste(exp_path, 'local_data', sep = '/')
setwd(exp_path)
source('classes/SexualPrecocityClassification.R')
sexual_precocity <- SexualPrecocityClassification$new(exp_path, data_path)
sexual_precocity$setUseless('NO','Bsae.LH','Base.FSH','Peak.LH',
                            'Peak.FSH', 'Peak.LH.FSH', 'X', 'X.1')
sexual_precocity$preprocess(filename = 'summary_table.csv', na.rm = F)
sexual_precocity$setLabel(label = 'Group')
sexual_precocity$setPartition(data = sexual_precocity$preprocessed_data, .7)

train.x <- sexual_precocity$train.x
train.y <- sexual_precocity$train.y
test.x <- sexual_precocity$test.x
test.y <- sexual_precocity$test.y


train.x.normalized <- sexual_precocity$normalize('z_score',train.x)
test.x.normalized <-sexual_precocity$normalize('z_score', test.x)
model <- sexual_precocity$train(model_type = 'logistic', train.x.normalized, train.y)
summary(model)

write.csv(t(apply(train.x[,-1], 2, function(x) {
  c(
    sum(!is.na(x)),
    mean(x, na.rm = T),
    sd(x, na.rm = T),
    median(x, na.rm = T),
    sqrt(var(x,na.rm=TRUE)/sum(!is.na(x))),
    wilcox.test(na.omit(x)[ !train.y], na.omit(x)[train.y])$p.value
  )
})), 'result/train.x.csv')
write.csv(t(apply(test.x[,-1], 2, function(x) {
  c(
    sum(!is.na(x)),
    mean(x, na.rm = T),
    sd(x, na.rm = T),
    median(x, na.rm = T),
    sqrt(var(x,na.rm=TRUE)/sum(!is.na(x))),
    wilcox.test(na.omit(x)[ !test.y], na.omit(x)[test.y])$p.value
  )
})), 'result/test.x.csv')

train.score <- sexual_precocity$predict(model, train.x.normalized, output_type = 'score')
test.score <- sexual_precocity$predict(model, test.x.normalized, output_type = 'score')
sexual_precocity$evaluate(train.y, train.score, indicators = list('global_auc'))
sexual_precocity$evaluate(test.y, test.score, indicators = list('global_auc'))
boxplot(train.score~train.y,col=c("darkgrey","orange"),outline=F,main="Y score",names=c("No","Yes"))
boxplot(test.score~test.y,col=c("darkgrey","orange"),outline=F,main="Y score",names=c("No","Yes"))

sum(!is.na(test.score))

table(test.score>0.791, test.y)

roc <- pROC::roc(test.y, test.score, ci = T)
mean(roc$sensitivities)
mean(roc$specificities)
mean((roc$sensitivities+roc$specificities)-1)
pROC::ci.thresholds(roc, threshold = 0.791)
