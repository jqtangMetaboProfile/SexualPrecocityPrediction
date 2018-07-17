Classifier <- R6::R6Class(
  public = list(
  
    initialize = function(model_type) {
      private$model_type <- model_type
    },
    setModelType = function(model_type) {
      private$model_type <- model_type
    },
    trainModel= function(train, label, ...) {
      switch(private$model_type,
             logistic = private$logistic(train, label, ...),
             c50 = private$c50(train, label, ...),
             svm = private$svm(train, label, ...),
             mlp = private$mlp(train, label, ...)
      )
    },
    
    predict = function(model, test) {
      
      model_type <- attr(model, 'model_type')
      if(model_type == 'logistic') {
        pred <- predict.glm(model, as.data.frame(test), type = 'response')
      } else if(model_type == 'c50') {
        pred <- predict.C5.0(model, test, type = 'prob')[,2]
      } else if(model_type == 'svm') {
        pred <- rep(NA, nrow(test))
        result <- predict(model, test, probability = T)
        pred[complete.cases(test)] <- attr(result, 'probabilities')[,2]
      } else if(model_type == 'mlp') {
        pred <- predict(model,  test, array.layout = 'rowmajor')[1,]
      }
      return(pred)
    },
    
    
    
    runCV = function(train, test, label, ...) {
      
      model <- self$trainModel(train, label, ...)
      
      self$predict(model, test)
    }
  ),
  
  private = list(
    
    model_type = NA,
    
    logistic = function(train, label, ...) {
      cat('Logistic regression ...\n')
      data <- cbind(data.frame(Group = label), train)
      formula <- as.formula(paste("Group", paste(colnames(train), collapse = "+"), sep = '~'))
      model <- glm(formula = formula, data = data, family = binomial('logit'))
      pvalue <- coef(summary(model))[-1,'Pr(>|z|)']
      formula <- as.formula(paste("Group", paste(names(pvalue[pvalue<0.05]), collapse = "+"), sep = '~'))
      print(formula)
      model <- glm(formula = formula, data = data, family = binomial('logit'))
      attr(model, 'model_type') <- 'logistic'
      return(model)
    },
    
    c50 = function(train, label, ...) {
      
      if(length(list(...))>0) {trials <- list(...)$trials}
      else trials <- 10
      cat('C5.0 decision tree ...\n')
      model <- C50::C5.0(train, as.factor(label), trials = trials)
      attr(model, 'model_type') <- 'c50'
      return(model)
    },
    
    svm = function(train, label, ...) {
      if(length(list(...))>0) {cost <- list(...)$cost}
      else cost <- 1
      
      cat('SVM ...\n')
      model <- e1071::svm(train, as.factor(label), probability = T, cost = cost)
      attr(model, 'model_type') <- 'c50'
      return(model)
    },
    
    mlp = function(train, label, ...) {
      cat('MLP ...\n ')

      id <- complete.cases(train)
      train <- train[id,]
      label <- label[id]
      
      model <- mxnet::mx.mlp(train, label, hidden_node=c(5), out_node=1,
                             activation = 'sigmoid', out_activation='logistic', ctx = mxnet::mx.cpu(),
                      num.round=1000, array.batch.size=length(label)/5, learning.rate=0.07, momentum=0.9,
                      array.layout = 'rowmajor',
                      eval.metric=mxnet::mx.metric.logistic_acc)
      attr(model, 'model_type') <- 'mlp'
      return(model)
    }
  )
)