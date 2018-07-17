source('classes/Prediction.R')
source('classes/Classifier.R')
source('classes/Evaluator.R')
Classification <- R6::R6Class(
  inherit = Prediction,
  public =  list(
    
    setLabel = function(label) {
      vars <- colnames(self$preprocessed_data)
      if(!label%in%vars) stop(paste(label, 'is not existed!'))
      private$label <- label
      private$data.x <- self$preprocessed_data[,setdiff(vars, label)]
      self$real_value <- self$preprocessed_data[,label]
      self$predicted_value <- vector(length = length(self$real_value))
    },
    
    normalize = function(method, data = NULL) {
      if(is.na(private$label)) stop('Please set label first!')
      if(is.null(data)) data <- private$data.x
      private$normalized_data <- switch (method,
        min_max = apply(data, 2, function(x) {
          (x-min(x, na.rm = T)) / (max(x, na.rm = T)-min(x, na.rm = T))
        }),
        z_score = scale(data)
      )
    },
    
    getNormalized  = function() {
      return(private$normalized_data)
    },
    
    setPartition  = function(data = NULL, train.p = 0.7) {
      if(is.null(data)) data <- self$preprocessed_data
      data <- sexual_precocity$preprocessed_data
      train_logical <- sample(c(T, F), size = nrow(data), replace = T, prob = c(train.p,1-train.p))
      self$train.x <- data[train_logical, setdiff(colnames(self$preprocessed_data), private$label)]
      self$test.x <- data[!train_logical, setdiff(colnames(self$preprocessed_data), private$label)]
      self$train.y <- data[train_logical, private$label]
      self$test.y <- data[!train_logical, private$label]
    },
    
    evaluate = function(label = self$real_value, pred =self$predicted_value, indicators = list()) {
      evaluator <- Evaluator$new(label, pred, private$folds)
      sapply(indicators, function(each_indicator) {
        switch (each_indicator,
                global_auc = evaluator$evaluate('global_auc'),
                average_auc = evaluator$evaluate('average_auc')
        )
      })
    },
    
    crossValidate = function(model_type, nomalize_type = z,k = 10, ...) {
      data <- private$normalized_data
      private$folds <- caret::createFolds(seq(nrow(data)), k = k)
      for (i in seq(length(private$folds))) {
        fold <- private$folds[[i]]
        cur_train <- data[-fold,]
        cur_test <- data[fold,]
        cur_real_value <- self$real_value[-fold]
        classfier <- Classifier$new(model_type)
        cat("Fold ", i, "\n")
        self$predicted_value[fold] <- classfier$runCV(cur_train, cur_test, cur_real_value, ...)
      }
    },
    
    train = function(model_type, train.x, train.y, ...) {
      classfier <- Classifier$new(model_type)
      classfier$trainModel(train.x, train.y, ...)
    },
    
    predict = function(model, test.x, output_type = "label") {
      classfier <- Classifier$new(attr(model, 'model_type'))
      pred <- classfier$predict(model, test.x)
      if(output_type == 'label') {
        cut_off <- quantile(pred, 1-mean(train.y))
        return(pred>cut_off)
      } else if (output_type == 'score') {
        return(pred)
      } else {
        stop('No such outpt type!')
      }
    }
  ),
  private = list(
    folds = NA,
    label = NA,
    data.x = NA,
    normalized_data = NA
  )
)