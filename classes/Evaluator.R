Evaluator <- R6::R6Class(
  public = list(
    initialize = function(real_value, predicted_value, folds = NULL) {
      private$real_value <- real_value
      private$predicted_value <- predicted_value
      private$folds <- folds
    },
    setIndicator = function(indicator) {
      private$indicator <- indicator
    },
    setFolds = function(folds) {
      private$folds <- folds
    },
    evaluate = function(indicator = self$indicator) {
      if(indicator == 'global_auc') {
          result <- ROCR::prediction(private$predicted_value, private$real_value)
          private$roc(result)
          text(0.9, 0.2, paste("AUC =",round(private$auc(result), 3)))
          res <- ROCR::performance(result, 'sens', 'spec')
          sens <- slot(res, 'y.values')[[1]]
          spec <- slot(res, 'x.values')[[1]]
          alpha <- slot(res, 'alpha.values')[[1]]
          cut_off <- alpha[which.max(sens+spec)]
          text(0.9, 0.1, paste("Cut off =", round(cut_off, 3)))
          return(private$auc(result))
      }
      else if(indicator == 'average_auc') {
        if(is.null(private$folds)) {error('There are no folds.')}
        return(mean(sapply(private$folds, function(fold) {
            result <- ROCR::prediction(private$predicted_value[fold], private$real_value[fold])
            return(private$auc(result))
          }
        )))
      }
    }
  ),
  
  private = list(
    real_value = NA,
    predicted_value = NA,
    indicator = NA,
    folds = NA,
    
    auc = function(result) {
      auc <- ROCR::performance(result, 'auc')
      unlist(slot(auc,"y.values"))
    },
    
    roc = function(result) {
      perf <- ROCR::performance(result,"tpr","fpr")
      # print(perf)
      ROCR::plot(perf, main = 'ROC Curve')
    }
  )
)