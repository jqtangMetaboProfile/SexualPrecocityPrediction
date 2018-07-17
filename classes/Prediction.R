source('classes/Experiment.R')
Prediction <- R6::R6Class(
  inherit = Experiment,
  
  public = list(
    train.x = NA, train.y = NA, test.x = NA, test.y = NA,
    real_value = NA, predicted_value = NA,
    useless  = NA,
    
    setUseless = function(...) {
      self$useless <- unlist(list(...))
    },
    
    eliminateUseless = function(data, useless) {
      data[,setdiff(colnames(data), useless)]
    }
  )
)
