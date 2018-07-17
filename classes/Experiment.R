Experiment <- R6::R6Class(
  public = list(
    
    experiment_path = NA, data_path = NA,
    raw_data = NA, preprocessed_data = NA,
    
    initialize = function(experiment_path, data_path) {
      # cat('Experiment\n')
      self$experiment_path <- experiment_path
      self$data_path <- data_path
    },
    
    preprocess  = function(filename) {
      self$raw_data <- read.csv(paste(data_path,filename,sep = '/'), stringsAsFactors = F)
      self$preprocessed_data <- self$raw_data
    }
  )
)