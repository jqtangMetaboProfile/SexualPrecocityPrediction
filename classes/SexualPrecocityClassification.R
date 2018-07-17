source('classes/Classification.R')
SexualPrecocityClassification <- R6::R6Class(
  inherit = Classification,
  public = list(

    preprocess = function(filename, na.rm = F) {
      
      # call from Experiment
      super$preprocess(filename)
      
      # eliminate useless vars
      tmp <- self$preprocessed_data
      tmp <- self$eliminateUseless(tmp, self$useless)
      
      # process special vars
      tmp$Breast <- gsub("B","",tmp$Breast)
      fuzzy_id <- grep('-', tmp$Breast)
      tmp$Breast[fuzzy_id] <- 
        sapply(strsplit(tmp$Breast[fuzzy_id], split = '-'), function(x) mean(as.numeric(x)))
      tmp$Breast <- as.numeric(tmp$Breast)
      
      # convert to numeric
      suppressWarnings(tmp <- apply(tmp, 2, as.numeric))
      
      # eliminate samples more than two NA
      tmp <-  tmp[!apply(tmp, 1, function(x) sum(is.na(x))>2),]
      
      if(na.rm) tmp <- tmp[complete.cases(tmp),]
      
      self$preprocessed_data <- tmp
      
    }
  )
)