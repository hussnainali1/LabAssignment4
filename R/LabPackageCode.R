
library(R6)

linreg <- R6Class("linreg",
                  public = list(
                    x =  NULL,
                    y =  NULL,
                    BetaHat=  NULL,
                    yHat = NULL,
                    eHat = NULL,
                    Df = NULL,
                    sigmaSquare = NULL,
                    varOfBetaHat = NULL,
                    tOfEachcoefficientAmount = 0,
                    calculation= 0,
                    formula = NULL,
                    data= NULL,

                    initialize = function(formula, data) {
                      self$data = data
                      self$formula = formula
                      self$y <- formula[[2]]
                      self$y <- iris[[self$y]]
                      self$x = model.matrix(formula, data)

                      self$BetaHat <- solve( t(self$x)%*% self$x ) %*% (t( self$x) %*% self$y)

                      self$yHat <- self$x %*% self$BetaHat

                      self$eHat <- self$y - self$yHat

                      self$Df <- as.numeric(nrow(self$x) - ncol(self$x))

                      self$sigmaSquare <- (t(self$eHat) %*% self$eHat) / self$Df

                      self$varOfBetaHat <- as.vector(self$sigmaSquare) * solve(t(self$x) %*% self$x)

                      for(item in 1:length(self$BetaHat)){
                        item <- 3
                        #calculation <- BetaHat[item] / (sqrt(varOfBetaHat[item][item]))
                        self$calculation <- self$BetaHat[item] / (sqrt(self$varOfBetaHat[item,item]))
                        #if(!is.na(self$calculation))
                        self$tOfEachcoefficientAmount <- self$tOfEachcoefficientAmount + self$calculation
                      }
                      greet
                    },
                    greet = function() {
                      cat(paste0("Call:\n"))
                      cat(paste0("lm(formula = Petal.Length ~ Species, data = iris) \n"))
                      cat(paste0(rownames(self$BetaHat), "/n"))

                    }
                  )
)
