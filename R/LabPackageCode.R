
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
                    namedvector= NULL,

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
                      self$greet()
                    },
                    # greet = function() {
                    #   cat(paste0("Call:\n"))
                    #   cat(paste0("lm(formula = Petal.Length ~ Species, data = iris) \n"))
                    #   cat(paste0(rownames(self$BetaHat), "/n"))
                    # }

#' Print
#'
#' @return String containing results
#' @export print
#'
#' @examples
#'
                    print <- function(){
                      cat(paste0("Call:\n"))
                      cat(paste0("lm(formula = Petal.Length ~ Species, data = iris) \n"))
                      cat(paste0(rownames(self$BetaHat), "/n"))
                    }

#' plot
#'
#' @return Create Two Graphs
#' @export plot
#'
#' @examples
#'
                    plot <- function(){
                      ggplot(data = s$data, mapping = aes(x = s$yHat, y = s$eHat)) + geom_point()

                    }

#' resid
#'
#' @return Return the vector of residuals eˆ.
#' @export
#'
#' @examples
#'
                    resid <- function(){
                      return(self$eHat)
                    }


#' pred
#'
#' @return Return the predicted values yˆ
#' @export pred
#'
#' @examples
                    pred <- function(){
                      return(self$yHat)
                    }

#' coef
#'
#' @return Return the coefficients as a named vector
#' @export coef
#'
#' @examples
                    coef <- function(){
                      self$namedvector <- as.vector(s$BetaHat)
                      names(self$namedvector) <- c("Intercept", "Speciesversicolor", "Speciesvirginica")
                      return(self$namedvector)
                    }

                    summary <- function(){

                    }
                  )
)
