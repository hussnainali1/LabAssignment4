

#' Package LinReg
#'
#' @field x matrix.
#' @field y numeric.
#' @field BetaHat matrix.
#' @field yHat matrix.
#' @field eHat matrix.
#' @field Df numeric.
#' @field sigmaSquare matrix.
#' @field varOfBetaHat matrix.
#' @field tOfEachcoefficientAmount numeric.
#' @field calculation numeric.
#' @field formula formula.
#' @field data data.frame.
#' @field namedvector numeric.
#'
#' @return Print out the coefficients and coefficient names, similar as done by the lm class.
#' @export linreg
#'
#' @examples
#' data(iris)
#'mod_object <- lm(Petal.Length~Species, data = iris)
#'print(mod_object)


linreg <- setRefClass("linreg",
                      fields = list(
                        x =  "matrix",
                        y =  "numeric",
                        BetaHat=  "matrix",
                        yHat = "matrix",
                        eHat = "matrix",
                        Df = "numeric",
                        sigmaSquare = "matrix",
                        varOfBetaHat = "matrix",
                        tOfEachcoefficientAmount = "numeric",
                        calculation= "numeric",
                        formula = "formula",
                        data= "data.frame",
                        namedvector= "numeric"
                        ),

                      methods = list(
                        initialize = function(formula, data) {
                          .self$data = data
                          .self$formula = formula
                          .self$y <- .self$data[[formula[[2]]]]
                          .self$x = model.matrix(formula, data)

                          .self$BetaHat <- solve( t(.self$x)%*% .self$x ) %*% (t( .self$x) %*% .self$y)

                          .self$yHat <- .self$x %*% .self$BetaHat

                          .self$eHat <- .self$y - .self$yHat

                          .self$Df <- as.numeric(nrow(.self$x) - ncol(.self$x))

                          .self$sigmaSquare <- (t(.self$eHat) %*% .self$eHat) / .self$Df

                          .self$varOfBetaHat <- as.vector(.self$sigmaSquare) * solve(t(.self$x) %*% .self$x)

                          .self$tOfEachcoefficientAmount <- c(1)
                          for(item in 1:length(.self$BetaHat)){
                            .self$calculation <- .self$BetaHat[item] / (sqrt(.self$varOfBetaHat[item,item]))
                            .self$tOfEachcoefficientAmount <- append(.self$tOfEachcoefficientAmount, .self$calculation)
                            #.self$tOfEachcoefficientAmount <- .self$tOfEachcoefficientAmount + .self$calculation
                          }
                          .self$tOfEachcoefficientAmount <- .self$tOfEachcoefficientAmount[-1]

                        },

                        print = function(){
                          cat(paste0("Call:\n"))
                          cat(paste0("lm(formula = Petal.Length ~ Species, data = iris) \n"))
                          cat(paste0(rownames(.self$BetaHat)))
                          cat("\n")
                          cat(paste0(.self$BetaHat))

                        },
                        plot = function(){
                         plot1 <-  ggplot2::ggplot(data = .self$data, mapping = ggplot2::aes(x = .self$yHat, y = .self$eHat)) +
                          ggplot2::geom_point() +
                          ggplot2::stat_summary(fun = median, geom = "line")

                         base::print(plot1)

                          plot2 <- ggplot2::ggplot(data = .self$data, mapping = ggplot2::aes(x =.self$yHat , y = sqrt(abs((.self$eHat - mean(.self$eHat) ) / sd(.self$eHat) ) ))) +
                            ggplot2::geom_point() +
                            ggplot2::stat_summary(fun = median, geom = "line")

                          base::print(plot2)
                        },
                        resid = function(){
                          return(.self$eHat)
                        },
                        pred = function(){
                          return(.self$yHat)
                        },
                        coef = function(){
                          .self$namedvector <- as.vector(.self$BetaHat)
                          names(.self$namedvector) <- c("Intercept", "Speciesversicolor", "Speciesvirginica")
                          return(.self$namedvector)
                        },
                        summary = function(){
                          newEstricVector <- c("1")
                          newMatrix <- matrix(.self$BetaHat)
                          newMatrix <- cbind(newMatrix, (sqrt(as.vector(diag(.self$varOfBetaHat)))))
                          newMatrix <- cbind(newMatrix, (.self$tOfEachcoefficientAmount))
                          rownames(newMatrix) <- c( "(Intercept)" , "Speciesversicolor" , "Speciesvirginica" )
                          # for(item in 1:length(tOfEachcoefficientAmount)){
                          #   prob <-  pt(tOfEachcoefficientAmount[item], .self$Df)
                          # if(prob<0.01) append(newEstricVector,c("***"))
                          # else if (prob>0.01 && prob<0.05) append(newEstricVector,c("**"))
                          # else if (prob>0.05 && prob<0.1) append(newEstricVector,c("*"))
                          # }
                          # newEstricVector <-newEstricVector[-1]
                          # newMatrix <- cbind(newMatrix, (newEstricVector))
                          base::print(newMatrix)
                          cat(paste0("Residual standerd Error ", .self$sigmaSquare, " on ", .self$Df , " Degrees of Freedom"))

                        }
                      )
)
