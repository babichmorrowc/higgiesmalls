#' Parameter Tuning Using Grid Search to Maximise AMS
#'
#' @param METHOD Either the function to be tuned or a character string naming such a function
#' @param train.x Either a formula or a matrix of predictors
#' @param train.y The response variable if [train.x] is a predictor matrix. Ignored if [train.x] is a formula
#' @param data If formula is used, this is the data set.
#' @param training_data_weights Vector of weights of the dataset used
#' @param validation.x Optional validation set (see documentation for [e1071::tune])
#' @param ranges Named list of parameter vectors
#' @param predict.func Optional predict function
#' @param cross Number of partitions for cross-validation
#' @param n_cores_use Number of cores to use for parallelisation
#' @param ... Further parameters to be passed to the training functions
#'
#' @return An object of class `tune` including the components:
#' \itemize{
#'  \item{[best.parameters] a 1 x k data frame where k is the number of parameters}
#'  \item{[best.performance]} best achieved performance
#'  \item{[performances]} A data frame of all parameter combinations along with the corresponding performance results
#'  \item{[train.ind]} List of index vectors used for splits into training and validation sets
#'  \item{[best.model]} The model trained on the complete training data using the best parameter combination
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' svm_radial_tune <- ams_tune_svm_parallel(svm,
#'                                          Label ~ .,
#'                                          data = higgs_training_20,
#'                                          kernel = "radial",
#'                                          cross = 5,
#'                                          training_data_weights = training_weights_20,
#'                                          ranges = list(
#'                                            cost = c(0.5, 1, 2),
#'                                            gamma = c(0.01, 0.1, 0.5)
#'                                          )
#' )
#' }
#'
ams_tune_svm_parallel <- function(METHOD, train.x, train.y = NULL, data = list(),
                                  training_data_weights = NULL,
                                  validation.x = NULL,
                                  ranges = NULL, predict.func = predict,
                                  cross = 5,
                                  n_cores_use = 16,
                                  ...
) {
  call <- match.call()

  # Set up parallelisation
  doParallel::registerDoParallel(n_cores_use)

  ## internal helper functions
  resp <- function(formula, data) {

    model.response(model.frame(formula, data))
  }

  # Function for extracting the data for validation
  getValidationData <- function(train.x, validation.x, useFormula, data, train_ind_sample) {
    if (!is.null(validation.x)) {
      return(validation.x)
    } else if (useFormula) {
      return(data[-train_ind_sample,, drop = FALSE])
    } else if (inherits(train.x, "matrix.csr")) {
      return(train.x[-train_ind_sample, ])
    } else {
      return(train.x[-train_ind_sample,, drop = FALSE])
    }
  }

  # Function for extracting true y values
  getTrueY <- function(useFormula, train.x, data, train_ind_sample) {
    if (useFormula) {
      if (!is.null(validation.x)) {
        return(resp(train.x, validation.x))
      } else {
        return(resp(train.x, data[-train_ind_sample,]))
      }
    } else {
      return(train.y[-train_ind_sample])
    }
  }

  # Function for computing performance metric
  computeAMS <- function(true.y, pred, data, weights, train_ind_sample) {
    adj_weights <- adjust_weights(cbind(data, weights),
                                  cbind(data, weights)[train_ind_sample, ],
                                  unadjusted_weight_col = as.character(quote(weights)))
    ams <- approx_median_sig(predictions = pred,
                             labels = true.y,
                             weights = adj_weights,
                             b_reg = 10)
    return(ams)
  }

  ## parameter handling
  validation.x <- NULL
  useFormula <- is.null(train.y)
  if (useFormula && (is.null(data) || length(data) == 0))
    data <- model.frame(train.x)
  if (is.vector(train.x)) train.x <- t(t(train.x))
  if (is.data.frame(train.y))
    train.y <- as.matrix(train.y)

  ## prepare training indices
  n <- nrow(if (useFormula) data else train.x)
  perm.ind <- sample(n)
  if (cross > n)
    stop(sQuote("cross"), " must not exceed sampling size!")
  if (cross == 1)
    stop(sQuote("cross"), " must be greater than 1!")
  train.ind <- tapply(1:n, cut(1:n, breaks = cross), function(x) perm.ind[-x])

  ## find best model
  parameters <- if (is.null(ranges))
    data.frame(dummyparameter = 0)
  else
    expand.grid(ranges)
  p <- nrow(parameters)
  model.variances <- model.ams <- c()

  ## - loop over all models
  sampling.ams <- foreach(para.set = 1:p, .combine = "cbind") %:%
    foreach(sample = 1:length(train.ind), .combine = "c") %dopar% {
      pars <- if (is.null(ranges)) {
        NULL
      } else {
        lapply(parameters[para.set,,drop = FALSE], unlist)
      }

      train_data <- if (useFormula) {
        list(train.x, data = data, subset = train.ind[[sample]], ...)
      } else {
        list(train.x[train.ind[[sample]],], y = train.y[train.ind[[sample]]], ...)
      }

      # Train models and predict validation set
      model <- do.call(METHOD, c(train_data, pars))
      pred <- predict.func(model, getValidationData(train.x, validation.x, useFormula, data, train.ind[[sample]]))

      ## compute performance measure
      true.y <- getTrueY(useFormula, train.x, data, train.ind[[sample]])

      if (is.null(true.y)) true.y <- rep(TRUE, length(models[[1]]))

      computeAMS(true.y, pred, data, training_data_weights, train.ind[[sample]])
    }

  # clean up the cluster
  doParallel::stopImplicitCluster()

  sampling.ams <- matrix(unlist(sampling.ams), nrow = length(train.ind), ncol = p)
  model.ams <- apply(sampling.ams, 2, mean)
  model.variances <- apply(sampling.ams, 2, sd)

  ## return results
  best <- which.max(model.ams)
  pars <- if (is.null(ranges))
    NULL
  else
    lapply(parameters[best,,drop = FALSE], unlist)
  structure(list(best.parameters  = parameters[best,,drop = FALSE],
                 best.performance = model.ams[best],
                 method           = if (!is.character(METHOD))
                   deparse(substitute(METHOD)) else METHOD,
                 nparcomb         = nrow(parameters),
                 train.ind        = train.ind,
                 sampling         = if (cross == n) "leave-one-out" else
                   paste(cross,"-fold cross validation", sep=""),
                 performances     = cbind(parameters, ams = model.ams, dispersion = model.variances),
                 best.model       = if (TRUE) {
                   modeltmp <- if (useFormula)
                     do.call(METHOD, c(list(train.x, data = data),
                                       pars, list(...)))
                   else
                     do.call(METHOD, c(list(x = train.x,
                                            y = train.y),
                                       pars, list(...)))
                   call[[1]] <- as.symbol("best.tune")
                   modeltmp$call <- call
                   modeltmp
                 }
  ),
  class = "tune"
  )
}
