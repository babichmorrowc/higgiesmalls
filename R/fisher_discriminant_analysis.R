#' Perform Fisher Discriminant Analysis in the binary classification case.
#'
#' @param data Dataset containing a column [label_col] with values for the positive and negative classes.
#' @param label_col Column of [data] containing the class labels (should have values of [class_pos] and [class_neg]).
#' @param class_pos Value of [label_col] for the positive class.
#' @param class_neg Value of [label_col] for the negative class.
#'
#' @return The vector result of FDA in the binary classification case.
#' @import ggplot2
#' @export
#' @examples
#' X_p <- matrix(rnorm(400) + 3, nrow = 200)
#' X_n <- matrix(rnorm(400) - 3, nrow = 200)
#' input_X <- rbind(X_p, X_n)
#' input_y <- c(rep(1, nrow(X_p)), rep(-1, nrow(X_n)))
#' input_Xy <- data.frame(cbind(input_X, input_y))
#' w_vec <- fisher_discrim(input_Xy, label_col = "input_y", class_pos = 1, class_neg = -1)
#' # Plot showing the direction of the vector:
#' ggplot2::ggplot() +
#' ggplot2::geom_point(data = input_Xy, ggplot2::aes(x = V1, y = V2, color = as.factor(input_y))) +
#' ggplot2::geom_abline(intercept = 0, slope = w_vec[1] / w_vec[2]) +
#' ggplot2::labs(x = "Dim 1", y = "Dim 2", color = "class")
#'
fisher_discrim <- function(data, label_col = "Label", class_pos, class_neg) {
  X_pos <- data %>%
    dplyr::filter(get(label_col) == class_pos) %>%
    dplyr::select(-{{label_col}})
  X_neg <- data %>%
    dplyr::filter(get(label_col) == class_neg) %>%
    dplyr::select(-{{label_col}})
  n_pos <- nrow(X_pos)
  n_neg <- nrow(X_neg)
  mu_pos <- colMeans(X_pos)
  mu_neg <- colMeans(X_neg)
  mu_diff <- matrix(mu_pos - mu_neg)
  cov_pos <- stats::cov(X_pos)
  cov_neg <- stats::cov(X_neg)

  S_w <- n_pos*cov_pos + n_neg*cov_neg
  w <- solve(S_w) %*% mu_diff
  return(w)
}

#' Calculate the ratio of between class scatterness to within class scatterness. This is the ratio maximised by Fisher Discriminant Analysis.
#'
#' @param data Dataset containing a column [label_col] with values for the positive and negative classes.
#' @param label_col Column of [data] containing the class labels (should have values of [class_pos] and [class_neg]).
#' @param class_pos Value of [label_col] for the positive class.
#' @param class_neg Value of [label_col] for the negative class.
#' @param w Vector result of performing FDA (can be the output of a call to [fisher_discrim()])
#'
#' @return A value for the ratio of between class scatterness to within class scatterness.
#' @export
#' @examples
#' X_p <- matrix(rnorm(400) + 3, nrow = 200)
#' X_n <- matrix(rnorm(400) - 3, nrow = 200)
#' input_X <- rbind(X_p, X_n)
#' input_y <- c(rep(1, nrow(X_p)), rep(-1, nrow(X_n)))
#' input_Xy <- data.frame(cbind(input_X, input_y))
#' w_vec <- fisher_discrim(input_Xy, label_col = "input_y", class_pos = 1, class_neg = -1)
#' scatter_ratio(input_Xy, label_col = "input_y", class_pos = 1, class_neg = -1, w = w_vec)
#'
scatter_ratio <- function(data, label_col = "Label", class_pos, class_neg, w) {
  data_matrix <- data %>%
    dplyr::select(-{{label_col}}) %>%
    as.matrix()
  pos_rows <- data[,label_col] == class_pos
  neg_rows <- data[,label_col] == class_neg
  wT_x <- apply(data_matrix, 1, function(x) sum(x*w))
  mu_pos <- sum(wT_x[pos_rows]) / sum(pos_rows)
  mu_neg <- sum(wT_x[neg_rows]) / sum(neg_rows)
  mu <- sum(wT_x) / nrow(data)

  # Calculate within class scatterness
  s_w_pos <- sum((wT_x[pos_rows] - mu_pos)^2)
  s_w_neg <- sum((wT_x[neg_rows] - mu_neg)^2)

  # Calculate between class scatterness
  s_b_pos <- sum(pos_rows) * (mu_pos - mu)^2
  s_b_neg <- sum(neg_rows) * (mu_neg - mu)^2

  ratio <- (s_b_pos + s_b_neg) / (s_w_pos + s_w_neg)
  return(ratio)
}
