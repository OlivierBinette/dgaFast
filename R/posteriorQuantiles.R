
#' Compute posterior quantiles
#'
#' @usage posteriorQuantiles(weights, N, levels=c(0.025, 0.975), normalize=TRUE)
#'
#' @param weights Matrix of posterior weights for the population size. Each row should be a (possibly un-normalized) posterior distribution on the parameter `N`, as in the result of `bma.cr`.
#' @param N Population sizes under consideration. Each row of `weights` represents a distribution on `N`.
#' @param levels Quantile levels.
#' @param normalize Whether or not to re-normalize the rows of the `weights` matrix.
#'
#' @return Matrix of computed posterior quantiles, where the columns correspond to the quantile levels. Each row corresponds to a row in `weights`.
#'
#' @examples
#' delta <- .5
#' Y <- c(0,27,37,19,4,4,1,1,97,22,37,25,2,1,3,5,83,36,34,18,3,5,0,2,30,5,23,8,0,3,0,2)
#' Y <- array(Y, dim=c(2,2,2,2,2))
#' Nmissing <- 1:300
#' N <- Nmissing + sum(Y)
#' data(graphs5)
#' weights <- bma.cr(Y,  Nmissing, delta, graphs5)
#'
#' # Quantiles of the full posterior
#' posteriorQuantiles(colSums(weights), N)
#'
#' # Model-wise posterior quantiles.
#' posteriorQuantiles(weights, N)
#'
#' @export
posteriorQuantiles <- function(weights, N, levels=c(0.025, 0.975), normalize=TRUE) {
  UseMethod("posteriorQuantiles", weights)
}

.posteriorQuantiles <- function(weights, N, levels=c(0.025, 0.975), normalize=TRUE) {
  if (normalize) weights = weights/sum(weights)

  cs = cumsum(weights)
  sapply(levels, function(level) N[which(cs > level)[1]])
}

#' @export
posteriorQuantiles.default <- function(weights, N, ...) {
  assert(is.numeric(weights),
         is.numeric(N))

  .posteriorQuantiles(weights, N, ...)
}

#' @export
posteriorQuantiles.matrix <- function(weights, N, levels=c(0.025, 0.975), normalize=TRUE) {
  assert(is.matrix(weights),
         is.numeric(N),
         ncol(weights) == length(N))

  if (normalize) weights = weights/rowSums(weights)

  m = sapply(1:nrow(weights), function(i) .posteriorQuantiles(weights[i,,drop=FALSE], N, levels, normalize=FALSE))

  rownames(m) = levels
  return(t(m))
}
