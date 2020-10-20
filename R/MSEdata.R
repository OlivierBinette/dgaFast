#' MSE data format
#'
#' The function \code{MSEdata()} transforms an existing dataframe to the "MSE" format,
#' ensuring it contains a "count" column and that the other columns refer to
#' inclusion (1) or exclusion (0) on a set of lists.
#'
#' Zero counts of unobserved capture patterns are added and duplicates capture patterns
#' are aggregated.
#'
#' @param data Original MSE dataframe. It should contain a column named "count"
#'     with the observed counts of capture patterns, as well as columns representing
#'     the different lists, as follows:
#'\preformatted{         c1    c2 count
#'          0     1     7
#'          1     0     3
#'          1     1     4}
#'
#' @seealso \code{\link{plotMSE}}
#' @export
MSEdata <- function(data) {
  assert(inherits(data, "data.frame"))

  # Validate count column
  assert("count" %in% names(data),
         msg = "A column named 'count' should be specified.")
  assert(is.numeric(data$count),
         all(data$count >= 0),
         all((data$count %% 1) == 0),
         msg = "Count column should only contain non-negative integers.")

  # Validate other columns
  listnames = base::setdiff(names(data), "count")
  for (list in listnames) {
    assert(is.numeric(data[,list, drop=TRUE]))
    assert(all(data[,list, drop=TRUE] %in% c(0,1)),
           msg="List columns can only contain zeros and ones.")
  }

  data = clean_MSE_data(data)

  attr(data, "class") <- c("MSEdata", attr(data, "class"))

  return(data)
}

#' Standardize MSE data format
#'
#' @param data MSE dataframe to be cleaned up.
#'
#' @importFrom dplyr %>%
clean_MSE_data <- function(data) {

  nlists = ncol(data) - 1
  data = data %>%
    group_by_at(vars(-count)) %>%
    count(wt=count, name="count") %>%
    ungroup()

  # Binary table with all combinations of zeros and ones
  X = eval(parse(text=
                   paste0("table(", paste0(rep("c(0,1)", nlists), collapse=","), ")")
  )) %>%
    as.data.frame.table %>%
    map_dfc(as.numeric) - 1

  # Removing the count for unobserved cases and removing superfluous column
  X = X[2:nrow(X), 1:nlists]

  X = data.frame(integer.base.b(1:(2^nlists - 1), 2))

  # Match column names of the data to those of the binary matrix
  listnames = setdiff(names(data), "count")
  colnames(X) = listnames

  # Join the binary table with the observed counts
  result = left_join(X, data, by=listnames)

  # Reorder observations
  o1 = order(rowApply(result, function(x) paste0(x, collapse="")))
  result = result[rev(o1),]
  o2 = order(rowSums(result[, listnames]))
  result = result[o2,]

  # Set NA counts to zero
  result[is.na(result[,"count"]), "count"] = 0

  rownames(result) = 1:nrow(result)

  return(result)
}


#' Inheritance check
#' @param data MSE dataframe.
is.MSEdata <- function(data) {
  inherits(data, "MSEdata")
}

#' Get list names
#'
#' @param mse_data object of class `MSEdata`.
#' @return names of the MSE lists.
#'
#' @export
list.names <- function(mse_data) {
  assert(is.MSEdata(mse_data))

  return(base::setdiff(names(mse_data), "count"))
}

#' Set list names
#'
#' @param mse_data MSE dataframe.
#' @param value list of names.
#'
`list.names<-` <- function(mse_data, value) {
  assert(is.MSEdata(mse_data))
  assert(length(value) == ncol(mse_data)-1)

  colnames(mse_data)[colnames(mse_data) != "count"] = value
  mse_data
}

#' Number of observed cases
#'
#' @param mse_data MSE dataframe.
nobs.MSEdata <- function(mse_data) {
  assert(is.MSEdata(mse_data))

  return(sum(mse_data$count))
}

#' Number of lists
#' @param mse_data MSE dataframe.
nlists <- function(mse_data) {
  assert(is.MSEdata(mse_data))

  return(length(list.names(mse_data)))
}

#' @param mse_data MSE dataframe.
#' @param lists lists to omi.
omit <- function(mse_data, lists) {
  assert(is.MSEdata(mse_data))

  cols = setdiff(names(mse_data), lists)

  return(MSEdata(mse_data[, cols]))
}

#' Merge lists
#'
#' @param mse_data MSEdata object from which to merge lists.
#' @param ... names of the lists to be merged.
#'
merge <- function(mse_data, ...) {
  assert(is.MSEdata(mse_data))

  args = list(...)

  data = MSEdata(mse_data)

  for (lists in args) {
    data[paste(lists, collapse="-")] <- 1*(rowSums(data[,lists]) > 0)
  }

  data = data[, setdiff(names(data), unlist(args))]

  return(MSEdata(data))
}


