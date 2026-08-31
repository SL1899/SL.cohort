

# ------------ 批量因子化函数 ------------


#' 批量设置变量的因子水平
#'
#' 根据变量名称与命名的水平列表，批量将指定变量转换为因子，并按照给定顺序
#' 设置因子水平。`level_list` 中每个变量的第一个水平通常作为默认参考水平。
#'
#' @param data 需要处理的数据框。
#' @param var_list 需要转换为因子的变量名字符向量。
#' @param level_list 命名列表，名称为变量名，每个元素为对应变量的因子水平。
#' @param ordered 是否将转换后的变量设置为有序因子，默认为 `FALSE`。
#'
#' @return 返回完成因子化后的数据框。
#'
#' @details
#' `level_list` 必须为命名列表，并通过变量名称与 `var_list` 进行匹配，
#' 因此二者的排列顺序不需要一致。例如：
#'
#' `var_list = c("gender", "smoke")`
#'
#' `level_list = list(smoke = c("never", "current", "quit"),
#' gender = c("male", "female"))`
#'
#' 对每个变量，`level_list` 中给定水平的顺序即为因子水平顺序。
#' 当 `ordered = FALSE` 时生成普通无序因子；当 `ordered = TRUE` 时生成有序因子。
#'
#' 函数在转换前会检查每个变量的全部非缺失原始取值是否均包含在对应的
#' `level_list` 中。如果存在未覆盖取值，将一次性列出所有存在问题的变量
#' 及其未覆盖取值，并停止转换，避免这些取值被 `factor()` 静默转换为 `NA`。
#'
#' 原始 `NA` 不参与上述覆盖检查，并在因子化后继续保持为 `NA`。
#' `level_list` 中可以包含原始数据当前尚未出现的额外水平，也可以包含
#' `var_list` 中未指定转换的其他变量。
#'
#' @examples
#' data <- data.frame(
#'   gender = c("male", "female", "male"),
#'   smoke = c("never", "current", "quit")
#' )
#'
#' var_list <- c("gender", "smoke")
#'
#' level_list <- list(
#'   smoke = c("never", "current", "quit"),
#'   gender = c("male", "female")
#' )
#'
#' data <- batch_factorize(
#'   data,
#'   var_list,
#'   level_list
#' )
#'
#' @export
batch_factorize <- function(data, var_list, level_list, ordered = FALSE) {

  # 检查level_list是否为命名列表
  if (
    !is.list(level_list) ||
    is.null(names(level_list)) ||
    any(names(level_list) == "")
  ) {
    stop("`level_list` must be a named list.")
  }

  # 检查var_list中是否存在重复变量
  duplicated_vars <- unique(var_list[duplicated(var_list)])

  if (length(duplicated_vars) > 0L) {
    stop(
      "Duplicated variables in `var_list`: ",
      paste(duplicated_vars, collapse = ", ")
    )
  }

  # 检查level_list中是否存在重复变量名
  duplicated_levels <- unique(names(level_list)[duplicated(names(level_list))])

  if (length(duplicated_levels) > 0L) {
    stop(
      "Duplicated variable names in `level_list`: ",
      paste(duplicated_levels, collapse = ", ")
    )
  }

  # 检查指定变量是否均存在于数据中
  missing_vars <- setdiff(var_list, names(data))

  if (length(missing_vars) > 0L) {
    stop(
      "Variables not found in `data`: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # 检查每个指定变量是否均存在对应的levels
  missing_levels <- setdiff(var_list, names(level_list))

  if (length(missing_levels) > 0L) {
    stop(
      "Variables without corresponding levels in `level_list`: ",
      paste(missing_levels, collapse = ", ")
    )
  }

  # 检查所有变量的原始非缺失取值是否均被指定levels覆盖
  uncovered_list <- list()

  for (var in var_list) {

    original_values <- unique(data[[var]])
    original_values <- original_values[!is.na(original_values)]

    uncovered_values <- setdiff(
      original_values,
      level_list[[var]]
    )

    if (length(uncovered_values) > 0L) {
      uncovered_list[[var]] <- uncovered_values
    }
  }

  # 如果存在未被levels覆盖的取值，一次性列出全部异常并停止转换
  if (length(uncovered_list) > 0L) {

    uncovered_message <- vapply(
      names(uncovered_list),
      function(var) {
        paste0(
          "  - ",
          var,
          ": ",
          paste(uncovered_list[[var]], collapse = ", ")
        )
      },
      character(1)
    )

    stop(
      "The following variables contain values not included in `level_list`:\n",
      paste(uncovered_message, collapse = "\n")
    )
  }

  # 全部校验通过后，按照变量名称匹配levels并批量转换为因子
  for (var in var_list) {
    data[[var]] <- factor(
      data[[var]],
      levels = level_list[[var]],
      ordered = ordered
    )
  }

  return(data)
}
