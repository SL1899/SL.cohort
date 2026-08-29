# ------------ 只提取分类水平 ------------

#' 提取分类变量的纯文本水平
#'
#' @description 提取字符型和因子型变量的水平，不包含频数。
#' 因子变量保留其固有水平顺序，字符变量按字典序排列。
#'
#' @param df 输入的数据框。
#' @param max_levels 整数。最多提取的水平数量，默认为 30。
#' @return 返回一个包含变量名、水平数量及纯文本水平连接串的 tibble 数据框。
#' @export
#'
#' @examples
#' \dontrun{
#' levels_summary <- extract_levels(iris)
#' }

extract_levels <- function(df, max_levels = 30) {

  # 找出字符型和因子型变量
  var_index <- which(
    vapply(
      df,
      function(x) is.character(x) || is.factor(x),
      logical(1)
    )
  )

  # 预先建立结果列表
  result_list <- vector("list", length(var_index))


  for (j in seq_along(var_index)) {

    i <- var_index[j]
    x <- df[[i]]


    # ------ 因子变量 ------

    if (is.factor(x)) {

      # 直接读取factor自身的levels，无需扫描整列
      level_values <- levels(x)

      # 与原table(useNA = "ifany")保持一致：
      # 如果存在NA，则把NA也视为一个水平
      if (anyNA(x)) {
        level_values <- c(level_values, NA_character_)
      }


      # ------ 字符变量 ------

    } else {

      # 只提取唯一值，不做频数统计
      level_values <- sort(
        unique(x),
        na.last = TRUE
      )
    }


    # 全部水平数
    total_levels <- length(level_values)

    # 最多展示max_levels个
    level_values <- utils::head(
      level_values,
      max_levels
    )

    # NA显示为NA
    level_text <- ifelse(
      is.na(level_values),
      "NA",
      level_values
    )

    # 拼接水平
    level_counts <- paste0(
      "'",
      level_text,
      "'",
      collapse = ", "
    )


    result_list[[j]] <- tibble::tibble(
      variable = names(df)[i],
      n_levels = length(level_values),
      total_levels = total_levels,
      level_counts = level_counts
    )
  }


  # 合并结果
  dplyr::bind_rows(result_list)
}
