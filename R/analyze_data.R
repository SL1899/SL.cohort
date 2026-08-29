# ------------ 完整数据探查模块 ------------

#' 综合数据探查与统计分析
#'
#' @description 针对大数据集优化的数据探查函数。
#' 对定量变量计算缺失、均值、标准差及分位数；
#' 对分类变量计算缺失、水平数及频数最高的若干水平。
#'
#' @param df 输入的数据框。
#' @param max_levels 整数。分类变量最多展示的水平数量，默认为30。
#' @return 返回变量统计信息的 tibble 数据框。
#' @export

analyze_data <- function(df, max_levels = 30) {

  n <- nrow(df)
  var_names <- names(df)

  # 预先建立结果列表，避免循环中不断扩展对象
  result_list <- vector("list", length(df))


  # ------ 逐变量分析 ------

  for (i in seq_along(df)) {

    x <- df[[i]]
    var_name <- var_names[i]

    # 缺失
    n_missing <- sum(is.na(x))
    miss_rate <- n_missing / n


    # ------ 数值型变量 ------

    if (is.numeric(x)) {

      # 全部缺失
      if (n_missing == n) {

        q <- rep(NA_real_, 5)
        x_mean <- NA_real_
        x_sd <- NA_real_

      } else {

        # 一次性计算min、P25、median、P75、max
        q <- stats::quantile(
          x,
          probs = c(0, 0.25, 0.5, 0.75, 1),
          na.rm = TRUE,
          names = FALSE
        )

        x_mean <- mean(x, na.rm = TRUE)
        x_sd <- stats::sd(x, na.rm = TRUE)
      }

      result_list[[i]] <- tibble::tibble(
        seqn = i,
        type = "numeric",
        variable = var_name,
        na = n_missing,
        na_pct = miss_rate,
        empty = NA_integer_,
        whitespace = NA_integer_,
        levels = NA_integer_,
        mean = x_mean,
        sd = x_sd,
        min = q[1],
        p25 = q[2],
        median = q[3],
        p75 = q[4],
        max = q[5],
        level = NA_integer_,
        is_ordered = NA,
        level_sortd = NA_character_
      )
    }


    # ------ 分类变量 ------

    else if (is.character(x) || is.factor(x)) {

      # 使用vctrs底层C实现进行频数统计
      # 比table()处理大规模character通常更快
      freq_table <- vctrs::vec_count(
        x,
        sort = "count"
      )

      total_levels <- nrow(freq_table)

      # 仅保留频数最高的max_levels个水平
      freq_top <- utils::head(freq_table, max_levels)

      level_name <- as.character(freq_top$key)
      level_name[is.na(freq_top$key)] <- "NA"

      level_counts <- paste0(
        level_name,
        " (",
        freq_top$count,
        ")",
        collapse = "; "
      )

      # character专属检查
      if (is.character(x)) {

        empty_n <- sum(x == "", na.rm = TRUE)
        whitespace_n <- sum(
          grepl("^\\s+$", x, perl = TRUE),
          na.rm = TRUE
        )

      } else {

        empty_n <- NA_integer_
        whitespace_n <- NA_integer_
      }

      result_list[[i]] <- tibble::tibble(
        seqn = i,
        type = if (is.factor(x)) "factor" else "character",
        variable = var_name,
        na = n_missing,
        na_pct = miss_rate,
        empty = empty_n,
        whitespace = whitespace_n,
        levels = total_levels,
        mean = NA_real_,
        sd = NA_real_,
        min = NA_real_,
        p25 = NA_real_,
        median = NA_real_,
        p75 = NA_real_,
        max = NA_real_,
        level = min(total_levels, max_levels),
        is_ordered = if (is.factor(x)) is.ordered(x) else NA,
        level_sortd = level_counts
      )
    }


    # ------ 日期及其他变量 ------

    else {

      result_list[[i]] <- tibble::tibble(
        seqn = i,
        type = class(x)[1],
        variable = var_name,
        na = n_missing,
        na_pct = miss_rate,
        empty = NA_integer_,
        whitespace = NA_integer_,
        levels = NA_integer_,
        mean = NA_real_,
        sd = NA_real_,
        min = NA_real_,
        p25 = NA_real_,
        median = NA_real_,
        p75 = NA_real_,
        max = NA_real_,
        level = NA_integer_,
        is_ordered = NA,
        level_sortd = NA_character_
      )
    }
  }


  # ------ 合并结果 ------

  final_result <- dplyr::bind_rows(result_list)

  return(final_result)
}
