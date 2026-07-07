

# ------------ 完整数据探查模块 ------------

#' 综合数据探查与统计分析
#'
#' @description 结合 skimr 基础统计与自定义的分类变量水平提取功能，生成数据框的详细概览。
#'
#' @param df 输入的数据框。
#' @param max_levels 整数。分类变量最多提取的水平数量，默认为 30。
#' @return 返回一个包含各变量统计信息和分类水平的 tibble 数据框。
#' @export
#'
#' @examples
#' \dontrun{
#' df_summary <- analyze_data(iris)
#' }

analyze_data <- function(df, max_levels = 30) {

  # --- 内部函数：提取分类变量的水平信息 ---
  extract_sorted_levels <- function(df, max_levels) {
    df %>%
      dplyr::select(tidyselect::where(is.character) | tidyselect::where(is.factor)) %>%
      purrr::imap(~ {
        freq_table <- table(.x, useNA = "ifany") %>%
          sort(decreasing = TRUE)

        if (length(freq_table) > max_levels) {
          freq_table <- freq_table[1:max_levels]
        }

        level_counts <- paste0(
          names(freq_table), " (", freq_table, ")",
          collapse = "; "
        )

        tibble::tibble(
          variable = .y,
          n_levels = length(freq_table),
          total_levels = length(table(.x, useNA = "ifany")),
          level_counts = level_counts
        )
      }) %>%
      dplyr::bind_rows()
  }

  # 1. 使用skimr获取基础统计
  skim_result <- as.data.frame(skimr::skim(df))

  # 2. 添加列序号和缺失率
  skim_result <- skim_result %>%
    dplyr::mutate(
      column_position = match(skim_variable, names(df)),
      miss_rate = 1 - complete_rate
    ) %>%
    dplyr::select(column_position, dplyr::everything())

  # 3. 提取分类变量水平信息
  categorical_levels <- extract_sorted_levels(df, max_levels)

  # 4. 合并所有结果
  result <- dplyr::left_join(
    skim_result,
    categorical_levels,
    by = c("skim_variable" = "variable")
  )

  # 5. 选择并重命名最终列
  final_result <- result %>%
    dplyr::select(
      seqn = column_position,
      type = skim_type,
      variable = skim_variable,
      na = n_missing,
      na_pct = miss_rate,
      dplyr::any_of(c(
        "empty" = "character.empty",
        "whitespace" = "character.whitespace",
        "levels" = "total_levels",
        "mean" = "numeric.mean",
        "sd" = "numeric.sd",
        "min" = "numeric.p0",
        "max" = "numeric.p100",
        "median" = "numeric.p50",
        "p25" = "numeric.p25",
        "p75" = "numeric.p75",
        "level" = "n_levels",
        "is_ordered",
        "level_sortd" = "level_counts"))
    )

  return(final_result)
}



# ------------ 只提取分类水平 ------------

#' 提取分类变量的纯文本水平
#'
#' @description 按因子固有顺序提取分类变量的水平，不包含频数数量。
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
  df %>%
    dplyr::select(tidyselect::where(is.character) | tidyselect::where(is.factor)) %>%
    purrr::imap(~ {
      freq_table <- table(.x, useNA = "ifany")

      if (length(freq_table) > max_levels) {
        freq_table <- freq_table[1:max_levels]
      }

      level_counts <- paste0(
        "'", names(freq_table), "'",
        collapse = ", "
      )

      tibble::tibble(
        variable = .y,
        n_levels = length(freq_table),
        total_levels = length(table(.x, useNA = "ifany")),
        level_counts = level_counts
      )
    }) %>%
    dplyr::bind_rows()
}

