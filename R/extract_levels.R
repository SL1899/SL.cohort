

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

