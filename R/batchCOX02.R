# ------------ 批量Cox回归 ------------


# --- 内部函数：获取模型应有的系数行名称 ---

.batchCOX02_level_names <- function(data, vars) {

  level_names <- tryCatch({

    if (nrow(data) == 0L) {
      return(vars)
    }

    # 仅取少量样本构建设计矩阵，避免在大数据上额外占用大量内存
    n_proto <- min(100L, nrow(data))
    proto <- data[seq_len(n_proto), vars, drop = FALSE]

    # character变量先按全数据取值构造factor，避免小样本未覆盖全部水平
    for (var in vars) {
      if (is.character(data[[var]])) {
        char_levels <- sort(unique(data[[var]][!is.na(data[[var]])]))
        proto[[var]] <- factor(proto[[var]], levels = char_levels)
      }
    }

    rhs_formula <- stats::as.formula(
      paste("~", paste(vars, collapse = "+"))
    )

    model_frame <- stats::model.frame(
      rhs_formula,
      data = proto,
      na.action = stats::na.pass
    )

    model_matrix <- stats::model.matrix(
      rhs_formula,
      data = model_frame
    )

    coef_names <- colnames(model_matrix)
    coef_names <- coef_names[coef_names != "(Intercept)"]

    if (length(coef_names) == 0L) {
      vars
    } else {
      coef_names
    }

  }, error = function(e) {

    # 设计矩阵无法构造时采用保守规则生成行名称
    unlist(
      lapply(vars, function(var) {

        x <- data[[var]]

        if (is.factor(x)) {

          lev <- levels(x)

          if (length(lev) <= 1L) {
            return(var)
          }

          if (is.ordered(x)) {

            n_coef <- length(lev) - 1L

            suffix <- if (n_coef == 1L) {
              ".L"
            } else if (n_coef == 2L) {
              c(".L", ".Q")
            } else if (n_coef == 3L) {
              c(".L", ".Q", ".C")
            } else {
              c(".L", ".Q", ".C", paste0("^", 4:n_coef))
            }

            return(paste0(var, suffix))
          }

          return(paste0(var, lev[-1]))
        }

        if (is.character(x)) {

          lev <- sort(unique(x[!is.na(x)]))

          if (length(lev) <= 1L) {
            return(var)
          }

          return(paste0(var, lev[-1]))
        }

        if (is.logical(x)) {
          return(paste0(var, "TRUE"))
        }

        var
      }),
      use.names = FALSE
    )
  })

  unique(as.character(level_names))
}


# --- 内部函数：生成单个失败模型的NA结果 ---

.batchCOX02_na_model <- function(data, vars, model_id) {

  level_names <- .batchCOX02_level_names(data, vars)

  if (length(level_names) == 0L) {
    level_names <- NA_character_
  }

  n_rows <- length(level_names)

  data.frame(
    model_id = rep(model_id, n_rows),
    var_number = rep(length(vars), n_rows),
    fixed_var = rep(vars[1], n_rows),
    adjust_vars = rep(
      if (length(vars) > 1) paste0(vars[-1], collapse = ", ") else NA_character_,
      n_rows
    ),
    level = level_names,
    beta = rep(NA_real_, n_rows),
    se = rep(NA_real_, n_rows),
    Z = rep(NA_real_, n_rows),
    HR = rep(NA_real_, n_rows),
    LCI = rep(NA_real_, n_rows),
    UCI = rep(NA_real_, n_rows),
    p_value = rep(NA_real_, n_rows),
    beta_trend = rep(NA_real_, n_rows),
    se_trend = rep(NA_real_, n_rows),
    Z_trend = rep(NA_real_, n_rows),
    p_trend = rep(NA_real_, n_rows),
    log_pv = rep(NA_real_, n_rows),
    wald_pv = rep(NA_real_, n_rows),
    score_pv = rep(NA_real_, n_rows),
    var_ph = rep(NA_character_, n_rows),
    global_ph = rep(NA_real_, n_rows),
    HRCI = rep(NA_character_, n_rows),
    P = rep(NA_character_, n_rows),
    Ptrend = rep(NA_character_, n_rows),
    stringsAsFactors = FALSE
  )
}


# --- 内部函数：生成整个失败status的NA结果 ---

.batchCOX02_na_status <- function(data, fixed_vars = NULL, adjust_lists = NULL) {

  if (!is.null(fixed_vars) && !is.character(fixed_vars)) {
    fixed_vars <- as.character(fixed_vars)
  }

  if (!is.null(adjust_lists) && !is.list(adjust_lists)) {
    adjust_lists <- list(adjust_lists)
  }

  variable_combos <- list()
  combo_id <- 1L

  for (fixed in fixed_vars) {

    if (is.null(adjust_lists)) {

      variable_combos[[combo_id]] <- fixed
      combo_id <- combo_id + 1L

    } else {

      for (adjust_group in adjust_lists) {
        variable_combos[[combo_id]] <- c(fixed, adjust_group)
        combo_id <- combo_id + 1L
      }
    }
  }

  results <- vector("list", length(variable_combos))

  for (i in seq_along(variable_combos)) {
    results[[i]] <- .batchCOX02_na_model(
      data = data,
      vars = variable_combos[[i]],
      model_id = i
    )
  }

  final_result <- do.call(rbind, results)
  rownames(final_result) <- NULL

  final_result
}


#' 批量 Cox 比例风险回归分析
#'
#' 针对多个暴露变量和多个协变量调整组合批量拟合 Cox
#' 比例风险模型，同时可选择计算风险比、置信区间、趋势检验和
#' 比例风险假定检验结果。
#'
#' @param data 用于分析的数据框。
#' @param time_var 随访时间变量名，字符型标量。
#' @param status_var 结局状态变量名，字符型标量。
#' @param fixed_vars 需要依次分析的主要暴露变量名。
#' @param adjust_lists 协变量调整组合构成的列表。
#' @param output_file 可选的 Excel 输出路径；为 `NULL` 时不导出。
#' @param ph_digits 比例风险假定检验 P 值保留的小数位数。
#' @param p_trend 是否计算 P for trend，默认为 `TRUE`。若为 `FALSE`，
#'   则不拟合趋势检验模型，但结果中仍保留趋势检验相关列，其值为 `NA`。
#' @param ph_test 是否进行比例风险假定检验，默认为 `TRUE`。若为 `FALSE`，
#'   则不运行 `cox.zph()`，但结果中仍保留 `var_ph` 和 `global_ph` 列，
#'   其值为 `NA`。
#'
#' @return 一个数据框，每行为一个模型系数，包含 beta、HR、
#'   95% CI、P 值、趋势检验和比例风险假定检验结果。
#'
#' @details
#' 对因子型暴露进行趋势检验时，函数按照该变量当前的
#' `levels()` 顺序赋值为 1、2、3……。因此，应确保因子水平
#' 顺序具有明确的等级含义。
#'
#' 若某个模型发生错误，该模型对应结果仍会保留，前 5 列
#' `model_id`、`var_number`、`fixed_var`、`adjust_vars` 和 `level`
#' 正常保留，第 6 列及之后全部返回 `NA`，并继续运行后续模型。
#' 模型级错误信息同时保存在返回数据框的 `model_errors` 属性中，
#' 供 [ParaCOX()] 汇总生成 `error.log`。
#'
#' @family Cox regression
#' @export
batchCOX02 <- function(data, time_var, status_var,
                       fixed_vars = NULL, adjust_lists = NULL,
                       output_file = NULL, ph_digits = 3,
                       p_trend = TRUE, ph_test = TRUE) {

  # 立即显示警告（等同于 warning() 调用）
  old_warn <- getOption("warn")

  on.exit(
    options(warn = old_warn),
    add = TRUE
  )

  options(warn = 1)

  # 初始化结果存储
  results <- list()
  model_errors <- data.frame(
    status_var = character(),
    model_id = integer(),
    error = character(),
    stringsAsFactors = FALSE
  )

  model_warnings <- data.frame(
    status_var = character(),
    model_id = integer(),
    warning = character(),
    stringsAsFactors = FALSE
  )

  # 检查趋势检验和PH检验开关
  if (!is.logical(p_trend) || length(p_trend) != 1L || is.na(p_trend)) {
    stop("`p_trend`必须为TRUE或FALSE。")
  }

  if (!is.logical(ph_test) || length(ph_test) != 1L || is.na(ph_test)) {
    stop("`ph_test`必须为TRUE或FALSE。")
  }

  # 确保fixed_vars是字符向量，adjust_lists是list
  if (!is.null(fixed_vars) && !is.character(fixed_vars)) {
    fixed_vars <- as.character(fixed_vars)
  }

  if (!is.null(adjust_lists) && !is.list(adjust_lists)) {
    adjust_lists <- list(adjust_lists)
  }


  # 生成所有变量组合：每个固定变量 + 每种调整组合
  variable_combos <- list()
  combo_id <- 1L

  for (fixed in fixed_vars) {

    if (is.null(adjust_lists)) {

      # 只有固定变量，无调整变量
      variable_combos[[combo_id]] <- fixed
      combo_id <- combo_id + 1L

    } else {

      # 固定变量 + 每种调整组合
      for (adjust_group in adjust_lists) {
        variable_combos[[combo_id]] <- c(fixed, adjust_group)
        combo_id <- combo_id + 1L
      }
    }
  }


  # 遍历所有变量组合
  for (i in seq_along(variable_combos)) {

    vars <- variable_combos[[i]]
    model_name <- paste0(vars, collapse = "+")


    # 实时进度提示
    message(sprintf(
      "\n[进度] 状态变量: %s | 固定变量: %s | 模型 %d/%d | \n      ~调整变量: %s",
      status_var,
      vars[1],
      i,
      length(variable_combos),
      ifelse(length(vars) > 1, paste0(vars[-1], collapse = "+"), "无")
    ))


    # 单个模型发生错误时，仅将该模型结果置为NA并继续后续模型
    # warning仍正常写入core日志，同时额外收集用于error.log汇总
    warning_messages <- character()

    res <- withCallingHandlers(
      tryCatch({

      # 构建公式
      formula <- stats::as.formula(
        paste(
          "survival::Surv(", time_var, ", ", status_var, ") ~",
          paste(vars, collapse = "+")
        )
      )

      # 拟合主Cox模型
      fit <- survival::coxph(
        formula,
        data = data,
        x = ph_test
      )

      sum_fit <- summary(fit)


      # PH假定检验
      var_ph_pvalue <- NA_character_
      global_ph_pvalue <- NA_real_

      if (ph_test) {

        ph_result <- survival::cox.zph(fit)

        # 提取全局PH检验P值
        global_ph_pvalue <- round(
          ph_result$table["GLOBAL", "p"],
          ph_digits
        )

        # 提取每个变量的PH检验P值（处理多水平变量）
        var_ph_pvalue <- if (length(vars) == 1) {

          as.character(
            round(ph_result$table[1, "p"], ph_digits)
          )

        } else {

          ph_p_values <- ph_result$table[
            rownames(ph_result$table) != "GLOBAL",
            "p"
          ]

          paste0(
            round(ph_p_values, ph_digits),
            collapse = "; "
          )
        }
      }


      # 对每个固定变量自动做趋势检验
      p_trend_value <- NA_real_
      beta_t <- NA_real_
      se_t <- NA_real_
      Z_t <- NA_real_

      if (p_trend) {

        current_var <- data[[vars[1]]]

        message(sprintf(
          "      1.正在进行趋势检验: %s (类型: %s)",
          vars[1],
          class(current_var)[1]
        ))

        # 根据变量类型构建趋势检验公式
        trend_formula <- if (is.numeric(current_var)) {

          # 连续变量直接使用
          stats::as.formula(
            paste(
              "survival::Surv(", time_var, ", ", status_var, ") ~",
              vars[1],
              if (length(vars) > 1) {
                paste("+", paste(vars[-1], collapse = "+"))
              } else {
                ""
              }
            )
          )

        } else if (is.ordered(current_var)) {

          # 有序因子转换为数值
          stats::as.formula(
            paste(
              "survival::Surv(", time_var, ", ", status_var,
              ") ~ as.numeric(", vars[1], ")",
              if (length(vars) > 1) {
                paste("+", paste(vars[-1], collapse = "+"))
              } else {
                ""
              }
            )
          )

        } else if (is.factor(current_var)) {

          # 无序因子按现有levels顺序临时转换为等级数值
          # 仅用于趋势检验，不改变原始Cox模型中暴露变量的无序因子属性
          ordered_value <- as.numeric(
            factor(
              current_var,
              levels = levels(current_var),
              ordered = TRUE
            )
          )

          message(
            paste0(
              "      2.",
              vars[1],
              "为无序因子，临时转有序：",
              paste0(levels(current_var), collapse = " -> ")
            )
          )

          stats::as.formula(
            paste(
              "survival::Surv(", time_var, ", ", status_var,
              ") ~ ordered_value",
              if (length(vars) > 1) {
                paste("+", paste(vars[-1], collapse = "+"))
              } else {
                ""
              }
            )
          )

        } else {

          stop(
            "Trend test requires numeric/factor variable. ",
            vars[1],
            " is of class: ",
            class(current_var)[1]
          )
        }

        # 执行趋势检验
        trend_fit <- survival::coxph(
          trend_formula,
          data = data
        )

        # 提取趋势检验的系数矩阵
        trend_coef <- summary(trend_fit)$coefficients

        # 提取第一行（即暴露变量作为连续/等级变量时的统计量）
        beta_t <- trend_coef[1, "coef"]
        se_t <- trend_coef[1, "se(coef)"]
        Z_t <- trend_coef[1, "z"]
        p_trend_value <- trend_coef[1, "Pr(>|z|)"]
      }


      # 提取结果
      res_model <- data.frame(
        model_id = i,
        var_number = length(vars),
        fixed_var = vars[1],
        adjust_vars = if (length(vars) > 1) {
          paste0(vars[-1], collapse = ", ")
        } else {
          NA_character_
        },
        level = rownames(sum_fit$coefficients),
        beta = sum_fit$coefficients[, "coef"],
        se = sum_fit$coefficients[, "se(coef)"],
        Z = sum_fit$coefficients[, "z"],
        HR = sum_fit$coefficients[, "exp(coef)"],
        LCI = sum_fit$conf.int[, "lower .95"],
        UCI = sum_fit$conf.int[, "upper .95"],
        p_value = sum_fit$coefficients[, "Pr(>|z|)"],
        beta_trend = beta_t,
        se_trend = se_t,
        Z_trend = Z_t,
        p_trend = p_trend_value,
        log_pv = sum_fit$logtest["pvalue"],
        wald_pv = sum_fit$waldtest["pvalue"],
        score_pv = sum_fit$sctest["pvalue"],
        var_ph = var_ph_pvalue,
        global_ph = global_ph_pvalue,
        stringsAsFactors = FALSE
      )

      # 添加格式化列
      res_model$HRCI <- sprintf(
        "%.2f (%.2f, %.2f)",
        res_model$HR,
        res_model$LCI,
        res_model$UCI
      )

      res_model$P <- ifelse(
        res_model$p_value < 0.001,
        "<0.001",
        sprintf("%.3f", res_model$p_value)
      )

      res_model$Ptrend <- ifelse(
        is.na(res_model$p_trend),
        NA_character_,
        ifelse(
          res_model$p_trend < 0.001,
          "<0.001",
          sprintf("%.3f", res_model$p_trend)
        )
      )

      res_model

    }, error = function(e) {

      model_errors <<- rbind(
        model_errors,
        data.frame(
          status_var = status_var,
          model_id = i,
          error = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      )

      message(sprintf(
        "[模型失败] 状态变量: %s | 模型 %d/%d | 固定变量: %s | 调整变量: %s | 错误: %s",
        status_var,
        i,
        length(variable_combos),
        vars[1],
        ifelse(length(vars) > 1, paste0(vars[-1], collapse = "+"), "无"),
        conditionMessage(e)
      ))

      .batchCOX02_na_model(
        data = data,
        vars = vars,
        model_id = i
      )
    }),
      warning = function(w) {
        warning_messages <<- c(
          warning_messages,
          conditionMessage(w)
        )
      }
    )

    # 汇总该模型的warning；不改变原warning输出和模型结果
    if (length(warning_messages) > 0L) {

      warning_messages <- unique(warning_messages)

      model_warnings <- rbind(
        model_warnings,
        data.frame(
          status_var = rep(status_var, length(warning_messages)),
          model_id = rep(i, length(warning_messages)),
          warning = warning_messages,
          stringsAsFactors = FALSE
        )
      )
    }

    results[[model_name]] <- res
  }


  # 合并结果
  final_result <- do.call(rbind, results)
  rownames(final_result) <- NULL

  # 保存模型级错误和warning信息，供ParaCOX汇总error.log
  attr(final_result, "model_errors") <- model_errors
  attr(final_result, "model_warnings") <- model_warnings


  # 可选：导出到Excel
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(final_result, output_file)
  }

  return(final_result)
}
