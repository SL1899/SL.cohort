

#' 批量 Cox 比例风险回归分析
#'
#' 针对多个暴露变量和多个协变量调整组合批量拟合 Cox
#' 比例风险模型，同时计算风险比、置信区间、趋势检验和
#' 比例风险假定检验结果。
#'
#' @param data 用于分析的数据框。
#' @param time_var 随访时间变量名，字符型标量。
#' @param status_var 结局状态变量名，字符型标量。
#' @param fixed_vars 需要依次分析的主要暴露变量名。
#' @param adjust_lists 协变量调整组合构成的列表。
#' @param output_file 可选的 Excel 输出路径；为 `NULL` 时不导出。
#' @param ph_digits 比例风险假定检验 P 值保留的小数位数。
#'
#' @return 一个数据框，每行为一个模型系数，包含 beta、HR、
#'   95% CI、P 值、趋势检验和比例风险假定检验结果。
#'
#' @details
#' 对因子型暴露进行趋势检验时，函数按照该变量当前的
#' `levels()` 顺序赋值为 1、2、3……。因此，应确保因子水平
#' 顺序具有明确的等级含义。
#'
#' @family Cox regression
#' @export



batchCOX02 <- function(data, time_var, status_var,
                       fixed_vars = NULL, adjust_lists = NULL,
                       output_file = NULL, ph_digits = 3) {

  # 立即显示警告（等同于 warning() 调用）
  old_warn <- getOption("warn")

  on.exit(
    options(warn = old_warn),
    add = TRUE
  )

  options(warn = 1)

  # 初始化结果存储
  results <- list()

  # 确保fixed_vars是字符向量，adjust_lists是list
  if (!is.null(fixed_vars) && !is.character(fixed_vars)) {
    fixed_vars <- as.character(fixed_vars)
  }
  if (!is.null(adjust_lists) && !is.list(adjust_lists)) {
    adjust_lists <- list(adjust_lists)
  }


  # # 对fix_vars中的无序因子预处理
  # for (var in fixed_vars) {
  #     if (is.factor(data[[var]]) && !is.ordered(data[[var]])) {
  #         new_var_name <- paste0(var, "_ordered")
  #         if (new_var_name %in% names(data)) {
  #             message("变量 ", new_var_name, " 已存在data里，将直接用该变量做趋势检验")
  #         } else {
  #             data[[new_var_name]] <- as.numeric(factor(data[[var]], levels = levels(data[[var]]), ordered = TRUE))
  #             message(sprintf("预处理变量: %s → %s (水平顺序: %s)",
  #                             var, new_var_name,
  #                             paste0(levels(data[[var]]), collapse = " < ")))
  #         }
  #     }
  # }


  # 生成所有变量组合：每个固定变量 + 每种调整组合
  variable_combos <- list()
  combo_id <- 1

  for (fixed in fixed_vars) {
    if (is.null(adjust_lists)) {
      # 只有固定变量，无调整变量
      variable_combos[[combo_id]] <- fixed
      combo_id <- combo_id + 1
    } else {
      # 固定变量 + 每种调整组合
      for (adjust_group in adjust_lists) {
        variable_combos[[combo_id]] <- c(fixed, adjust_group)
        combo_id <- combo_id + 1
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


    # 构建公式
    formula <- stats::as.formula(
      paste("survival::Surv(", time_var, ", ", status_var, ") ~",
            paste(vars, collapse = "+"))
    )

    # 拟合模型
    fit <- survival::coxph(
      formula,
      data = data,
      x = TRUE
    )
    sum_fit <- summary(fit)


    # PH假定检验
    ph_test <- survival::cox.zph(fit)
    # 提取全局的PH检验P值
    global_ph_pvalue <- round(ph_test$table["GLOBAL", "p"], ph_digits)
    # 提取每个变量的PH检验P值（处理多水平变量）
    var_ph_pvalue <- if (length(vars) == 1) {
      # 单变量模型：直接提取p值
      round(ph_test$table[1, "p"], ph_digits)  # 第一行是变量（非GLOBAL）
    } else {
      # 多变量模型：提取所有变量的p值（可能包含多水平）
      ph_p_values <- ph_test$table[rownames(ph_test$table) != "GLOBAL", "p"]
      paste0(round(ph_p_values, ph_digits), collapse = "; ")  # 用分号分隔多个p值
    }


    # 对每个固定变量自动做趋势检验
    p_trend <- NA
    beta_t  <- NA  # 新增：初始化趋势检验的beta
    se_t    <- NA  # 新增：初始化趋势检验的SE
    Z_t     <- NA  # 新增：初始化趋势检验的Z值

    current_var <- data[[vars[1]]]

    message(sprintf("      1.正在进行趋势检验: %s (类型: %s)",
                    vars[1], class(current_var)))

    # 根据变量类型构建趋势检验公式
    trend_formula <- if (is.numeric(current_var)) {
      # 连续变量直接使用
      stats::as.formula(
        paste("survival::Surv(", time_var, ", ", status_var, ") ~", vars[1],
              if (length(vars) > 1) paste("+", paste(vars[-1], collapse = "+")) else "")
      )
    } else if (is.ordered(current_var)) {
      # 有序因子转换为数值
      stats::as.formula(
        paste("survival::Surv(", time_var, ", ", status_var, ") ~ as.numeric(", vars[1], ")",
              if (length(vars) > 1) paste("+", paste(vars[-1], collapse = "+")) else "")
      )
    } else if (is.factor(current_var)) {

      # 无序因子转为有序（按现有levels顺序）

      # 之前采用ordered_value临时变量，确保只有趋势检验里面的暴露是有序的，原始COX模型仍是无序
      ordered_value <- as.numeric(factor(current_var, levels=levels(current_var), ordered=TRUE))
      message(paste0('      2.', vars[1], "为无序因子，临时转有序：",
                     paste0(levels(current_var), collapse = " -> ")))
      stats::as.formula(
        paste("survival::Surv(", time_var, ", ", status_var, ") ~",
              "ordered_value",  # 直接使用转换后的数值
              if (length(vars) > 1) paste("+", paste(vars[-1], collapse = "+")) else "")
      )

      # # 现在直接在循环前检验关键变量的类型，如果是无序因子就已生成一个新的有序因子变量，直接采用
      # message(paste0('      2.', vars[1], "为无序因子，趋势检验采用：", paste0(vars[1], "_ordered")))
      # stats::as.formula(
      #     paste("Surv(", time_var, ", ", status_var, ") ~",
      #           paste0(vars[1], "_ordered"),  # 使用转换后的有序变量
      #           if (length(vars) > 1) paste("+", paste(vars[-1], collapse = "+")) else "")
      # )

    } else {
      stop(paste("Trend test requires numeric/factor variable.", vars[1],
                 "is of class:", class(current_var)))
    }

    # 统一执行趋势检验（所有类型变量共用）
    if (!is.null(trend_formula)) {
      trend_fit <- survival::coxph(
        trend_formula,
        data = data,
        x = TRUE
      )
      # 提取趋势检验的系数矩阵
      trend_coef <- summary(trend_fit)$coefficients
      # 提取第一行（即暴露变量作为连续/等级变量时的统计量）
      beta_t  <- trend_coef[1, "coef"]
      se_t    <- trend_coef[1, "se(coef)"]
      Z_t     <- trend_coef[1, "z"]
      p_trend <- trend_coef[1, "Pr(>|z|)"]
    }


    # 提取结果
    res <- data.frame(
      model_id = i,
      var_number = length(vars),
      fixed_var = vars[1],  # 直接取第一个元素（已知是固定变量）
      adjust_vars = if (length(vars) > 1) paste0(vars[-1], collapse = ", ") else NA,
      level = rownames(sum_fit$coefficients),
      beta = sum_fit$coefficients[, "coef"],
      se = sum_fit$coefficients[, "se(coef)"],
      Z = sum_fit$coefficients[, "z"],
      HR = sum_fit$coefficients[, "exp(coef)"],
      LCI = sum_fit$conf.int[, "lower .95"],
      UCI = sum_fit$conf.int[, "upper .95"],
      p_value = sum_fit$coefficients[, "Pr(>|z|)"],
      # --- 以下是新增/修改的趋势检验相关列 ---
      beta_trend = beta_t,    # 趋势检验的回归系数
      se_trend   = se_t,      # 趋势检验的标准误
      Z_trend    = Z_t,       # 趋势检验的Z值
      p_trend    = p_trend,   # 趋势检验的P值
      # ---------------------------------------
      log_pv = sum_fit$logtest["pvalue"],
      wald_pv = sum_fit$waldtest["pvalue"],
      score_pv = sum_fit$sctest["pvalue"],
      var_ph = var_ph_pvalue,
      global_ph = global_ph_pvalue,
      stringsAsFactors = FALSE
    )
    # 添加格式化列（使用res中的列）
    res$HRCI <- sprintf("%.2f (%.2f, %.2f)", res$HR, res$LCI, res$UCI)
    res$P <- ifelse(res$p_value < 0.001, "<0.001", sprintf("%.3f", res$p_value))
    res$Ptrend <- ifelse(res$p_trend < 0.001, "<0.001", sprintf("%.3f", res$p_trend))

    results[[model_name]] <- res
  }

  # 合并结果
  final_result <- do.call(rbind, results)
  rownames(final_result) <- NULL

  # 可选：导出到Excel
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(final_result, output_file)
  }

  return(final_result)
}
