

#' 并行执行多个结局的批量 Cox 回归
#'
#' 针对多个结局状态变量调用 [batchCOX02()]，可使用
#' PSOCK 集群并行完成计算，并将不同结局的结果输出到
#' Excel 的不同工作表。
#'
#' @inheritParams batchCOX02
#' @param status_vars 一个或多个结局状态变量名。
#' @param pararun 是否使用并行计算。
#' @param cores 并行工作进程数量；为 `NULL` 时自动计算。
#' @param log_file 日志文件路径。
#'
#' @return 命名列表。每个元素对应一个结局状态变量的
#'   Cox 回归结果数据框。
#'
#' @family Cox regression
#' @importFrom foreach %dopar%
#' @export



ParaCOX <- function(data, time_var, status_vars,  # 改为复数，status_vars才是需要的并行数量
                    fixed_vars = NULL, adjust_lists = NULL,
                    output_file = NULL, ph_digits = 3,
                    pararun = TRUE, cores = NULL,
                    log_file = "ParaCOX.log") {

  # 在设置新的重定向之前，先清理可能存在的旧重定向
  while(sink.number(type = "message") > 2) {
    sink(type = "message")
  }

  # 创建日志文件
  log_con <- file(log_file, open = "wt")
  sink(log_con, type = "message")  # 重定向message到日志
  on.exit({
    # 确保完全解除所有重定向
    while(sink.number(type = "message") > 2) {
      sink(type = "message")
    }                       # 恢复message输出
    close(log_con)          # 关闭日志文件
    message("日志保存到: ", log_file)
  },
  add = TRUE
  )

  # 记录开始时间
  start_time <- Sys.time()
  message("分析开始时间: ", start_time)
  message("分析状态变量: ", paste(status_vars, collapse = ", "))

  # 设置并行计算
  if (pararun) {
    # 确定使用的核心数
    if (is.null(cores)) {
      available_cores <- parallel::detectCores(logical = FALSE)

      if (is.na(available_cores)) {
        available_cores <- 1L
      }

      cores <- max(1L, available_cores - 6L)  # 保留6个核心给系统
    }

    # 转换整数型
    cores <- as.integer(cores)

    # 因为可能是负整数
    if (cores < 1L) {
      stop("`cores`必须为大于或等于1的整数。")
    }

    # 没有必要让核心数超过结局数量
    cores <- min(cores, length(status_vars))

    # 创建并注册集群
    paraclus <- parallel::makeCluster(cores, outfile = log_file)  # 重定向输出
    doParallel::registerDoParallel(paraclus)

    # 确保结束时关闭集群
    on.exit({
      parallel::stopCluster(paraclus)
      foreach::registerDoSEQ()
      message("已关闭并行集群")
    },
    add = TRUE,
    after = FALSE
    )

    message("使用 ", cores, " 个核心进行并行计算")

  } else {
    message("使用串行计算")
    foreach::registerDoSEQ()  # 注册串行后端
  }


  # 确保status_vars是字符向量
  if (!is.character(status_vars)) {
    status_vars <- as.character(status_vars)
  }

  # 对每个status_var单独分析（并行）
  all_results <- foreach::foreach(
                        status_var = status_vars,
                         .export = "batchCOX02",  # 关键：导出自定义函数
                         .verbose = TRUE,         # 显示详细进度
                         .combine = c,
                         .inorder = TRUE
                        ) %dopar% {

                           # 在工作进程中记录
                           message("\n",
                                   strrep("=", 20), '|', "开始分析status: ", status_var, '|', strrep("=", 20),
                                   "\n")

                           # 调用原函数（batchCOX02）处理当前status_var
                           result <- batchCOX02(
                             data = data,
                             time_var = time_var,
                             status_var = status_var,  # 每次传入单个status_var
                             fixed_vars = fixed_vars,
                             adjust_lists = adjust_lists,
                             output_file = NULL,  # 不单独导出，最后统一导出
                             ph_digits = ph_digits
                           )

                           # 返回结果（使用列表命名）
                           stats::setNames(list(result), status_var)
                         }

  # 可选：导出到Excel（每个status_var一个sheet）
  if (!is.null(output_file)) {
    wb <- openxlsx::createWorkbook()

    for (status_var in names(all_results)) {
      openxlsx::addWorksheet(wb, sheetName = status_var)
      openxlsx::writeData(
        wb,
        sheet = status_var,
        x = all_results[[status_var]]
      )
    }

    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    message("已导出结果到: ", output_file)
  }

  # 记录结束时间
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  message("分析完成时间: ", end_time)
  message("总耗时: ", round(duration, 1), " 分钟")

  return(all_results)  # 返回列表，每个元素对应一个status_var的结果
}
