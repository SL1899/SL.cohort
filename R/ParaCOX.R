# ------------ 多结局并行Cox回归 ------------


# --- 内部函数：清理日志文件名 ---

.ParaCOX_safe_name <- function(x) {

  x <- paste(x, collapse = "_")
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)

  if (!nzchar(x)) {
    x <- "none"
  }

  x
}


# --- 内部函数：单个status分析并写入所属core日志 ---

.ParaCOX_run_status <- function(
  data, time_var, status_var,
  fixed_vars, adjust_lists,
  ph_digits, p_trend, ph_test,
  core_id, core_log
) {

  # 将该worker当前status的输出写入所属core日志
  log_con <- file(core_log, open = "at")
  output_sink_before <- sink.number(type = "output")
  message_sink_before <- sink.number(type = "message")

  sink(log_con, type = "output")
  sink(log_con, type = "message")

  on.exit(
    {
      while (sink.number(type = "message") > message_sink_before) {
        sink(type = "message")
      }

      while (sink.number(type = "output") > output_sink_before) {
        sink(type = "output")
      }

      close(log_con)
    },
    add = TRUE
  )

  status_start <- Sys.time()

  message("\n", strrep("=", 70))
  message(sprintf(
    "Core %d | 开始分析status: %s | 时间: %s",
    core_id,
    status_var,
    status_start
  ))
  message(strrep("=", 70), "\n")

  status_error <- NA_character_

  # 整个status发生错误时，返回完整NA结果框架，不中断其他status
  result <- tryCatch(
    {
      batchCOX02(
        data = data,
        time_var = time_var,
        status_var = status_var,
        fixed_vars = fixed_vars,
        adjust_lists = adjust_lists,
        output_file = NULL,
        ph_digits = ph_digits,
        p_trend = p_trend,
        ph_test = ph_test
      )
    },
    error = function(e) {

      status_error <<- conditionMessage(e)

      message(sprintf(
        "[结局失败] 状态变量: %s | 错误: %s",
        status_var,
        status_error
      ))

      .batchCOX02_na_status(
        data = data,
        fixed_vars = fixed_vars,
        adjust_lists = adjust_lists
      )
    }
  )

  model_errors <- attr(result, "model_errors")

  if (is.null(model_errors)) {
    model_errors <- data.frame(
      status_var = character(),
      model_id = integer(),
      error = character(),
      stringsAsFactors = FALSE
    )
  }

  model_warnings <- attr(result, "model_warnings")

  if (is.null(model_warnings)) {
    model_warnings <- data.frame(
      status_var = character(),
      model_id = integer(),
      warning = character(),
      stringsAsFactors = FALSE
    )
  }

  status_end <- Sys.time()
  duration <- difftime(status_end, status_start, units = "mins")

  message(sprintf(
    "Core %d | status完成: %s | 耗时: %.1f 分钟",
    core_id,
    status_var,
    as.numeric(duration)
  ))

  list(
    result = result,
    model_errors = model_errors,
    model_warnings = model_warnings,
    status_error = status_error,
    core_id = core_id
  )
}


# --- 内部函数：写入汇总error.log ---

.ParaCOX_write_error_log <- function(
  error_file,
  status_errors,
  model_errors,
  model_warnings
) {

  lines <- c(
    "ParaCOX Error Summary",
    paste0("Generated: ", Sys.time()),
    ""
  )

  lines <- c(lines, "[Status-level errors]")

  if (nrow(status_errors) == 0L) {
    lines <- c(lines, "None")
  } else {
    for (i in seq_len(nrow(status_errors))) {
      lines <- c(
        lines,
        sprintf(
          "%s | %s",
          status_errors$status_var[i],
          status_errors$error[i]
        )
      )
    }
  }

  lines <- c(lines, "", "[Model-level errors]")

  if (nrow(model_errors) == 0L) {
    lines <- c(lines, "None")
  } else {
    for (i in seq_len(nrow(model_errors))) {
      lines <- c(
        lines,
        sprintf(
          "%s | model_id=%d | %s",
          model_errors$status_var[i],
          model_errors$model_id[i],
          model_errors$error[i]
        )
      )
    }
  }

  lines <- c(lines, "", "[Model-level warnings]")

  if (nrow(model_warnings) == 0L) {
    lines <- c(lines, "None")
  } else {
    for (i in seq_len(nrow(model_warnings))) {
      lines <- c(
        lines,
        sprintf(
          "%s | model_id=%d | %s",
          model_warnings$status_var[i],
          model_warnings$model_id[i],
          model_warnings$warning[i]
        )
      )
    }
  }

  writeLines(lines, con = error_file, useBytes = TRUE)
}


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
#' @param log_folder 日志文件夹路径。参数值即为实际创建和使用的
#'   文件夹名称或路径。例如 `log_folder = "cox_cause_allvars_log"`
#'   将创建并使用 `cox_cause_allvars_log` 文件夹。
#'
#' @return 命名列表。每个元素对应一个结局状态变量的
#'   Cox 回归结果数据框。
#'
#' @details
#' 每个并行worker使用独立日志文件，避免多个worker输出交错。
#' 分析结束后，日志文件名会包含该worker实际处理的status变量名。
#' 例如 `core_1_death_MABD.log` 表示core 1依次处理了
#' `death` 和 `MABD`。
#'
#' 若某个模型发生错误，该模型前 5 列正常保留，第 6 列及之后
#' 全部为 `NA`；若某个status整体失败，则该status所有模型的
#' 第 6 列及之后全部为 `NA`。所有错误统一汇总到 `error.log`。
#'
#' @family Cox regression
#' @importFrom foreach %dopar%
#' @export
ParaCOX <- function(data, time_var, status_vars,
                    fixed_vars = NULL, adjust_lists = NULL,
                    output_file = NULL, ph_digits = 3,
                    p_trend = TRUE, ph_test = TRUE,
                    pararun = TRUE, cores = NULL,
                    log_folder = "ParaCOX_log") {

  # 确保status_vars是字符向量
  if (!is.character(status_vars)) {
    status_vars <- as.character(status_vars)
  }

  if (length(status_vars) == 0L) {
    stop("`status_vars`不能为空。")
  }


  # ------ 确定日志文件夹 ------

  if (!is.character(log_folder) ||
      length(log_folder) != 1L ||
      is.na(log_folder) ||
      !nzchar(log_folder)) {
    stop("`log_folder`必须为非空字符型路径。")
  }

  if (!dir.exists(log_folder)) {
    dir.create(
      log_folder,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  log_folder <- normalizePath(
    log_folder,
    winslash = "/",
    mustWork = TRUE
  )

  # 清理该日志文件夹内上一次运行留下的log文件
  old_logs <- list.files(
    log_folder,
    pattern = "\\.log$",
    full.names = TRUE
  )

  if (length(old_logs) > 0L) {
    unlink(old_logs)
  }


  # ------ 记录开始时间 ------

  start_time <- Sys.time()
  message("分析开始时间: ", start_time)
  message("分析状态变量: ", paste(status_vars, collapse = ", "))


  # ------ 设置并行计算 ------

  paraclus <- NULL

  if (pararun) {

    if (is.null(cores)) {

      available_cores <- parallel::detectCores(logical = FALSE)

      if (is.na(available_cores)) {
        available_cores <- 1L
      }

      cores <- max(1L, available_cores - 6L)
    }

    cores <- as.integer(cores)

    if (cores < 1L) {
      stop("`cores`必须为大于或等于1的整数。")
    }

    # 核心数不超过结局数量
    cores <- min(cores, length(status_vars))

  } else {

    cores <- 1L
  }


  # 每个core先建立一个临时独立日志文件
  core_log_files <- file.path(
    log_folder,
    sprintf("core_%d.log", seq_len(cores))
  )

  for (core_log in core_log_files) {
    cat("", file = core_log)
  }


  if (pararun) {

    # worker自身标准输出不再写入同一个公共文件
    null_output <- if (.Platform$OS.type == "windows") {
      "nul:"
    } else {
      "/dev/null"
    }

    paraclus <- parallel::makeCluster(
      cores,
      outfile = null_output
    )

    # 建立worker PID与core编号的对应关系
    worker_pids <- unlist(
      parallel::clusterCall(
        paraclus,
        Sys.getpid
      )
    )

    core_map <- stats::setNames(
      seq_along(worker_pids),
      as.character(worker_pids)
    )

    doParallel::registerDoParallel(paraclus)

    on.exit(
      {
        if (!is.null(paraclus)) {
          parallel::stopCluster(paraclus)
          foreach::registerDoSEQ()
        }
      },
      add = TRUE
    )

    message("使用 ", cores, " 个核心进行并行计算")

  } else {

    core_map <- stats::setNames(
      1L,
      as.character(Sys.getpid())
    )

    foreach::registerDoSEQ()
    message("使用串行计算")
  }


  # ------ 对每个status_var进行分析 ------

  raw_results <- foreach::foreach(
    status_var = status_vars,
    .export = c(
      "batchCOX02",
      ".batchCOX02_level_names",
      ".batchCOX02_na_model",
      ".batchCOX02_na_status",
      ".ParaCOX_run_status",
      "core_map",
      "core_log_files"
    ),
    .verbose = FALSE,
    .combine = c,
    .inorder = TRUE
  ) %dopar% {

    worker_pid <- as.character(Sys.getpid())
    core_id <- unname(core_map[worker_pid])

    if (length(core_id) == 0L || is.na(core_id)) {
      core_id <- 1L
    }

    task_result <- .ParaCOX_run_status(
      data = data,
      time_var = time_var,
      status_var = status_var,
      fixed_vars = fixed_vars,
      adjust_lists = adjust_lists,
      ph_digits = ph_digits,
      p_trend = p_trend,
      ph_test = ph_test,
      core_id = core_id,
      core_log = core_log_files[core_id]
    )

    stats::setNames(
      list(task_result),
      status_var
    )
  }


  # 并行任务全部结束后立即关闭集群，确保日志文件句柄释放
  if (pararun && !is.null(paraclus)) {
    parallel::stopCluster(paraclus)
    foreach::registerDoSEQ()
    paraclus <- NULL
  }


  # ------ 整理分析结果 ------

  all_results <- lapply(
    raw_results,
    function(x) x$result
  )

  names(all_results) <- names(raw_results)


  # ------ 汇总status级和模型级错误 ------

  status_errors <- do.call(
    rbind,
    lapply(names(raw_results), function(status_var) {

      error_message <- raw_results[[status_var]]$status_error

      if (length(error_message) == 1L && !is.na(error_message)) {
        data.frame(
          status_var = status_var,
          error = error_message,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })
  )

  if (is.null(status_errors)) {
    status_errors <- data.frame(
      status_var = character(),
      error = character(),
      stringsAsFactors = FALSE
    )
  }

  model_errors <- do.call(
    rbind,
    lapply(raw_results, function(x) {
      if (nrow(x$model_errors) > 0L) {
        x$model_errors
      } else {
        NULL
      }
    })
  )

  if (is.null(model_errors)) {
    model_errors <- data.frame(
      status_var = character(),
      model_id = integer(),
      error = character(),
      stringsAsFactors = FALSE
    )
  }

  model_warnings <- do.call(
    rbind,
    lapply(raw_results, function(x) {
      if (nrow(x$model_warnings) > 0L) {
        x$model_warnings
      } else {
        NULL
      }
    })
  )

  if (is.null(model_warnings)) {
    model_warnings <- data.frame(
      status_var = character(),
      model_id = integer(),
      warning = character(),
      stringsAsFactors = FALSE
    )
  }


  # ------ 重命名每个core日志，加入实际处理的status名称 ------

  for (core_id in seq_len(cores)) {

    core_status <- names(raw_results)[
      vapply(
        raw_results,
        function(x) identical(as.integer(x$core_id), as.integer(core_id)),
        logical(1)
      )
    ]

    status_suffix <- .ParaCOX_safe_name(core_status)

    final_core_log <- file.path(
      log_folder,
      sprintf(
        "core_%d_%s.log",
        core_id,
        status_suffix
      )
    )

    if (file.exists(final_core_log)) {
      unlink(final_core_log)
    }

    renamed <- file.rename(
      core_log_files[core_id],
      final_core_log
    )

    # Windows下若重命名失败，则复制后删除临时文件
    if (!renamed) {
      file.copy(
        core_log_files[core_id],
        final_core_log,
        overwrite = TRUE
      )
      unlink(core_log_files[core_id])
    }
  }


  # ------ 生成汇总error.log ------

  error_file <- file.path(
    log_folder,
    "error.log"
  )

  .ParaCOX_write_error_log(
    error_file = error_file,
    status_errors = status_errors,
    model_errors = model_errors,
    model_warnings = model_warnings
  )


  # ------ 可选：导出到Excel，每个status_var一个sheet ------

  if (!is.null(output_file)) {

    wb <- openxlsx::createWorkbook()

    for (status_var in names(all_results)) {

      openxlsx::addWorksheet(
        wb,
        sheetName = status_var
      )

      openxlsx::writeData(
        wb,
        sheet = status_var,
        x = all_results[[status_var]]
      )
    }

    openxlsx::saveWorkbook(
      wb,
      output_file,
      overwrite = TRUE
    )

    message("已导出结果到: ", output_file)
  }


  # ------ 记录结束时间 ------

  end_time <- Sys.time()

  duration <- difftime(
    end_time,
    start_time,
    units = "mins"
  )

  message("分析完成时间: ", end_time)
  message("总耗时: ", round(duration, 1), " 分钟")
  message("日志文件夹: ", log_folder)

  return(all_results)
}
