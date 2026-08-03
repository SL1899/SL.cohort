


# ------------ 包启动提示及GitHub版本检查模块 ------------


# 从GitHub主分支的DESCRIPTION文件中读取最新版本号
# 该函数仅供包内部调用，不需要添加@export
# 默认最多等待5秒，也允许用户通过options()自行调整
.github_latest_version <- function(
    timeout = getOption("SL.cohort.update_timeout", 6)
) {

  # GitHub仓库中DESCRIPTION文件的原始地址
  description_url <- paste0(
    "https://raw.githubusercontent.com/",
    "SL1899/SL.cohort/main/DESCRIPTION"
  )

  # 请求远程DESCRIPTION文件
  # 设置超时时间，避免网络异常时长时间阻塞包的加载
  response <- httr2::request(description_url) |>
    httr2::req_headers(`User-Agent` = "SL.cohort") |>
    httr2::req_timeout(timeout) |>
    httr2::req_perform()

  # 将返回内容转换为文本
  description_text <- httr2::resp_body_string(response)

  # 创建文本连接，以便read.dcf()解析DESCRIPTION
  description_connection <- textConnection(description_text)

  # 函数退出时关闭文本连接
  on.exit(close(description_connection), add = TRUE)

  # 读取DESCRIPTION中的Version字段
  description_data <- read.dcf(
    description_connection,
    fields = "Version"
  )

  # 提取并清理GitHub上的版本号
  github_version <- trimws(
    unname(description_data[1L, "Version"])
  )

  # 检查版本号是否有效
  if (
    length(github_version) != 1L ||
    is.na(github_version) ||
    !nzchar(github_version)
  ) {
    stop("GitHub package version could not be identified.")
  }

  github_version
}


# 用户通过library()或require()加载包时自动执行
.onAttach <- function(libname, pkgname) {

  # 获取当前加载的本地包版本号
  local_version <- as.character(
    utils::packageVersion(
      pkgname,
      lib.loc = libname
    )
  )

  # 设置分隔线
  separator <- "-------------------------------------------------"
  status_separator <- "\n  - Version Check"

  # 显示包的基本信息
  packageStartupMessage(separator)
  packageStartupMessage("  - ", pkgname, " (", local_version, ")")
  packageStartupMessage("  - Maintainer: lannon1899@qq.com")
  packageStartupMessage("  - Have a wonderful day, my friend! (●'◡'●)")

  # 是否启用GitHub版本检查
  # 用户可在加载包前通过以下命令关闭检查：
  # options(SL.cohort.check_updates = FALSE)
  check_updates <- isTRUE(
    getOption(
      "SL.cohort.check_updates",
      TRUE
    )
  )

  # 仅在交互式R会话中进行联网检查
  # 避免R CMD check、后台任务和批处理脚本自动访问GitHub
  if (interactive() && check_updates) {

    packageStartupMessage(status_separator)

    # 尝试读取GitHub版本
    # 网络异常不会影响SL.cohort的正常加载
    github_version <- tryCatch(
      .github_latest_version(),
      error = function(e) NA_character_
    )

    if (is.na(github_version)) {

      packageStartupMessage(
        "  • Status: GitHub version check unavailable."
      )

    } else {

      # 比较本地版本和GitHub版本
      #
      # 返回值：
      # −1：本地版本低于GitHub版本
      #  0：本地版本与GitHub版本一致
      #  1：本地版本高于GitHub版本
      version_comparison <- tryCatch(
        utils::compareVersion(
          local_version,
          github_version
        ),
        error = function(e) NA_integer_
      )

      if (is.na(version_comparison)) {

        # 版本号格式异常或比较失败
        packageStartupMessage(
          "  • Status: GitHub version check unavailable."
        )

      } else if (version_comparison == 0L) {

        # 本地版本与GitHub版本一致
        # 此时不再显示其他信息
        packageStartupMessage(
          "  • Status: Local version matches GitHub."
        )

      } else if (version_comparison > 0L) {

        # 本地版本高于GitHub版本
        # 常见于本地已更新但尚未Push
        packageStartupMessage(
          "  • Status: Local version is newer than GitHub."
        )

      } else {

        # GitHub版本高于本地版本
        # 仅在此时显示最新版本号和更新命令
        packageStartupMessage(
          "  • Status: A newer version is available."
        )
        packageStartupMessage(
          "  • Latest version on GitHub: ",
          github_version
        )
        packageStartupMessage(
          '  • Update with the codes: pak::pak("SL1899/SL.cohort")'
        )
      }
    }
  }

  packageStartupMessage(separator)
}
