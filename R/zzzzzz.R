

# ------------ 启动提示语模块 ------------

# --- 1. 定义包加载时的欢迎动作 ---
.onAttach <- function(libname, pkgname) {
  # 获取当前包的动态版本号
  pkg_version <- utils::packageVersion("SL.cohort")

  packageStartupMessage("---------------------------------------\n")
  packageStartupMessage("  - SL.cohort (", pkg_version, ")")
  packageStartupMessage("  - Maintainer: lannon1899@qq.com")
  packageStartupMessage("  - Have a wonderful day, my friend! (●'◡'●)")
  packageStartupMessage("\n---------------------------------------")
}
