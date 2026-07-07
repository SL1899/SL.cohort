

# ------------ 启动提示语模块 ------------

# --- 1. 定义包加载时的欢迎动作 ---
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("---------------------------------------\n")
  packageStartupMessage("  - SL.cohort (0.0.26.0707)")
  packageStartupMessage("  - designed by lannon1899@qq.com")
  packageStartupMessage("  - My friend, wish you a nice day！ (●'◡'●)")
  packageStartupMessage("\n---------------------------------------")
}
