# =====================================================
# HW6 自動チェックプログラム
# =====================================================
# このファイルは HW6.R と同じフォルダーに置いてください。

rm(list = ls())

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

source_result <- tryCatch(
  {
    source("HW6.R")
    TRUE
  },
  error = function(e) {
    cat("HW6.R の実行中にエラーが発生しました:\n")
    cat(conditionMessage(e), "\n")
    FALSE
  }
)

# 数値が（許容誤差つきで）一致するか
approx_equal <- function(a, b, tol = 1e-6) {
  tryCatch(
    isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol)),
    error = function(e) FALSE
  )
}

score <- 0

if (!source_result) {
  cat("\n===== チェック終了 =====\n")
  cat("総得点： 0 / 10\n")
  quit(status = 1)
}

# ------------------------------
# HW6.R のコードを構文解析し、
# 「そのobjectが、指定ループの中で代入されているか」を判定する。
# コメントや書く順番には依存しない。
# ------------------------------
.parsed <- tryCatch(parse("HW6.R"), error = function(e) NULL)

# 代入式の左辺から、元のobject名を取り出す（squares[i] <- ... → "squares"）
.lhs_name <- function(l) {
  while (is.call(l)) l <- l[[2]]
  if (is.symbol(l)) as.character(l) else NA_character_
}

# 式の中で代入されているobject名をすべて集める
.assigned_vars <- function(e) {
  vars <- character(0)
  if (is.call(e)) {
    op <- if (is.symbol(e[[1]])) as.character(e[[1]]) else ""
    if (op %in% c("<-", "=", "<<-") && length(e) >= 3) {
      v <- .lhs_name(e[[2]])
      if (!is.na(v)) vars <- c(vars, v)
    }
    for (i in seq_along(e)) {
      vars <- c(vars, tryCatch(.assigned_vars(e[[i]]), error = function(x) character(0)))
    }
  }
  unique(vars)
}

# for / while の中で代入されたobject名を集める
.loop_vars <- new.env()
.loop_vars$for_vars   <- character(0)
.loop_vars$while_vars <- character(0)

.walk <- function(e) {
  if (!is.call(e)) return(invisible(NULL))
  op <- if (is.symbol(e[[1]])) as.character(e[[1]]) else ""
  if (op == "for" && length(e) >= 4) {
    .loop_vars$for_vars <- union(.loop_vars$for_vars, .assigned_vars(e[[4]]))
  } else if (op == "while" && length(e) >= 3) {
    .loop_vars$while_vars <- union(.loop_vars$while_vars, .assigned_vars(e[[3]]))
  }
  for (i in seq_along(e)) {
    tryCatch(.walk(e[[i]]), error = function(x) NULL)
  }
  invisible(NULL)
}

if (!is.null(.parsed)) {
  for (.e in as.list(.parsed)) .walk(.e)
}

# obj が指定ループ（"for" / "while"）の中で代入されているか
.in_loop <- function(obj, keyword) {
  pool <- if (keyword == "for") .loop_vars$for_vars else .loop_vars$while_vars
  obj %in% pool
}

# 各問2点：値が正しく、かつ指定ループを使っている場合のみ2点。
# どちらか一方だけでは0点。
.check_q <- function(label, val_ok, loop_ok, loop_name) {
  val_ok  <- tryCatch(isTRUE(val_ok),  error = function(e) FALSE)
  loop_ok <- tryCatch(isTRUE(loop_ok), error = function(e) FALSE)
  if (val_ok && loop_ok) {
    cat(label, "2点：値・ループともに正解です\n")
    return(2)
  }
  reasons <- c()
  if (!val_ok)  reasons <- c(reasons, "値が正しくありません")
  if (!loop_ok) reasons <- c(reasons, paste0(loop_name, " ループの中で計算されていません"))
  cat(label, "0点：", paste(reasons, collapse = "／"), "\n")
  return(0)
}

# ------------------------------
# 参照解（standard answer）を内部で計算
# ------------------------------
.ref_squares <- (1:10)^2

.ref_money <- 100
.ref_year  <- 0
while (.ref_money <= 180) {
  .ref_money <- .ref_money * 1.04
  .ref_year  <- .ref_year + 1
}

.ref_cum <- cumsum(1:20)

.ref_sales <- c(120, 135, 128, 150, 163, 158, 171, 185, 190, 204)
.ref_growth <- numeric(10)
.ref_growth[1] <- NA
for (.i in 2:10) {
  .ref_growth[.i] <- (.ref_sales[.i] - .ref_sales[.i - 1]) / .ref_sales[.i - 1] * 100
}

.ref_scores <- c(62, 71, NA, 55, 80, NA, 90, 45, NA, 58)
.ref_n_na   <- sum(is.na(.ref_scores))
.ref_n_over <- sum(.ref_scores >= 60, na.rm = TRUE)

# ------------------------------
# Q1 squares（for）
# ------------------------------
score <- score + .check_q(
  "Q1 squares：",
  exists("squares") && is.numeric(squares) && length(squares) == 10 &&
    approx_equal(squares, .ref_squares),
  .in_loop("squares", "for"),
  "for"
)

# ------------------------------
# Q2 year（while）
# ------------------------------
score <- score + .check_q(
  "Q2 year：",
  exists("year") && is.numeric(year) && length(year) == 1 &&
    approx_equal(year, .ref_year),
  .in_loop("year", "while"),
  "while"
)

# ------------------------------
# Q3 cum（for）
# ------------------------------
score <- score + .check_q(
  "Q3 cum：",
  exists("cum") && is.numeric(cum) && length(cum) == 20 &&
    approx_equal(cum, .ref_cum),
  .in_loop("cum", "for"),
  "for"
)

# ------------------------------
# Q4 growth（for、1年目はNA）
# ------------------------------
score <- score + .check_q(
  "Q4 growth：",
  exists("growth") && is.numeric(growth) && length(growth) == 10 &&
    is.na(growth[1]) && approx_equal(growth[2:10], .ref_growth[2:10]),
  .in_loop("growth", "for"),
  "for"
)

# ------------------------------
# Q5 n_na・n_over（for）
# ------------------------------
score <- score + .check_q(
  "Q5 n_na・n_over：",
  exists("n_na") && is.numeric(n_na) && length(n_na) == 1 &&
    approx_equal(n_na, .ref_n_na) &&
    exists("n_over") && is.numeric(n_over) && length(n_over) == 1 &&
    approx_equal(n_over, .ref_n_over),
  .in_loop("n_na", "for") && .in_loop("n_over", "for"),
  "for"
)

cat("\n===== チェック終了 =====\n")
cat("総得点：", score, "/ 10\n")

if (score < 10) {
  quit(status = 1)
}
