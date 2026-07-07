# =====================================================
# HW6 採点ヘルパー（GitHub Actions 用）
# 使い方: Rscript .github/check_q.R <問題番号 1-5>
# 値が正しく、かつ指定ループの中で計算されていれば exit 0、それ以外は exit 1
# =====================================================

ok_source <- tryCatch({ source("HW6.R"); TRUE },
                      error = function(e) { cat("HW6.R 実行エラー:", conditionMessage(e), "\n"); FALSE })
if (!ok_source) quit(status = 1)

# 注意: HW6.R の先頭に rm(list = ls()) があるため、
# 引数の読み取りは source() の後に行う
q <- as.integer(commandArgs(trailingOnly = TRUE)[1])

approx_equal <- function(a, b, tol = 1e-6) {
  tryCatch(isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol)),
           error = function(e) FALSE)
}

# ---- 構文解析：for / while の中で代入された object 名を集める ----
.parsed <- tryCatch(parse("HW6.R"), error = function(e) NULL)

.lhs_name <- function(l) {
  while (is.call(l)) l <- l[[2]]
  if (is.symbol(l)) as.character(l) else NA_character_
}

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

.for_vars   <- character(0)
.while_vars <- character(0)

.walk <- function(e) {
  if (!is.call(e)) return(invisible(NULL))
  op <- if (is.symbol(e[[1]])) as.character(e[[1]]) else ""
  if (op == "for" && length(e) >= 4) {
    .for_vars <<- union(.for_vars, .assigned_vars(e[[4]]))
  } else if (op == "while" && length(e) >= 3) {
    .while_vars <<- union(.while_vars, .assigned_vars(e[[3]]))
  }
  for (i in seq_along(e)) tryCatch(.walk(e[[i]]), error = function(x) NULL)
  invisible(NULL)
}

if (!is.null(.parsed)) for (.e in as.list(.parsed)) .walk(.e)

in_for   <- function(obj) obj %in% .for_vars
in_while <- function(obj) obj %in% .while_vars

# ---- 参照解 ----
.ref_squares <- (1:10)^2

.ref_money <- 100; .ref_year <- 0
while (.ref_money <= 180) { .ref_money <- .ref_money * 1.04; .ref_year <- .ref_year + 1 }

.ref_cum <- cumsum(1:20)

.ref_sales <- c(120, 135, 128, 150, 163, 158, 171, 185, 190, 204)
.ref_growth <- c(NA, (.ref_sales[2:10] - .ref_sales[1:9]) / .ref_sales[1:9] * 100)

.ref_scores <- c(62, 71, NA, 55, 80, NA, 90, 45, NA, 58)
.ref_n_na   <- sum(is.na(.ref_scores))
.ref_n_over <- sum(.ref_scores >= 60, na.rm = TRUE)

# ---- 判定 ----
result <- switch(as.character(q),
  "1" = exists("squares") && is.numeric(squares) && length(squares) == 10 &&
        approx_equal(squares, .ref_squares) && in_for("squares"),
  "2" = exists("year") && is.numeric(year) && length(year) == 1 &&
        approx_equal(year, .ref_year) && in_while("year"),
  "3" = exists("cum") && is.numeric(cum) && length(cum) == 20 &&
        approx_equal(cum, .ref_cum) && in_for("cum"),
  "4" = exists("growth") && is.numeric(growth) && length(growth) == 10 &&
        is.na(growth[1]) && approx_equal(growth[2:10], .ref_growth[2:10]) && in_for("growth"),
  "5" = exists("n_na") && is.numeric(n_na) && length(n_na) == 1 &&
        approx_equal(n_na, .ref_n_na) &&
        exists("n_over") && is.numeric(n_over) && length(n_over) == 1 &&
        approx_equal(n_over, .ref_n_over) &&
        in_for("n_na") && in_for("n_over"),
  FALSE
)

if (isTRUE(result)) quit(status = 0) else quit(status = 1)
