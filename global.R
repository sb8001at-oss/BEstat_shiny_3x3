library(shiny)
library(bslib)
library(readxl)
library(showtext)
library(zip)
library(ggplot2)
library(nlme)
library(openxlsx2)
library(tidyr)

# システムの zip コマンドおよび bsdtar の使用を無効化し、zip パッケージを使わせる
options("openxlsx2.no_utils_zip" = TRUE)
options("openxlsx2.no_bsdtar" = TRUE)
options("openxlsx2.no_maybe_zip" = TRUE)

font_paths("./")
font_add("noto", "NotoSansJP-VariableFont_wght.ttf")
showtext_auto()

downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

data_pk <- read_excel("./example_excel_file_3x3.xlsx", sheet = "PK")
data_time <- read_excel("./example_excel_file_3x3.xlsx", sheet = "time")

# 読み込んだデータからPKパラメータを計算する関数
pkParam <- function(pk, time){
  if(sum(pk$subject != time$subject) > 0 | sum(pk$period != time$period) > 0 | sum(pk$treatment != time$treatment) > 0){return(NULL)}
  
  # pkと時間をデータフレームに成形
  pk_d <- data.frame(
    s_t = paste(pk$subject, pk$treatment),
    subject = pk$subject,
    period = pk$period,
    treatment = pk$treatment,
    group = pk$subject |> substr(1, 1),
    value = pk[, 4:ncol(pk)] |> unlist(),
    time = time[, 4:ncol(time)] |> unlist()
  ) |> na.omit()
  
  if(!is.numeric(pk_d$value) | !is.numeric(pk_d$time)){return(NULL)}
  
  # PKパラメータの計算（NonCompart::tblNCAを利用）
  pkparam <- NonCompart::tblNCA(pk_d, key="s_t", colTime="time", colConc="value", excludeDelta = 0.3, R2ADJ = 0)
  
  pkparam <- 
    data.frame(
      subject = pk$subject,
      period = pk$period,
      treatment = pk$treatment,
      group = pk$subject |> substr(1, 1),
      AUC = pkparam$AUCLST,
      AUCinf = pkparam$AUCIFO |> round(4),
      Cmax = pkparam$CMAX,
      tmax = pkparam$TMAX,
      kel = pkparam$LAMZ |> round(4),
      RApoint = pkparam$LAMZNPT |> round(4),
      CorrCoef = pkparam$R2 |> round(4),
      thalf = pkparam$LAMZHL |> round(4),
      MRT = pkparam$MRTEVLST |> round(4),
      MRTinf = pkparam$MRTEVIFO |> round(4),
      AUCratio = (pkparam$AUCLST / pkparam$AUCIFO) |> round(4)
    )
  
  pkparam$treatment <- factor(pkparam$treatment, levels = c("標準製剤", "試験製剤1", "試験製剤2"))
  pkparam$period <- factor(pkparam$period)
  return(pkparam)
}

# NonCompartでNAが出ることがあるので、自作のPKパラメータ評価の関数を用いる
ncm_self <- function(conc, time){
  Cmax <- max(conc)
  tmax <- time[which(conc == Cmax)]
  index_tmax <- which(conc == Cmax)
  
  if(conc[1] == 0){
    conc_omitZero <- c(0, conc[conc > 0])
    time_omitZero <- c(0, time[conc > 0])
  } else {
    conc_omitZero <- conc[conc > 0]
    time_omitZero <- time[conc > 0]
  }
  
  AUC <- 
    (head(conc_omitZero + conc_omitZero[-1], -1) *
       head(-time_omitZero + time_omitZero[-1], -1) / 2) |> 
    sum() |> suppressWarnings()
  
  conc_s <- conc_omitZero[-(1:(index_tmax - 1))]
  time_s <- time_omitZero[-(1:(index_tmax - 1))]
  
  
  cor_select <- function(i){  
    if(i == 0){return(cor(conc_s, time_s))} else {
      seq_row <- 1:i |> unique()
      cor(log(conc_s[-seq_row]), time_s[-seq_row])
    }
  }
  
  cor_pk <- mapply(cor_select, 0:(length(conc_s)-1))
  min_cor <- cor_pk[cor_pk != -1]  |> na.omit() |> _[1]
  
  index_mincor <- (which(cor_pk == min_cor) - 1 + index_tmax):(index_tmax + length(conc_s) - 1)
  
  RApoint <- length(index_mincor)
  kel <- lm(log(conc[index_mincor]) ~ time[index_mincor]) |> coef() |> _[2]
  names(kel) <- NULL
  thalf <- -log(2)/kel
  Clast <- rev(conc_omitZero)[1]
  AUCinf <- AUC - Clast/kel
  
  ct <- conc * time
  ct_omitZero <- c(0, ct[ct > 0])
  AUMC <- 
    (head(ct_omitZero + ct_omitZero[-1], -1) *
       head(-time_omitZero + time_omitZero[-1], -1) / 2) |> 
    sum() |> suppressWarnings()
  AUMCinf <- 
    AUMC - time[conc == Clast] * Clast / kel + Clast/kel^2
  MRT <- AUMC/AUC
  MRTinf <- AUMCinf/AUCinf
  
  out <- c(AUC, AUCinf, Cmax, tmax, -kel, RApoint, CorrCoef = -min_cor, thalf, MRT, MRTinf, AUCratio = AUC/AUCinf)
  names(out) <- c("AUC", "AUCinf", "Cmax", "tmax", "kel", "RApoint", "CorrCoef", "thalf", "MRT", "MRTinf", "AUCratio")
  out
}

# PKパラメータの要約を作成するための関数
pk_summary <- function(pkparam, group = "all"){
  
  groups <- pkparam$group |> unique()
  
  if(group == 1){
    pkparam <- pkparam[pkparam$group == groups[1], ]
  } else if(group == 2){
    pkparam <- pkparam[pkparam$group == groups[2], ]
  } else if(group == 3){
    pkparam <- pkparam[pkparam$group == groups[3], ]
  }
  
  mean_value <- apply(pkparam[, 5:15], 2, mean) |> round(4)
  sd_value <- apply(pkparam[, 5:15], 2, sd) |> round(4)
  max_value <- apply(pkparam[, 5:15], 2, max) |> round(4)
  min_value <- apply(pkparam[, 5:15], 2, min) |> round(4)
  n_value <- apply(pkparam[, 5:15], 2, length)
  
  temp_ms <- cbind(mean_value, sd_value, max_value, min_value, n_value) |> t() |> as.data.frame()
  temp_ms <- cbind(label = c("平均値", "標準偏差", "最小値", "最大値", "例数"), temp_ms)
  
  colnames(temp_ms) <- c(" ", colnames(pkparam)[5:15])
  rownames(temp_ms) <- NULL
  
  return(temp_ms)
}

# PKパラメータの要約を作成するための関数（製剤間）
pk_summary_treat <- function(pkparam, treatment_drugs){
  
  if(treatment_drugs == "試験製剤1"){
    pkparam <- pkparam[pkparam$treatment == "試験製剤1", ]
  } else if(treatment_drugs == "試験製剤2"){
    pkparam <- pkparam[pkparam$treatment == "試験製剤2", ]
  } else if(treatment_drugs == "標準製剤"){
    pkparam <- pkparam[pkparam$treatment == "標準製剤", ]
  }
  
  mean_value <- apply(pkparam[, 5:15], 2, mean) |> round(4)
  sd_value <- apply(pkparam[, 5:15], 2, sd) |> round(4)
  max_value <- apply(pkparam[, 5:15], 2, max) |> round(4)
  min_value <- apply(pkparam[, 5:15], 2, min) |> round(4)
  n_value <- apply(pkparam[, 5:15], 2, length)
  
  temp_ms <- cbind(mean_value, sd_value, max_value, min_value, n_value) |> t() |> as.data.frame()
  temp_ms <- cbind(label = c("平均値", "標準偏差", "最小値", "最大値", "例数"), temp_ms)
  
  colnames(temp_ms) <- c(" ", colnames(pkparam)[5:15])
  rownames(temp_ms) <- NULL
  
  return(temp_ms)
}

# PKパラメータの要約を作成するための関数（時期間）
pk_summary_period <- function(pkparam, period){
  
  periods <- pkparam$period |> unique()
  
  if(period == periods[1]){
    pkparam <- pkparam[pkparam$period == periods[1], ]
  } else if(period == periods[2]){
    pkparam <- pkparam[pkparam$period == periods[2], ]
  } else if(period == periods[3]){
    pkparam <- pkparam[pkparam$period == periods[3], ]
  }
  
  mean_value <- apply(pkparam[, 5:15], 2, mean) |> round(4)
  sd_value <- apply(pkparam[, 5:15], 2, sd) |> round(4)
  max_value <- apply(pkparam[, 5:15], 2, max) |> round(4)
  min_value <- apply(pkparam[, 5:15], 2, min) |> round(4)
  n_value <- apply(pkparam[, 5:15], 2, length)
  
  temp_ms <- cbind(mean_value, sd_value, max_value, min_value, n_value) |> t() |> as.data.frame()
  temp_ms <- cbind(label = c("平均値", "標準偏差", "最小値", "最大値", "例数"), temp_ms)
  
  colnames(temp_ms) <- c(" ", colnames(pkparam)[5:15])
  rownames(temp_ms) <- NULL
  
  return(temp_ms)
}

# pkと時間のデータフレームを成形する関数（pivot_longerに近い）
pktime_long <- function(pk, time){
  data.frame(
    s_t = paste(pk$subject, pk$treatment),
    subject = pk$subject,
    period = pk$period,
    treatment = pk$treatment,
    group = pk$subject |> substr(1, 1),
    value = pk[, 4:ncol(pk)] |> unlist(),
    time = time[, 4:ncol(time)] |> unlist()
  ) |> na.omit()
}

# PKの平均値・標準偏差（薬剤ごと）を返す関数
pk_treatment_ms <- function(pk_d){ # pk_dは上のpktime_longの返り値
  pk_d_fc <- factor(paste(pk_d$time, pk_d$treatment, sep = ","))
  
  pk_d_m <- tapply(pk_d$value, pk_d_fc, mean)
  pk_d_s <- tapply(pk_d$value, pk_d_fc, sd)
  
  names_data <- names(pk_d_m) |> strsplit(",") |> as.data.frame() |> t()
  rownames(names_data) <- NULL
  
  out <- data.frame(names_data, pk_d_m, pk_d_s)
  colnames(out) <- c("時間", "薬剤", "平均値", "標準偏差")
  out <- out |> 
    pivot_wider(names_from = `薬剤`, values_from = c(`平均値`, `標準偏差`)) |> 
    _[order(out$`時間`), ]
  out$`時間` <- as.numeric(out$`時間`)
  out <- na.omit(out)
  return(out)
}


# PKの平均値・標準偏差をグラフにする関数
pk_summary_plot <- function(pk, time){
  
  pk_d <- pktime_long(pk, time)

  pk_d_fc <- factor(paste(pk_d$time, pk_d$treatment, sep = ","))
  
  pk_d_m <- tapply(pk_d$value, pk_d_fc, mean)
  pk_d_s <- tapply(pk_d$value, pk_d_fc, sd)
  
  tags <- names(pk_d_m) |> strsplit(",") |> as.data.frame() |> t() |> as.data.frame()
  colnames(tags) <- c("time", "treat")
  rownames(tags) <- NULL
  tags$time <- tags$time |> as.numeric()
  tags <- cbind(tags, m = pk_d_m, s = pk_d_s)
  ggplot(tags, aes(x = time, y = m, ymax = m + s, ymin = m - s, color = treat)) +
    geom_point(size = 4) +
    geom_linerange(linewidth = 1) +
    geom_line(linewidth = 1) +
    labs(x = "時間（h）", y = "血漿中薬物濃度", color = NULL, caption = paste0("平均±標準偏差，n = ", pk$subject |> unique() |> length())) +
    theme(
      axis.text.x = element_text(size = 15), 
      axis.text.y = element_text(size = 15), 
      axis.title.y = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      legend.position = "bottom",
      legend.text = element_text(size = 15))
}

# 個々の被験者のPKをグラフにする関数
pk_each_plot <- function(pk, time){
  # pkと時間をデータフレームに成形
  pk_d <- data.frame(
    s_t = paste(pk$subject, pk$treatment),
    subject = pk$subject,
    period = pk$period,
    treatment = pk$treatment,
    group = pk$subject |> substr(1, 1),
    value = pk[, 4:ncol(pk)] |> unlist(),
    time = time[, 4:ncol(time)] |> unlist()
  ) |> na.omit()

  ggplot(pk_d, aes(x = time, y = value, color = treatment)) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    labs(x = "時間（h）", y = "血漿中薬物濃度", color = NULL) +
    facet_wrap(~subject) +
    theme(
      axis.text.x = element_text(size = 15), 
      axis.text.y = element_text(size = 15), 
      axis.title.y = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      legend.position = "bottom",
      legend.text = element_text(size = 15),
      strip.text = element_text(size = 15))
}

# PKパラメータの個別の列をMMRMで解析し，結果と信頼区間を返す関数
mmrm_f <- function(pkparam, col_name, logarithm = TRUE){
  if(class(pkparam$treatment) != "factor"){
    pkparam$treatment <- factor(pkparam$treatment, levels = c("標準製剤", "試験製剤1", "試験製剤2"))
  }
  if(class(pkparam$period) != "factor"){
    pkparam$period <- factor(pkparam$period)
  }
  
  l <- 6:7
  
  formulaLme <- 
    ifelse(logarithm, 
      paste("log(", col_name, ")", "~ group + period + treatment"),
      paste(col_name, "~ group + period + treatment")) |> as.formula()
  
  lmm_out = lme(formulaLme, random=~1|subject, data=pkparam)
  # エラー回避のため intervals() を使わず summary() から直接固定効果と標準誤差を取得
  fixed_tab <- summary(lmm_out)$tTable
  est <- fixed_tab[l, "Value"]
  se  <- fixed_tab[l, "Std.Error"]
  df_val <- fixed_tab[l, "DF"]
  
  # 90%信頼区間の計算 (t分布を使用)
  t_val <- qt(0.95, df = df_val)
  ci_lower <- est - t_val * se
  ci_upper <- est + t_val * se
  
  ci_vec <- cbind(lower = ci_lower, est = est, upper = ci_upper)

  if(logarithm){
    ci_out <- exp(ci_vec)
  } else {
    ci_out <- ci_vec
  }
  
  return(list(lmm_out = lmm_out, ci_out = ci_out))
}

# lmeによるMMRMの結果をデータフレームにする関数
conv_lme_df <- function(lme_obj){
  temp <- summary(lme_obj)$tTable |> as.data.frame()
  temp <- cbind(term = rownames(temp), temp)
  return(temp)
}


# 桁が小さい時に指数表記にする関数
fmt_p <- function(x){
  ifelse(
    x < 0.0001,
    format(x, scientific = TRUE, digits = 4),
    sprintf("%.4f", x)
  )
}

# lmeによるMMRMの結果をデータフレームにする関数（ランダム効果）
conv_lme_rand_df <- function(lme_obj){
  temp <- ranef(lme_obj)
  
  temp <- cbind(tag = rownames(temp), fmt_p(temp[, 1]))
  return(temp)
}

# 信頼区間をデータフレームにする関数
conv_ci_df <- function(ci_obj){
  temp <- cbind(label = c("下側90%", "中央値", "上側90%"), ci_obj |> t()|> round(4) |> as.data.frame())
  colnames(temp) <- c("", "試験製剤1", "試験製剤2")
  temp
}

# MMRMを一度に演算する関数
mmrm_params_f <- function(pkparam){
  calc_cols <- c("AUC", "AUCinf", "Cmax", "tmax", "kel", "thalf", "MRT", "MRTinf")
  out <- list()
  for(i in 1:8){
    if(calc_cols[i] == "tmax"){
      out[[i]] <- mmrm_f(pkparam, calc_cols[i], logarithm = FALSE)
    } else {
      out[[i]] <- mmrm_f(pkparam, calc_cols[i])
    }
  }
  return(out)
}

# PKパラメータの箱ひげ図・ジッタープロットを作成するための関数
pkparam_bj_plot <- function(pkparam, type){
  p <- 
    pkparam[,c(1:9, 12:14)] |> 
      pivot_longer(5:12) |> 
      ggplot(aes(x = treatment, y = value, color = treatment)) +
      facet_wrap(~name, scales = "free_y") +
      labs(x = NULL, y = NULL) +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        legend.position = "none",
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15))

  if(type == "boxplot"){
    p + geom_boxplot()
  } else if(type == "jitter"){
    p + geom_jitter(size = 2, width=0.2, height = 0)
  }
}

# Shapiro-Wilkの結果をデータフレームにする関数
sw_df <- function(sw_out){
  statistic <- sw_out$statistic |> round(4)
  p_value <- sw_out$p.value |> round(4)
  
  d <- data.frame(statistic, p_value)
  colnames(d) <- c("統計量", "p-value")
  rownames(d) <- NULL
  d
}

# Shapiro-Wilk testを実行し，正規性を評価するための関数
normality_test <- function(pkparam){
  names_param <- c("AUC", "AUCinf", "Cmax", "tmax", "kel", "thalf", "MRT", "MRTinf")
  out <- list()
  for(i in 1:8){
    tname <- names_param[i]
    if(tname == "tmax"){
      out[[i]] <- pkparam[, tname] |> shapiro.test() |> sw_df()
    } else {
      out[[i]] <- pkparam[, tname] |> log() |> shapiro.test() |> sw_df() |> as.data.frame()
    }
  }
  return(out)
}

# PKの試験製剤・標準製剤間の比を計算する関数
pk_ratio_calc <- function(pkparam){
  temp_auc <- 
    pkparam[, c("subject", "treatment", "AUC")] |> 
    pivot_wider(names_from = treatment, values_from = AUC)
  
  temp_cmax <- 
    pkparam[, c("subject", "treatment", "Cmax")] |> 
    pivot_wider(names_from = treatment, values_from = Cmax)
  
  out <- data.frame(
    subject = temp_auc$subject,
    AUC_test1 = temp_auc[, 3, drop = TRUE] / temp_auc[, 2, drop = TRUE],
    Cmax_test1 = temp_cmax[, 3, drop = TRUE] / temp_cmax[, 2, drop = TRUE],
    AUC_test2 = temp_auc[, 4, drop = TRUE] / temp_auc[, 2, drop = TRUE],
    Cmax_test2 = temp_cmax[, 4, drop = TRUE] / temp_cmax[, 2, drop = TRUE]
    )
  
  colnames(out) <- c("被験者", "AUC（試験製剤1）", "Cmax（試験製剤1）", "AUC（試験製剤2）", "Cmax（試験製剤2）")
  
  out_s <- 
    data.frame(
      `平均値` = out[, 2:5] |> apply(2, mean) |> round(4),
      `標準偏差` = out[, 2:5] |> apply(2, sd) |> round(4),
      `最大値` = out[, 2:5] |> apply(2, max) |> round(4),
      `最小値` = out[, 2:5] |> apply(2, min) |> round(4)
    )
  
  out_s <- cbind(`パラメータ` = c("AUC（試験製剤1）", "Cmax（試験製剤1）", "AUC（試験製剤2）", "Cmax（試験製剤2）"), out_s)
  
  return(list(out, out_s))
}

# mmrm_f関数の結果（個々のパラメータに対する計算結果）からCVw（被験者内分散），信頼区間の例数設計，検出力を求める関数
calc_cv_ss <- function(mmrm_out_ep, each_group_n_sbj, test){
  epsilon <- mmrm_out_ep[[1]]$sigma
  CVw <- (exp(epsilon ^ 2) - 1) ^ 0.5
  
  if(test == 1){
    theta_ <- mmrm_out_ep[[2]][1, 1]
  } else {
    theta_ <- mmrm_out_ep[[2]][2, 1]
  }
  
  powerf <-
    \(n_sbj){PowerTOST::power.TOST(CV = CVw, theta0 = theta_, n = n_sbj)}
  
  subjects_n <- seq(4, each_group_n_sbj * 2, by = 2) |> as.integer()
  
  power_tost <- data.frame(subjects_n, power = sapply(subjects_n, powerf) |> round(4))
  power_tost$samplesize <- ifelse(power_tost$power > 0.8, "＊", "-")
  
  n_sbj <- mmrm_out_ep[[1]]$data |> nrow()
  
  cibe_f <- \(n_sbj){PowerTOST::CI.BE(pe = theta_, CV = CVw, n = n_sbj)}
  
  ci_be <- cbind(subjects_n, sapply(subjects_n, cibe_f) |> round(4) |> t() |> as.data.frame())
  ci_be$samplesize <- ifelse(ci_be$lower > 0.8 & ci_be$upper < 1.25, "＊", "-")
  
  list(epsilon, CVw, power_tost, ci_be)
}

# AUCとCmaxの検出力・信頼区間・例数を計算する関数
calc_cv <- function(mmrm_out, each_group_n_sbj){
  AUC_mmrm <- mmrm_out[[1]]
  Cmax_mmrm <- mmrm_out[[3]]
  
  AUC_sample_size1 <- calc_cv_ss(AUC_mmrm, each_group_n_sbj, 1)
  Cmax_sample_size1 <- calc_cv_ss(Cmax_mmrm, each_group_n_sbj, 1)
  AUC_sample_size2 <- calc_cv_ss(AUC_mmrm, each_group_n_sbj, 2)
  Cmax_sample_size2 <- calc_cv_ss(Cmax_mmrm, each_group_n_sbj, 2)
  
  list(AUC_sample_size1, Cmax_sample_size1, AUC_sample_size2, Cmax_sample_size2)
}

prompt_text <- 
  "以下のようなプロンプトを利用すると，添付文書っぽいExcelのグラフを作成することができます．うまく書けなかった時は，うまく書けていない部分を指摘し，もう一度描画させてください．
  
  シート名「血漿中濃度平均値（グラフ用）」のデータを使って，グラフを描画します．グラフは医薬品の添付文書に利用できるようにします．
  - グラフは「血漿中濃度平均値（グラフ用）」のシートに示して下さい．
  - Excelのグラフ挿入を利用してグラフを作成して下さい．
  - 時間は数値で，x軸にします．時間は昇順に並べ替えてからグラフにして下さい．
  - 値は平均値_試験製剤と平均値_標準製剤の2つです．
  - エラーバーの大きさはそれぞれ標準偏差_試験製剤と標準偏差_標準製剤です．エラーバーの値を計算する必要があれば，F，G，H，I列に追記して下さい．
  - エラーバーが重ならないよう，点とエラーバーを少し横方向にずらして表示して下さい．
  - 線の色は黒で，標準製剤は破線にして下さい．線の太さは1ptとして下さい．
  - 点は試験製剤が□，標準製剤が●とします．
  - 縦軸は血漿中未変化体濃度，横軸は投与後時間と表記して下さい．
  - 縦軸の単位は(ng/mL)で，軸の上に配置して下さい．
  - 横軸の単位は(時間)で，横軸の右側に配置して下さい．
  - 凡例の表記は標準製剤・試験製剤として下さい．Excelの標準の凡例（記号）は残して下さい．
  - 凡例は右上に表示して下さい．
  - 凡例の下に「Mean±S.D.，n=○○」と記載します．○○には，血漿中薬物濃度のシートのsubjectの人数を半分にした数値を入力して下さい．
  - 文字は日本語フォントを「MS明朝」で，英語フォントを「Century」で表示して下さい．
  - 文字サイズは10ポイントにして下さい．グラフエリアのサイズは高さ10.6cm，幅14.4cmとして下さい．
  - 目盛線は「なし」にして下さい．
  - 目盛の種類は「外向き」，補助目盛の種類は「なし」にして下さい．
  - 挿入したテキストボックスの枠線は消し，背景色は無色として下さい．" |> 
  wb_comment(author = "", width = 12, height = 12)

# ******この関数から見直す******
# Excelで結果を出力するための関数
out_excel <- function(pk, time, pkparam, n_sbj = 15){
  # PKパラメータの要約を計算
  pk_summary_all <- pk_summary(pkparam)
  pk_summary_test1 <- pk_summary_treat(pkparam, "試験製剤1")
  pk_summary_test2 <- pk_summary_treat(pkparam, "試験製剤2")
  pk_summary_ref <- pk_summary_treat(pkparam, "標準製剤")
  pk_summary_group1 <- pk_summary(pkparam, 1)
  pk_summary_group2 <- pk_summary(pkparam, 2)
  pk_summary_group3 <- pk_summary(pkparam, 3)
  pk_summary_period1 <- pk_summary_period(pkparam, 1)
  pk_summary_period2 <- pk_summary_period(pkparam, 2)
  pk_summary_period3 <- pk_summary_period(pkparam, 3)
  
  # PKパラメータのグラフ
  pk_plot_ms <- pk_summary_plot(pk, time) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 35), 
      axis.text.y = element_text(size = 35), 
      axis.title.y = element_text(size = 45),
      axis.title.x = element_text(size = 45),
      legend.position = "bottom",
      legend.text = element_text(size = 35),
      plot.caption = element_text(size = 30))
  
  
  pk_plot_each <- pk_each_plot(pk, time) +
    theme_bw()   +
    theme(
      axis.text.x = element_text(size = 35), 
      axis.text.y = element_text(size = 35), 
      axis.title.y = element_text(size = 45),
      axis.title.x = element_text(size = 45),
      legend.position = "bottom",
      legend.text = element_text(size = 35),
      strip.text = element_text(size = 35)) 
  
  # PKの平均値・標準偏差を計算
  pk_ms_time <- pk_treatment_ms(pktime_long(pk, time))
  
  # MMRMの演算結果
  mmrm_result <- mmrm_params_f(pkparam)
  
  # 治験薬ごとのPKの分布のグラフ
  p_pkparam_boxplot <- pkparam_bj_plot(pkparam, "boxplot") +
    theme_bw()   +
    theme(
      axis.text.x = element_text(size = 35), 
      axis.text.y = element_text(size = 35), 
      axis.title.y = element_text(size = 45),
      axis.title.x = element_text(size = 45),
      legend.position = "bottom",
      legend.text = element_text(size = 35),
      strip.text = element_text(size = 35))
  
  p_pkparam_jitter <- pkparam_bj_plot(pkparam, "jitter") +
    theme_bw()  +
    theme(
      axis.text.x = element_text(size = 35), 
      axis.text.y = element_text(size = 35), 
      axis.title.y = element_text(size = 45),
      axis.title.x = element_text(size = 45),
      legend.position = "bottom",
      legend.text = element_text(size = 35),
      strip.text = element_text(size = 35)) 
  
  
  # 正規性の検定に関する計算
  normality <- normality_test(pkparam)
  
  # 試験製剤/標準製剤の比の計算
  pkratio <- pk_ratio_calc(pkparam)
  
  # 例数設計の計算
  cv_ssn_calc <- calc_cv(mmrm_result, n_sbj)
  
  # 信頼区間をデータフレームにする関数（空の要素を認めてくれないので追加）
  conv_ci_df2 <- function(ci_obj){
    temp <- cbind(label = c("上側90%", "中央値", "下側90%"), ci_obj |> t() |> as.data.frame())
    colnames(temp) <- c("範囲", "試験製剤1", "試験製剤2")
    temp
  }
  
  # tmaxだけ違う関数が必要（空の要素を認めてくれないので追加）
  conv_ci_df_tmax2 <- function(ci_obj){
    temp <- cbind(label = c("上側90%", "中央値", "下側90%"), ci_obj |> t() |> as.data.frame())
    colnames(temp) <- c("範囲", "試験製剤1", "試験製剤2")
    temp
  }
  
  tmp <- tempfile(fileext = ".png")
  tmp2 <- tempfile(fileext = ".png")
  tmp3 <- tempfile(fileext = ".png")
  tmp4 <- tempfile(fileext = ".png")
  
  ggsave(filename = tmp, plot = pk_plot_ms, width = 8, height = 6, dpi = 300)
  ggsave(filename = tmp2, plot = pk_plot_each, width = 16, height = nrow(pk)/2, dpi = 300)
  ggsave(filename = tmp3, plot = p_pkparam_boxplot, width = 8, height = 8, dpi = 300)
  ggsave(filename = tmp4, plot = p_pkparam_jitter, width = 8, height = 8, dpi = 300)
  
  wb <- wb_workbook()$
    add_worksheet(sheet = "血漿中薬物濃度")$
    add_data_table("血漿中薬物濃度", pk)$
    set_col_widths(widths = 11, cols = 1:ncol(pk))$
    add_worksheet(sheet = "採血時間")$
    add_data_table("採血時間", time)$
    set_col_widths(widths = 11, cols = 1:ncol(time))$
    add_worksheet("PKパラメータ")$
    add_data_table("PKパラメータ", pkparam)$
    set_col_widths(widths = 11, cols = 1:ncol(pkparam))$
    add_worksheet("PKパラメータの要約")$
    add_data("PKパラメータの要約", "被験者全員の要約値", start_row = 1)$
    add_data_table("PKパラメータの要約", pk_summary_all, start_row = 2)$
    add_data("PKパラメータの要約", "試験製剤1の要約値", start_row = 9)$
    add_data_table("PKパラメータの要約", pk_summary_test1, start_row = 10)$
    add_data("PKパラメータの要約", "試験製剤2の要約値", start_row = 17)$
    add_data_table("PKパラメータの要約", pk_summary_test2, start_row = 18)$
    add_data("PKパラメータの要約", "標準製剤の要約値", start_row = 25)$
    add_data_table("PKパラメータの要約", pk_summary_ref, start_row = 26)$
    add_data("PKパラメータの要約", "群1の要約値", start_row = 33)$
    add_data_table("PKパラメータの要約", pk_summary_group1, start_row = 34)$
    add_data("PKパラメータの要約", "群2の要約値", start_row = 41)$
    add_data_table("PKパラメータの要約", pk_summary_group2, start_row = 42)$
    add_data("PKパラメータの要約", "群3の要約値", start_row = 49)$
    add_data_table("PKパラメータの要約", pk_summary_group3, start_row = 50)$
    add_data("PKパラメータの要約", "時期1の要約値", start_row = 57)$
    add_data_table("PKパラメータの要約", pk_summary_period1, start_row = 58)$
    add_data("PKパラメータの要約", "時期2の要約値", start_row = 65)$
    add_data_table("PKパラメータの要約", pk_summary_period2, start_row = 66)$
    add_data("PKパラメータの要約", "時期3の要約値", start_row = 73)$
    add_data_table("PKパラメータの要約", pk_summary_period3, start_row = 74)$
    add_data("PKパラメータの要約", "＊AUC：AUClast、RApoint：遡及点、CorrCoef：相関係数、thalf：t1/2、AUCratio：AUC/AUCinf", start_row = 81)$
    set_col_widths(widths = 11, cols = 1:ncol(pk_summary_all))$
    add_worksheet("血漿中薬物濃度グラフ（平均値）")$
    add_image("血漿中薬物濃度グラフ（平均値）", file = tmp, dims = "A1", width = 8, height = 6)$
    add_worksheet("血漿中薬物濃度グラフ（個々の被験者）")$
    add_image("血漿中薬物濃度グラフ（個々の被験者）", file = tmp2, dims = "A1", width = 16, height = nrow(pk)/2)$
    add_worksheet("分散分析結果")$
    add_data("分散分析結果", "AUC", start_row = 1)$
    add_data_table("分散分析結果", mmrm_result[[1]][[1]] |> conv_lme_df(), start_row = 2)$
    add_data("分散分析結果", "AUCinf", start_row = 11)$
    add_data_table("分散分析結果", mmrm_result[[2]][[1]] |> conv_lme_df(), start_row = 12)$
    add_data("分散分析結果", "Cmax", start_row = 21)$
    add_data_table("分散分析結果", mmrm_result[[3]][[1]] |> conv_lme_df(), start_row = 22)$
    add_data("分散分析結果", "tmax", start_row = 31)$
    add_data_table("分散分析結果", mmrm_result[[4]][[1]] |> conv_lme_df(), start_row = 32)$
    add_data("分散分析結果", "kel", start_row = 41)$
    add_data_table("分散分析結果", mmrm_result[[5]][[1]] |> conv_lme_df(), start_row = 42)$
    add_data("分散分析結果", "t1/2", start_row = 51)$
    add_data_table("分散分析結果", mmrm_result[[6]][[1]] |> conv_lme_df(), start_row = 52)$
    add_data("分散分析結果", "MRT", start_row = 61)$
    add_data_table("分散分析結果", mmrm_result[[7]][[1]] |> conv_lme_df(), start_row = 62)$
    add_data("分散分析結果", "MRTinf", start_row = 71)$
    add_data_table("分散分析結果", mmrm_result[[8]][[1]] |> conv_lme_df(), start_row = 72)$
    set_col_widths(widths = 12, cols = 1:6)$
    set_col_widths(widths = 18.5, cols = 1)$
    add_worksheet("信頼区間")$
    add_data("信頼区間", "AUC", start_row = 1)$
    add_data_table("信頼区間", mmrm_result[[1]][[2]] |> conv_ci_df2(), start_row = 2)$
    add_data("信頼区間", "AUCinf", start_row = 7)$
    add_data_table("信頼区間", mmrm_result[[2]][[2]] |> conv_ci_df2(), start_row = 8)$
    add_data("信頼区間", "Cmax", start_row = 13)$
    add_data_table("信頼区間", mmrm_result[[3]][[2]] |> conv_ci_df2(), start_row = 14)$
    add_data("信頼区間", "tmax", start_row = 19)$
    add_data_table("信頼区間", mmrm_result[[4]][[2]] |> conv_ci_df_tmax2(), start_row = 20)$
    add_data("信頼区間", "kel", start_col = 5, start_row = 1)$
    add_data_table("信頼区間", mmrm_result[[5]][[2]] |> conv_ci_df2(), start_col = 5, start_row = 2)$
    add_data("信頼区間", "t1/2", start_col = 5, start_row = 7)$
    add_data_table("信頼区間", mmrm_result[[6]][[2]] |> conv_ci_df2(), start_col = 5, start_row = 8)$
    add_data("信頼区間", "MRT", start_col = 5, start_row = 13)$
    add_data_table("信頼区間", mmrm_result[[7]][[2]] |> conv_ci_df2(), start_col = 5, start_row = 14)$
    add_data("信頼区間", "MRTinf", start_col = 5, start_row = 19)$
    add_data_table("信頼区間", mmrm_result[[8]][[2]] |> conv_ci_df2(), start_col = 5, start_row = 20)$
    set_col_widths("信頼区間", widths = 11, cols = 1:7)$
    add_worksheet("正規性の検定結果")$
    add_data("正規性の検定結果", "AUC", start_row = 1)$
    add_data_table("正規性の検定結果", normality[[1]], start_row = 2)$
    add_data("正規性の検定結果", "AUCinf", start_row = 5)$
    add_data_table("正規性の検定結果", normality[[2]], start_row = 6)$
    add_data("正規性の検定結果", "Cmax", start_row = 9)$
    add_data_table("正規性の検定結果", normality[[3]], start_row = 10)$
    add_data("正規性の検定結果", "tmax", start_row = 13)$
    add_data_table("正規性の検定結果", normality[[4]], start_row = 14)$
    add_data("正規性の検定結果", "kel", start_col = 4, start_row = 1)$
    add_data_table("正規性の検定結果", normality[[5]], start_col = 4, start_row = 2)$
    add_data("正規性の検定結果", "t1/2", start_col = 4, start_row = 5)$
    add_data_table("正規性の検定結果", normality[[6]], start_col = 4, start_row = 6)$
    add_data("正規性の検定結果", "MRT", start_col = 4, start_row = 9)$
    add_data_table("正規性の検定結果", normality[[7]], start_col = 4, start_row = 10)$
    add_data("正規性の検定結果", "MRTinf", start_col = 4, start_row = 13)$
    add_data_table("正規性の検定結果", normality[[8]], start_col = 4, start_row = 14)$
    set_col_widths("正規性の検定結果", widths = 11, cols = 1:5)$
    add_worksheet("箱ひげ図")$
    add_image("箱ひげ図", file = tmp3, dims = "A1", width = 8, height = 8)$
    add_worksheet("ジッター")$
    add_image("ジッター", file = tmp4, dims = "A1", width = 8, height = 8)$
    add_worksheet("試験製剤・標準製剤の比")$
    add_data("試験製剤・標準製剤の比", "試験製剤/標準製剤", start_row = 1)$
    add_data_table("試験製剤・標準製剤の比", pkratio[[1]], start_row = 2)$
    add_data("試験製剤・標準製剤の比", "要約", start_row = nrow(pk)/3 |> round() + 4)$
    add_data_table("試験製剤・標準製剤の比", pkratio[[2]], start_row = nrow(pk)/3 |> round() + 5)$
    set_col_widths("試験製剤・標準製剤の比", widths = 17, cols = 1:5)$
    add_worksheet("例数設計AUC")$
    add_data("例数設計AUC", "検出力", start_row = 1)$
    add_data("例数設計AUC", paste0("個体内分散：", cv_ssn_calc[[1]][[2]] |> round(4), "（例数設計は2剤2期として評価しています）"), start_col = 1, start_row = n_sbj + 3)$
    add_data_table("例数設計AUC", cv_ssn_calc[[1]][[3]], start_row = 2)$
    add_data("例数設計AUC", "信頼区間", start_col = 5, start_row = 1)$
    add_data_table("例数設計AUC", cv_ssn_calc[[1]][[4]], start_col = 5, start_row = 2)$
    set_col_widths("例数設計AUC", widths = 11, cols = 1:8)$
    add_worksheet("例数設計Cmax")$
    add_data("例数設計Cmax", "検出力", start_row = 1)$
    add_data("例数設計Cmax", paste0("個体内分散：", cv_ssn_calc[[2]][[2]] |> round(4), "（例数設計は2剤2期として評価しています）"), start_col = 1, start_row = n_sbj + 3)$
    add_data_table("例数設計Cmax", cv_ssn_calc[[2]][[3]], start_row = 2)$
    add_data("例数設計Cmax", "信頼区間", start_col = 5, start_row = 1)$
    add_data_table("例数設計Cmax", cv_ssn_calc[[2]][[4]], start_col = 5, start_row = 2)$
    set_col_widths("例数設計Cmax", widths = 11, cols = 1:8)$
    add_worksheet("血漿中濃度平均値（グラフ用）")$
    add_data_table("血漿中濃度平均値（グラフ用）", pk_ms_time)$
    add_comment("血漿中濃度平均値（グラフ用）", prompt_text, dims = "F1")$
    set_page_setup(1, orientation = "landscape", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(2, orientation = "landscape", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(3, orientation = "landscape", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(4, orientation = "portrait", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(5, orientation = "landscape", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(6, orientation = "portrait", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(7, orientation = "portrait", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(8, orientation = "portrait", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(9, orientation = "portrait", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(10, orientation = "portrait", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(11, orientation = "portrait", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(12, orientation = "portrait", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(13, orientation = "portrait", fit_to_width = 1, fit_to_height = 1)$
    set_page_setup(14, orientation = "portrait", fit_to_width = 1, fit_to_height = 1)$
    set_header_footer(1, header = c(NA, "血漿中薬物濃度", NA))$
    set_header_footer(2, header = c(NA, "採血時間", NA))$
    set_header_footer(3, header = c(NA, "PKパラメータ", NA))$
    set_header_footer(4, header = c(NA, "PKパラメータの要約", NA))$
    set_header_footer(5, header = c(NA, "血漿中薬物濃度グラフ（平均値）", NA))$
    set_header_footer(6, header = c(NA, "血漿中薬物濃度グラフ（個々の被験者）", NA))$
    set_header_footer(7, header = c(NA, "分散分析結果", NA))$
    set_header_footer(8, header = c(NA, "信頼区間", NA))$
    set_header_footer(9, header = c(NA, "正規性の検定結果", NA))$
    set_header_footer(10, header = c(NA, "箱ひげ図", NA))$
    set_header_footer(11, header = c(NA, "ジッター", NA))$
    set_header_footer(12, header = c(NA, "試験製剤・標準製剤の比", NA))$
    set_header_footer(13, header = c(NA, "例数設計AUC", NA))$
    set_header_footer(14, header = c(NA, "例数設計Cmax", NA))
}
