source("global.R")

pk <- data_pk
time <- data_time
pkparam <- pkParam(pk, time)
n_sbj <- 15

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
  temp <- cbind(label = c("上側90%", "中央値", "下側90%"), (1/ci_obj) |> as.data.frame())
  colnames(temp) <- c("範囲", "値")
  temp
}

# tmaxだけ違う関数が必要（空の要素を認めてくれないので追加）
conv_ci_df_tmax2 <- function(ci_obj){
  temp <- cbind(label = c("上側90%", "中央値", "下側90%"), rev(ci_obj) |> as.data.frame())
  colnames(temp) <- c("範囲", "値")
  temp
}

tmp <- tempfile(fileext = ".png")
tmp2 <- tempfile(fileext = ".png")
tmp3 <- tempfile(fileext = ".png")
tmp4 <- tempfile(fileext = ".png")

ggsave(filename = tmp, plot = pk_plot_ms, width = 8, height = 6, dpi = 300)
ggsave(filename = tmp2, plot = pk_plot_each, width = 10, height = nrow(pk)/2, dpi = 300)
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
  add_image("血漿中薬物濃度グラフ（個々の被験者）", file = tmp2, dims = "A1", width = 10, height = nrow(pk)/2)$
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


wb$save("temp.xlsx")
