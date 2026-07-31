setwd("D:/nhanse/git")
library(dplyr)
library(foreign)
library(tidyr)
library(mice)      # 多重插补
library(ggmice)    # 可视化插补结果
library(ggplot2)   # 绘图
library(naniar)    # 缺失值可视化
library(VIM)       # 缺失模式分析
library(purrr)     # 函数式编程
library(nhanesR)
library(plotRCS)
library(survey)
library(rms)
library(ggpubr)
library(patchwork)
library(mediation)
library(plyr)
library(nhanesR)
library(ggsignif)
library(svyROC)
library(caret)
base_out <- "D:/nhanse/git"
dir.create(base_out, showWarnings = FALSE, recursive = TRUE)
set.seed(111)
#清洗数据
clean<-function(data){
  trans <- function(a){
    a = case_when(
      a == 1 ~ 1,     
      a == 2 ~ 0,     
      a > 2 ~ NA_real_
    )
    return(a)
  }
  d1 <- data
  summary(d1)
  d1$diabetes = case_when(
    d1$diabetes == 1 ~ 1,
    d1$diabetes %in% c(2,3) ~ 0,
    d1$diabetes > 3 ~ NA_real_
  )
  summary(d1)
  
  d2<-d1
  d2$MCQ160B <- trans(d2$MCQ160B)
  d2$MCQ160C <- trans(d2$MCQ160C)
  d2$MCQ160D <- trans(d2$MCQ160D)
  d2$MCQ160E <- trans(d2$MCQ160E)
  d2$MCQ160F <- trans(d2$MCQ160F)
  
  d2 <- d2 %>%
    mutate(
      CVD = case_when(
        # 有任何一个是1 → CVD=1
        MCQ160B == 1 | MCQ160C == 1 | MCQ160D == 1 | 
          MCQ160E == 1 | MCQ160F == 1 ~ 1,
        
        # 所有已知值都是0 → CVD=0
        (!is.na(MCQ160B) & MCQ160B == 0) &
          (!is.na(MCQ160C) & MCQ160C == 0) &
          (!is.na(MCQ160D) & MCQ160D == 0) &
          (!is.na(MCQ160E) & MCQ160E == 0) &
          (!is.na(MCQ160F) & MCQ160F == 0) ~ 0,
        
        # 其他情况 → NA
        TRUE ~ NA_real_
      )
    )
  summary(d2)
  
  d3<-d2
  d3$smoking= case_when(
    d3$smoking %in% c(1,2) ~ 1,
    d3$smoking == 3 ~ 0,
    d3$smoking > 4 ~ NA_real_
  )
  summary(d3)
  
  d4<-d3
  d4$drinking<-ifelse(d4$drinking %in% c(777,999),NA,d4$drinking)
  summary(d4)
  
  d5<-d4
  d5$sedentary_time<-ifelse(d5$sedentary_time %in% c(7777,9999),NA,d5$sedentary_time)
  summary(d5)
  
  d7<-d5
  d7$gender<-trans(d7$gender)  
  d8<-d7
  d8$pregnant<-case_when(
    d8$pregnant == 1 ~ 1,
    TRUE ~ 0
  )
  summary(d8)
  
  d9<-d8
  d9$cancer<-trans(d9$cancer)
  summary(d9)
  
  d10<-d9
  d10$hypertension<-trans(d10$hypertension)
  summary(d10)
  
  d11<-d10
  d11$COPD<-trans(d11$COPD)
  summary(d11)
  
  return(d11)
}
# 完整增强版
mg <- function(name = "", 
               save = FALSE, 
               n = 20, 
               dir = "D:/nhanse/COPD/",
               return_data = TRUE,
               verbose = TRUE) {
  cat("正在合并", name, "数据...\n")
  
  all_data <- list()
  loaded_files <- 0
  missing_files <- 0
  
  for (i in 1:n) {
    # 构建文件路径
    csv_file <- paste0(dir, name, "_", i, ".csv")
    
    if (file.exists(csv_file)) {
      # 读取数据
      data <- read.csv(csv_file)
      all_data[[i]] <- data
      loaded_files <- loaded_files + 1
      
      if (verbose) {
        cat("  ✓ 已加载：", basename(csv_file), 
            "（", nrow(data), "行，", ncol(data), "列）\n")
      }
    } else {
      all_data[[i]] <- NULL
      missing_files <- missing_files + 1
      
      if (verbose) {
        cat("  ⚠️ 文件不存在：", basename(csv_file), "\n")
      }
    }
  }
  
  # 保存逻辑
  if (save) {
    rds_file <- paste0(dir, "all_", name, "_data.rds")
    saveRDS(all_data, rds_file)
    
    cat("\n已保存RDS文件\n")
    cat("   文件位置：", rds_file, "\n")
    cat("   文件大小：", format(file.size(rds_file), big.mark = ","), "字节\n")
  } else {
    if (verbose) {
      cat("\n未保存RDS文件（save = FALSE）\n")
    }
  }
  
  # 汇总信息
  cat("\n=== 合并完成 ===\n")
  cat("数据名称：", name, "\n")
  cat("目标文件数：", n, "\n")
  cat("成功加载：", loaded_files, "个文件\n")
  cat("缺失文件：", missing_files, "个\n")
  
  if (loaded_files > 0) {
    # 计算统计信息
    valid_datasets <- all_data[!sapply(all_data, is.null)]
    total_rows <- sum(sapply(valid_datasets, nrow))
    total_cols <- if (length(valid_datasets) > 0) ncol(valid_datasets[[1]]) else 0
    
    cat("总观测数：", format(total_rows, big.mark = ","), "\n")
    cat("变量数：", total_cols, "\n")
  }
  
  # 返回值控制
  if (return_data) {
    return(all_data)
  } else {
    cat("（不返回数据对象）\n")
    return(invisible(NULL))
  }
}
# 接受 mice 对象，直接返回合并后的数据
retr <- function(dat, imp_obj, i = 1) {
  # 输入验证
  if (!is.data.frame(dat)) {
    stop("dat 必须是数据框")
  }
  
  if (class(imp_obj) != "mids") {
    stop("imp_obj 必须是 mice 对象 (class = 'mids')")
  }
  
  if (i > imp_obj$m) {
    stop("索引 i 超出范围，只有 ", imp_obj$m, " 个插补数据集")
  }
  
  cat("=== 从 mice 对象提取第", i, "个插补数据集 ===\n")
  
  # 步骤1：提取第 i 个插补数据集
  imp_data <- complete(imp_obj, i)
  cat("提取完成，维度：", dim(imp_data), "\n")
  
  # 步骤2：定义要替换的变量
  ipt_vars <- c(
    "COPD",
    "age",
    "gender",
    "BMI",              # 体重指数
    "drinking",         # 饮酒
    "smoking",          # 吸烟
    "sedentary_time",   # 久坐时间（注意：与您的函数中拼写一致）
    
    # 合并症
    "CVD",              # 心血管疾病
    "diabetes",         # 糖尿病
    "hypertension",     # 高血压
    "cancer"            # 癌症
  )
  
  # 步骤3：检查变量在插补数据中是否存在
  available_vars <- intersect(ipt_vars, names(imp_data))
  cat("在插补数据中找到的变量：", paste(available_vars, collapse = ", "), "\n")
  
  if (length(available_vars) == 0) {
    warning("在插补数据中未找到目标变量")
    cat("尝试查找相似变量名...\n")
    
    # 查找相似变量
    for (pattern in c("BMI", "drink", "smok", "sedentary", "CVD", 
                      "diabetes", "hypertension", "cancer")) {
      matches <- grep(pattern, names(imp_data), value = TRUE, ignore.case = TRUE)
      if (length(matches) > 0) {
        cat("  包含 '", pattern, "' 的变量：", paste(matches, collapse = ", "), "\n")
      }
    }
    return(dat)
  }
  
  # 步骤4：检查原始数据中是否有这些变量
  vars_in_original <- intersect(available_vars, names(dat))
  if (length(vars_in_original) > 0) {
    cat("将从原始数据中移除的变量：", paste(vars_in_original, collapse = ", "), "\n")
    # 从原始数据中移除
    dat_clean <- dat[, setdiff(names(dat), vars_in_original), drop = FALSE]
  } else {
    dat_clean <- dat
  }
  
  # 步骤5：从插补数据中提取变量
  imp_selected <- imp_data[, available_vars, drop = FALSE]
  cat("从插补数据中提取的变量：", paste(names(imp_selected), collapse = ", "), "\n")
  
  # 步骤6：检查行数
  if (nrow(dat_clean) != nrow(imp_selected)) {
    cat("警告：行数不匹配 (", nrow(dat_clean), " vs ", nrow(imp_selected), ")\n")
    
    # 尝试通过 SEQN 合并
    if ("SEQN" %in% names(dat_clean) && "SEQN" %in% names(imp_selected)) {
      cat("尝试通过 SEQN 合并...\n")
      result <- merge(dat_clean, imp_selected, by = "SEQN", all.x = TRUE)
    } else {
      # 如果行数不同但没有 ID，尝试按位置合并
      cat("按行位置合并（假设顺序相同）...\n")
      min_rows <- min(nrow(dat_clean), nrow(imp_selected))
      result <- cbind(dat_clean[1:min_rows, , drop = FALSE], 
                      imp_selected[1:min_rows, , drop = FALSE])
    }
  } else {
    # 行数匹配，直接合并
    result <- cbind(dat_clean, imp_selected)
  }
  
  # 步骤7：验证结果
  cat("\n合并完成：\n")
  cat("  原始数据维度：", dim(dat), "\n")
  cat("  结果数据维度：", dim(result), "\n")
  cat("  新增变量数：", length(available_vars), "\n")
  
  # 检查缺失值修复情况
  cat("\n缺失值修复统计：\n")
  for (var in available_vars) {
    if (var %in% names(dat) && var %in% names(result)) {
      orig_miss <- sum(is.na(dat[[var]]))
      new_miss <- sum(is.na(result[[var]]))
      fixed <- orig_miss - new_miss
      
      if (fixed > 0) {
        cat("  ", var, "：修复了", fixed, "个缺失值 (", 
            orig_miss, " → ", new_miss, ")\n")
      } else if (fixed == 0) {
        cat("  ", var, "：无变化 (", orig_miss, " 个缺失)\n")
      } else {
        cat("  ", var, "：警告！缺失值增加了", -fixed, "个\n")
      }
    }
  }
  
  return(result)
}
# 简单版：为20个数据集运行qt_serum
run_qt <- function(name = "serum",m,func = NULL) {
  func_name <- deparse(substitute(func))
  cat("正在为",m,"个", name, "数据集运行",func_name,"...\n")
  for (i in 1:m) {
    # 构建数据名
    data_name <- paste0("dc_",name, "_", i)
    
    # 检查数据是否存在
    if (exists(data_name, envir = .GlobalEnv)) {
      # 获取数据
      dataset <- get(data_name)
      
      # 运行qt_serum
      qt_data <- func(dataset)
      
      # 保存到工作空间
      qt_name <- paste0("qt_", name, "_", i)
      assign(qt_name, qt_data, envir = .GlobalEnv)
      
      # 保存为CSV
      csv_file <- paste0("D:/nhanse/COPD/qt_", name, "_", i, ".csv")
      write.csv(qt_data, csv_file, row.names = FALSE)
      
      cat("已处理：", qt_name, "\n")
    } else {
      cat("⚠️ 数据不存在：", data_name, "\n")
    }
  }
  
  cat("\n完成！已为20个数据集运行",func_name,"\n")
}

run <- function(name = "", newname = "", m = 20, func = NULL, 
                save = TRUE, format = "csv", dir = "D:/nhanse/COPD/") {
  func_name <- deparse(substitute(func))
  cat("正在为", m, "个", name, "数据集运行", func_name, "...\n")
  
  for (i in 1:m) {
    # 构建数据名
    data_name <- paste0(name, "_", i)
    
    # 检查数据是否存在
    if (exists(data_name, envir = .GlobalEnv)) {
      # 获取数据
      dataset <- get(data_name)
      
      # 运行func
      result <- func(dataset)
      
      # 保存到工作空间
      new_name <- paste0(newname, "_", i)
      assign(new_name, result, envir = .GlobalEnv)
      
      # 根据save参数决定是否保存
      if (save) {
        # 构建文件路径
        file_path <- paste0(dir, newname, "_", i, ".", format)
        
        if (format == "csv") {
          # 检查结果是否是数据框
          if (is.data.frame(result)) {
            write.csv(result, file_path, row.names = FALSE)
            cat("已处理：", new_name, "，保存为CSV\n")
          } else {
            cat("⚠️ ", new_name, "不是数据框，无法保存为CSV，尝试保存为RDS\n")
            # 自动转为RDS格式
            rds_path <- paste0(dir, newname, "_", i, ".rds")
            saveRDS(result, rds_path)
            cat("     已保存为RDS格式：", rds_path, "\n")
          }
        } else if (format == "rds") {
          saveRDS(result, file_path)
          cat("已处理：", new_name, "，保存为RDS\n")
        } else {
          cat("⚠️ 不支持的格式：", format, "，跳过保存\n")
        }
      } else {
        cat("已处理：", new_name, "（不保存文件）\n")
      }
    } else {
      cat("⚠️ 数据不存在：", data_name, "\n")
    }
  }
  
  cat("\n完成！已为", m, "个数据集运行", func_name, "\n")
}
# 简单版：生成20个数据集
md <- function(original_data, imp_obj, name = "" ) {
  cat("正在生成20个数据集：", name, "\n")
  
  # 生成20个数据集
  for (i in 1:20) {
    # 生成数据
    data_name <- paste0(name, "_", i)
    dataset <- retr(original_data, imp_obj, i)
    
    # 保存到工作空间
    assign(data_name, dataset, envir = .GlobalEnv)
    
    # 保存为CSV
    csv_file <- paste0("D:/nhanse/COPD/", name, "_", i, ".csv")
    write.csv(dataset, csv_file, row.names = FALSE)
    
    cat("已生成：", data_name, "\n")
  }
  
  cat("\n✅ 完成！已生成20个数据集\n")
}

#计算四分位数
qt_serum<-function(dt){
  # 计算四分位数
  dt$serum_copperQ<-quant(dt$serum_copper,n=4,,Q=T,round = 2)
  dt$serum_ironQ<-quant(dt$serum_iron,n=4,,Q=T,round = 2)
  dt$serum_seleniumQ<-quant(dt$serum_selenium,n=4,,Q=T,round = 2)
  dt$serum_zincQ<-quant(dt$serum_zinc,n=4,,Q=T,round = 2)
  
  
  # 手动定义分界点
  dt <- dt %>%
    mutate(
      ageQ = case_when(
        age < 40 ~ "20-40",
        age >= 40 & age <60 ~ "40-60",
        age >= 60 ~ "≥60",
        TRUE ~ NA_character_  # 处理缺失值
      )
    )
  dt <- dt %>%
    mutate(
      sedentary_timeQ = case_when(
        sedentary_time < 180 ~ "<3h",
        sedentary_time >= 180 & sedentary_time <=360 ~ "3-6h",
        sedentary_time > 360 ~ ">6h",
        TRUE ~ NA_character_  # 处理缺失值
      )
    )
  dt <- dt %>%
    mutate(
      BMIQ = case_when(
        BMI < 18.5 ~ "Underweight",
        BMI >= 18.5 & BMI <25 ~ "Normalweight",
        BMI >= 25 & BMI <30 ~ "Overweight",
        BMI >= 30 ~ "Obesity",
        TRUE ~ NA_character_  # 处理缺失值
      )
    )
  dt <- dt %>%
    mutate(
      drinkingQ = case_when(
        is.na(drinking) | drinking %in% c(777, 999) ~ NA_character_,  # 处理缺失和特殊值
        drinking == 1 ~ "Occasional (<1 drink)",      # 偶尔，少于1杯
        drinking >= 2 & drinking <= 3 ~ "Low Risk (1-3)",  # 低风险，1-3杯
        drinking >= 4 & drinking <= 6 ~ "Moderate Risk (4-6)",  # 中风险，4-6杯
        drinking >= 7 ~ "High Risk (7+)",             # 高风险，7杯以上
        TRUE ~ "Other"
      ),
      drinkingQ = factor(drinkingQ,
                         levels = c("Occasional (<1 drink)", "Low Risk (1-3)", 
                                    "Moderate Risk (4-6)", "High Risk (7+)"))
    )
  
  dt$ageQ<-factor(dt$ageQ,levels = c("20-40","40-60","≥60"))
  dt$sedentary_timeQ<-factor(dt$sedentary_timeQ,levels = c("<3h","3-6h",">6h"))
  dt$BMIQ<-factor(dt$BMIQ,levels = c("Underweight","Normalweight","Overweight","Obesity"))
  return(dt)
}
#计算四分位数
qt_serum_n<-function(dt){
  # 计算四分位数
  dt$serum_copperQ<-quant(dt$serum_copper,n=4,,Q=T,round = 2)
  dt$serum_seleniumQ<-quant(dt$serum_selenium,n=4,,Q=T,round = 2)
  dt$serum_zincQ<-quant(dt$serum_zinc,n=4,,Q=T,round = 2)
  
  
  # 手动定义分界点
  dt <- dt %>%
    mutate(
      ageQ = case_when(
        age < 40 ~ "20-40",
        age >= 40 & age <60 ~ "40-60",
        age >= 60 ~ "≥60",
        TRUE ~ NA_character_  # 处理缺失值
      )
    )
  dt <- dt %>%
    mutate(
      sedentary_timeQ = case_when(
        sedentary_time < 180 ~ "<3h",
        sedentary_time >= 180 & sedentary_time <=360 ~ "3-6h",
        sedentary_time > 360 ~ ">6h",
        TRUE ~ NA_character_  # 处理缺失值
      )
    )
  dt <- dt %>%
    mutate(
      BMIQ = case_when(
        BMI < 18.5 ~ "Underweight",
        BMI >= 18.5 & BMI <25 ~ "Normalweight",
        BMI >= 25 & BMI <30 ~ "Overweight",
        BMI >= 30 ~ "Obesity",
        TRUE ~ NA_character_  # 处理缺失值
      )
    )
  dt <- dt %>%
    mutate(
      drinkingQ = case_when(
        is.na(drinking) | drinking %in% c(777, 999) ~ NA_character_,  # 处理缺失和特殊值
        drinking == 1 ~ "Occasional (<1 drink)",      # 偶尔，少于1杯
        drinking >= 2 & drinking <= 3 ~ "Low Risk (1-3)",  # 低风险，1-3杯
        drinking >= 4 & drinking <= 6 ~ "Moderate Risk (4-6)",  # 中风险，4-6杯
        drinking >= 7 ~ "High Risk (7+)",             # 高风险，7杯以上
        TRUE ~ "Other"
      ),
      drinkingQ = factor(drinkingQ,
                         levels = c("Occasional (<1 drink)", "Low Risk (1-3)", 
                                    "Moderate Risk (4-6)", "High Risk (7+)"))
    )
  
  dt$ageQ<-factor(dt$ageQ,levels = c("20-40","40-60","≥60"))
  dt$sedentary_timeQ<-factor(dt$sedentary_timeQ,levels = c("<3h","3-6h",">6h"))
  dt$BMIQ<-factor(dt$BMIQ,levels = c("Underweight","Normalweight","Overweight","Obesity"))
  return(dt)
}



#计算四分位数
qt_serum_f<-function(dt){
  # 计算四分位数
  dt$serum_ironQ<-quant(dt$serum_iron,n=4,,Q=T,round = 2)
  
  # 手动定义分界点
  dt <- dt %>%
    mutate(
      ageQ = case_when(
        age < 40 ~ "20-40",
        age >= 40 & age <60 ~ "40-60",
        age >= 60 ~ "≥60",
        TRUE ~ NA_character_  # 处理缺失值
      )
    )
  dt <- dt %>%
    mutate(
      sedentary_timeQ = case_when(
        sedentary_time < 180 ~ "<3h",
        sedentary_time >= 180 & sedentary_time <=360 ~ "3-6h",
        sedentary_time > 360 ~ ">6h",
        TRUE ~ NA_character_  # 处理缺失值
      )
    )
  dt <- dt %>%
    mutate(
      BMIQ = case_when(
        BMI < 18.5 ~ "Underweight",
        BMI >= 18.5 & BMI <25 ~ "Normalweight",
        BMI >= 25 & BMI <30 ~ "Overweight",
        BMI >= 30 ~ "Obesity",
        TRUE ~ NA_character_  # 处理缺失值
      )
    )
  dt <- dt %>%
    mutate(
      drinkingQ = case_when(
        is.na(drinking) | drinking %in% c(777, 999) ~ NA_character_,  # 处理缺失和特殊值
        drinking == 1 ~ "Occasional (<1 drink)",      # 偶尔，少于1杯
        drinking >= 2 & drinking <= 3 ~ "Low Risk (1-3)",  # 低风险，1-3杯
        drinking >= 4 & drinking <= 6 ~ "Moderate Risk (4-6)",  # 中风险，4-6杯
        drinking >= 7 ~ "High Risk (7+)",             # 高风险，7杯以上
        TRUE ~ "Other"
      ),
      drinkingQ = factor(drinkingQ,
                         levels = c("Occasional (<1 drink)", "Low Risk (1-3)", 
                                    "Moderate Risk (4-6)", "High Risk (7+)"))
    )
  
  dt$ageQ<-factor(dt$ageQ,levels = c("20-40","40-60","≥60"))
  dt$sedentary_timeQ<-factor(dt$sedentary_timeQ,levels = c("<3h","3-6h",">6h"))
  dt$BMIQ<-factor(dt$BMIQ,levels = c("Underweight","Normalweight","Overweight","Obesity"))
  return(dt)
}




#插补
ipt<-function(d11,element = "NONE"){
  set.seed(111)
  impute_vars <- c(
    "COPD",
    # 协变量
    "age",
    "gender",
    "BMI",            # 体重指数
    "drinking",       # 饮酒
    "smoking",        # 吸烟
    "sedentary_time", # 久坐时间
    # 合并症
    "CVD",            # 心血管疾病
    "diabetes",       # 糖尿病
    "hypertension",   # 高血压
    "cancer"          # 癌症
  )
  # 如果element不为"NONE"，则添加到插补变量列表
  if (element != "NONE") {
    impute_vars <- c(element, impute_vars)
  }
  # 检查这些变量是否存在于dc中
  existing_vars <- intersect(impute_vars, names(d11))
  missing_vars <- setdiff(impute_vars, names(d11))
  
  # 创建用于插补的数据子集
  dc_impute <- d11[, existing_vars]
  
  # 4. 分析缺失模式（参照论文图2）
  
  # 4.1 计算各变量的缺失比例
  missing_summary <- data.frame(
    variable = names(dc_impute),
    n_missing = colSums(is.na(dc_impute)),
    pct_missing = colSums(is.na(dc_impute)) / nrow(dc_impute) * 100
  ) %>%
    arrange(desc(pct_missing))
  
  cat("\n=== 缺失比例汇总 ===\n")
  print(missing_summary, row.names = FALSE)
  
  
  # 4.3 图2B：连续和分类变量的缺失模式
  # 首先确定变量类型
  var_types <- sapply(dc_impute, function(x) {
    if(is.numeric(x)) {
      if(length(unique(na.omit(x))) <= 5) "binary/categorical" else "continuous"
    } else if(is.factor(x)) {
      "categorical"
    } else {
      "other"
    }
  })
  
  cat("\n=== 变量类型 ===\n")
  print(table(var_types))
  
  
  # 5. 准备多重插补
  
  # 5.1 确定插补方法
  init <- mice(dc_impute, maxit = 0, print = FALSE)
  method <- init$method
  
  # 为不同变量类型指定方法
  for (var in names(dc_impute)) {
    if (is.numeric(dc_impute[[var]])) {
      if (length(unique(na.omit(dc_impute[[var]]))) <= 5) {
        method[var] <- "logreg"  # 二分类变量
      } else {
        method[var] <- "pmm"     # 连续变量 - 预测均值匹配
      }
    } else if (is.factor(dc_impute[[var]]) && nlevels(dc_impute[[var]]) > 2) {
      method[var] <- "polyreg"   # 多分类变量
    }
  }
  
  cat("\n=== 插补方法分配 ===\n")
  print(method)
  
  # 5.2 创建预测矩阵
  pred <- quickpred(dc_impute, 
                    mincor = 0.1,    # 最小相关系数
                    minpuc = 0.3,    # 最小预测变量使用比例
                    exclude = c("SEQN"))  # 排除ID变量（如果有）
  
  cat("\n预测矩阵维度：", dim(pred), "\n")
  
  # 6. 执行多重插补（参照论文：m=20）
  
  imp <- mice(dc_impute,
              m = 20,          # 20个插补数据集
              maxit = 20,      # 20次迭代
              method = method,
              predictorMatrix = pred,
              seed = 111,      # 与论文一致
              print = TRUE)    # 显示进度
  
  
  return(imp)
}
lgst <- function(data_prefix = "cu_serum_df", model_name = "serum_copper",element = "serum_copperQ") {
  cat("=== 运行逻辑回归 ===\n")
  cat("数据集前缀：", data_prefix, "\n")
  
  # 存储所有模型
  m1_list <- list()
  m2_list <- list()
  m3_list <- list()
  
  for (i in 1:20) {
    data_name <- paste0(data_prefix, "_", i)
    
    if (exists(data_name)) {
      cat("处理数据集", i, "...")
      
      design <- get(data_name)
      
      formula_m1 <- as.formula(paste("COPD ~", element))
      formula_m2 <- as.formula(paste("COPD ~", element, 
                                     "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ"))
      formula_m3 <- as.formula(paste("COPD ~", element, 
                                     "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ", 
                                     "+ CVD + cancer + diabetes + hypertension"))
      
      # 运行模型
      m1_list[[i]] <- svyglm(formula_m1,
                             family = quasibinomial(),
                             design = design)
      
      m2_list[[i]] <- svyglm(formula_m2,
                             family = quasibinomial(),
                             design = design)
      
      m3_list[[i]] <- svyglm(formula_m3,
                             family = quasibinomial(),
                             design = design)
      
      cat("完成\n")
    }
  }
  
  # 过滤掉NULL结果
  m1_list <- m1_list[!sapply(m1_list, is.null)]
  m2_list <- m2_list[!sapply(m2_list, is.null)]
  m3_list <- m3_list[!sapply(m3_list, is.null)]
  
  # 转换为mira对象
  cat("\n转换为mira对象...\n")
  m1_mira <- as.mira(m1_list)
  m2_mira <- as.mira(m2_list)
  m3_mira <- as.mira(m3_list)
  
  # 使用pool函数
  cat("使用mice::pool()合并结果...\n")
  pooled_m1 <- pool(m1_mira)
  pooled_m2 <- pool(m2_mira)
  pooled_m3 <- pool(m3_mira)
  
  # 提取汇总结果
  summary_m1 <- summary(pooled_m1)
  summary_m2 <- summary(pooled_m2)
  summary_m3 <- summary(pooled_m3)
  
  # 添加OR和CI
  add_or_ci <- function(summary_df) {
    summary_df$OR <- exp(summary_df$estimate)
    summary_df$OR_ci_lower <- exp(summary_df$estimate - 1.96 * summary_df$std.error)
    summary_df$OR_ci_upper <- exp(summary_df$estimate + 1.96 * summary_df$std.error)
    return(summary_df)
  }
  
  summary_m1 <- add_or_ci(summary_m1)
  summary_m2 <- add_or_ci(summary_m2)
  summary_m3 <- add_or_ci(summary_m3)
  
  # 保存结果 - 使用自定义模型名称
  write.csv(summary_m1, paste0("D:/nhanse/COPD/", model_name, "_m1.csv"), row.names = FALSE)
  write.csv(summary_m2, paste0("D:/nhanse/COPD/", model_name, "_m2.csv"), row.names = FALSE)
  write.csv(summary_m3, paste0("D:/nhanse/COPD/", model_name, "_m3.csv"), row.names = FALSE)
  
  cat("\n✅ 结果已保存\n")
  cat("生成文件：\n")
  cat(model_name, "_m1.csv\n", sep = "")
  cat(model_name, "_m2.csv\n", sep = "")
  cat(model_name, "_m3.csv\n", sep = "")
  
  # 创建模型名称（前缀 + 固定后缀）
  model_name_m1 <- paste0(model_name, "_m1")
  model_name_m2 <- paste0(model_name, "_m2")
  model_name_m3 <- paste0(model_name, "_m3")
  
  # 使用自定义名称返回结果
  result_list <- list()
  result_list[[model_name_m1]] <- list(pooled = pooled_m1, summary = summary_m1)
  result_list[[model_name_m2]] <- list(pooled = pooled_m2, summary = summary_m2)
  result_list[[model_name_m3]] <- list(pooled = pooled_m3, summary = summary_m3)
  
  return(result_list)
}

# 读取2015-2016周期（I）的数据
ALQ_I <- read.xport("D:/d/ALQ_I.xpt")
CUSEZN_I <- read.xport("D:/d/CUSEZN_I.xpt")
DR1TOT_I <- read.xport("D:/d/DR1TOT_I.xpt")
DR2TOT_I <- read.xport("D:/d/DR2TOT_I.xpt")
SMQ_I <- read.xport("D:/d/SMQ_I.xpt")
BMX_I <- read.xport("D:/d/BMX_I.xpt")
PAQ_I <- read.xport("D:/d/PAQ_I.xpt")
MCQ_I <- read.xport("D:/d/MCQ_I.xpt")
DIQ_I <- read.xport("D:/d/DIQ_I.xpt")
BPQ_I <- read.xport("D:/d/BPQ_I.xpt")
DEMO_I <- read.xport("D:/d/DEMO_I.xpt")
CBC_I <- read.xport("D:/d/CBC_I.xpt")
BIOPRO_I <- read.xport("D:/d/BIOPRO_I.xpt")
# 读取2013-2014周期（H）的数据
ALQ_H <- read.xport("D:/d/ALQ_H.xpt")
CUSEZN_H <- read.xport("D:/d/CUSEZN_H.xpt")
DR1TOT_H <- read.xport("D:/d/DR1TOT_H.xpt")
DR2TOT_H <- read.xport("D:/d/DR2TOT_H.xpt")
SMQ_H <- read.xport("D:/d/SMQ_H.xpt")
BMX_H <- read.xport("D:/d/BMX_H.xpt")
PAQ_H <- read.xport("D:/d/PAQ_H.xpt")
MCQ_H <- read.xport("D:/d/MCQ_H.xpt")
DIQ_H <- read.xport("D:/d/DIQ_H.xpt")
BPQ_H <- read.xport("D:/d/BPQ_H.xpt")
DEMO_H <- read.xport("D:/d/DEMO_H.xpt")
CBC_H <- read.xport("D:/d/CBC_H.xpt")
BIOPRO_H <- read.xport("D:/d/BIOPRO_H.xpt")
# 读取2017-2018周期（J）的数据
ALQ_J <- read.xport("D:/d/ALQ_J.xpt")
DR1TOT_J <- read.xport("D:/d/DR1TOT_J.xpt")
DR2TOT_J <- read.xport("D:/d/DR2TOT_J.xpt")
FETIB_J <- read.xport("D:/d/FETIB_J.xpt")
SMQ_J <- read.xport("D:/d/SMQ_J.xpt")
BMX_J <- read.xport("D:/d/BMX_J.xpt")
PAQ_J <- read.xport("D:/d/PAQ_J.xpt")
MCQ_J <- read.xport("D:/d/MCQ_J.xpt")
DIQ_J <- read.xport("D:/d/DIQ_J.xpt")
BPQ_J <- read.xport("D:/d/BPQ_J.xpt")
DEMO_J <- read.xport("D:/d/DEMO_J.xpt")
CBC_J <- read.xport("D:/d/CBC_J.xpt")
BIOPRO_J <- read.xport("D:/d/BIOPRO_J.xpt")
# 创建函数减少重复代码
fj <- function(a, b, c, d, e, 
               f, g, h, i, j, k ,l ,m, yr) {
  A<-a %>%
    full_join(b, by = "SEQN") %>%
    full_join(c, by = "SEQN") %>%
    full_join(d, by = "SEQN") %>%
    full_join(e, by = "SEQN") %>%
    full_join(f, by = "SEQN") %>%
    full_join(g, by = "SEQN") %>%
    full_join(h, by = "SEQN") %>%
    full_join(i, by = "SEQN") %>%
    full_join(j, by = "SEQN") %>%
    full_join(k, by = "SEQN") %>%
    full_join(l, by = "SEQN") %>%
    full_join(m, by = "SEQN") %>%
    mutate(year = yr)
  return(A)
}

# 使用函数合并每个周期的数据
df_H <- fj(DEMO_H, CUSEZN_H, DR1TOT_H, DR2TOT_H, SMQ_H, ALQ_H, PAQ_H, MCQ_H, DIQ_H, BPQ_H, BMX_H,CBC_H,BIOPRO_H, "2013-2014")
df_I <- fj(DEMO_I, CUSEZN_I, DR1TOT_I, DR2TOT_I, SMQ_I, ALQ_I, PAQ_I, MCQ_I, DIQ_I, BPQ_I, BMX_I,CBC_I,BIOPRO_I, "2015-2016")
df_J <- fj(DEMO_J, FETIB_J, DR1TOT_J, DR2TOT_J, SMQ_J, ALQ_J, PAQ_J, MCQ_J, DIQ_J, BPQ_J, BMX_J, CBC_J,BIOPRO_J,"2017-2018")



# 合并所有数据
data <- bind_rows(df_H, df_I, df_J)

dfa <- data.frame(
  SEQN=data$SEQN,
  year=data$year,
  serum_copper=data$LBXSCU,#血铜
  serum_selenium=data$LBXSSE,#血硒
  serum_zinc=data$LBXSZN,#血锌
  serum_iron=data$LBXSIR,#血铁
  gender=data$RIAGENDR,#性别
  age=data$RIDAGEYR,#年龄
  pregnant=data$RIDEXPRG,#是否怀孕
  drinking=data$ALQ130,#饮酒频率
  smoking=data$SMQ040,#是否吸烟
  BMI=data$BMXBMI,#BMI
  physical_act=data$PAQ605,#锻炼频率
  sedentary_time=data$PAD680,#久坐时间
  MCQ160B=data$MCQ160B,#心衰
  MCQ160C=data$MCQ160C,#冠心病
  MCQ160D=data$MCQ160D,#心绞痛
  MCQ160E=data$MCQ160E,#心脏病发作
  MCQ160F=data$MCQ160F,#中风
  COPD=data$MCQ160O,#COPD
  cancer=data$MCQ220,#患癌症
  diabetes=data$DIQ010,#糖尿病
  hypertension=data$BPQ020,#高血压
  WTSA2YR=data$WTSA2YR,#子样本权重
  WTMEC2YR=data$WTMEC2YR,
  WTDR2D=data$WTDR2D.x,
  wbc=data$LBXWBCSI,
  monocyte=data$LBDMONO,
  lymphocyte=data$LBDLYMNO,
  neutrophil=data$LBDNENO,
  platelet=data$LBXPLTSI,
  sdmvpsu=data$SDMVPSU,
  sdmvstra=data$SDMVSTRA
)
summary(dfa)
dfa$serum_wt<-1/3*dfa$WTSA2YR
dfa$fe_wt<-1/2*dfa$WTMEC2YR
write.csv(dfa,file="rawdata.csv") 

dfa<-read.csv("rawdata.csv")
dfa_clean <- clean(dfa)
dfa_clean$WTSA2YR <- dfa_clean$serum_wt
dfa_clean <- dfa_clean[, !names(dfa_clean) %in% c("MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", "MCQ160F","physical_act","WTMEC2YR","fe_wt","WTDR2D.x","WTDR2D.y","wbc","monocyte","lymphocyte","neutrophil","platelet","WTSA2YR","WTMEC2YR","WTDR2D","serum_wt","X")]

cat("删除后列数:", ncol(dfa_clean), "\n")
cat("确认 MCQ160B-F 还存在吗:", any(c("MCQ160B","MCQ160C","MCQ160D","MCQ160E","MCQ160F","physical_act") %in% names(dfa_clean)), "\n")
# dfa_clean 的列对应关系（按顺序）
correct_names <- c(
  "SEQN",            # SEQN → SEQN (already uppercase)
  "Year",            # year → Year
  "Serum_copper",    # serum_copper → Serum_copper
  "Serum_selenium",  # serum_selenium → Serum_selenium
  "Serum_zinc",      # serum_zinc → Serum_zinc
  "Serum_iron",      # serum_iron → Serum_iron
  "Gender",          # gender → Gender
  "Age",             # age → Age
  "Pregnant",        # pregnant → Pregnant
  "Drinking",        # drinking → Drinking
  "Smoking",         # smoking → Smoking
  "BMI",             # BMI → BMI (no change)
  "Sedentary_time",  # sedentary_time → Sedentary_time
  "COPD",            # COPD → COPD (no change)
  "Cancer",          # cancer → Cancer
  "Diabetes",        # diabetes → Diabetes
  "Hypertension",    # hypertension → Hypertension
  "sdmvpsu",         # sdmvpsu → Sdmvpsu
  "sdmvstra",        # sdmvstra → Sdmvstra
  "CVD"              # CVD → CVD (no change)
)

names(dfa_clean) <- correct_names
names(dfa_clean)
plot_pattern(dfa_clean,npat = 3,rotate = T)
ggsave("missing_pattern.png",width = 10,height = 6)
# 指定变量及顺序
target_vars <- c("serum_copper", "serum_zinc", "serum_iron", "serum_selenium",
                 "smoking", "drinking", "BMI", "sedentary_time",
                 "CVD", "hypertension", "diabetes", "cancer", "COPD")

# 从 dfa_clean 提取缺失指示矩阵 (1=缺失, 0=不缺失)
missing_mat <- as.data.frame(lapply(dfa_clean[target_vars], function(x) as.numeric(is.na(x))))

# 只保留有缺失的变量
has_miss <- names(missing_mat)[colSums(missing_mat) > 0]
missing_mat <- missing_mat[has_miss]

# 计算相关系数矩阵
cor_mat <- cor(missing_mat, use = "everything")

# 按缺失比例排序显示
miss_pct <- colSums(missing_mat) / nrow(missing_mat) * 100
cat("缺失比例:\n")
print(round(miss_pct, 2))

cat("\n缺失相关系数矩阵:\n")
miss_relv<-round(cor_mat, 4)
print(miss_relv)
write.csv(miss_relv, file = "missing_correlation_matrix.csv", row.names = TRUE)

#清洗COPD
dr_COPD<-drop_na(data = dfa,"COPD")%>%
  filter(!(COPD %in% c(7, 9)))
summary(dr_COPD)

#dr_COPD <- read.csv("D:/nhanse/COPD/dr_COPD.csv")

#清洗怀孕
dr_preg <- dr_COPD %>%
  filter(is.na(pregnant) | pregnant != 1)  # 保留NA和pregnant!=1的行
summary(dr_preg)

#清洗血清
dr_serum<-dr_preg %>%
  filter(!(is.na(serum_copper)&is.na(serum_selenium)&is.na(serum_zinc)&is.na(serum_iron)))
df_serum<-clean(dr_serum)
summary(df_serum)
write.csv(dr_serum,file = "dr_serum.csv")

dr_serum <- read.csv("dr_serum.csv")
counts <- with(dr_serum, table(year, COPD))
prevalence <- prop.table(counts, margin = 1)[,"1"] * 100
pre<-data.frame(
  year = c("2013-2014","2015-2016","2017-2018"),
  COPD = counts[c("2013-2014","2015-2016","2017-2018"), "1"],
  Non_COPD = counts[c("2013-2014","2015-2016","2017-2018"), "2"],
  COPD_prevalence = round(prevalence[c("2013-2014","2015-2016","2017-2018")], 2)
)
pre <- rbind(
  pre,
  data.frame(
    year = "Total",
    COPD = sum(counts[, "1"]),
    Non_COPD = sum(counts[, "2"]),
    COPD_prevalence = round(sum(counts[, "1"]) / sum(counts) * 100, 2)
  )
)
pre
write.csv(pre,file = "prevalence.csv")

# 删除血清缺失
df_serum_cu<-drop_na(df_serum,serum_copper)
summary(df_serum_cu)
df_serum_zn<-drop_na(df_serum,serum_zinc)
summary(df_serum_zn)
df_serum_se<-drop_na(df_serum,serum_selenium)
summary(df_serum_se)
df_serum_fe<-drop_na(df_serum,serum_iron) 
summary(df_serum_fe)
df_serum_cu<-subset(df_serum_cu,select = -c(serum_iron))
df_serum_zn<-subset(df_serum_zn,select = -c(serum_iron))
df_serum_se<-subset(df_serum_se,select = -c(serum_iron))
df_serum_fe<-subset(df_serum_fe,select = -c(serum_copper,serum_zinc,serum_selenium,WTSA2YR,serum_wt))
# 插补生成20个插补集
dc_serum<-ipt(df_serum)
dc_serum_cu<-ipt(df_serum_cu,element = "serum_copper")
dc_serum_zn<-ipt(df_serum_zn,element = "serum_zinc")
dc_serum_se<-ipt(df_serum_se,element = "serum_selenium")
dc_serum_fe<-ipt(df_serum_fe,element = "serum_iron")

# 将插补数据插回原数据集
#dc_serum_1<-retr(df_serum,dc_serum,i=1)
#dc_dietary_1<-retr(df_dietary,dc_dietary,i=1)

# 保存插补集
#saveRDS(dc_serum,file = "dc_serum.rds")
#load("dc_serum.rds")
# 保存第一个插补完的第一个数据集
#write.csv(dc_serum_1,file = "D:/nhanse/COPD/dc_serum.csv" )
#write.csv(dc_dietary_1,file = "D:/nhanse/COPD/dc_diatary.csv")

# 生成20个插补数据集
md(df_serum, dc_serum, "dc_serum")
md(df_serum_cu, dc_serum_cu, "dc_serum_cu")
md(df_serum_zn, dc_serum_zn, "dc_serum_zn")
md(df_serum_se, dc_serum_se, "dc_serum_se")
md(df_serum_fe, dc_serum_fe, "dc_serum_fe")

model_fit_s <- with(dc_serum,glm(COPD ~ age + gender + BMI + sedentary_time + smoking + drinking + CVD + cancer + diabetes + hypertension,binomial()))
model_fit_s_cu <- with(dc_serum_cu,glm(COPD ~ serum_copper + age + gender + BMI + sedentary_time + smoking + drinking + CVD + cancer + diabetes + hypertension,binomial()))
model_fit_s_zn <- with(dc_serum_zn,glm(COPD ~ serum_zinc + age + gender + BMI + sedentary_time + smoking + drinking + CVD + cancer + diabetes + hypertension,binomial()))
model_fit_s_se <- with(dc_serum_se,glm(COPD ~ serum_selenium + age + gender + BMI + sedentary_time + smoking + drinking + CVD + cancer + diabetes + hypertension,binomial()))
model_fit_s_fe <- with(dc_serum_fe,glm(COPD ~ serum_iron + age + gender + BMI + sedentary_time + smoking + drinking + CVD + cancer + diabetes + hypertension,binomial()))
findbest <- function(model_fit) {
  glance_df <- summary(model_fit, type = "glance")
  
  min_aic_idx <- which.min(glance_df$AIC)
  min_bic_idx <- which.min(glance_df$BIC)
  
  cat("=== AIC 最低 ===\n")
  cat("数据集：", min_aic_idx, "\n")
  cat("AIC：", round(glance_df$AIC[min_aic_idx], 2), "\n\n")
  
  cat("=== BIC 最低 ===\n")
  cat("数据集：", min_bic_idx, "\n")
  cat("BIC：", round(glance_df$BIC[min_bic_idx], 2), "\n")
  
  invisible(glance_df)
}

cat("========== 模型 1: model_fit_s (基线) ==========\n")
findbest(model_fit_s)
cat("\n\n========== 模型 2: model_fit_s_cu (血清铜) ==========\n")
findbest(model_fit_s_cu)
cat("\n\n========== 模型 3: model_fit_s_zn (血清锌) ==========\n")
findbest(model_fit_s_zn)
cat("\n\n========== 模型 4: model_fit_s_se (血清硒) ==========\n")
findbest(model_fit_s_se)
cat("\n\n========== 模型 5: model_fit_s_fe (血清铁) ==========\n")
findbest(model_fit_s_fe)

summary(model_fit_s,type="glance")#18
summary(model_fit_s_cu,type="glance")#9
summary(model_fit_s_zn,type="glance")#2
summary(model_fit_s_se,type="glance")#16
summary(model_fit_s_fe,type="glance")#14

# 20数据集
dc_serum_rds <- mg("dc_serum",save = T ,verbose = F)
dc_dietary_rds <- mg("dc_dietary",save = T ,verbose = F)

# 四分位数
run(name = "dc_serum",newname = "qt_serum",func = qt_serum)
run(name = "dc_serum_cu",newname = "qt_serum_cu",func = qt_serum_n)
run(name = "dc_serum_zn",newname = "qt_serum_zn",func = qt_serum_n)
run(name = "dc_serum_se",newname = "qt_serum_se",func = qt_serum_n)
run(name = "dc_serum_fe",newname = "qt_serum_fe",func = qt_serum_f)

qt_serum_zn_2$cu_zn<-round(qt_serum_zn_2$serum_copper/qt_serum_zn_2$serum_zinc,2)
qt_serum_zn_2$cu_znQ<-quant(qt_serum_zn_2$cu_zn,n=4,Q=T,round = 2)

#保存qt文件
write.csv(qt_serum_cu_9,file = "s_cu.csv")
write.csv(qt_serum_zn_2,file = "s_zn.csv")
write.csv(qt_serum_se_16,file = "s_se.csv")
write.csv(qt_serum_fe_14,file = "s_fe.csv")


# 铜
cu_non <- qt_serum_cu_9$serum_copper[qt_serum_cu_9$COPD == 0]
cu_copd <- qt_serum_cu_9$serum_copper[qt_serum_cu_9$COPD == 1]
max_len <- max(length(cu_non), length(cu_copd))
cu_out <- data.frame(
  non_COPD = c(cu_non, rep(NA, max_len - length(cu_non))),
  COPD     = c(cu_copd, rep(NA, max_len - length(cu_copd)))
)
write.xlsx(cu_out, "violin_copper.xlsx")

# 锌
zn_non <- qt_serum_zn_2$serum_zinc[qt_serum_zn_2$COPD == 0]
zn_copd <- qt_serum_zn_2$serum_zinc[qt_serum_zn_2$COPD == 1]
max_len <- max(length(zn_non), length(zn_copd))
zn_out <- data.frame(
  non_COPD = c(zn_non, rep(NA, max_len - length(zn_non))),
  COPD     = c(zn_copd, rep(NA, max_len - length(zn_copd)))
)
write.xlsx(zn_out, "violin_zinc.xlsx")

# 硒
se_non <- qt_serum_se_16$serum_selenium[qt_serum_se_16$COPD == 0]
se_copd <- qt_serum_se_16$serum_selenium[qt_serum_se_16$COPD == 1]
max_len <- max(length(se_non), length(se_copd))
se_out <- data.frame(
  non_COPD = c(se_non, rep(NA, max_len - length(se_non))),
  COPD     = c(se_copd, rep(NA, max_len - length(se_copd)))
)
write.xlsx(se_out, "violin_selenium.xlsx")

# 铁
fe_non <- qt_serum_fe_14$serum_iron[qt_serum_fe_14$COPD == 0]
fe_copd <- qt_serum_fe_14$serum_iron[qt_serum_fe_14$COPD == 1]
max_len <- max(length(fe_non), length(fe_copd))
fe_out <- data.frame(
  non_COPD = c(fe_non, rep(NA, max_len - length(fe_non))),
  COPD     = c(fe_copd, rep(NA, max_len - length(fe_copd)))
)
write.xlsx(fe_out, "violin_iron.xlsx")

cat("行数统计:\n")
cat("Copper:   non-COPD =", length(cu_non), " COPD =", length(cu_copd), "\n")
cat("Zinc:     non-COPD =", length(zn_non), " COPD =", length(zn_copd), "\n")
cat("Selenium: non-COPD =", length(se_non), " COPD =", length(se_copd), "\n")
cat("Iron:     non-COPD =", length(fe_non), " COPD =", length(fe_copd), "\n")

# 四分位数20数据集
qt_serum_rds <- mg("qt_serum")

# 加权
svy <- function(df,weights = ~WTMEC2YR){
  svydesign(
    id = ~sdmvpsu,
    strata = ~sdmvstra,
    weights = weights,
    data = df,
    nest = TRUE
  )
}

run(name = "qt_serum",newname = "serum_df",save = F,func = function(df)svy(df,weight=~fe_wt))
run(name = "qt_serum_cu",newname = "serum_cu_df",save = F,func = function(df)svy(df,weight=~serum_wt))
run(name = "qt_serum_zn",newname = "serum_zn_df",save = F,func = function(df)svy(df,weights=~serum_wt))
run(name = "qt_serum_se",newname = "serum_se_df",save = F,func = function(df)svy(df,weights=~serum_wt))
run(name = "qt_serum_fe",newname = "serum_fe_df",save = F,func = function(df)svy(df,weights=~fe_wt))

svy_tableone(design = serum_df_18,
             cv=c("serum_copper","serum_zinc","serum_selenium","serum_iron"),
             gv=c("serum_copperQ","serum_zincQ","serum_seleniumQ","serum_ironQ",
                  "gender","ageQ","BMIQ","sedentary_timeQ","drinkingQ","smoking","CVD","cancer","diabetes","hypertension"),
             by="COPD",
             total = T,
             xlsx = "baseline_serum.xlsx"
)

 # 逻辑回归
#s_cu_9
formula_m1 <- as.formula(paste("COPD ~", "serum_copperQ"))
formula_m2 <- as.formula(paste("COPD ~", "serum_copperQ", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ"))
formula_m3 <- as.formula(paste("COPD ~", "serum_copperQ", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ", 
                               "+ CVD + cancer + diabetes + hypertension"))

m1<-svyglm(formula_m1,
           family=quasibinomial(),
           design=serum_cu_df_9) 
reg_table(m1,
          xlsx = "cu_m1.xlsx"
) 
p4trend(m1,x="serum_copperQ")# "<0.001" 
m2<-svyglm(formula_m2,
           family=quasibinomial(),
           design=serum_cu_df_9) 
reg_table(m2,
          xlsx = "cu_m2.xlsx"
) 
p4trend(m2,x="serum_copperQ")# 0.002 
m3<-svyglm(formula_m3,
           family=quasibinomial(),
           design=serum_cu_df_9) 
reg_table(m3,
          xlsx = "cu_m3.xlsx"
) 
p4trend(m3,x="serum_copperQ")# 0.005 

#s_zn_2
formula_m1 <- as.formula(paste("COPD ~", "serum_zincQ"))
formula_m2 <- as.formula(paste("COPD ~", "serum_zincQ", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ"))
formula_m3 <- as.formula(paste("COPD ~", "serum_zincQ", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ", 
                               "+ CVD + cancer + diabetes + hypertension"))

m1<-svyglm(formula_m1,
           family=quasibinomial(),
           design=serum_zn_df_2) 
reg_table(m1,
          xlsx = "zn_m1.xlsx"
) 
p4trend(m1,x="serum_zincQ")# 0.003 
m2<-svyglm(formula_m2,
           family=quasibinomial(),
           design=serum_zn_df_2) 
reg_table(m2,
          xlsx = "zn_m2.xlsx"
) 
p4trend(m2,x="serum_zincQ")# 0.013 
m3<-svyglm(formula_m3,
           family=quasibinomial(),
           design=serum_zn_df_2) 
reg_table(m3,
          xlsx = "zn_m3.xlsx"
) 
p4trend(m3,x="serum_zincQ")# 0.014

#s_se_12
formula_m1 <- as.formula(paste("COPD ~", "serum_seleniumQ"))
formula_m2 <- as.formula(paste("COPD ~", "serum_seleniumQ", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ"))
formula_m3 <- as.formula(paste("COPD ~", "serum_seleniumQ", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ", 
                               "+ CVD + cancer + diabetes + hypertension"))

m1<-svyglm(formula_m1,
           family=quasibinomial(),
           design=serum_se_df_16) 
reg_table(m1,
          xlsx = "se_m1.xlsx"
) 
p4trend(m1,x="serum_seleniumQ") #0.489
m2<-svyglm(formula_m2,
           family=quasibinomial(),
           design=serum_se_df_16) 
reg_table(m2,
          xlsx = "se_m2.xlsx"
) 
p4trend(m2,x="serum_seleniumQ") #0.45
m3<-svyglm(formula_m3,
           family=quasibinomial(),
           design=serum_se_df_16) 
reg_table(m3,
          xlsx = "se_m3.xlsx"
) 
p4trend(m3,x="serum_seleniumQ") #0.36

#s_fe_14
formula_m1 <- as.formula(paste("COPD ~", "serum_ironQ"))
formula_m2 <- as.formula(paste("COPD ~", "serum_ironQ", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ"))
formula_m3 <- as.formula(paste("COPD ~", "serum_ironQ", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ", 
                               "+ CVD + cancer + diabetes + hypertension"))

m1<-svyglm(formula_m1,
           family=quasibinomial(),
           design=serum_fe_df_14) 
reg_table(m1,
          xlsx = "fe_m1.xlsx"
) 
p4trend(m1,x="serum_ironQ") #<0.0001
m2<-svyglm(formula_m2,
           family=quasibinomial(),
           design=serum_fe_df_14) 
reg_table(m2,
          xlsx = "fe_m2.xlsx"
) 
p4trend(m2,x="serum_ironQ") #<0.0001
m3<-svyglm(formula_m3,
           family=quasibinomial(),
           design=serum_fe_df_14) 
reg_table(m3,
          xlsx = "fe_m3.xlsx"
) 
p4trend(m3,x="serum_ironQ") #0.004


#连续变量逻辑回归
#roc
wtroc<-function(model = m3,data = qt_serum_cu_9,weight = qt_serum_cu_9$serum_wt,file = "s_cu.csv"){
  phat <- predict(model, newdata = data,type = "response")
  myaucw <- wauc(response.var = data$COPD, 
                 phat.var = phat,
                 weights.var = weight,
                 
  )
  print(myaucw)
  myrocw <- wroc(response.var = data$COPD, 
                 phat.var = phat,
                 weights.var = weight,
                 
  )
  wroc.plot(myrocw)
  tpr<-myrocw$wroc.curve$Sew.values
  fpr<-myrocw$wroc.curve$Spw.values
  roc<-data.frame(TPR=tpr,FPR=fpr)
  write.csv(roc,file = file)
}
#s_cu_9
formula_m1 <- as.formula(paste("COPD ~", "serum_copper"))
formula_m2 <- as.formula(paste("COPD ~", "serum_copper", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ"))
formula_m3 <- as.formula(paste("COPD ~", "serum_copper", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ", 
                               "+ CVD + cancer + diabetes + hypertension"))

m1<-svyglm(formula_m1,
           family=quasibinomial(),
           design=serum_cu_df_9) 
reg_table(m1,
          xlsx = "con_cu_m1.xlsx"
) 
m2<-svyglm(formula_m2,
           family=quasibinomial(),
           design=serum_cu_df_9) 
reg_table(m2,
          xlsx = "con_cu_m2.xlsx"
) 
m3<-svyglm(formula_m3,
           family=quasibinomial(),
           design=serum_cu_df_9) 
reg_table(m3,
          xlsx = "con_cu_m3.xlsx"
) 
wtroc(model = m1,data = qt_serum_cu_9,file = "roc_cu_1.csv")
wtroc(model = m2,data = qt_serum_cu_9,file = "roc_cu_2.csv")
wtroc(model = m3,data = qt_serum_cu_9,file = "roc_cu_3.csv")
#s_zn_2
formula_m1 <- as.formula(paste("COPD ~", "serum_zinc"))
formula_m2 <- as.formula(paste("COPD ~", "serum_zinc", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ"))
formula_m3 <- as.formula(paste("COPD ~", "serum_zinc", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ", 
                               "+ CVD + cancer + diabetes + hypertension"))

m1<-svyglm(formula_m1,
           family=quasibinomial(),
           design=serum_zn_df_2) 
reg_table(m1,
          xlsx = "con_zn_m1.xlsx"
) 
m2<-svyglm(formula_m2,
           family=quasibinomial(),
           design=serum_zn_df_2) 
reg_table(m2,
          xlsx = "con_zn_m2.xlsx"
) 
m3<-svyglm(formula_m3,
           family=quasibinomial(),
           design=serum_zn_df_2) 
reg_table(m3,
          xlsx = "con_zn_m3.xlsx"
) 
wtroc(model = m1,data = qt_serum_zn_2,weight = qt_serum_zn_2$serum_wt,file = "roc_zn_1.csv")
wtroc(model = m2,data = qt_serum_zn_2,weight = qt_serum_zn_2$serum_wt,file = "roc_zn_2.csv")
wtroc(model = m3,data = qt_serum_zn_2,weight = qt_serum_zn_2$serum_wt,file = "roc_zn_3.csv")
#s_se_16
formula_m1 <- as.formula(paste("COPD ~", "serum_selenium"))
formula_m2 <- as.formula(paste("COPD ~", "serum_selenium", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ"))
formula_m3 <- as.formula(paste("COPD ~", "serum_selenium", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ", 
                               "+ CVD + cancer + diabetes + hypertension"))

m1<-svyglm(formula_m1,
           family=quasibinomial(),
           design=serum_se_df_16) 
reg_table(m1,
          xlsx = "con_se_m1.xlsx"
) 
m2<-svyglm(formula_m2,
           family=quasibinomial(),
           design=serum_se_df_16) 
reg_table(m2,
          xlsx = "con_se_m2.xlsx"
) 
m3<-svyglm(formula_m3,
           family=quasibinomial(),
           design=serum_se_df_16) 
reg_table(m3,
          xlsx = "con_se_m3.xlsx"
) 
wtroc(model = m1,data = qt_serum_se_16,weight = qt_serum_se_16$serum_wt,file = "roc_se_1.csv")
wtroc(model = m2,data = qt_serum_se_16,weight = qt_serum_se_16$serum_wt,file = "roc_se_2.csv")
wtroc(model = m3,data = qt_serum_se_16,weight = qt_serum_se_16$serum_wt,file = "roc_se_3.csv")
#s_fe_14
formula_m1 <- as.formula(paste("COPD ~", "serum_iron"))
formula_m2 <- as.formula(paste("COPD ~", "serum_iron", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ"))
formula_m3 <- as.formula(paste("COPD ~", "serum_iron", 
                               "+ gender + ageQ + BMIQ + sedentary_timeQ + smoking + drinkingQ", 
                               "+ CVD + cancer + diabetes + hypertension"))

m1<-svyglm(formula_m1,
           family=quasibinomial(),
           design=serum_fe_df_14) 
reg_table(m1,
          xlsx = "con_fe_m1.xlsx"
) 
m2<-svyglm(formula_m2,
           family=quasibinomial(),
           design=serum_fe_df_14) 
reg_table(m2,
          xlsx = "con_fe_m2.xlsx"
) 
m3<-svyglm(formula_m3,
           family=quasibinomial(),
           design=serum_fe_df_14) 
reg_table(m3,
          xlsx = "con_fe_m3.xlsx"
) 
wtroc(model = m1,data = qt_serum_fe_14,weight = qt_serum_fe_14$fe_wt ,file = "roc_fe_1.csv")
wtroc(model = m2,data = qt_serum_fe_14,weight = qt_serum_fe_14$fe_wt ,file = "roc_fe_2.csv")
wtroc(model = m3,data = qt_serum_fe_14,weight = qt_serum_fe_14$fe_wt ,file = "roc_fe_3.csv")

# 创建汇总数据框
p4trend_summary <- data.frame(
  element = c(
    "血清铜", "血清锌", "血清硒", "血清铁"
  ),
  dataset = c(
    "serum_cu_df_9", "serum_zn_df_2", "serum_se_df_16", "serum_fe_df_14"
  ),
  p4trend_m1 = c(
    "<0.001", "0.003", "0.489", "<0.0001"
  ),
  p4trend_m2 = c(
    "0.002", "0.013", "0.45", "<0.0001"
  ),
  p4trend_m3 = c(
    "0.005", "0.014", "0.36", "0.004"
  ),
  stringsAsFactors = FALSE
)

write.xlsx(p4trend_summary,file = "p4trend_summary.xlsx")  

#rcs

#s_cu_9
r1<-svyglm(COPD~rcs(serum_copper,3),
           family=quasibinomial(),
           design=serum_cu_df_9)
rcs1<-RCS(r1,ref.zero = T,reference = "median")
rcs1$yhat<-exp(rcs1$yhat)
rcs1$lower<-exp(rcs1$lower)
rcs1$upper<-exp(rcs1$upper)
rcs_m1 <- ggplot(rcs1, color = "blue", xlab = "Serum copper (μg/dL)", ylab = "Odds ratio (95%CI)", vline = T, hline = 1) +
  annotate("text", x = 200, y = 4, 
           label = "P overall = 0.0001\nNL-Pvalue = 0.0029",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

print(rcs_m1)
ggsave("rcs_cu_m1.png",rcs_m1)

r2<-svyglm(COPD~rcs(serum_copper,3)+gender+age+BMI+sedentary_time+smoking+drinking,
           family=quasibinomial(),
           design=serum_cu_df_9)
rcs2<-RCS(r2,ref.zero = T,reference = "median")
rcs2$yhat<-exp(rcs2$yhat)
rcs2$lower<-exp(rcs2$lower)
rcs2$upper<-exp(rcs2$upper)
rcs_m2 <- ggplot(rcs2, color = "blue", xlab = "Serum copper (μg/dL)", ylab = "Odds ratio (95%CI)", vline = T, hline = 1) +
  annotate("text", x = 200, y = 20, 
           label = "P overall = < 0.0001\nNL-Pvalue = 0.1654",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

print(rcs_m2)
ggsave("rcs_cu_m2.png",rcs_m2)

r3<-svyglm(COPD~rcs(serum_copper,3)+gender+age+BMI+sedentary_time+smoking+drinking+CVD+cancer+diabetes+hypertension,
           family=quasibinomial(),
           design=serum_cu_df_9) 
rcs3<-RCS(r3,ref.zero = T,reference = "median")
rcs3$yhat<-exp(rcs3$yhat)
rcs3$lower<-exp(rcs3$lower)
rcs3$upper<-exp(rcs3$upper)
rcs_m3 <- ggplot(rcs3, color = "blue", xlab = "Serum copper (μg/dL)", ylab = "Odds ratio (95%CI)", vline = T, hline = 1) +
  annotate("text", x = 200, y = 17, 
           label = "P overall = 0.0001\nNL-Pvalue = 0.1585",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )
print(rcs_m3)
ggsave("rcs_cu_m3.png",rcs_m3)

#s_zn_2
r1 <- svyglm(COPD ~ rcs(serum_zinc, 3),
             family = quasibinomial(),
             design = serum_zn_df_2)
rcs1 <- RCS(r1, ref.zero = T, reference = "median")
rcs1$yhat <- exp(rcs1$yhat)
rcs1$lower <- exp(rcs1$lower)
rcs1$upper <- exp(rcs1$upper)
rcs_m1 <- ggplot(rcs1, color = "blue", 
                 xlab = "Serum zinc (μg/dL)", 
                 ylab = "Odds ratio (95%CI)", 
                 vline = T, hline = 1) +
  annotate("text", x = 75, y = 1.5, 
           label = "P overall = 0.0012\nNL-Pvalue = 0.0022",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

print(rcs_m1)
ggsave("rcs_zn_m1.png", rcs_m1)

r2 <- svyglm(COPD ~ rcs(serum_zinc, 3) + gender + age + BMI + sedentary_time + smoking + drinking,
             family = quasibinomial(),
             design = serum_zn_df_2)
rcs2 <- RCS(r2, ref.zero = T, reference = "median")
rcs2$yhat <- exp(rcs2$yhat)
rcs2$lower <- exp(rcs2$lower)
rcs2$upper <- exp(rcs2$upper)
rcs_m2 <- ggplot(rcs2, color = "blue", 
                 xlab = "Serum zinc (μg/dL)", 
                 ylab = "Odds ratio (95%CI)", 
                 vline = T, hline = 1)+
  annotate("text", x = 75, y = 1.5, 
           label = "P overall = 0.0007\nNL-Pvalue = 0.002",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )
print(rcs_m2)
ggsave("rcs_zn_m2.png", rcs_m2)

r3 <- svyglm(COPD ~ rcs(serum_zinc, 3) + gender + age + BMI + sedentary_time + smoking + drinking + CVD + cancer + diabetes + hypertension,
             family = quasibinomial(),
             design = serum_zn_df_2) 
rcs3 <- RCS(r3, ref.zero = T, reference = "median")
rcs3$yhat <- exp(rcs3$yhat)
rcs3$lower <- exp(rcs3$lower)
rcs3$upper <- exp(rcs3$upper)
rcs_m3 <- ggplot(rcs3, color = "blue", 
                 xlab = "Serum zinc (μg/dL)", 
                 ylab = "Odds ratio (95%CI)", 
                 vline = T, hline = 1)+
  annotate("text", x = 75, y = 1.5, 
           label = "P overall = 0.001\nNL-Pvalue = 0.0014",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )
print(rcs_m3)
ggsave("rcs_zn_m3.png", rcs_m3)

#s_se_16
r1 <- svyglm(COPD ~ rcs(serum_selenium, 3),
             family = quasibinomial(),
             design = serum_se_df_16)
rcs1 <- RCS(r1, ref.zero = T, reference = "median")
rcs1$yhat <- exp(rcs1$yhat)
rcs1$lower <- exp(rcs1$lower)
rcs1$upper <- exp(rcs1$upper)
rcs_m1 <- ggplot(rcs1, color = "blue", 
                 xlab = "Serum selenium (μg/dL)", 
                 ylab = "Odds ratio (95%CI)", 
                 vline = T, hline = 1)+
  annotate("text", x = 175, y = 5, 
           label = "P overall = 0.1345\nNL-Pvalue = 0.1094",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )
print(rcs_m1)
ggsave("rcs_se_m1.png", rcs_m1)

r2 <- svyglm(COPD ~ rcs(serum_selenium, 3) + gender + age + BMI + sedentary_time + smoking + drinking,
             family = quasibinomial(),
             design = serum_se_df_16)
rcs2 <- RCS(r2, ref.zero = T, reference = "median")
rcs2$yhat <- exp(rcs2$yhat)
rcs2$lower <- exp(rcs2$lower)
rcs2$upper <- exp(rcs2$upper)
rcs_m2 <- ggplot(rcs2, color = "blue", 
                 xlab = "Serum selenium (μg/dL)", 
                 ylab = "Odds ratio (95%CI)", 
                 vline = T, hline = 1)+
  annotate("text", x = 175, y = 3, 
           label = "P overall = 0.4079\nNL-Pvalue = 0.4492",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )
print(rcs_m2)
ggsave("rcs_se_m2.png", rcs_m2)

r3 <- svyglm(COPD ~ rcs(serum_selenium, 3) + gender + age + BMI + sedentary_time + smoking + drinking + CVD + cancer + diabetes + hypertension,
             family = quasibinomial(),
             design = serum_se_df_16) 
rcs3 <- RCS(r3, ref.zero = T, reference = "median")
rcs3$yhat <- exp(rcs3$yhat)
rcs3$lower <- exp(rcs3$lower)
rcs3$upper <- exp(rcs3$upper)
rcs_m3 <- ggplot(rcs3, color = "blue", 
                 xlab = "Serum selenium (μg/dL)", 
                 ylab = "Odds ratio (95%CI)", 
                 vline = T, hline = 1)+
  annotate("text", x = 175, y = 2.8, 
           label = "P overall = 0.6449\nNL-Pvalue = 0.957",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )
print(rcs_m3)
ggsave("rcs_se_m3.png", rcs_m3)

#s_fe_14
r1 <- svyglm(COPD ~ rcs(serum_iron, 3),
             family = quasibinomial(),
             design = serum_fe_df_14)
rcs1 <- RCS(r1, ref.zero = T, reference = "median")
rcs1$yhat <- exp(rcs1$yhat)
rcs1$lower <- exp(rcs1$lower)
rcs1$upper <- exp(rcs1$upper)
rcs_m1 <- ggplot(rcs1, color = "blue", 
                 xlab = "Serum iron (μg/dL)", 
                 ylab = "Odds ratio (95%CI)", 
                 vline = T, hline = 1)+
  annotate("text", x = 200, y = 2.75, 
           label = "P overall = < 0.0001\nNL-Pvalue = 0.411",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )
print(rcs_m1)
ggsave("rcs_fe_m1.png", rcs_m1)

r2 <- svyglm(COPD ~ rcs(serum_iron, 3) + gender + age + BMI + sedentary_time + smoking + drinking,
             family = quasibinomial(),
             design = serum_fe_df_14)
rcs2 <- RCS(r2, ref.zero = T, reference = "median")
rcs2$yhat <- exp(rcs2$yhat)
rcs2$lower <- exp(rcs2$lower)
rcs2$upper <- exp(rcs2$upper)
rcs_m2 <- ggplot(rcs2, color = "blue", 
                 xlab = "Serum iron (μg/dL)", 
                 ylab = "Odds ratio (95%CI)", 
                 vline = T, hline = 1)+
  annotate("text", x = 200, y = 3, 
           label = "P overall = < 0.0001\nNL-Pvalue = 0.003",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )
print(rcs_m2)
ggsave("rcs_fe_m2.png", rcs_m2)

r3 <- svyglm(COPD ~ rcs(serum_iron, 3) + gender + age + BMI + sedentary_time + smoking + drinking + CVD + cancer + diabetes + hypertension,
             family = quasibinomial(),
             design = serum_fe_df_14) 
rcs3 <- RCS(r3, ref.zero = T, reference = "median")
rcs3$yhat <- exp(rcs3$yhat)
rcs3$lower <- exp(rcs3$lower)
rcs3$upper <- exp(rcs3$upper)
rcs_m3 <- ggplot(rcs3, color = "blue", 
                 xlab = "Serum iron (μg/dL)", 
                 ylab = "Odds ratio (95%CI)", 
                 vline = T, hline = 1)+
  annotate("text", x = 200, y = 2.75, 
           label = "P overall = < 0.0001\nNL-Pvalue = 0.0346",
           hjust = 1.05, vjust = 1.5, size = 5, color = "black") +
  theme(
    text = element_text(family = "sans", face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )
print(rcs_m3)
ggsave("rcs_fe_m3.png", rcs_m3)

#中介分析
smdf <- function(mediation_obj) {
  # 获取summary
  summ <- summary(mediation_obj)
  
  # 从summary对象中提取
  results_df <- data.frame(
    Estimate = c(summ$d0, summ$z0, summ$tau.coef, summ$n0),
    CI_Lower = c(summ$d0.ci[1], summ$z0.ci[1], summ$tau.ci[1], summ$n0.ci[1]),
    CI_Upper = c(summ$d0.ci[2], summ$z0.ci[2], summ$tau.ci[2], summ$n0.ci[2]),
    p_value = c(summ$d0.p, summ$z0.p, summ$tau.p, summ$n0.p)
  )
  
  rownames(results_df) <- c("ACME", "ADE", "Total_Effect", "Prop_Mediated")
  
  return(results_df)
}

#cu
mid_cu<-drop_na(qt_serum_cu_9,wbc,monocyte)
mid_cu$sii<-mid_cu$neutrophil*mid_cu$platelet/mid_cu$lymphocyte
mid_cu$siri<-mid_cu$monocyte*mid_cu$neutrophil/mid_cu$lymphocyte

mx_sii = lm(sii ~ serum_copper,data = mid_cu)
yx_sii = lm(COPD ~ serum_copper + sii,data = mid_cu)

mx_siri = lm(siri ~ serum_copper,data = mid_cu)
yx_siri = lm(COPD ~ serum_copper + siri,data = mid_cu)

m_cu_sii<-mediate(model.m = mx_sii,
              model.y = yx_sii,
              treat = "serum_copper",#自变量
              mediator = "sii",#中介变量
              sims = 1000,
              boot = T
              )
summary(m_cu_sii)
plot(m_cu_sii)
m_cu_sii_df<-smdf(m_cu_sii)
write.csv(m_cu_sii_df,file = "mid_cu_sii.csv")

m_cu_siri<-mediate(model.m = mx_siri,
                  model.y = yx_siri,
                  treat = "serum_copper",#自变量
                  mediator = "siri",#中介变量
                  sims = 1000,
                  boot = T
)
summary(m_cu_siri)
plot(m_cu_siri)
m_cu_siri_df<-smdf(m_cu_siri)
write.csv(m_cu_siri_df,file = "mid_cu_siri.csv")

#zn
mid_zn <- drop_na(qt_serum_zn_2,wbc,monocyte)
mid_zn$sii <- mid_zn$neutrophil * mid_zn$platelet / mid_zn$lymphocyte
mid_zn$siri <- mid_zn$monocyte * mid_zn$neutrophil / mid_zn$lymphocyte

mx_sii = lm(sii ~ serum_zinc, data = mid_zn)
yx_sii = lm(COPD ~ serum_zinc + sii, data = mid_zn)

mx_siri = lm(siri ~ serum_zinc, data = mid_zn)
yx_siri = lm(COPD ~ serum_zinc + siri, data = mid_zn)

m_zn_sii <- mediate(model.m = mx_sii,
                    model.y = yx_sii,
                    treat = "serum_zinc",  # 自变量
                    mediator = "sii",      # 中介变量
                    sims = 1000,
                    boot = T
)
summary(m_zn_sii)
plot(m_zn_sii)
m_zn_sii_df <- smdf(m_zn_sii)
write.csv(m_zn_sii_df, file = "mid_zn_sii.csv")

m_zn_siri <- mediate(model.m = mx_siri,
                     model.y = yx_siri,
                     treat = "serum_zinc",  # 自变量
                     mediator = "siri",     # 中介变量
                     sims = 1000,
                     boot = T
)
summary(m_zn_siri)
plot(m_zn_siri)
m_zn_siri_df <- smdf(m_zn_siri)
write.csv(m_zn_siri_df, file = "mid_zn_siri.csv")

#se
mid_se<-drop_na(qt_serum_se_16,wbc,monocyte)
mid_se$sii <- mid_se$neutrophil * mid_se$platelet / mid_se$lymphocyte
mid_se$siri <- mid_se$monocyte * mid_se$neutrophil / mid_se$lymphocyte

mx_sii = lm(sii ~ serum_selenium, data = mid_se)
yx_sii = lm(COPD ~ serum_selenium + sii, data = mid_se)

mx_siri = lm(siri ~ serum_selenium, data = mid_se)
yx_siri = lm(COPD ~ serum_selenium + siri, data = mid_se)

m_se_sii <- mediate(model.m = mx_sii,
                    model.y = yx_sii,
                    treat = "serum_selenium",  # 自变量
                    mediator = "sii",          # 中介变量
                    sims = 1000,
                    boot = T
)
summary(m_se_sii)
plot(m_se_sii)
m_se_sii_df <- smdf(m_se_sii)
write.csv(m_se_sii_df, file = "mid_se_sii.csv")

m_se_siri <- mediate(model.m = mx_siri,
                     model.y = yx_siri,
                     treat = "serum_selenium",  # 自变量
                     mediator = "siri",         # 中介变量
                     sims = 1000,
                     boot = T
)
summary(m_se_siri)
plot(m_se_siri)
m_se_siri_df <- smdf(m_se_siri)
write.csv(m_se_siri_df, file = "mid_se_siri.csv")

#fe
mid_fe<-drop_na(qt_serum_fe_14,wbc,monocyte)
mid_fe$sii <- mid_fe$neutrophil * mid_fe$platelet / mid_fe$lymphocyte
mid_fe$siri <- mid_fe$monocyte * mid_fe$neutrophil / mid_fe$lymphocyte

mx_sii = lm(sii ~ serum_iron, data = mid_fe)
yx_sii = lm(COPD ~ serum_iron + sii, data = mid_fe)

mx_siri = lm(siri ~ serum_iron, data = mid_fe)
yx_siri = lm(COPD ~ serum_iron + siri, data = mid_fe)

m_fe_sii <- mediate(model.m = mx_sii,
                    model.y = yx_sii,
                    treat = "serum_iron",  # 自变量
                    mediator = "sii",      # 中介变量
                    sims = 1000,
                    boot = T
)
summary(m_fe_sii)
plot(m_fe_sii)
m_fe_sii_df <- smdf(m_fe_sii)
write.csv(m_fe_sii_df, file = "mid_fe_sii.csv")

m_fe_siri <- mediate(model.m = mx_siri,
                     model.y = yx_siri,
                     treat = "serum_iron",  # 自变量
                     mediator = "siri",     # 中介变量
                     sims = 1000,
                     boot = T
)
summary(m_fe_siri)
plot(m_fe_siri)
m_fe_siri_df <- smdf(m_fe_siri)
write.csv(m_fe_siri_df, file = "mid_fe_siri.csv")

#亚组分析
#s_cu
scu <- dc_serum_cu_9 %>%
  mutate(
    ageQ = case_when(
      age <60 ~ "<60",
      age >= 60 ~ "≥60",
      TRUE ~ NA_character_  # 处理缺失值
    )
  )
scu$ageQ<-factor(scu$ageQ,levels = c("<60","≥60"))
scu$serum_copperQ<-quant(scu$serum_copper,n=4,Q=T,round = 2)
sscu<-svy(scu,weight=~serum_wt)
stratum_model(
  object = sscu,
  y = "COPD",
  x = "serum_copperQ",
  stratum = c("ageQ","smoking"),
  round = 2,
  xlsx = "sub_cu.csv"
)

#s_zn
szn <- dc_serum_zn_2 %>%
  mutate(
    ageQ = case_when(
      age <60 ~ "<60",
      age >= 60 ~ "≥60",
      TRUE ~ NA_character_  # 处理缺失值
    )
  )
szn$ageQ<-factor(szn$ageQ,levels = c("<60","≥60"))
szn$serum_zincQ<-quant(szn$serum_zinc,n=4,Q=T,round = 2)
sszn<-svy(szn,weight=~serum_wt)
stratum_model(
  object = sszn,
  y = "COPD",
  x = "serum_zincQ",
  stratum = c("ageQ","smoking"),
  round = 2,
  xlsx = "sub_zn.csv"
)

#s_se
sse <- dc_serum_se_16 %>%
  mutate(
    ageQ = case_when(
      age <60 ~ "<60",
      age >= 60 ~ "≥60",
      TRUE ~ NA_character_  # 处理缺失值
    )
  )
sse$ageQ<-factor(sse$ageQ,levels = c("<60","≥60"))
sse$serum_seleniumQ<-quant(sse$serum_selenium,n=4,Q=T,round = 2)
ssse<-svy(sse,weight=~serum_wt)
stratum_model(
  object = ssse,
  y = "COPD",
  x = "serum_seleniumQ",
  stratum = c("ageQ","smoking"),
  round = 2,
  xlsx = "sub_se.csv"
)

#s_fe
sfe <- dc_serum_fe_14 %>%
  mutate(
    ageQ = case_when(
      age <60 ~ "<60",
      age >= 60 ~ "≥60",
      TRUE ~ NA_character_  # 处理缺失值
    )
  )
sfe$ageQ<-factor(sfe$ageQ,levels = c("<60","≥60"))
sfe$serum_ironQ<-quant(sfe$serum_iron,n=4,Q=T,round = 2)
ssfe<-svy(sfe,weight=~fe_wt)
stratum_model(
  object = ssfe,
  y = "COPD",
  x = "serum_ironQ",
  stratum = c("ageQ","smoking"),
  round = 2,
  xlsx = "sub_fe.csv"
)

#roc

# 完整的最佳截断点计算函数
cutoff <- function(roc_data, method = "youden", 
                                              sens_weight = 0.5, spec_weight = 0.5) {
  # 确保必要列存在
  required_cols <- c("threshold", "TPR", "FPR")
  if (!all(required_cols %in% names(roc_data))) {
    stop("ROC数据需要包含: threshold, TPR, FPR 列")
  }
  
  # 计算特异度
  roc_data$specificity <- 1 - roc_data$FPR
  
  if (method == "youden") {
    # 约登指数法
    roc_data$score <- roc_data$TPR + roc_data$specificity - 1
  } else if (method == "weighted") {
    # 加权评分法
    roc_data$score <- sens_weight * roc_data$TPR + spec_weight * roc_data$specificity
  } else if (method == "closest_topleft") {
    # 最接近左上角法
    roc_data$score <- sqrt((1 - roc_data$TPR)^2 + roc_data$FPR^2)
    # 找最小值
    best_idx <- which.min(roc_data$score)
  } else {
    stop("不支持的method，可选: youden, weighted, closest_topleft")
  }
  
  if (method != "closest_topleft") {
    best_idx <- which.max(roc_data$score)
  }
  
  best_row <- roc_data[best_idx, ]
  
  result <- list(
    optimal_threshold = best_row$threshold,
    sensitivity = best_row$TPR,
    specificity = best_row$specificity,
    FPR = best_row$FPR,
    method = method,
    score = best_row$score,
    all_thresholds = roc_data[, c("threshold", "TPR", "specificity", "FPR", "score")]
  )
  cat("约登指数法最佳阈值:", result$optimal_threshold, "\n")
  cat("灵敏度:", result$sensitivity, "\n")
  cat("特异度:", result$specificity, "\n")
}

rscu<-svy_roc(serum_cu_df_9,"serum_copper","COPD")
cutoff(attributes(rscu)[["roc"]], "youden")

rszn<-svy_roc(serum_zn_df_2,"serum_zinc","COPD")
cutoff(attributes(rszn)[["roc"]], "youden")

se<-serum_se_df_16
se[["variables"]][["COPD"]] <- case_when(
  se[["variables"]][["COPD"]] == 1 ~ 0,
  se[["variables"]][["COPD"]] == 0 ~ 1
)

rsse<-svy_roc(se,"serum_selenium","COPD")
cutoff(attributes(rsse)[["roc"]], "youden")


fe<-serum_fe_df_14
fe[["variables"]][["COPD"]] <- case_when(
  fe[["variables"]][["COPD"]] == 1 ~ 0,
  fe[["variables"]][["COPD"]] == 0 ~ 1
  
)
rsfe<-svy_roc(fe,"serum_iron","COPD")
cutoff(attributes(rsfe)[["roc"]], "youden")
svy_roc_plot(rsfe)

svy_roc_plot(rscu,rszn,rsse,rsfe,legend.names = c("Serum copper","Serum zinc","Serum selenium","Serum iron"))

ggsave("roc.png", width = 8, height = 6, dpi = 300)

roc_summary <- data.frame(
  Element = c("Serum copper", "Serum zinc", "Serum selenium", "Serum iron"),
  AUC = c(0.6248, 0.5734, 0.5355, 0.5827),
  Threshold = c(123.3, 80.6, 126.7, 77.0),
  Sensitivity = c(0.5484, 0.6641, 0.5620, 0.5772),
  Specificity = c(0.6540, 0.4972, 0.5533, 0.5675)
)
write.csv(roc_summary, file = "roc_summary.csv", row.names = FALSE)
# ========================= 1. 数据加载 =========================
# 使用单集分析选出的最佳插补集 (铜, #9, AIC最优)
df <- qt_serum_cu_9

# 11个特征: 血清铜 + 10个协变量
features <- c(
  "serum_copper",    # 血清铜 (μg/dL) — 核心暴露
  "age",             # 年龄 (岁)
  "gender",          # 性别 (1=男, 0=女)
  "BMI",             # 体重指数 (kg/m²)
  "smoking",         # 吸烟 (1=是, 0=否)
  "drinking",        # 饮酒频率 (次/周)
  "sedentary_time",  # 久坐时间 (分钟/天)
  "CVD",             # 心血管疾病 (1=有, 0=无)
  "diabetes",        # 糖尿病 (1=有, 0=无)
  "hypertension",    # 高血压 (1=有, 0=无)
  "cancer"           # 癌症 (1=有, 0=无)
)
target <- "COPD"       # 结局: 1=COPD, 0=非COPD

# 保留分析所需列
model_df <- df[, c(features, target, "serum_wt")]
model_df <- na.omit(model_df)  # 删除任何残留缺失
cat("样本量:", nrow(model_df), " COPD=", sum(model_df$COPD), "\n")

# ========================= 2. 加权重采样 =========================
# 按NHANES调查权重做bootstrap，稀有事件(COPD)获得更高采样概率
wts <- model_df$serum_wt / sum(model_df$serum_wt)
idx <- sample(1:nrow(model_df), size = nrow(model_df), replace = TRUE, prob = wts)
model_df <- model_df[idx, ]

# ========================= 3. 训练/测试划分 =========================
train_idx <- createDataPartition(model_df$COPD, p = 0.7, list = FALSE)
train_data <- model_df[train_idx, ]; test_data <- model_df[-train_idx, ]

X_train <- as.matrix(train_data[, features]); y_train <- train_data$COPD
X_test  <- as.matrix(test_data[, features]);  y_test  <- test_data$COPD

train_df <- as.data.frame(X_train); train_df$COPD <- y_train
test_df  <- as.data.frame(X_test);  test_df$COPD <- y_test

# 类别权重 (COPD患病率约3.3%, neg/pos≈24.8)
neg <- sum(y_train == 0); pos <- sum(y_train == 1)
scale_weight <- if (pos > 0) neg / pos else 1
cat("训练集:", nrow(train_data), " 测试集:", nrow(test_data),
    " 类别权重:", round(scale_weight, 1), "\n\n")

# 因子版标签 (caret/SVM/RF等需要)
y_train_factor <- factor(y_train, levels = c(1, 0), labels = c("COPD", "control"))
y_test_factor  <- factor(y_test,  levels = c(1, 0), labels = c("COPD", "control"))
temp_train <- train_df; temp_train$COPD <- y_train_factor
temp_test  <- test_df;  temp_test$COPD  <- y_test_factor

# caret训练控制 (5折CV, 以ROC为优化目标)
train_ctrl <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# ========================= 4. 评估函数 =========================
# 计算混淆矩阵 → 准确率/灵敏度/特异度/F1
calc_metrics <- function(prob, obs, cutoff) {
  pred_class <- as.integer(prob >= cutoff)
  cm <- table(factor(pred_class, levels = c(0, 1)),
              factor(obs, levels = c(0, 1)))
  tp <- ifelse(ncol(cm) >= 2 && nrow(cm) >= 2, cm[2, 2], 0)
  fp <- ifelse(nrow(cm) >= 2, cm[2, 1], 0)
  fn <- ifelse(ncol(cm) >= 2, cm[1, 2], 0)
  tn <- cm[1, 1]
  r  <- roc(obs, as.numeric(prob), quiet = TRUE)
  data.frame(
    AUC         = round(as.numeric(auc(r)), 4),
    Accuracy    = round((tp + tn) / sum(cm), 4),
    Sensitivity = round(tp / (tp + fn), 4),
    Specificity = round(tn / (tn + fp), 4),
    F1          = round(tp / (tp + 0.5 * (fp + fn)), 4),
    cutoff      = round(cutoff, 4)
  )
}

# 约登指数找最优分类阈值
find_cutoff <- function(prob_train, y_train) {
  r <- roc(y_train, as.numeric(prob_train), quiet = TRUE)
  coords(r, "best", best.method = "youden")$threshold[1]
}

# ========================= 5. 训练 10 种模型 =========================
cat("========== 训练10模型 ==========\n")
probs_train <- list()  # 训练集预测概率
probs_test  <- list()  # 测试集预测概率

# ---- [1] 逻辑回归 ----
cat("[1/10] Logistic\n")
glm_fit <- glm(COPD ~ ., data = train_df, family = binomial(),
               weights = ifelse(y_train == 1, scale_weight, 1))
probs_train[["Logistic"]] <- predict(glm_fit, train_df, type = "response")
probs_test[["Logistic"]]  <- predict(glm_fit, test_df,  type = "response")

# ---- [2] LASSO ----
cat("[2/10] LASSO\n")
wts_vec <- ifelse(y_train == 1, scale_weight, 1)
lasso_fit <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1,
                       weights = wts_vec, nfolds = 5)
probs_train[["LASSO"]] <- predict(lasso_fit, X_train, s = "lambda.min",
                                  type = "response")[, 1]
probs_test[["LASSO"]]  <- predict(lasso_fit, X_test,  s = "lambda.min",
                                  type = "response")[, 1]

# ---- [3] KNN (5折CV选k) ----
cat("[3/10] KNN\n")
knn_fit <- train(COPD ~ ., data = temp_train, method = "knn",
                 trControl = train_ctrl, metric = "ROC",
                 tuneGrid = expand.grid(k = seq(5, 25, 5)))
probs_train[["KNN"]] <- predict(knn_fit, temp_train, type = "prob")[, "COPD"]
probs_test[["KNN"]]  <- predict(knn_fit, temp_test,  type = "prob")[, "COPD"]

# ---- [4] SVM ----
cat("[4/10] SVM\n")
svm_fit <- svm(COPD ~ ., data = temp_train, probability = TRUE,
               class.weights = c("COPD" = scale_weight, "control" = 1))
probs_train[["SVM"]] <- attr(predict(svm_fit, temp_train,
                                      probability = TRUE), "probabilities")[, "COPD"]
probs_test[["SVM"]]  <- attr(predict(svm_fit, temp_test,
                                      probability = TRUE), "probabilities")[, "COPD"]

# ---- [5] 决策树 (CART) ----
cat("[5/10] CART\n")
rpart_fit <- rpart(COPD ~ ., data = temp_train, method = "class",
                   weights = ifelse(y_train == 1, scale_weight, 1))
probs_train[["CART"]] <- predict(rpart_fit, temp_train, type = "prob")[, "COPD"]
probs_test[["CART"]]  <- predict(rpart_fit, temp_test,  type = "prob")[, "COPD"]

# ---- [6] 随机森林 ----
cat("[6/10] Random Forest\n")
rf_fit <- randomForest(COPD ~ ., data = temp_train, ntree = 500,
                       classwt = c("COPD" = scale_weight, "control" = 1))
probs_train[["RF"]] <- predict(rf_fit, temp_train, type = "vote")[, "COPD"]
probs_test[["RF"]]  <- predict(rf_fit, temp_test,  type = "vote")[, "COPD"]

# ---- [7] GBM ----
cat("[7/10] GBM\n")
train_gbm <- train_df; train_gbm$COPD <- y_train
test_gbm  <- test_df;  test_gbm$COPD  <- y_test
gbm_fit <- gbm(COPD ~ ., data = train_gbm, distribution = "bernoulli",
               n.trees = 300, interaction.depth = 3, shrinkage = 0.03,
               cv.folds = 5, weights = ifelse(y_train == 1, scale_weight, 1))
n_trees <- gbm.perf(gbm_fit, plot.it = FALSE, method = "cv")
probs_train[["GBM"]] <- predict(gbm_fit, train_gbm, n.trees = n_trees,
                                type = "response")
probs_test[["GBM"]]  <- predict(gbm_fit, test_gbm,  n.trees = n_trees,
                                type = "response")

# ---- [8] XGBoost ----
cat("[8/10] XGBoost\n")
dtrain <- xgb.DMatrix(X_train, label = y_train)
dtest  <- xgb.DMatrix(X_test,  label = y_test)
xgb_params <- list(objective = "binary:logistic", eval_metric = "auc",
                   max_depth = 4, eta = 0.05,
                   scale_pos_weight = scale_weight)
xgb_fit <- xgb.train(params = xgb_params, data = dtrain,
                     nrounds = 300, verbose = 0)
probs_train[["XGBoost"]] <- predict(xgb_fit, dtrain)
probs_test[["XGBoost"]]  <- predict(xgb_fit, dtest)

# ---- [9] 朴素贝叶斯 ----
cat("[9/10] Naive Bayes\n")
nb_fit <- NaiveBayes(COPD ~ ., data = temp_train)
probs_train[["NB"]] <- predict(nb_fit, temp_train)$posterior[, "COPD"]
probs_test[["NB"]]  <- predict(nb_fit, temp_test)$posterior[, "COPD"]

# ---- [10] 神经网络 ----
cat("[10/10] Neural Net\n")
nnet_fit <- nnet(COPD ~ ., data = train_gbm, size = 5, decay = 0.01,
                 maxit = 300, trace = FALSE,
                 weights = ifelse(y_train == 1, scale_weight, 1))
probs_train[["NNet"]] <- as.numeric(predict(nnet_fit, train_gbm, type = "raw"))
probs_test[["NNet"]]  <- as.numeric(predict(nnet_fit, test_gbm,  type = "raw"))

# ========================= 6. 结果汇总 =========================
cat("\n========== 10模型对比 ==========\n")
results <- do.call(rbind, lapply(names(probs_test), function(nm) {
  ct <- find_cutoff(probs_train[[nm]], y_train)
  cbind(data.frame(Model = nm), calc_metrics(probs_test[[nm]], y_test, ct))
}))
results <- results[order(results$AUC, decreasing = TRUE), ]
rownames(results) <- NULL
print(results)

# 保存
write.csv(results, file.path(base_out, "model_comparison_single.csv"),
          row.names = FALSE)

# ========================= 7. ROC 曲线 =========================
cat("\n--- ROC 曲线 ---\n")

# 测试集 ROC
roc_data <- do.call(rbind, lapply(names(probs_test), function(m) {
  r <- roc(y_test, as.numeric(probs_test[[m]]), quiet = TRUE)
  data.frame(Model = m, FPR = 1 - r$specificities, TPR = r$sensitivities)
}))
auc_labels <- results[, c("Model", "AUC")]
auc_labels$Label <- paste0(auc_labels$Model, " (AUC=",
                           sprintf("%.3f", auc_labels$AUC), ")")

p_roc <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(linewidth = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_discrete(labels = setNames(auc_labels$Label, auc_labels$Model)) +
  labs(title = "ROC Curves — 10 Models (Single Best Dataset)",
       subtitle = "Serum Copper + 10 Covariates | NHANES 2013–2018",
       x = "1 – Specificity (FPR)", y = "Sensitivity (TPR)",
       color = "Model (AUC)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"))
ggsave(file.path(base_out, "roc_10models_single.png"), p_roc,
       width = 12, height = 8, dpi = 300)

# 训练集 ROC (检测过拟合)
roc_train <- do.call(rbind, lapply(names(probs_train), function(m) {
  r <- roc(y_train, as.numeric(probs_train[[m]]), quiet = TRUE)
  data.frame(Model = m, FPR = 1 - r$specificities, TPR = r$sensitivities)
}))
train_auc <- sapply(probs_train, function(p)
  as.numeric(auc(roc(y_train, as.numeric(p), quiet = TRUE))))
train_auc_df <- data.frame(Model = names(train_auc),
                           AUC = round(train_auc, 4))
train_auc_df$Label <- paste0(train_auc_df$Model, " (AUC=",
                             sprintf("%.3f", train_auc_df$AUC), ")")

p_train_roc <- ggplot(roc_train, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(linewidth = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_discrete(labels = setNames(train_auc_df$Label, train_auc_df$Model)) +
  labs(title = "Training ROC — 10 Models", x = "1 – Specificity", y = "Sensitivity") +
  theme_minimal(base_size = 12)
ggsave(file.path(base_out, "roc_10models_train.png"), p_train_roc,
       width = 12, height = 8, dpi = 300)

# ========================= 8. RF SHAP 可解释性 =========================
cat("\n========== RF SHAP ==========\n")

# 8.1 训练RF (同参数)
cat("训练随机森林 (ntree=500)...\n")
y_factor <- factor(y_train, levels = c(1, 0), labels = c("COPD", "control"))
rf_fit <- randomForest(x = X_train, y = y_factor, ntree = 500,
                       classwt = c("COPD" = scale_weight, "control" = 1))

# 8.2 计算SHAP值
cat("计算SHAP值 (nsim=30)...\n")
pred_fun <- function(object, newdata) {
  predict(object, newdata, type = "vote")[, "COPD"]
}
bg_sample <- sample(1:nrow(X_test), min(200, nrow(X_test)))
shap_rf <- fastshap::explain(
  rf_fit,
  X           = as.data.frame(X_test),
  pred_wrapper = pred_fun,
  nsim        = 30,
  adjust      = TRUE,
  bg_X        = as.data.frame(X_test[bg_sample, ])
)

# 8.3 特征重要性
shap_imp <- colMeans(abs(as.matrix(shap_rf)))
imp_df <- data.frame(
  Feature = names(shap_imp),
  SHAP    = round(as.numeric(shap_imp), 5)
)
imp_df <- imp_df[order(imp_df$SHAP, decreasing = TRUE), ]
cat("\n=== SHAP 特征重要性 (RF) ===\n")
print(imp_df)

# 柱状图
p_bar <- ggplot(imp_df, aes(x = reorder(Feature, SHAP), y = SHAP)) +
  geom_col(fill = "#E64B35", alpha = 0.85) +
  coord_flip() +
  labs(title = "Random Forest: SHAP Feature Importance",
       subtitle = "mean(|SHAP|) over all test samples",
       x = "", y = "mean(|SHAP|)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(base_out, "shap_rf_importance.png"), p_bar,
       width = 8, height = 6)

# 8.4 蜂群图
shap_long <- as.data.frame(shap_rf)
colnames(shap_long) <- colnames(X_test)
shap_long_df <- shap_long %>%
  pivot_longer(everything(), names_to = "Feature", values_to = "SHAP")
shap_long_df$Feature <- factor(shap_long_df$Feature,
                                levels = rev(imp_df$Feature))

p_bee <- ggplot(shap_long_df, aes(x = SHAP, y = Feature)) +
  ggforce::geom_sina(alpha = 0.3, size = 0.7, color = "#E64B35") +
  labs(title = "Random Forest: SHAP Bee Swarm",
       x = "SHAP value", y = "") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(base_out, "shap_rf_beeswarm.png"), p_bee,
       width = 8, height = 6)

# 8.5 血清铜依赖图
copper_idx <- which(colnames(shap_rf) == "serum_copper")
shap_copper <- data.frame(
  serum_copper = X_test[, "serum_copper"],
  SHAP         = as.numeric(shap_rf[, copper_idx])
)
p_dep <- ggplot(shap_copper, aes(x = serum_copper, y = SHAP)) +
  geom_point(alpha = 0.3, color = "#E64B35", size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "grey70") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "RF SHAP: Serum Copper Dependence Plot",
       x = "Serum copper (μg/dL)", y = "SHAP value") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(file.path(base_out, "shap_rf_dependence_copper.png"), p_dep,
       width = 8, height = 6)

# 8.6 瀑布图 (shapviz)
cat("构建瀑布图...\n")
baseline_rf <- mean(predict(rf_fit, X_test, type = "vote")[, "COPD"])
shp_rf <- shapviz(as.matrix(shap_rf), X = as.data.frame(X_test),
                  baseline = baseline_rf)

# COPD 患者面板
pos_idx <- which(y_test == 1)
n_show <- min(6, length(pos_idx))
wf_list <- list()
for (i in seq_len(n_show)) {
  prob <- predict(rf_fit, X_test[pos_idx[i], , drop = FALSE],
                  type = "vote")[, "COPD"]
  wf_list[[i]] <- sv_waterfall(
    shp_rf, row_id = pos_idx[i], max_display = 8,
    fill_colors = c("#d73027", "#4575b4")
  ) +
    labs(
      title = paste0("COPD Patient #", i, "  Pred = ", round(prob, 3)),
      subtitle = paste0(
        "Age = ", round(X_test[pos_idx[i], "age"]),
        "  Cu = ", round(X_test[pos_idx[i], "serum_copper"], 1), " μg/dL",
        "  Smoke = ", X_test[pos_idx[i], "smoking"],
        "  CVD = ", X_test[pos_idx[i], "CVD"]
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
}
p_wf <- wrap_plots(wf_list, ncol = 2) +
  plot_annotation(
    title = "Random Forest: SHAP Waterfall — COPD Patients",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
  )
ggsave(file.path(base_out, "shap_rf_waterfall_copd.png"), p_wf,
       width = 16, height = 4 * ceiling(n_show / 2), dpi = 300)


# 非COPD对照面板
neg_idx <- which(y_test == 0)
n_show_neg <- min(6, length(neg_idx))
wf_neg_list <- list()
for (i in seq_len(n_show_neg)) {
  prob <- predict(rf_fit, X_test[neg_idx[i], , drop = FALSE],
                  type = "vote")[, "COPD"]
  wf_neg_list[[i]] <- sv_waterfall(
    shp_rf, row_id = neg_idx[i], max_display = 8,
    fill_colors = c("#d73027", "#4575b4")
  ) +
    labs(
      title = paste0("Non-COPD Control #", i, "  Pred = ", round(prob, 3)),
      subtitle = paste0(
        "Age = ", round(X_test[neg_idx[i], "age"]),
        "  Cu = ", round(X_test[neg_idx[i], "serum_copper"], 1), " μg/dL",
        "  BMI = ", round(X_test[neg_idx[i], "BMI"], 1)
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
}
p_wf_neg <- wrap_plots(wf_neg_list, ncol = 2) +
  plot_annotation(
    title = "Random Forest: SHAP Waterfall — Non-COPD Controls",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
  )
ggsave(file.path(base_out, "shap_rf_waterfall_control.png"), p_wf_neg,
       width = 16, height = 10, dpi = 300)

# ========================= 9. 输出汇总 =========================
cat("\n========================================\n")
cat("单集ML完成！输出文件:\n")
cat("  model_comparison_single.csv       — 10模型AUC对比\n")
cat("  roc_10models_single.png           — 测试集ROC曲线\n")
cat("  roc_10models_train.png            — 训练集ROC曲线\n")
cat("  shap_rf_importance.png            — RF SHAP特征重要性\n")
cat("  shap_rf_beeswarm.png              — RF SHAP蜂群图\n")
cat("  shap_rf_waterfall_copd.png        — RF 瀑布图(COPD患者)\n")
cat("  shap_rf_dependence_copper.png     — 血清铜SHAP依赖图\n")
cat("路径:", base_out, "\n")
cat("========================================\n")
