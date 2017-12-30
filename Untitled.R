
#======通用开始=======

#设置平均值


rebuild <- function(list) {
  rebuild_c <- c()
  rebuild_index <- 1
  for (item in list) {
    for(index in 1:3) {
      rebuild_c[rebuild_index] <- item
      rebuild_index <- rebuild_index + 1
    }
  }
  return(rebuild_c)
}

yaqiang_mean_sd <- function(mean, sd) {
  ct_mean <- 0
  ct_sd <- 0
  while((ct_mean <= mean-0.005 | ct_mean >= mean+0.005) | (ct_sd < 0.6 | ct_sd > 2.3)) {
    ct <- rnorm(3,mean = mean,sd = sd)
    ct_mean <- mean(ct)
    ct_sd <- sd(ct)
  }
  
  ct[4] <- ct_mean
  ct[5] <- ct_sd
  
  return(ct)
}

#你输入的地方
input <- 1:32
if(length(input) != 32) {
  print("你input输错了")
} else {
  #处理平均值
  input <- round(input + 0.5658948545236, digits = 13);
  
  #设置标准差
  sd_list <- round(runif(32, min = 0.6, max = 2.3), digits = 13);
  #======通用结束=======
  
  #======生成ct表开始=======
  ct_c_show <- c();
  ct_index_show <- 1;
  input_index <- 1;
  
  input_show <- c();
  input_sd <- c();

  for(sd in sd_list) {
    every_cts <- round(yaqiang_mean_sd(mean = input[input_index], sd = sd), digits = 13);
    for (ct in every_cts[1:3]) {
      ct_c_show[ct_index_show] <- ct
      ct_index_show <- ct_index_show + 1;
    }
    
    input_show[input_index] <- every_cts[4]
    input_sd[input_index] <- every_cts[5]
    
    input_index <- input_index + 1
  }
  
  source_chat <- matrix(ct_c_show, nrow = 8, ncol = 12, byrow=TRUE)
  View(source_chat)
  #======生成ct表结束=======
  
  #======生成delta_ct表开始=======
  ct_m_option <- matrix(ct_c_show, nrow = 12);
  delta_ct_c <- c()
  delta_ct_index <- 1
  for (line in ct_m_option) {
    line_head_mean <- mean(line[1:3])
    for (item in line) {
      delta_ct_c[delta_ct_index] <- item - line_head_mean
      delta_ct_index <- delta_ct_index + 1
    }
  }
  ct_123_avg = mean(ct_c_show[1: 3]);
  delta_ct_c <- ct_c_show - ct_123_avg;
  delta_ct_chart <- matrix(delta_ct_c, nrow = 8, ncol = 12, byrow=TRUE)
  #======生成delta_ct表开始=======
  
  #设置一个每三个一组的矩阵
  delta_ct_m <- matrix(delta_ct_c, ncol = 3, byrow = TRUE)
  
  #======生成每组deltaCT的平均值，标准差，标准误 表开始=======
  delta_ct_mean_c <- c();
  delta_ct_sd_c <- c();
  delta_ct_se_c <- c();
  delta_ct_mss_index <- 1;
  
  for (index in 1:(length(ct_c_show) / 3)) {
    ct_delta_c <- delta_ct_m[index,]
    delta_ct_mean_c[delta_ct_mss_index] <- mean(ct_delta_c);
    delta_ct_sd_c[delta_ct_mss_index] <- sd(ct_delta_c);
    delta_ct_se_c[delta_ct_mss_index] <- sd(ct_delta_c) / sqrt(length(ct_delta_c));
    delta_ct_mss_index <- delta_ct_mss_index + 1;
  }
  
  delta_ct_mean_chart <- matrix(delta_ct_mean_c, nrow = 8, ncol = 4, byrow = TRUE);
  delta_ct_sd_chart <- matrix(delta_ct_sd_c, nrow = 8, ncol = 4, byrow = TRUE);
  delta_ct_se_chart <- matrix(delta_ct_se_c, nrow = 8, ncol = 4, byrow = TRUE);
  #======生成每组deltaCT的平均值，标准差，标准误 表结束=======
  
  #======生成(每行（1：3）delta_ct_mean) - 该行每一个delta_ct=======
  delta_ct_chart_copy <- delta_ct_chart
  delta_delta_ct_c <- c()
  delta_delta_ct_index <- 1
  for (index in 1:8) {
    delta_ct_chart_copy_every_line <- delta_ct_chart_copy[index,]
    delta_ct_chart_copy_every_line_delta_ct_avg <- mean(delta_ct_chart_copy_every_line[1:3])
    for(every_delta_delta_ct in (delta_ct_chart_copy_every_line - delta_ct_chart_copy_every_line_delta_ct_avg)) {
      delta_delta_ct_c[delta_delta_ct_index] <- every_delta_delta_ct
      delta_delta_ct_index <- delta_delta_ct_index + 1
    }
  }
  
  delta_delta_ct_chart <- matrix(delta_delta_ct_c, nrow = 8, byrow = TRUE)
  
  #======生成总表======
  ct <- ct_c_show #ct
  ct_mean <- rebuild(input_show) #ct_mean 平均值
  ct_sd <- rebuild(input_sd) #ct_sd 标准差
  
  delta_ct <- delta_ct_c
  delta_ct_mean <- rebuild(delta_ct_mean_c)
  delta_ct_sd <- rebuild(delta_ct_sd_c)
  delta_ct_se <- rebuild(delta_ct_se_c)
  delta_delta_ct <- delta_delta_ct_c
  delta_delta_frame <- round(data.frame(ct, ct_mean, ct_sd,
                                        delta_ct,
                                        delta_ct_mean, delta_ct_sd, delta_ct_se,
                                        delta_delta_ct), digits = 13)
  setwd("/Users/chenchangxing/Documents/R/deltaCT")
  write.table(delta_delta_frame,"delta_ct.csv",sep=",", quote = FALSE, row.names = FALSE,
              col.names = TRUE)
}
