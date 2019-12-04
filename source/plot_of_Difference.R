library(tidyverse)
ぃ
Tourlist_old         <- read.csv("input/訪日外客数.csv", fileEncoding = "CP932") 
Tourlist             <- read.csv("input/訪日外客数_修正.csv", fileEncoding = "CP932") 


Tourlist$New_old     <- 'New'
Tourlist_old$New_old <- 'Old'

New_old_list <- Tourlist %>% 
  dplyr::bind_rows(Tourlist_old)

cols <- colnames(New_old_list)[-c(1,22)]
for(col in 1:length(cols)){
  png(filename = paste0('output/pic', cols[col],'.png'),width = 750, height = 750)
  plot(c(1:192),New_old_list[,col+1][New_old_list$New_old=='New'],
       ylim = c(0,max(New_old_list[,col+1][New_old_list$New_old=='New'])),
       type = 'b')
  par(new=T)
  plot(c(1:192),New_old_list[,col+1][New_old_list$New_old=='Old'],
       ylim = c(0, max(New_old_list[,col+1][New_old_list$New_old=='New'])),
       col = 'red',type = 'o')
  dev.off()
  diff_plot <- New_old_list[,col+1][New_old_list$New_old=='New'] - New_old_list[,col+1][New_old_list$New_old=='Old']
  png(filename = paste0('output/diff_plot_pic_', cols[col],'.png'))
  plot(x = New_old_list$年月[New_old_list$New_old=='New'], y =diff_plot)
  dev.off()
}
