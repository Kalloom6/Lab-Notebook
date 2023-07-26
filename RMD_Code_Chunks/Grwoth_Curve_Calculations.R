counts <- c(25, 26, 12, 15, 14, 6, 6, 6, 5, 3, 3, 6)
Cell_Line1 <- "N2"
Cell_Line2 <- "BLLW"

cl1 <- rep(Cell_Line1, times = 6)
cl2 <- rep(Cell_Line2, times = 6)
row1 <- c(rep(Cell_Line1, times = 6), rep(Cell_Line2, times = 6))
row2 <- c(rep("+ FDC", times = 3), rep("- FDC", times = 3), rep("+ FDC", times = 3), rep("- FDC", times = 3))
countx10 <- function(x){
  y <- (x * 4)*20000
  return(y)
}
cpmL <- countx10(counts)
Combo <- function(A,B,C,D){
  combo <- data.frame(A,B,C,D)
  return(combo)
}
table <- Combo(row1, row2, counts, cpmL)
colnames(table) <- c('Cell Line','FDC Status','Count', 'Cell Number')

library(dplyr)
library(kableExtra)
options(scipen = 1, digits = 2)

table %>%
  kable(.)
