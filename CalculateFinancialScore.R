

library(XLConnect)

xs <- readWorksheetFromFile("C:/Users/d80070/Documents/VTB/InputData/analysis_ratios_banks_internal_vs_external.xlsx", 
                            sheet="RESULT")

windsorise <- function(x, minval, maxval){
  x <- x*(x> minval &x<= maxval) + minval * (x<=minval) + maxval * (x>maxval)
  return(x)
}

logC <- function(x){
  x <- ifelse(x>=.0001, log10(x), -4)
  return(x)
}
 
calc_sbil1 <- function(x){
  x <- logC(x)
  x_w <- windsorise(x, minval =-2.7, maxval=-0.25)
  sbil1 <- (2.0386*x_w*x_w) + (5.6568 * x_w) - 0.3096
   return(sbil1)
}

calc_sbil3 <- function(x){
  x <- logC(x)
  x_w <- windsorise(x, minval =-1, maxval=0)
  sbil3 <- x_w
  return(sbil3)
}

calc_sbil7 <- function(x){
  x_w <- windsorise(x, minval =0, maxval=0.13)
  sbil7 <- (460.4030*x_w*x_w) - 50.9863*x_w - 2.4907
  return(sbil7)
}


calc_sbil10 <- function(x){
  x_w <- windsorise(x, minval =0, maxval=0.5)
  sbil10 <- x_w
  return(sbil10)
}

calc_sbil12 <- function(x){
  
  x <- logC(x)
  x_w <- windsorise(x, minval =-2.2, maxval=-0.5)
  sbil12 <- (2.9576*x_w*x_w) + (8.3717*x_w) + 2.0953
  return(sbil12)
}

calc_sbil18 <- function(x){
  x <- logC(x)
  x_w <- windsorise(x, minval =0, maxval=1.1)
  sbil18 <- (4.3140*x_w*x_w) - 3.5359*x_w - 3.1069
  return(sbil18)
}

calc_sbil19 <- function(x){
  x <- logC(x)
  x_w <- windsorise(x, minval =3, maxval=10)
  sbil19 <- x_w
  return(sbil19)
}

#Winsorize (bil) & then log transform (sbil)
sbil1 <- sapply(xs$bil_1_prod, calc_sbil1)
#winsorize the log transforms
#sbil1w <- windsorise(sbil1, minval =-2.7, maxval=-0.25)

sbil3 <- sapply(xs$bil_3_prod, calc_sbil3)
#sbil3w <- windsorise(sbil3, minval =-1, maxval=0)

sbil7 <- sapply(xs$bil_7_prod, calc_sbil7)
#sbil7w <- windsorise(sbil7, minval =0, maxval=0.13)

sbil10 <- sapply(xs$bil_10_prod, calc_sbil10)
#sbil10w <- windsorise(sbil10, minval =0, maxval=0.5)

sbil12 <- sapply(xs$bil_12_prod, calc_sbil12)
#sbil12w <- windsorise(sbil12, minval =-2.2, maxval=-0.5)

sbil18 <- sapply(xs$bil_18_prod, calc_sbil18)
#sbil18w <- windsorise(sbil18, minval =0, maxval=1.1)

sbil19 <-  sapply(xs$bil_19_prod, calc_sbil19)
#sbil19w <- windsorise(sbil19, minval =3, maxval=10)


df <- data.frame(sbil1, sbil3, sbil7, sbil10, sbil12, sbil18, sbil19)
v <- c(0.6426, 0.3285, 0.3635, -3.7152, 0.1938, 0.1403, -0.1218)

#score calculation
result <- as.matrix(df) %*% as.matrix(v)
scores <- result + 2.609

fin_PD <- 1/(1+exp(-scores))
pd_cons <- sapply(fin_PD, function(x) min(1, 1.33*1.11*x))

#plot the difference
hist(xs$rac_financialratingval-scores, breaks=50)






