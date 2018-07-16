#-----------------------------------------------------------------------------------------------------------------------
#
# d-fine's Calibration Routines for the DRC Engine 
#
#-----------------------------------------------------------------------------------------------------------------------
#
#****f* Calibration/select_SystematicRiskFactors
# 
# NAME
# PD_KDE_Plot
# 
# AUTHOR
# Rory Robinson DipABRSM(d-fine)
# 
# CREATION DATE
# 24.06.2018
# 
# DESCRIPTION
# This routine uses kernel density estimations as part of a univariate analysis to test the suitability of inidividual factors in credit models. 
# After calculation of the density kernels, a PD is generated for all suitable values of the assessed variable.
# A bootstrap analysis is performed in order to generate confidence intervals, and a binning performed based on the underlying variable for further
# comparison. The results are plotted.
#
# 
# SYNOPSIS
# TODO
# 
# INPUTS
# Data: table which is considered the bootstrap original sample
# StrUnivariateVar: the variable to be bootstrapped
# IntBootstrapReps: the number of repetitions in the bootstrap
# StrExportDirectory: the disk location where the plot will be exported to
# 
# RESULT
# A plot of the PD estimate with confidence intervals, binning averages and percentiles of sample distributions
#
# USES
# Plotting the KDE plots in the univariate analyses of underlying variables in the credit models
# 
#****
#-----------------------------------------------------------------------------------------------------------------------
#function inputs
StrUnivariateVar <- "rac_total_assets"   #NB This could be a sequence of variables
default_var <- "default_1y"
IntDataCount <- 200

#DfDemo <- read.csv("C:/Users/d80070/Downloads/sampleMortgageData/sampleMortgageData.csv")
DfDemo <- readRDS(paste0(StrImportDirectory,"/02_ValidationSamples/01_Output/", "prod_sample_corp.rds"))

#Rory DfDemo <- read.csv("C:/Users/d80082/Downloads/Desktop/01 - Projects/08_VTB I/06_Samples/sampleMortgageData.csv")
#make cut(s) on dataset
#DfDemo <- DfDemo[DfDemo[, "TOTALASSETS"] > 1e8 & !is.na(DfDemo[, "TOTALASSETS"]), ]

#DfDemo_d <- DfDemo[DfDemo[, "default_time"] == 1, ]
#DfDemo_d <- DfDemo_d[sample(nrow(DfDemo_d), IntDataCount/4), ]
#DfDemo_nd <- DfDemo[DfDemo[, "default_time"] == 0, ]
#DfDemo_nd <- DfDemo_nd[sample(nrow(DfDemo_nd), 3 * IntDataCount/4), ]
#DfDemo_f <- rbind(DfDemo_d, DfDemo_nd)
#Data_mat <- DfDemo_f[, StrUnivariateVar  ]
#DfDefaultFlag <- DfDemo_f[, "default_time" ]
#DfDataInput <- t(as.data.frame(rbind(Data_mat, DfDefaultFlag)))
DfDataInput <- DfDemo[,c(StrUnivariateVar, default_var) ] 
DfDataI = DfData[, DfData[2, ] == 0]
#colnames(DfDataInput) <- c(StrUnivariateVar, "DfDefaultFlag")

IntBootstrapReps <- 1000
StrExportDirectory <- "C:/Users/d80070/Downloads/sampleMortgageData/"


CalculatePDKDEPlot <- function(DfDataInput, StrUnivariateVar, default_var, IntBootstrapReps, StrExportDirectory) {
  
  DfData <- DfDataInput#[ c(StrUnivariateVar, default_var),]
  
  #min/max values for density estimations
  NumXMin <- min(DfData[1, ])
  NumXMax <- max(DfData[1, ])
  
  NumDataCount <- ncol(DfData)
  #Perform KDE on original sample for comparison purposes
  DfDataP = DfData[ DfData[,default_var ] == 0,]
  NumDataCount_p <- nrow(DfDataP)
  
  #default population
  DfDataNP = DfData[DfData[,default_var] ==1,]#DfData[, DfData[2, ] == 1]
  NumDataCount_np <- nrow(DfDataNP)
  
  #finding kde's for each population
  DfDataKDEp_raw = density(DfDataP[,1 ], from = NumXMin, to = NumXMax)
  DfDataKDEnp_raw = density(DfDataNP[,1], from = NumXMin, to = NumXMax)
  
  #Generate Simple Kernel Density Estimates using the default R function
  DfDataKDEp <- as.data.frame(cbind("x" = DfDataKDEp_raw$x, "p_y" = DfDataKDEp_raw$y))
  DfDataKDEnp <- as.data.frame(cbind("x" = DfDataKDEnp_raw$x, "np_y" = DfDataKDEnp_raw$y))
  
  #Merge together over common value range
  DfDataKDE <- merge(DfDataKDEp,DfDataKDEnp, by= c("x"))
  
  #Calculate PDs based on kernel densities
  DfDataPd <- ifelse(
    is.na(NumDataCount_np * DfDataKDE[, "np_y"] / ( NumDataCount_np * DfDataKDE[, "np_y"] + NumDataCount_p * DfDataKDE[, "p_y"] )), 
    1, 
    NumDataCount_np * DfDataKDE[, "np_y"] / ( NumDataCount_np * DfDataKDE[, "np_y"] + NumDataCount_p * DfDataKDE[, "p_y"] )
  )

  
  #---------------------------------------------------------------------
  
  #Bootstrap starts
  boot_results_all <- replicate(IntBootstrapReps,
                                { 
                                  #Sample with replacement, for the bootstrap from the
                                  #original dataset and save the resample to boot_results
                                  sample_columns <- sample(c(1:NumDataCount), replace=TRUE)
                                  bs <- DfData[ , sample_columns]
                                  
                                  #non-default population
                                  bs_p = bs[, bs[2, ] == 0]
                                  bs_count_p <- ncol(bs_p)
                                  
                                  #default population
                                  bs_np = bs[, bs[2, ] == 1]
                                  bs_count_np <- ncol(bs_np)
                                  
                                  #finding kde's for each population
                                  #TODO: perform the densities on the min/max values for each particular bootstrap
                                  bs_kde_p_raw = density(t(bs_p[1, ]), from = NumXMin, to = NumXMax)
                                  bs_kde_np_raw = density(t(bs_np[1, ]), from = NumXMin, to = NumXMax)
                                  
                                  #Generate Simple Kernel Density Estimates using the default R function
                                  bs_kde_p <- as.data.frame(cbind("x" = bs_kde_p_raw$x, "p_y" = bs_kde_p_raw$y))
                                  bs_kde_np <- as.data.frame(cbind("x" = bs_kde_np_raw$x, "np_y" = bs_kde_np_raw$y))
                                  
                                  #Merge together over common value range
                                  bs_kde <- merge(bs_kde_p,bs_kde_np, by= c("x"))
                                  
                                  #Calculate PDs based on kernel densities
                                  bs_pd <- ifelse(
                                    is.na(bs_count_np * bs_kde[, "np_y"] / (bs_count_np * bs_kde[, "np_y"] + bs_count_p * bs_kde[, "p_y"] )),
                                    1, 
                                    bs_count_np * bs_kde[, "np_y"] / (bs_count_np * bs_kde[, "np_y"] + bs_count_p * bs_kde[, "p_y"] )
                                  )
                                  
                                  #Return pd for each sample
                                  list(bs_pd, bs_kde_p[ , 2], bs_kde_np[ , 2])
                                  
                                })
  
  
  #------------------------------------------------------------------------------------
  #P Kernel 
  boot_results_kde_p <- do.call(rbind, boot_results_all[2,])
  
  #Apply the quantile function to the y coordinates to get the bounds 
  q50 <- apply(t(boot_results_kde_p), 1, quantile, c(0.5))
  Confi_int90 <- t(apply(t(boot_results_kde_p), 1, quantile, c(0.05,0.95)))
  Confi_int50 <- t(apply(t(boot_results_kde_p), 1, quantile, c(0.25,0.75)))
  
  #plot preparations
  plot_lines <- cbind(q50, Confi_int90, Confi_int50, DfDataKDEp[, 2])
  plot_lines <- plot_lines[, c(3,5,1,4,2, 6)]
  colnames(plot_lines) <- c("95%", "75%", "50%", "25%", "5", "initial")
  colours <- c("blue", "green", "red", "green", "blue", "black")
  
  #Plot curves
  #matplot(DfDataKDE$x,
  #        plot_lines,
  #        type = "l",
  #        lty = c(2, 2, 1, 3, 3, 1),
  #        lwd = 2,
  #        col = colours,
  #        main = 'KDE-Plot - P',
  #        xlab = 'Data values',
  #        ylab = "Density"
  #)
  
  #------------------------------------------------------------------------------------
  #NP Kernel 
  boot_results_kde_np <- do.call(rbind, boot_results_all[3,])
  
  #Apply the quantile function to the y coordinates to get the bounds 
  q50 <- apply(t(boot_results_kde_np), 1, quantile, c(0.5))
  Confi_int90 <- t(apply(t(boot_results_kde_np), 1, quantile, c(0.05,0.95)))
  Confi_int50 <- t(apply(t(boot_results_kde_np), 1, quantile, c(0.25,0.75)))
  
  #plot preparations
  plot_lines <- cbind(q50, Confi_int90, Confi_int50, DfDataKDEnp[, 2])
  plot_lines <- plot_lines[, c(3,5,1,4,2, 6)]
  colnames(plot_lines) <- c("95%", "75%", "50%", "25%", "5", "initial")
  colours <- c("blue", "green", "red", "green", "blue", "black")
  
  
  #Plot curves
  #matplot(DfDataKDE$x,
  #        plot_lines,
  #        type = "l",
  #        lty = c(2, 2, 1, 3, 3, 1),
  #        lwd = 2,
  #        col = colours,
  #        main = 'KDE-Plot - NP',
  #        xlab = 'Data values',
  #        ylab = "Density"
  #)
  
  #------------------------------------------------------------------------------------
  #PD estimates 
  boot_results <- do.call(rbind, boot_results_all[1,])
  
  #Apply the quantile function to the y coordinates to get the bounds 
  q50 <- apply(t(boot_results), 1, quantile, c(0.5))
  Confi_int90 <- t(apply(t(boot_results), 1, quantile, c(0.05,0.95)))
  Confi_int50 <- t(apply(t(boot_results), 1, quantile, c(0.25,0.75)))
  
  #plot preparations
  plot_lines <- cbind(q50, Confi_int90, Confi_int50, DfDataPd)
  plot_lines <- plot_lines[, c(3,5,1,4,2, 6)]
  colnames(plot_lines) <- c("95%", "75%", "50%", "25%", "5", "initial")
  colours <- c("blue", "green", "red", "green", "blue", "black")
  
  #Plot curves
  matplot(DfDataKDE$x,
          plot_lines,
          type = "l",
          lty = c(2, 2, 1, 3, 3, 1),
          lwd = 2,
          col = colours,
          main = 'KDE-Plot',
          xlab = StrUnivariateVar,
          ylab = "PD"
  )
  
  #Export as PDF
  png(paste(StrExportDirectory, "KDE-Plot-", StrUnivariateVar, ".png", sep=""), width=600, height=400)
  
  #Plot curves
  matplot(DfDataKDE$x,
          plot_lines,
          type = "l",
          lty = c(2, 2, 1, 3, 3, 1),
          lwd = 2,
          col = colours,
          main = 'KDE-Plot',
          xlab = StrUnivariateVar,
          ylab = "PD"
  )
  
  #---------------------------------------------------------------------
  #Incorporate the binning methodology now from the SAS code
  
  data_for_debug <- rbind(DfData, "bin_num" = round(14 * (1.0001 * DfData[StrUnivariateVar,] - NumXMin) / (NumXMax - NumXMin), 0) + 1)
  
  bin_counts <- aggregate(x = t(data_for_debug), by = list(t(data_for_debug["bin_num", ])), FUN = "length")[ , c(1,2)]
  bin_means <- aggregate(x = t(data_for_debug), by = list(t(data_for_debug["bin_num", ])), FUN = "mean")
  
  lib_boot_debug <- cbind(bin_counts, "counter" = round(bin_counts[,2] * bin_means[,3], 0), bin_means[, 2:3])
  colnames(lib_boot_debug) <- c("bin_num", "counter", "num_D", "bin_x", "bin_pd")
  
  bin_x <- lib_boot_debug[, "bin_x"]
  bin_pd <- lib_boot_debug[, "bin_pd"]
  
  points(bin_x, bin_pd, col = "black", pch = 4)
  
  #print(lib_boot_debug)
  
  #---------------------------------------------------------------------
  #Incorporate percentile strips for default and non-default populations
  
  quantiles_p <- quantile(t(DfData[StrUnivariateVar, DfData[2, ] == 0]), c(.025, .05, .1, .25, .75, .9, .95, .975))
  quantiles_np <- quantile(t(DfData[StrUnivariateVar, DfData[2, ] == 1]), c(.025, .05, .1, .25, .75, .9, .95, .975))
  
  abline(v = quantiles_p, col = "navy")
  abline(v = quantiles_np, col = "indianred4", lty=2)
  
  #print(quantiles_p)
  #print(quantiles_np)
  
  dev.off()
  
  return()
  
}

CalculatePDKDEPlot(DfDataInput, StrUnivariateVar, default_var, IntBootstrapReps, StrExportDirectory)