
suppressMessages(library("plyr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("data.table", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("crayon", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggplot2", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("farver", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("labeling", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("optparse", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("dplyr", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("backports", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("broom", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("rstudioapi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cli", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tzdb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggeasy", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("sandwich", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("digest", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tidyverse", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("RColorBrewer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cowplot", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)



data_wrangling = function(option_list)
{

  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")

  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("out_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  #### READ Table_S6 ----
  
  Table_S6<-readRDS(file=opt$Table_S6)
  
  
  cat("Table_S6_0\n")
  cat(str(Table_S6))
  cat("\n")
  cat(str(unique(Table_S6$VAR)))
  cat("\n")
 
  
  Table_S6_Double_Positives<-droplevels(Table_S6[which(Table_S6$Mechanistic_Class%in%c('DE','DE + ATU')),])
  
  
  cat("Table_S6_Double_Positives_0\n")
  cat(str(Table_S6_Double_Positives))
  cat("\n")
  cat(str(unique(Table_S6_Double_Positives$VAR)))
  cat("\n")
  
  Table_S6_Double_Positives<-Table_S6_Double_Positives[which(Table_S6_Double_Positives$MPRA_CLASS == 'MPRA positive'),]
  
  cat("Table_S6_Double_Positives_1\n")
  cat(str(Table_S6_Double_Positives))
  cat("\n")
  cat(str(unique(Table_S6_Double_Positives$VAR)))
  cat("\n")
    
    
  #### READ MPRA_DIRECTIONALITY ----
  
  MPRA_DIRECTIONALITY<-readRDS(file=opt$MPRA_DIRECTIONALITY)
  
  # colnames(MPRA_DIRECTIONALITY)[which(colnames(MPRA_DIRECTIONALITY) == 'DIRECTIONALITY')]<-'MPRA_DIRECTIONALITY'
  
  
  cat("MPRA_DIRECTIONALITY_0\n")
  cat(str(MPRA_DIRECTIONALITY))
  cat("\n")
  cat(str(unique(MPRA_DIRECTIONALITY$VAR)))
  cat("\n")
  
  
  #### READ DE_DIRECTIONALITY ----
  
  DE_DIRECTIONALITY<-readRDS(file=opt$DE_DIRECTIONALITY)
  
  # colnames(DE_DIRECTIONALITY)[which(colnames(DE_DIRECTIONALITY) == 'DIRECTIONALITY')]<-'DE_DIRECTIONALITY'
  
  
  cat("DE_DIRECTIONALITY_0\n")
  cat(str(DE_DIRECTIONALITY))
  cat("\n")
  cat(str(unique(DE_DIRECTIONALITY$VAR)))
  cat("\n")
  
  
  ### Merge DIRECTIONALITIES ----
  
  DE_DIRECTIONALITY<-merge(DE_DIRECTIONALITY,
                           MPRA_DIRECTIONALITY,
                           by=c('VAR','DIRECTIONALITY'),
                           all.x=T)
  
  
  
  cat("DE_DIRECTIONALITY_1\n")
  cat(str(DE_DIRECTIONALITY))
  cat("\n")
  cat(str(unique(DE_DIRECTIONALITY$VAR)))
  cat("\n")
  
  DE_DIRECTIONALITY$CONCORDANCE<-NA
  
  DE_DIRECTIONALITY$CONCORDANCE[!is.na(DE_DIRECTIONALITY$string_Cell_Type)]<-'CONCORDANT_MPRA_DE'
  
  indx.na<-which(is.na(DE_DIRECTIONALITY$string_Cell_Type) == TRUE)
  
  cat("indx.na_0\n")
  cat(str(indx.na))
  cat("\n")
  
  indx.VAR<-which(DE_DIRECTIONALITY$VAR%in%MPRA_DIRECTIONALITY$VAR)
  
  cat("indx.VAR_0\n")
  cat(str(indx.VAR))
  cat("\n")
  
  
  indx.overlap<-indx.na[which(indx.na%in%indx.VAR)]
  
  cat("indx.overlap_0\n")
  cat(str(indx.overlap))
  cat("\n")
  
  DE_DIRECTIONALITY$CONCORDANCE[indx.overlap]<-'DISCORDANT_MPRA_DE'
  
  DE_DIRECTIONALITY$CONCORDANCE<-factor(DE_DIRECTIONALITY$CONCORDANCE,
                                         levels = c('DISCORDANT_MPRA_DE','CONCORDANT_MPRA_DE'),
                                         ordered=T)
  
  DE_DIRECTIONALITY<-DE_DIRECTIONALITY[order(DE_DIRECTIONALITY$VAR,DE_DIRECTIONALITY$CONCORDANCE),]
  
  cat("DE_DIRECTIONALITY_0\n")
  cat(str(DE_DIRECTIONALITY))
  cat("\n")
  
  DE_DIRECTIONALITY_DP<-DE_DIRECTIONALITY[which(DE_DIRECTIONALITY$VAR%in%Table_S6_Double_Positives$VAR),]
  
  cat("DE_DIRECTIONALITY_DP_0\n")
  cat(str(DE_DIRECTIONALITY_DP))
  cat("\n")
  
 
  #### SAVE -------
  
  setwd(out)
  
  saveRDS(DE_DIRECTIONALITY, file="CONCORDANCE_RESULTS.rds")
  
  write.table(DE_DIRECTIONALITY, file="CONCORDANCE_RESULTS.tsv", sep="\t", quote=F, row.names = F)
  
  
  saveRDS(DE_DIRECTIONALITY_DP, file="CONCORDANCE_RESULTS_DP.rds")
  
  write.table(DE_DIRECTIONALITY_DP, file="CONCORDANCE_RESULTS_DP.tsv", sep="\t", quote=F, row.names = F)
  
  

 
 
}



printList = function(l, prefix = "    ") {
  list.df = data.frame(val_name = names(l), value = as.character(l))
  list_strs = apply(list.df, MARGIN = 1, FUN = function(x) { paste(x, collapse = " = ")})
  cat(paste(paste(paste0(prefix, list_strs), collapse = "\n"), "\n"))
}


#### main script ----

main = function() {
  cmd_line = commandArgs()
  cat("Command line:\n")
  cat(paste(gsub("--file=", "", cmd_line[4], fixed=T),
            paste(cmd_line[6:length(cmd_line)], collapse = " "),
            "\n\n"))
  option_list <- list(
    make_option(c("--Table_S6"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--MPRA_DIRECTIONALITY"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--DE_DIRECTIONALITY"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--type"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--out"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required.")
  )
  parser = OptionParser(usage = "140__Rscript_v106.R
                        --subset type
                        --TranscriptEXP FILE.txt
                        --cadd FILE.txt
                        --ncboost FILE.txt
                        --type type
                        --out filename",
                        option_list = option_list)
  opt <<- parse_args(parser)
  
  data_wrangling(opt)


  
  
}


###########################################################################

system.time( main() )