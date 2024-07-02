
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
suppressMessages(library("cowplot", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggupset", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("RColorBrewer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)



data_classification = function(option_list)
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
  
  Table_S6$chr<-gsub("_.+$","",Table_S6$VAR)
  Table_S6$pos<-gsub("^[^_]+_","",Table_S6$VAR)
  Table_S6$pos<-as.integer(gsub("_.+$","",Table_S6$pos))
  Table_S6$ref<-gsub("^[^_]+_[^_]+_","",Table_S6$VAR)
  Table_S6$ref<-gsub("_.+$","",Table_S6$ref)
  Table_S6$alt<-gsub("^[^_]+_[^_]+_[^_]+_","",Table_S6$VAR)
  
  Table_S6$chr<-factor(Table_S6$chr,
                       levels=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11",
                                "chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21",
                                "chr22","chr23","chrX","chrY"), ordered=T)
  
  
  cat("Table_S6_1\n")
  cat(str(Table_S6))
  cat("\n")
  cat(str(unique(Table_S6$chr)))
  cat("\n")
  cat(str(unique(Table_S6$pos)))
  cat("\n")
  cat(str(unique(Table_S6$ref)))
  cat("\n")
  cat(str(unique(Table_S6$alt)))
  cat("\n")
  
  indx.int<-c(which(colnames(Table_S6) == 'VAR'),which(colnames(Table_S6) == 'rs'),which(colnames(Table_S6) == 'chr'),which(colnames(Table_S6) == 'pos'),which(colnames(Table_S6) == 'ref'),which(colnames(Table_S6) == 'alt'))
  
  Table_S6_subset<-unique(Table_S6[,indx.int])
  
  cat("Table_S6_subset_0\n")
  cat(str(Table_S6_subset))
  cat("\n")
  cat(str(unique(Table_S6_subset$chr)))
  cat("\n")
  cat(str(unique(Table_S6_subset$pos)))
  cat("\n")
  cat(str(unique(Table_S6_subset$ref)))
  cat("\n")
  cat(str(unique(Table_S6_subset$alt)))
  cat("\n")
  
  #### READ Table_S7 ----
  
  Table_S7<-readRDS(file=opt$Table_S7)
  
  cat("Table_S7_0\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7$RNASeq_source))))
  cat("\n")
  
  Table_S7$Analysis[which(Table_S7$Analysis == 'DTU')]<-'ATU'
  
  Table_S7<-merge(Table_S6_subset,
                  Table_S7,
                  by=c('VAR','rs'))
  
  cat("Table_S7_1\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  
  Table_S7<-Table_S7[order(Table_S7$chr,Table_S7$pos),]
  
  rs_vector<-unique(Table_S7$rs)
  
  cat("Table_S7_1.5\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  cat("rs_vector_0\n")
  cat(str(rs_vector))
  cat("\n")
  
  
  
  Table_S7$RNASeq_source_2<-revalue(Table_S7$RNASeq_source, 
                                                               c("Whole blood"="Whole blood",
                                                                 "Monocyte"="Monocytes",
                                                                 "Neutrophil"="Neutrophils",
                                                                 "Tcell"="naive T-CD4 Cells"))
  
  Table_S7<-Table_S7[,-which(colnames(Table_S7) == 'RNASeq_source')]
  
  colnames(Table_S7)[which(colnames(Table_S7) == 'RNASeq_source_2')]<-'RNASeq_source'
  
  Table_S7$Analysis<-factor(Table_S7$Analysis,
                                                       levels = c('DE','ATU'),
                                                       ordered=T)
  
  Table_S7$RNASeq_source<-factor(Table_S7$RNASeq_source,
                                                              levels=c("Whole blood","Monocytes","Neutrophils","naive T-CD4 Cells"),
                                                              ordered=T)
  
  Table_S7$rs<-factor(Table_S7$rs,
                                 levels=rs_vector,
                                 ordered=T)
  
  cat("Table_S7_2\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7$RNASeq_source))))
  cat("\n")
  
  Table_S7$Significance<-NA
  
  Table_S7$Significance[which(Table_S7$adjusted_minus_logpval >= 1.3)]<-'YES'
  Table_S7$Significance[which(Table_S7$adjusted_minus_logpval < 1.3)]<-'NO'
  
  
  Table_S7$Significance<-factor(Table_S7$Significance,
                            levels = c('NO','YES'),
                            ordered=T)
  
  cat("Table_S7_3\n")
  cat(str(Table_S7))
  cat("\n")
  
  
  #### Subset results for DE significative ----
  
  Table_S7_DE<-droplevels(Table_S7[which(Table_S7$Analysis == "DE"),])
  
  cat("Table_S7_DE_0\n")
  cat(str(Table_S7_DE))
  cat("\n")
  cat(str(unique(Table_S7_DE$VAR)))
  cat("\n")
  cat(str(unique(Table_S7_DE$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_DE$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_DE$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_DE$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_DE$RNASeq_source))))
  cat("\n")
  
  Table_S7_DE_SIG<-droplevels(Table_S7_DE[which(Table_S7_DE$Significance == 'YES'),])
  
  cat("Table_S7_DE_SIG_0\n")
  cat(str(Table_S7_DE_SIG))
  cat("\n")
  cat(str(unique(Table_S7_DE_SIG$VAR)))
  cat("\n")
  cat(str(unique(Table_S7_DE_SIG$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_DE_SIG$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_DE_SIG$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_DE_SIG$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_DE_SIG$RNASeq_source))))
  cat("\n")
  
  
  Table_S7_DE_SIG$DIRECTIONALITY<-NA
  
  
  Table_S7_DE_SIG$DIRECTIONALITY[which(Table_S7_DE_SIG$Beta < 0)]<-'REF'
  Table_S7_DE_SIG$DIRECTIONALITY[which(Table_S7_DE_SIG$Beta > 0)]<-'ALT'
  
  Table_S7_DE_SIG$DIRECTIONALITY<-factor(Table_S7_DE_SIG$DIRECTIONALITY,
                                levels = c('REF','ALT'),
                                ordered=T)
  
  cat("Table_S7_DE_SIG_1\n")
  cat(str(Table_S7_DE_SIG))
  cat("\n")
  
  
  #### Now collapse the genes per variant and DIRECTIONALITY -----
  
  Table_S7_DE_SIG.dt<-data.table(Table_S7_DE_SIG, key=c("VAR","DIRECTIONALITY"))
  
  # cat("Table_S7_DE_SIG.dt_0\n")
  # cat(str(Table_S7_DE_SIG.dt))
  # cat("\n")
  
  Table_S7_DE_SIG_collapsed<-as.data.frame(Table_S7_DE_SIG.dt[,.(string_Symbol=paste(HGNC, collapse=";")), by=key(Table_S7_DE_SIG.dt)], stringsAsFactors=F)
  
  
  cat("Table_S7_DE_SIG_collapsed_0\n")
  cat(str(Table_S7_DE_SIG_collapsed))
  cat("\n")
  cat(str(unique(Table_S7_DE_SIG_collapsed$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_DE_SIG_collapsed$DIRECTIONALITY)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_DE_SIG_collapsed$DIRECTIONALITY))))
  cat("\n")
  
  #### SAVE -------
  
  setwd(out)
  
  saveRDS(Table_S7_DE_SIG_collapsed, file="Table_S7_DE_SIG_collapsed_DIRECTIONALITY.rds")
  
  write.table(Table_S7_DE_SIG_collapsed, file="Table_S7_DE_SIG_collapsed_DIRECTIONALITY.tsv", sep="\t", quote=F, row.names = F)
  

  
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
    make_option(c("--Table_S7"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Table_S6"), type="numeric", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--VAR_to_gene_body_correspondence"), type="character", default=NULL, 
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
  
  data_classification(opt)
    
  
}


###########################################################################

system.time( main() )