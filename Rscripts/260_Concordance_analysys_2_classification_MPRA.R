
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
  
 
  #### READ Table_S5 ----
  
  Table_S5<-readRDS(file=opt$Table_S5)
  
  
  Table_S5$chr<-factor(Table_S5$chr,
                       levels=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11",
                                "chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21",
                                "chr22","chr23","chrX","chrY"), ordered=T)
  
  
  cat("Table_S5_0\n")
  cat(str(Table_S5))
  cat("\n")
  cat(str(unique(Table_S5$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S5$ASSAY_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S5$ASSAY_CLASS))))
  cat("\n")
  
  #### deplete Allele and genomic_sequence----
  
  indx.dep<-c(which(colnames(Table_S5) == 'Allele'),which(colnames(Table_S5) == 'genomic_sequence'))
  
  Table_S5_depleted<-unique(Table_S5[,-indx.dep])
  
  
  cat("Table_S5_depleted_0\n")
  cat(str(Table_S5_depleted))
  cat("\n")
  cat(str(unique(Table_S5_depleted$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S5_depleted$ASSAY_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S5_depleted$ASSAY_CLASS))))
  cat("\n")
  

  #### Subset ----
  
  Table_S5_depleted$carried_to_VAR<-paste('chr',Table_S5_depleted$carried_variants,sep='')

  
  
  Table_S5_depleted_subset<-unique(droplevels(Table_S5_depleted[which(Table_S5_depleted$carried_to_VAR == Table_S5_depleted$VAR &
                                                                        Table_S5_depleted$ASSAY_CLASS == 'Screened variant'),]))
  
 
  
  
  cat("Table_S5_depleted_subset_0\n")
  cat(str(Table_S5_depleted_subset))
  cat("\n")
  cat(str(unique(Table_S5_depleted_subset$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S5_depleted_subset$ASSAY_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S5_depleted_subset$ASSAY_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S5_depleted_subset$Cell_Type)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S5_depleted_subset$Cell_Type))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S5_depleted_subset$TILE)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S5_depleted_subset$TILE))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S5_depleted_subset$Per_tile_experimental_class)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S5_depleted_subset$Per_tile_experimental_class))))
  cat("\n")
  
  ### Keep ASE ----
  
  Table_S5_depleted_subset_ASE<-droplevels(Table_S5_depleted_subset[which(Table_S5_depleted_subset$Per_tile_experimental_class%in%c('EA&ASE','ASE')),])
  
  
  cat("Table_S5_depleted_subset_ASE_0\n")
  cat(str(Table_S5_depleted_subset_ASE))
  cat("\n")
  cat(str(unique(Table_S5_depleted_subset_ASE$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S5_depleted_subset_ASE$ASSAY_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S5_depleted_subset_ASE$ASSAY_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S5_depleted_subset_ASE$Cell_Type)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S5_depleted_subset_ASE$Cell_Type))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S5_depleted_subset_ASE$TILE)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S5_depleted_subset_ASE$TILE))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S5_depleted_subset_ASE$Per_tile_experimental_class)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S5_depleted_subset_ASE$Per_tile_experimental_class))))
  cat("\n")
  
  Table_S5_depleted_subset_ASE$DIRECTIONALITY<-NA
  
  
  Table_S5_depleted_subset_ASE$DIRECTIONALITY[which(Table_S5_depleted_subset_ASE$ASE < 1)]<-'REF'
  Table_S5_depleted_subset_ASE$DIRECTIONALITY[which(Table_S5_depleted_subset_ASE$ASE > 1)]<-'ALT'
  
  Table_S5_depleted_subset_ASE$DIRECTIONALITY<-factor(Table_S5_depleted_subset_ASE$DIRECTIONALITY,
                                         levels = c('REF','ALT'),
                                         ordered=T)
  
  cat("Table_S5_depleted_subset_ASE_1\n")
  cat(str(Table_S5_depleted_subset_ASE))
  cat("\n")
  
  
  #### Now collapse the genes per variant and DIRECTIONALITY -----
  
  Table_S5_depleted_subset_ASE.dt<-data.table(Table_S5_depleted_subset_ASE, key=c("VAR","DIRECTIONALITY"))
  
  # cat("Table_S5_depleted_subset_ASE.dt_0\n")
  # cat(str(Table_S5_depleted_subset_ASE.dt))
  # cat("\n")
  
  Table_S5_depleted_subset_ASE_collapsed<-as.data.frame(Table_S5_depleted_subset_ASE.dt[,.(string_TILE=paste(TILE, collapse=";"),
                                                                             string_Cell_Type=paste(Cell_Type, collapse=";")), by=key(Table_S5_depleted_subset_ASE.dt)], stringsAsFactors=F)
  
  
  cat("Table_S5_depleted_subset_ASE_collapsed_0\n")
  cat(str(Table_S5_depleted_subset_ASE_collapsed))
  cat("\n")
  cat(str(unique(Table_S5_depleted_subset_ASE_collapsed$VAR)))
  cat("\n")
  
  #### SAVE -------
  
  setwd(out)
  
  saveRDS(Table_S5_depleted_subset_ASE_collapsed, file="Table_S5_depleted_subset_ASE_collapsed_DIRECTIONALITY.rds")
  
  write.table(Table_S5_depleted_subset_ASE_collapsed, file="Table_S5_depleted_subset_ASE_collapsed_DIRECTIONALITY.tsv", sep="\t", quote=F, row.names = F)

 
 
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
    make_option(c("--Table_S5"), type="character", default=NULL, 
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