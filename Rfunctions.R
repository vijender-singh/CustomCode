#Violinplot from dataframe

#Plots Violinplot from a dataframe

vlndf<- function(df){
  library(ggplot2)
  library(tidyverse)
  library(cowplot)
  p1<-df %>% gather(sample,counts) %>%
    ggplot(aes(sample,log10(counts+1),fill=factor(sample)))+
    geom_violin()
  p2<-df %>% gather(sample,counts) %>%
    ggplot(aes(sample,log(counts),fill=factor(sample)))+
    geom_violin()
  plot_grid(p1,p2)
}


#Converts Dataframe to IPA input
de2IPA<-function(csvResultFile){
  library(DescTools)
  df<-read.csv(csvResultFile,header=T)
  pickgeneColname<-colnames(df)[grep("gene",colnames(df),ignore.case = T)] # Picking coloumn with geneName nby mathing pattern
  df<-df[df$padj<0.05 ,]
  df<-df[df[[pickgeneColname]]!="" | !is.na(df[[pickgeneColname]]),] #checking that geneName exists, if not remove them
  df<-df[complete.cases(df),]
  outdf<-df[,c(pickgeneColname,"log2FoldChange")]
  sp<-DescTools::SplitPath(csvResultFile)
  OutFile<-paste0(sp$dirname,"IPA_",sp$fullfilename)
  write.csv(outdf,file=OutFile,row.names = FALSE)
  #return(df[df$padj<0.05,])
}
