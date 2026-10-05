library(ggplot2)
library(openxlsx)
library(dplyr)
library(ggpubr)

##### F4A #####
data = read.csv('Figure4A data.csv', check.names = FALSE)

re.plot=function(re.ALL,maintitle){
  re.ALL$class=factor(re.ALL$class,levels = c('Glucose homeostasis','Body fat',
                                              'Lipoproteins','Blood pressure',
                                              "Inflammation",'Kidney function',"Liver function"))
  re.ALL$variable=factor(re.ALL$variable,levels = rev(c("Glucose", "Insulin", "C peptide", "Leptin", "HbA1c", "Glucagon", "HOMA-beta", "HOMA-IR", 
                                                        "BMI", "FMI", "BFP", "Body fat mass", "WC", "WHR", "VAT volume", 
                                                        "Triglycerides", "Total cholesterol", "HDL", "LDL", "ApoA-I", "ApoB", 
                                                        "SBP", "DBP", "PP","HS-CRP", "IL-6", "Creatinine", "eGFR", "Uric acid", "ALT", "AST", "GGT")))
  
  re.ALL$p_col=NA
  re.ALL$p_col[which(re.ALL$beta>=0 & re.ALL$pvalue<0.05)]='Positive effect(p<0.05)'
  re.ALL$p_col[which(re.ALL$beta>=0 & re.ALL$pvalue>=0.05)]='Positive effect(p>=0.05)'
  re.ALL$p_col[which(re.ALL$beta<0 & re.ALL$pvalue<0.05)]='Negtive effect(p<0.05)'
  re.ALL$p_col[which(re.ALL$beta<0 & re.ALL$pvalue>=0.05)]='Negtive effect(p>=0.05)'
  
  re.ALL$color=c(rep('#e7a40e',8),rep('#78bee5',7),rep('#1c6891',6),
                 rep('#a59d70',3),rep('#4f4a30',2),rep('#B395BD',3),rep('#FF99CC',3))
  df1=re.ALL
  
  p=ggplot(df1,aes(x = beta,y = variable,xmin = min, xmax = max)) +
    geom_point(aes(color = p_col),size = 3) +
    geom_errorbarh(aes(color = p_col),height = 0,show.legend = F,size=1) +
    geom_vline(xintercept = 0,linetype="dashed") +
    xlab("") +
    ylab("") +
    ggtitle(maintitle)+
    theme_bw()+
    theme(plot.title = element_text(size=14,hjust = 0.5, color = 'black'),
          panel.grid = element_blank(),
          panel.border = element_rect(color = 'black',size=1),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.4, 1, 0.05, 1), "cm"),
          axis.text.y = element_text(size = 11, color = 'black'),
          axis.title.x = element_text(size = 12, color = 'black'),
          axis.text.x = element_text(size = 12, color = 'black'))+
    # geom_text(aes(color=p_col,label = ifelse(pvalue < 0.001, "***",ifelse(pvalue < 0.01, "**",ifelse(pvalue < 0.05, "*","")))), x = max(df1$max),
    #           hjust = -0.5, vjust = 0.8,show.legend = F,size=6)+
    theme(strip.text = element_text(size = 10, color = 'black'),
          axis.text.y = element_text(color = rev(df1$color)))+
    annotate('rect',
             ymin = c(0.5,3.5,6.5,8.5,11.5,17.5,24.5),#8 6 8 3 3 3 2
             ymax = c(3.5,6.5,8.5,11.5,17.5,24.5,32.5),
             xmin = -1,xmax = 1.5,alpha = 0.2,
             fill=rev(unique(df1$color)))+
    scale_color_manual(name = '',
                       values = c('Positive effect(p<0.05)'='#d55e00',
                                  'Positive effect(p>=0.05)'='#9C9C9C',
                                  'Negtive effect(p<0.05)'='#0072b2',
                                  'Negtive effect(p>=0.05)'='#9C9C9C'))+
    scale_x_continuous(limits = c(-1,1.5),breaks = seq(-1,1.5,by=0.5),
                       expand = c(0.01,0.01))
  print(p)
  
  
}

title = c('ImmuneAgeAccel','InnateAgeAccel','AdaptiveAgeAccel')
list_plot=lapply(1:3, function(i) {
  dat = data[which(data$group == title[i]),]
  plot = re.plot(re.ALL = dat,maintitle = title[i])
  return(plot)
})

##### F4B #####
data = read.csv('Figure4B data.csv', check.names = FALSE)

score_plot=function(re.ALL,title){
  re.ALL$p_col=NA
  re.ALL$p_col[which(re.ALL$beta>=0 & re.ALL$pvalue<0.05)]='Positive effect (p<0.05)'
  re.ALL$p_col[which(re.ALL$beta>=0 & re.ALL$pvalue>=0.05)]='Positive effect (p>=0.05)'
  re.ALL$p_col[which(re.ALL$beta<0 & re.ALL$pvalue<0.05)]='Negtive effect (p<0.05)'
  re.ALL$p_col[which(re.ALL$beta<0 & re.ALL$pvalue>=0.05)]='Negtive effect (p>=0.05)'
  re.ALL$color=c(rep('#F9F1EC',3))
  re.ALL$variable=factor(re.ALL$variable,levels = rev(c("Normal", "Intermediate", "High")))
  
  df1=re.ALL
  p=ggplot(df1,aes(x = beta,y = variable,xmin = min, xmax = max)) +
    annotate('rect',
             ymin = c(0.5),
             ymax = c(3.5),
             xmin = -1.5,xmax = 6.2,alpha = 0.6,#xmax = 2.5
             fill=rev(unique(df1$color)))+
    geom_point(aes(color = p_col),size = 3) +
    geom_errorbarh(aes(color = p_col),height = 0,show.legend = F) +
    geom_vline(xintercept = 0,linetype="dashed") +
    xlab("") +
    ylab("") +
    ggtitle(title)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
          axis.text.y = element_text(size = 12,color = 'black'),
          axis.title.x = element_text(size = 12,color = 'black'),
          axis.text.x = element_text(size = 12,color = 'black'))+
    theme(strip.text = element_text(size = 10),
          axis.text.y = element_text(color = 'black'))+
    
    scale_color_manual(name = '',
                       values = c('Positive effect (p<0.05)'='#d55e00',
                                  'Positive effect (p>=0.05)'='#9C9C9C',
                                  'Negtive effect (p<0.05)'='#0072b2',
                                  'Negtive effect (p>=0.05)'='#9C9C9C'))+
    scale_x_continuous(limits = c(-1.5,6.2),breaks = seq(-1,6.2,by=2),
                       expand = c(0.01,0.01))+
    scale_y_discrete(expand = c(0.01,0.01))
  print(p)
}

title = c('ImmuneAgeAccel','InnateAgeAccel','AdaptiveAgeAccel')
list_plot=lapply(1:3, function(i) {
  dat = data[which(data$group == title[i]),]
  plot=score_plot(re.ALL=dat,title = title[i])
  return(plot)
})
list_plot[[2]]

##### F4C #####
data = read.csv('Figure4C data.csv')
color = c("#000000",'#BD4C4E','#DB9168')
p <- ggplot(data, aes(x=factor(c("Normal weight\n(n=657)",
                                "Overweight\n(n=241)",
                                "Obesity\n(n=75)"),
                              levels=c("Normal weight\n(n=657)",
                                       "Overweight\n(n=241)",
                                       "Obesity\n(n=75)")), y = beta, fill=variable)) +
  geom_bar(stat = "identity", width = 0.7,colour='black') +
  geom_errorbar(aes(ymin = beta - SE, ymax = beta + SE), width = 0.2) +
  labs(title = "Body mass index (BMI)", x = "", y = "ImmuneAgeAccel (years)") +
  theme_minimal()+
  theme_bw()+
  theme(legend.position="none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,size=18),
        axis.text.x   = element_text(color = 'black', size = 13, angle = 0),
        axis.text.y   = element_text(color = 'black', size = 15, angle = 0),
        axis.title.x  = element_text(color = 'black', size = 15, angle = 0),
        axis.title.y  = element_text(color = 'black', size = 16, angle = 90),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
  )+scale_fill_manual(values = color)+
  ylim(-0.9,2.2)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))+
  geom_text(aes(label = ifelse(pvalue < 0.001, "***", ifelse(pvalue < 0.01, "**", ifelse(pvalue < 0.05, "*","")))), 
                   size = 6, color = "black", vjust = -3.5)
p

##### F4D #####
data = read.csv('Figure4D data.csv')
color = c("#FFFFBB","#FFCC33","#000000",'#DB9168','#BD4C4E')
p <- ggplot(data, aes(x=factor(variable,levels=c("Q1","Q2","Q3","Q4","Q5")), y = beta, fill=variable)) +
  geom_bar(stat = "identity", width = 0.7,colour='black') +
  geom_errorbar(aes(ymin = beta - SE, ymax = beta + SE), width = 0.2) +
  labs(title = 'Fat mass index (FMI)', x = "", y = "ImmuneAgeAccel (years)") +
  theme_bw()+
  theme_minimal()+
  theme_bw()+
  theme(legend.position="none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,size=18),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.text.x   = element_text(color = 'black', size = 15, angle = 0),
        axis.text.y   = element_text(color = 'black', size = 15, angle = 0),
        axis.title.x  = element_text(color = 'black', size = 15, angle = 0),
        axis.title.y  = element_text(color = 'black', size = 16, angle = 90),
  )+scale_fill_manual(values = color)+
  ylim(-0.9,2.2)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))+ 
  geom_text(aes(label = ifelse(pvalue < 0.001, "***", ifelse(pvalue < 0.01, "**", ifelse(pvalue < 0.05, "*","")))), 
                   size = 6, color = "black", vjust = -3.8)
p
