library(ggplot2)
library(dplyr)
library(ggpubr)

##### F3A #####
list_data=lapply(1:3,function(i){
  data=read.xlsx('github data\\Figure3A data.xlsx',sheet = i)
  return(data)
})

x1=c("Non drinker","Past drinker","Current drinker",
      "Non smoker","Past smoker","Current smoker",
     "energy_kca1","Protein intake","Fat intake","Carbohydrate intake",
     'Normal sleep duration','Short sleep duration','Long sleep duration',"Snore sound","Sleep quality",
     "Sedentary behavior",'Light MET','Moderate MET','Heavy MET')

x2=c("Non drinker","Past drinker","Current drinker",
      "Non smoker","Past smoker","Current smoker",
     'Daily energy intake (per SD)',"Protein intake (per SD)","Fat intake (per SD)","Carbohydrate intake (per SD)",
     'Normal sleep duration','Short sleep duration','Long sleep duration',"Snore","Poor sleep quality",
     "Sedentary behavior",'Light MET','Moderate MET','Heavy MET')

re.plot=function(re.ALL,title){
  re.ALL$p_col=NA
  re.ALL$p_col[which(re.ALL$beta>=0 & re.ALL$pvalue<0.05)]='Positive effect (p<0.05)'
  re.ALL$p_col[which(re.ALL$beta>=0 & re.ALL$pvalue>=0.05)]='Positive effect (p>=0.05)'
  re.ALL$p_col[which(re.ALL$beta<0 & re.ALL$pvalue<0.05)]='Negtive effect (p<0.05)'
  re.ALL$p_col[which(re.ALL$beta<0 & re.ALL$pvalue>=0.05)]='Negtive effect (p>=0.05)'
  
  re.ALL=re.ALL[match(x1,re.ALL$variable),]
  re.ALL$variable=x2
  re.ALL$variable=factor(re.ALL$variable,levels = rev(x2))
  
  re.ALL$class <- ifelse(re.ALL$variable %in% c("Non drinker","Past drinker","Current drinker"), "Drinking alcohol",
                         ifelse(re.ALL$variable %in% c("Non smoker","Past smoker","Current smoker"), "Smoking",
                                ifelse(re.ALL$variable %in% c("Protein intake","Fat intake","Carbohydrate intake"), "Diet",
                                       ifelse(re.ALL$variable %in% c('Normal sleep duration','Short sleep duration','Long sleep duration',
                                                                     "Snore sound","Sleep quality"),"Sleep","Exercise"))))
  
  re.ALL$color=c(rep('#e7a40e',3),rep('#78bee5',3),rep('#1c6891',4),
                 rep('#a59d70',5),rep('#4f4a30',4))
  df1=re.ALL
  df1$class <- factor(df1$class, levels = c("Drinking alcohol","Smoking","Diet","Sleep","Exercise"))
 
  p=ggplot(df1,aes(x = beta,y = variable,xmin = min, xmax = max)) +
    geom_point(aes(color = p_col),size = 3) +
    geom_errorbarh(aes(color = p_col),height = 0,show.legend = F,size=1) +
    geom_vline(xintercept = 0,linetype="dashed") +
    xlab("") +
    ylab("") +
    ggtitle(title)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(1, 0.5, 1, 0.5), "cm"),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size = 12))+
    theme(strip.text = element_text(size = 10),
          axis.text.y = element_text(color = rev(df1$color)))+
    annotate('rect',
             ymin = c(0.5,4.5,9.5,13.5,16.5),
             ymax = c(4.5,9.5,13.5,16.5,19.5),
             xmin = -3.1,xmax = 5.5,alpha = 0.2,
             fill=rev(unique(df1$color)))+
    scale_color_manual(name = '',
                       values = c('Positive effect (p<0.05)'='#d55e00',
                                  'Positive effect (p>=0.05)'='#9C9C9C',
                                  'Negtive effect (p<0.05)'='#0072b2',
                                  'Negtive effect (p>=0.05)'='#9C9C9C'))+
    scale_x_continuous(limits = c(-3.1,5.5),breaks = seq(-3,5,by=1.5),
                       expand = c(0.01,0.01))
  print(p)
}

title = c('ImmuneAgeAccel','InnateAgeAccel','AdaptiveAgeAccel')
list_plot=lapply(1:3,function(i){
  data=list_data[[i]]
  p=re.plot(re.ALL=data,title=title[i])
  return(p)
})

##### F3B #####
list_data=lapply(1:3,function(i){
  data=read.xlsx('github data\\Figure3B data.xlsx',sheet = i)
  return(data)
})

score_plot=function(re.ALL,title){
  re.ALL$p_col=NA
  re.ALL$p_col[which(re.ALL$beta>=0 & re.ALL$pvalue<0.05)]='Positive effect (p<0.05)'
  re.ALL$p_col[which(re.ALL$beta>=0 & re.ALL$pvalue>=0.05)]='Positive effect (p>=0.05)'
  re.ALL$p_col[which(re.ALL$beta<0 & re.ALL$pvalue<0.05)]='Negtive effect (p<0.05)'
  re.ALL$p_col[which(re.ALL$beta<0 & re.ALL$pvalue>=0.05)]='Negtive effect (p>=0.05)'
  re.ALL$color=c(rep('#F8ECE4',3))
  re.ALL$variable=factor(re.ALL$variable,levels = rev(c("Favorable", "Intermediate", "Unfavorable")))
  
  df1=re.ALL
  p=ggplot(df1,aes(x = beta,y = variable,xmin = min, xmax = max)) +
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
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size = 12))+
    # geom_text(aes(color=p_col,label = ifelse(pvalue < 0.001, "***",ifelse(pvalue < 0.01, "**",ifelse(pvalue < 0.05, "*","")))), x = max(df1$max),
    #           hjust = -0, vjust = 0.8,show.legend = F,size=6)+
    theme(strip.text = element_text(size = 10),
          axis.text.y = element_text(color = 'black'))+
    annotate('rect',
             ymin = c(0.5),
             ymax = c(3.5),
             xmin = -1.5,xmax = 3,alpha = 0.5,#xmax = 2.5
             fill=rev(unique(df1$color)))+
    scale_color_manual(name = '',
                       values = c('Positive effect (p<0.05)'='#d55e00',
                                  'Positive effect (p>=0.05)'='#9C9C9C',
                                  'Negtive effect (p<0.05)'='#0072b2',##9C9C9C
                                  'Negtive effect (p>=0.05)'='#9C9C9C'))+
    scale_x_continuous(limits = c(-1.5,3),breaks = seq(-1,3,by=1),#limits = c(-1.2,2.5)
                       expand = c(0.01,0.01))+
    scale_y_discrete(expand = c(0.01,0.01))
  print(p)
}

title = c('ImmuneAgeAccel','InnateAgeAccel','AdaptiveAgeAccel')
list_plot=lapply(1:3,function(i){
  data=list_data[[i]]
  p=score_plot(re.ALL=data,title=title[i])
  return(p)
})


##### F3C #####
data = read.csv('github data\\Figure3C data.csv')
color = c("#F4E4D2","#EDAB98","#D54645")

p <- ggplot(data,aes(x=factor(group,levels=c("Decelerated","Normal","Accelerated")),y=percentage,fill=TotalScore))+
  geom_bar(stat = 'identity',width = 0.5,colour='black')+
  labs(x='',y='Percentage')+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=11))+
  theme_bw()+
  scale_y_continuous(breaks=seq(0,100,25),
                     labels=c('0','25%','50%','75%','100%'))+
  labs(fill="")+ 
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold", size = 12),  
    legend.text = element_text(color = "black", size = 12),
    axis.text.x   = element_text(color = 'black', size = 16, angle = 45,hjust=1),
    axis.text.y   = element_text(color = 'black', size = 12, angle = 0),
    axis.title.x  = element_text(color = 'black', size = 16, angle = 0),
    axis.title.y  = element_text(color = 'black', size = 16, angle = 90),
  )+scale_fill_manual(values = color)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
