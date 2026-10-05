library(dplyr)
library(ggplot2)
library(ggpubr)

##### F5A #####
data = read.xlsx('Figure5 data.xlsx',sheet = 1)
color = c("#DDB77C","#C94C50","#000000","#8DB9CD")
p<- ggplot(data, aes(x=factor(variable,levels=data$variable), y = beta, fill=variable)) +
  geom_bar(stat = "identity", width = 0.8,colour='black') +
  geom_errorbar(aes(ymin = beta - SE, ymax = beta + SE), width = 0.2) +
  labs(title = "Metabolic Score", x = "", y = "ImmuneAgeAccel (years)") +
  theme_bw()+
  theme_classic()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),        
        plot.title  = element_text(color = 'black', size = 15,hjust=0.5),
        axis.text.x   = element_text(color = 'black', size = 13, angle = 30,hjust=1),
        axis.text.y   = element_text(color = 'black', size = 15, angle = 0),
        axis.title.x  = element_text(color = 'black', size = 15, angle = 0),
        axis.title.y  = element_text(color = 'black', size = 15, angle = 90),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = unit(c(3, 1, 0, 4.5), "cm"),
  )+scale_fill_manual(values = color)+
  ylim(-1,6)+
  geom_text(aes(label = ifelse(pvalue < 0.001, "***", ifelse(pvalue < 0.01, "**", ifelse(pvalue < 0.05, "*","")))), 
            size = 6, color = "black", vjust = -3.5)
p

##### F5B #####
data = read.xlsx('Figure5 data.xlsx',sheet = 2)
color = c("#000000","#8DB9CD","#DDB77C","#C94C50")
p <- ggplot(data, aes(x=factor(variable,levels=data$variable), y = beta, fill=variable)) +
  geom_bar(stat = "identity", width = 0.5,colour='black') +
  geom_errorbar(aes(ymin = beta - SE, ymax = beta + SE), width = 0.2) +
  labs(title = "BMI", x = "", y = "ImmuneAgeAccel (years)") +
  theme_bw()+
  theme_classic()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),        
        plot.title  = element_text(color = 'black', size = 15,hjust=0.5),
        axis.text.x   = element_text(color = 'black', size = 13, angle = 30,hjust=1),
        axis.text.y   = element_text(color = 'black', size = 15, angle = 0),
        axis.title.x  = element_text(color = 'black', size = 15, angle = 0),
        axis.title.y  = element_text(color = 'black', size = 15, angle = 90),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = unit(c(3, 1, 0, 5), "cm"),
  )+scale_fill_manual(values = color)+ylim(-1,6)+
  geom_text(aes(label = ifelse(pvalue < 0.001, "***", ifelse(pvalue < 0.01, "**", ifelse(pvalue < 0.05, "*","")))), 
            size = 6, color = "black", vjust = -3.5)
p

##### F5C-E #####
additive=function(data,x,title,vjust){
  p <- ggplot(data, aes(x=x, y = beta, fill=variable)) +
    geom_bar(stat = "identity", width = 0.8,colour='black') +
    geom_errorbar(aes(ymin = beta - SE, ymax = beta + SE), width = 0.2) +
    labs(title = title, x = "", y = "ImmuneAgeAccel (years)") +
    theme_bw()+
    theme_classic()+
    theme(legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),        
          plot.title  = element_text(color = 'black', size = 15,hjust=0.5),
          axis.text.x   = element_text(color = 'black', size = 13, angle = 30,hjust=1),
          axis.text.y   = element_text(color = 'black', size = 15, angle = 0),
          axis.title.x  = element_text(color = 'black', size = 15, angle = 0),
          axis.title.y  = element_text(color = 'black', size = 15, angle = 90),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          plot.margin = unit(c(3, 1, 0, 5), "cm"),
    )+
    scale_fill_manual(values = c("#DDB77C","#C94C50","#000000","#8DB9CD"))+
    ylim(-1,6)+
    geom_text(aes(label = ifelse(pvalue < 0.001, "***", ifelse(pvalue < 0.01, "**", ifelse(pvalue < 0.05, "*","")))), 
              size = 6, color = "black", vjust = vjust)
  print(p)
}

data = read.xlsx('Figure5 data.xlsx',sheet = 3)
plot.FMI=additive(data=data,x=factor(data$variable,levels = c(data$variable)),
                  title = 'FMI',vjust = -4)

data = read.xlsx('Figure5 data.xlsx',sheet = 4)
plot.VAT=additive(data=data,x=factor(data$variable,levels = c(data$variable)),
                  title = 'VAT',vjust = -4)

data = read.xlsx('Figure5 data.xlsx',sheet = 5)
plot.WC=additive(data=data,x=factor(data$variable,levels = c(data$variable)),
                 title = 'WC',vjust = -4)
