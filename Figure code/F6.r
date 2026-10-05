library(ggplot2)
library(gground)


##### F6 B&E #####
data = read.xlsx('github data\\Figure6BandE data.xlsx')

data$ONTOLOGY = factor(data$ONTOLOGY,
                               levels = c('BP', 'CC', 'MF'))
data$Description=factor(data$Description, 
                               levels = unique(data$Description[order(data$p.adjust)]))


pal=c('#E0C09C','#a1a9d0',"#CC9999",'#E0C09C','#a1a9d0',"#CC9999")

data$ONTOLOGY <- data$group
data$ONTOLOGY <- gsub('Protein_intake', 'Protein\nintake', data$ONTOLOGY)
data$ONTOLOGY <- gsub('Carbohydrate_intake', 'Carbohydrate intake', data$ONTOLOGY)
data$ONTOLOGY <- gsub('TotalScore', 'LifestyleScore', data$ONTOLOGY)

use_pathway <- data %>%
  subset(., p.adjust<0.05) %>% 
  group_by(ONTOLOGY) %>%
  slice_min(order_by = p.adjust, n = 10, with_ties = FALSE) %>%  
  group_by(p.adjust) %>%
  slice_max(order_by = Count, n = 10, with_ties = FALSE) %>%     
  ungroup() %>%
  arrange(ONTOLOGY, -p.adjust) %>%
  mutate(Description = factor(Description, levels = Description)) %>%
  tibble::rowid_to_column('index')

width <- 0.5
xaxis_max <- max(-log10(use_pathway$p.adjust)) + 1
rect.data <- group_by(use_pathway, ONTOLOGY) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  mutate(
    xmin = -1 * width,
    xmax = -0.3 * width,
    ymax = cumsum(n),
    ymin = lag(ymax, default = 0) + 0.6,
    ymax = ymax + 0.4
  )

use_pathway %>%
  ggplot(aes(-log10(p.adjust), y = index, fill = ONTOLOGY)) +
  geom_round_col(
    aes(y = Description), width = 0.7, alpha = 0.8
  ) +
  geom_text(
    aes(x = 0.05, label = Description),
    hjust = 0, size = 4.1
  ) +
  geom_round_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = ONTOLOGY),
    data = rect.data,
    radius = unit(2, 'mm'),
    inherit.aes = FALSE
  ) +
  geom_text(
    aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = ONTOLOGY),
    angle = 90,
    data = rect.data,
    inherit.aes = FALSE,
    lineheight = 0.9
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = xaxis_max, yend = 0),
    linewidth = 1.5,
    inherit.aes = FALSE
  ) +
  labs(y = NULL,x='-log10 (P)') +
  scale_fill_manual(name = 'Category', values = pal) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(
    breaks = seq(0, xaxis_max, 2), 
    expand = expansion(c(0, 0))
  ) +
  theme_prism() +
  theme(
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=10,colour = 'black'),
    axis.title.x = element_text(size=12,colour = 'black'), 
    legend.position = 'none',
    plot.margin = unit(c(0.2,0.5,0.2,0.5),'cm')
  )
