library(ggplot2)
library(dplyr)
library(democracyData)
library(tidyr)
library(systemfonts)
library(ggrepel)
library(ggtext)
library(ggsave)
library(cowplot)


theme_set(theme_minimal(base_family = "Avenir Next Condensed"))
theme_update(
  axis.title = element_blank(),
  axis.text = element_text(color = "grey40"),
  axis.text.x = element_text(size = 16, margin = margin(t = 5)),
  axis.text.y = element_text(size = 15, margin = margin(r = 5)),
  axis.ticks = element_line(color = "grey91", size = .5),
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  panel.grid = element_blank(),
  plot.margin = margin(20, 40, 20, 40),
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  plot.title = element_text(color = "grey10", size = 30, face = "bold",
                            margin = margin(t = 15)),
  plot.subtitle = element_markdown(color = "grey30", size = 14, 
                                   lineheight = 1.35,
                                   margin = margin(t = 15, b = 40)),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(color = "grey30", size = 12,
                              lineheight = 1.2, hjust = 0, 
                              margin = margin(t = 40)),
  legend.position = "top",
  legend.direction = "horizontal",
  legend.text = element_text(size = 12) 
  
)



## Data Preparation


data_fh <- download_fh(verbose = FALSE)

world <- aggregate(data_fh,
                   by = list(data_fh$year),
                   FUN = mean)

world$fh_country <- "world"

world$extended_country_name <- "world"

world$Group.1 <- NULL

hun_df <- filter(data_fh, fh_country == "Hungary")
hun_df <- select(hun_df, year, fh_total_reversed)
names(hun_df)[2] <- 'score_hun'

world_df <- select(world, year, fh_total_reversed)
names(world_df)[2] <- 'score_world'

new_df <- merge(hun_df, world_df, by = 'year')



## Plotting



p1 <- ggplot(data = new_df,
             aes(x = year)) + 
  
              geom_line(aes(y = score_hun,
                            color = "black")
             ) +
              geom_line(aes(y = score_world,
                            color = "lightgrey")
                        
             ) +
             labs(
               title = "Hungarian Democracy in Decline - Authoritarian Rule on the Rise?",
               subtitle = "With the fall of the Iron Curtain in 1989, Francis Fukuyama proclaimed the end of history. This refers to the triumph of liberal democracy as a form of rule. Since then,<br> the number of democratic states has steadily increased. However, counter-movements can also be observed in more recent times. In Hungary in particular, with its<br> Prime Minister Viktor Orbán, fundamental elements of liberal democracy are being curtailed.",
               caption = "Visualization by Alexander Hentschel | Data by Freedom House and Xavier Marquez's R Package democracyData | The chart shows the reversed values of the freedom in the world index"
               
             ) +
             scale_x_continuous(
             expand = c(0, 0),
             limits = c(1970, 2020), 
             breaks = seq(1970, 2020, by = 5)
             
             ) +
  
  
            scale_y_continuous(
              expand = c(0, 0),
              limits = c(0, 15),
              breaks = seq(1, 13, by = 3)
              
              ) +

  
             scale_color_identity(name = NULL,
                                  breaks = c("black", "lightgrey"),
                                  labels = c("Hungary", "Worldwide"),
                                  guide = "legend"
                                  
              ) +
  
              annotate("text", x = 2008, y = 14,
                label = "18th of April 2011: \n Adoption of Hungarys new constitution.",
                family = "Avenir Next Condensed",
                size = 3.3,
                color = "grey55",
                fontface = "bold.italic",
                lineheight = .85,
                hjust = 0
                
              ) +
              
              annotate(
                "curve", x = 2013, xend = 2012,
                y = 13, yend = 12,
                curvature = -.20,
                color = "grey55",
                size = .6,
                arrow = arrow(length = unit(0.09, "inches"),
                  type = "closed")
              ) +
  
              annotate("text", x = 2005, y = 5,
                label = "10th of April 2010: \n Fidesz wins 2/3 majority in parlamentary election.\n Viktor Orbán takes office as Prime Minister.",
                family = "Avenir Next Condensed",
                size = 3.3,
                color = "grey55",
                fontface = "bold.italic",
                lineheight = .85,
                hjust = 0
           
            ) +
  
          annotate(
          "curve", x = 2009, xend = 2010,
          y = 8, yend = 11.5,
          curvature = -.20,
          color = "grey55",
          size = .6,
          arrow = arrow(length = unit(0.09, "inches"),
                  type = "closed")
          
          
          ) +
  
          annotate("text", x = 1975, y = 14,
            label = "23rd of October 1989: \nHungary becomes a democratic parliamentary republic.",
            family = "Avenir Next Condensed",
            size = 3.3,
            color = "grey55",
            fontface = "bold.italic",
            lineheight = .85,
            hjust = 0
            
            ) +
  
          annotate(
          "curve", x = 1984, xend = 1989,
          y = 12, yend = 10,
          curvature = .20,
          color = "grey55",
          size = .6,
          arrow = arrow(length = unit(0.09, "inches"),
                  type = "closed"))
  
  
  
  
              
              
            
p1


