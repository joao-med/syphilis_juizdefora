library(tidyverse)
library(cowplot)
#Processing SPW data
tabela_gest <- 
  read.csv('gest_data.csv',
           encoding = 'UTF-8', 
           sep = ";") %>% 
  as.tibble() %>% 
  mutate(Year = Year %>% as.integer(),
         Categorys = key)

#building up the figures

theme_set(theme_classic())
tabela_gest_split <- split(tabela_gest, f=tabela_gest$class)

p1 <- tabela_gest_split$`Classificacao clinica` %>% 
  ggplot(aes (x = Year, y = value, color = Categorys))+
  geom_line(lwd= 1.2)+
  scale_colour_grey()+
  geom_point(size = 3, aes(shape = Categorys))+
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank())+
  ylab("Relative frequency in %")+
  guides(color= guide_legend(nrow=2, byrow=TRUE))+
  scale_x_continuous(
    breaks = c(2010:2019),
    labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))+
  scale_y_continuous(limits = c(0,90))

#repeating the process for the next figures
p2 <- p1 %+% tabela_gest_split$`Cor raca`
p3 <- p1 %+% tabela_gest_split$`Faixa etaria`
p4 <- p1 %+% tabela_gest_split$`Idade gestacional`
p5 <- p1 %+% tabela_gest_split$Escolaridade

# creating final visualization
title = 
  cowplot::ggdraw() + 
  cowplot::draw_label("Relative Frequency of Demographic Data of \n Syphilis in Pregnant Women Notifications by Year", size = 20)

plot_top = plot_grid(p3,
                     p4, 
                     labels = 'AUTO',
                     vjust =-6 ,
                     label_x = 0, 
                     label_y = 0)

plot_bottom = plot_grid(NULL,
                        p2, 
                        NULL, 
                        ncol=3, 
                        rel_widths=c(0.25,0.5,0.25), 
                        labels = 'C',
                        vjust = -6,
                        hjust = -15,
                        label_x = 0, 
                        label_y = 0)
plot_sifilis_gest <- 
  plot_grid(title, 
            plot_top, 
            plot_bottom , 
            ncol = 1, 
            rel_heights = c(0.2,1,1)) 
#saving

ggsave('syphilis_gest.tiff',dpi= 1200, 
       plot = plot_sifilis_gest,
       units = "px", 
       width = 12000,
       height = 11000)


#processing congenital syphilis data

tabela_cong <- 
  read.csv("cong_data.csv", 
           encoding = 'UTF-8', 
           sep = ",") %>% 
  as.tibble() %>% 
  mutate( Year = Ano %>% as.integer(),
          Categorys = key)
#building up the figures

theme_set(theme_classic())
tabela_cong_split <- split(tabela_cong, f=tabela_cong$class)

p1 <- tabela_cong_split$`Diagnostico final` %>% 
  ggplot(aes (x = Year , y = value, color = Categorys))+
  geom_line(lwd= 1.2)+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank())+
  guides(color= guide_legend(nrow=2, byrow=TRUE))+
  ylab ('Relative frequency in %')+
  geom_point( size = 2, aes(shape = Categorys))+
  scale_color_grey()+
  scale_x_continuous(
    breaks = c(2010:2019),
    labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))+
  scale_y_continuous(limits = c(0,100))
#repeating the process for the next figures

p2 <- p1 %+% tabela_cong_split$`Escolaridade da mae`
p3 <- p1 %+% tabela_cong_split$`Faixa etaria da mae`
p4 <- p1 %+% tabela_cong_split$`gestational age`
p5 <- p1 %+% tabela_cong_split$`Momento de diagnostico`
p6 <- p1 %+% tabela_cong_split$`Raca cor`
p7 <- p1 %+% tabela_cong_split$`Realizacao do pre natal`

# creating final visualization
title <-  
  cowplot::ggdraw() + 
  cowplot::draw_label("Relative Frequency of Demographic Data of \n Congenital Syphilis Notifications by Year", size = 20)
plot <- cowplot::plot_grid(p1,p3,p4,p5,p6,p7, 
                           ncol = 2, 
                           labels = 'AUTO',
                           vjust =-6 ,
                           label_x = 0, 
                           label_y = 0)

plot_sifilis_congenita <- plot_grid(title,
                                    plot, 
                                    ncol = 1,
                                    rel_heights = c(0.1,1))


#saving

ggsave('syphilis_cong.tiff',dpi= 1200, 
       plot = plot_sifilis_congenita,
       units = "px", 
       width = 14000,
       height = 12000)

cases_sex <- read.csv("figures_data//cases_per_sex-Base.csv", sep = ";", header = T) %>% 
  as.tibble() %>%
  # mutate(
  #   total_women_percentage = (Women + Pregnant.Women)/ (Women + Pregnant.Women + Men),
  #   total_women_percentage = total_women_percentage %>% round(digits = 2)) %>%  
  gather(category, 
         cases,
         Men, 
         Women, 
         Pregnant.Women,
         # -total_women_percentage,
         -Year) %>% 
  select(Year, cases, category)
cases_sex <- cases_sex %>% mutate(
  category = category %>% 
    str_replace("Pregnant.Women","Pregnant Women"))

cases_sex_percentage <- read.csv("figures_data//cases_per_sex-Base.csv", sep = ";", header = T) %>% as.tibble %>% 
  gather(percentage_cat,
         percentage,
         percentage_men,
         percentage_women,
         percentage_preg_women) %>% 
  select(Year, percentage_cat, percentage) %>% 
  select(-Year) %>% 
  mutate(
    percentage_cat = percentage_cat %>% 
      str_replace("percentage_men", "Men") %>% 
      str_replace("percentage_women", "Women") %>% 
      str_replace("percentage_preg_women", "Pregnant Women")
  )

cases_sex <- tibble(cases_sex, cases_sex_percentage) 

plot_sex <- 
  cases_sex %>% 
  ggplot(aes (x = Year, y = percentage, color = percentage_cat))+
  geom_line(lwd= 1.2)+
  geom_point(size = 3, aes(shape = percentage_cat), show.legend = T)+
  geom_text(aes(label = cases), vjust= 2, size= 3.5, check_overlap = T, show.legend = F)+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank())+
  ylab ('Relative frequency')+
  ggtitle("Relative Frequency of Reported Cases of Syphilis in Men, \n Pregnant Women and Women ")+
  scale_color_grey()+
  scale_x_continuous(
    breaks = c(2010:2019),
    labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))+
  scale_y_continuous(labels = scales::percent) +
  ggeasy::easy_center_title()



ggsave('relative_freq_sex.tiff',dpi= 1200, units = "px", height = 7100, width = 8000 )

cases_women <- read.csv("figures_data//cases_per_sex-Base_women.csv", sep = ";", header = T) %>% 
  as.tibble() 
plot_women <- cases_women %>% 
  ggplot(aes (x = Year, y = Percentage, color = Percentage_cat))+
  geom_line(lwd= 1.2)+
  geom_text(aes(label = cases), vjust= -2, size=3.5, check_overlap = T, show.legend = F)+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank())+
  ylab ('Relative frequency')+
  ggtitle("Relative Frequency of Reported Cases of Syphilis \n between Pregnant Women and Women ")+
  geom_point(size = 3, aes(shape = Percentage_cat))+
  scale_color_grey()+
  scale_x_continuous(
    breaks = c(2010:2019),
    labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))+
  scale_y_continuous(labels = scales::percent) +
  ggeasy::easy_center_title()


unite_sex_plot <- cowplot::plot_grid(plot_sex, plot_women,
                   ncol = 2, 
                   labels = "AUTO",
                   vjust =-6 ,
                   label_x = 0, 
                   label_y = 0)
ggsave('unite_sex_plot.tiff',
       plot = unite_sex_plot, 
       dpi= 1200, 
       width = 15000,
       height = 6150, 
       units = "px")

