#pie charts for brads pak choi data
library(tidyverse)
library(lme4)
library(readxl)
library(svglite)

setwd("C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Pakchoi/")

data<-read_excel('Eddy Pie chart short.xlsx',sheet='Pies data pak choi surveys')
unique(data$`New Inseck key name`)
unique(data$Farm_name)
unique(data$Season)
unique(data$`Farm type`)

data<-read_excel('Eddy Pie chart short.xlsx',sheet='Pies data pak choi surveys')
brads_col_2<-read_excel('Eddy Pie chart short.xlsx',sheet='Colours')
head(brads_col_2)
head(data)

#there is a few duplicate rows getting rid of the counts here, summarizing across and summing counts
data<-data %>% group_by(`Farm type`,Farm_name,Season,Boundary_descriptor,`Insect key number`,`New Inseck key name`) %>% summarise(`Sum of Insects/100 flowers`=sum(`Sum of Insects/100 flowers`) )

data_per<-data%>% mutate(Boundary_year=paste0(Boundary_descriptor,'_',Season)) %>% select(-`Insect key number`,-Boundary_descriptor,-Season) %>% group_by(`Farm type`,Farm_name,Boundary_year,`New Inseck key name`) %>% summarise(total_count=sum(`Sum of Insects/100 flowers`)) %>% ungroup() %>% group_by(`Farm type`,Farm_name,Boundary_year) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()

 data_per %>%   ggplot(aes(x = Boundary_year, y = per, fill = `New Inseck key name`)) +
  geom_bar(position='stack',stat='identity',width=0.4) +
  facet_grid(`Farm type`~Farm_name, scales = "free", space = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  ggtitle(paste('test'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))  

 #make pies
 
data<- data %>% mutate(Boundary_year=paste0(Boundary_descriptor,'_',Season))

totals<-data %>% select(`Farm type`, Farm_name, Boundary_year, `New Inseck key name`,`Sum of Insects/100 flowers`) %>% group_by(`Farm type`, Farm_name, Boundary_year) %>% summarise(total=sum(`Sum of Insects/100 flowers`))


colnames(data)
data %>% select(-`Insect key number`) %>% group_by_all() %>% summarise(COUNT = n()) %>% nrow()
data %>% mutate(Boundary_year=paste0(Boundary_descriptor,'_',Season)) %>% select(`Farm type`, Farm_name, Boundary_year, `New Inseck key name`,`Sum of Insects/100 flowers`)%>% group_by_all() %>% summarise(COUNT = n()) %>% nrow()

mydf<-left_join(data,totals,by=c('Boundary_year'='Boundary_year','Farm_name'='Farm_name','Farm type'='Farm type'))

#relevel to brads order
brads_col_2$`New Inseck key name` <- factor(brads_col_2$`New Inseck key name`, levels=brads_col_2$`New Inseck key name`[order(brads_col_2$`Insect key number`)], ordered=TRUE)
#brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)
levels(brads_col_2$`New Inseck key name`)

#relevel to brads order
mydf$`New Inseck key name` <- forcats::fct_relevel(mydf$`New Inseck key name` ,levels(brads_col_2$`New Inseck key name`))
levels(mydf$`New Inseck key name` )



mydf %>% ggplot(aes(x = total/2, y = `Sum of Insects/100 flowers`, fill = as.factor(`New Inseck key name`), width = total)) +
   geom_bar(stat = "identity", position = "fill") +
   facet_grid( Farm_name~Boundary_year ) +
   coord_polar("y", start = 0, direction = -1) +
   theme_bw(base_size = 12) +
   theme(axis.title = element_blank(),
         strip.text.x = element_text(angle = 90,size=8,hjust=0),
         strip.text.y = element_text(angle = 0,size=8,hjust=0),
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         panel.grid = element_blank(),
         panel.border = element_blank(),
         legend.title = element_text(size = 6), 
         strip.background = element_rect(fill = NA, colour = NA))+
   #  strip.text = element_text(size = 4))+
   scale_fill_manual(values=with(brads_col_2,setNames(Brad_colours,`New Inseck key name`)))+
   #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
   ggtitle('Pak choi')+
   theme(
     legend.text = element_text(size = 6), 
     legend.title = element_text(size = 6),
     legend.key.size = unit(1,"line"))

#log +1
#logging straight doesnt work as not all 0's are recoreded (otherwise there would be 16K rows), so the +1 on those with a 0 biases things so clean them out first

#then we need to calculate back based on percentage and log the total
#need to calculate the percentage of each insect:
data_per<-data %>% select(-`Insect key number`,-Boundary_descriptor,-Season) %>% group_by(`Farm type`,Farm_name,Boundary_year,`New Inseck key name`) %>% summarise(total_count=sum(`Sum of Insects/100 flowers`)) %>% ungroup() %>% group_by(`Farm type`,Farm_name,Boundary_year) %>% mutate(per=total_count/sum(total_count)) %>% ungroup()

#log total
totals$total_log<-log(totals$total+1)

#total  x % - new fraction
mydf<-left_join(data_per,totals,by=c('Boundary_year'='Boundary_year','Farm_name'='Farm_name','Farm type'='Farm type'))

mydf$total_count_scaled<-mydf$per*mydf$total_log

mydf$`New Inseck key name` <- forcats::fct_relevel(mydf$`New Inseck key name` ,levels(brads_col_2$`New Inseck key name`))


mydf %>% ggplot(aes(x = total_log/2, y = total_count_scaled, fill = as.factor(`New Inseck key name`), width = total_log)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid( Farm_name~Boundary_year) +
  coord_polar("y", start = 0, direction = -1) +
  theme_bw(base_size = 12) +
  theme(axis.title = element_blank(),
        strip.text.x = element_text(angle = 90,size=8,hjust=0),
        strip.text.y = element_text(angle = 0,size=8,hjust=0),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.title = element_text(size = 6), 
        strip.background = element_rect(fill = NA, colour = NA))+
  #  strip.text = element_text(size = 4))+
  scale_fill_manual(values=with(brads_col_2,setNames(Brad_colours,`New Inseck key name`)))+
  #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
  ggtitle('Pak choi')+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))


#then this doesnt work either as you need to squareroot the total and work back
data2<-data %>% filter(`Sum of Insects/100 flowers`!=0)
data2<-data
data2$`Sum of Insects/100 flowers log`<-log(data2$`Sum of Insects/100 flowers`)
data2$`Sum of Insects/100 flowers sqrt`<-sqrt(data2$`Sum of Insects/100 flowers`)

totals2<-data2 %>% select(`Farm type`, Farm_name, Boundary_year, `New Inseck key name`,`Sum of Insects/100 flowers sqrt`) %>% group_by(`Farm type`, Farm_name, Boundary_year) %>% summarise(total=sum(`Sum of Insects/100 flowers sqrt`))
mydf2<-left_join(data2,totals2,by=c('Boundary_year'='Boundary_year','Farm_name'='Farm_name','Farm type'='Farm type'))
mydf2$`New Inseck key name` <- forcats::fct_relevel(mydf2$`New Inseck key name` ,levels(brads_col_2$`New Inseck key name`))

