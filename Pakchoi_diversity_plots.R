library(tidyverse)
library(lme4)
library(readxl)
library(svglite)
library(vegan)

setwd("C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Pakchoi/")

data<-read_excel('Eddy Pie chart short.xlsx',sheet='Pies data pak choi surveys')
unique(data$`New Inseck key name`)
unique(data$Farm_name)
unique(data$Season)
unique(data$`Farm type`)

data<-read_excel('C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Pakchoi/Copy of Pak choi pollinators Heather analysis Brad update.xlsx',sheet='DataSheet')
brads_col_2<-read_excel('Eddy Pie chart short.xlsx',sheet='Colours')
head(brads_col_2)
head(data)
data_raw<-read_excel('Eddy Pie chart short.xlsx',sheet='Pies data pak choi surveys')


#tidy up some names etc

data$Boundary_descriptor<-gsub('one year old','One year old',data$Boundary_descriptor)
data$Boundary_descriptor<-gsub('two year old','Two year old',data$Boundary_descriptor)
data$Boundary_descriptor<-gsub('three year old','Three year old',data$Boundary_descriptor)
data$Boundary_descriptor<-gsub('Bare_fence','Bare fence',data$Boundary_descriptor)
data$Boundary_descriptor<-gsub('Control_farm','Control farm',data$Boundary_descriptor)


#summarising across etc
data<-data %>% select(-`Broad Insect category`,-Origin,-`Boundary age`) %>% group_by(`Farm type`,Farm_name,Season,Boundary_descriptor,`Insect key number`,`New Inseck key name`) %>% summarise(`Sum of Insects/100 flowers`=sum(`Sum of Insects/100 flowers`) )

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

#dversity statistics

#lets do

#bar chart relative abundances % bare fence, control year 1, year 2 year3 and old

mydf_byboundary<-mydf%>% select(-`Insect key number`,-Season,-`Farm type`,-Farm_name,-Boundary_year) %>% group_by(Boundary_descriptor,`New Inseck key name`) %>% summarise(total_count=sum(`Sum of Insects/100 flowers`)) %>% ungroup() %>% group_by(Boundary_descriptor) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()

mydf_byboundary %>%   ggplot(aes(x = Boundary_descriptor, y = per, fill = `New Inseck key name`)) +
  geom_bar(position='stack',stat='identity',width=0.4) +
 # facet_grid(`Farm type`~Farm_name, scales = "free", space = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  ggtitle(paste('test'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))  

#recolour and tidy
mydf_byboundary$Boundary_descriptor <- factor(mydf_byboundary$Boundary_descriptor,levels=c('Control farm','Bare fence','One year old','Two year old','Three year old','Old'),ordered = T)

mydf_byboundary %>%   ggplot(aes(x = Boundary_descriptor, y = per, fill = `New Inseck key name`)) +
  geom_bar(position='stack',stat='identity',width=0.4) +
  # facet_grid(`Farm type`~Farm_name, scales = "free", space = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  scale_fill_manual(values=with(brads_col_2,setNames(Brad_colours,`New Inseck key name`)))+
  ggtitle(paste('Relative abundance by boundary'))+ylab('Proportion')+xlab('Boundary type')+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))  


#adding in season 
#bar chart relative abundances % bare fence, control year 1, year 2 year3 and old

mydf_byboundary_yr<-mydf%>% select(-`Insect key number`,-Season,-`Farm type`,-Farm_name,-Boundary_descriptor) %>% group_by(Boundary_year,`New Inseck key name`) %>% summarise(total_count=sum(`Sum of Insects/100 flowers`)) %>% ungroup() %>% group_by(Boundary_year) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()

mydf_byboundary_yr<-mydf_byboundary_yr %>% separate_wider_delim(Boundary_year,'_',names=c("Boundary_descriptor","Season"))

#recolour and tidy
mydf_byboundary_yr$Boundary_descriptor <- factor(mydf_byboundary_yr$Boundary_descriptor,levels=c('Control farm','Bare fence','One year old','Two year old','Three year old','Old'),ordered = T)

mydf_byboundary_yr %>%   ggplot(aes(x = Boundary_descriptor, y = per, fill = `New Inseck key name`)) +
  geom_bar(position='stack',stat='identity',width=0.4) +
  facet_wrap(~Season,scales="free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  scale_fill_manual(values=with(brads_col_2,setNames(Brad_colours,`New Inseck key name`)))+
  ggtitle(paste('Relative abundance by boundary'))+ylab('Proportion')+xlab('Boundary type')+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))  



##bar chart raw abundances .....
#cant just be raw count
#needs to be average by sampling number

mydf_byboundary_farm_count<-mydf %>% select(-`Insect key number`,-`Farm type`,-Boundary_year) %>% mutate(unique_ob=paste(Boundary_descriptor,Farm_name,Season)) %>% select(-Farm_name,-Season) %>% group_by(Boundary_descriptor,`New Inseck key name`) %>% summarise(count_farms_perobs=sum(`Sum of Insects/100 flowers`)/n_distinct(unique_ob)) 
mydf_byboundary_farm_count$Boundary_descriptor <- factor(mydf_byboundary_farm_count$Boundary_descriptor,levels=c('Control farm','Bare fence','One year old','Two year old','Three year old','Old'),ordered = T)


mydf_byboundary_farm_count %>%   ggplot(aes(x = Boundary_descriptor, y = count_farms_perobs, fill = `New Inseck key name`)) +
  geom_bar(position='stack',stat='identity',width=0.4) +
  # facet_grid(`Farm type`~Farm_name, scales = "free", space = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  scale_fill_manual(values=with(brads_col_2,setNames(Brad_colours,`New Inseck key name`)))+
  ggtitle(paste('Counts by boundary'))+ylab('Average count per 100 flowers')+xlab('Boundary type')+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))  

#Adding in season
##bar chart raw abundances .....
#cant just be raw count
#needs to be average by sampling number

mydf_byboundary_farm_count_yr<-mydf %>% select(-`Insect key number`,-`Farm type`,-Boundary_descriptor,-Season) %>% mutate(unique_ob=paste(Boundary_year,Farm_name)) %>% select(-Farm_name) %>% group_by(Boundary_year,`New Inseck key name`) %>% summarise(count_farms_perobs=sum(`Sum of Insects/100 flowers`)/n_distinct(unique_ob)) 

mydf_byboundary_farm_count_yr<-mydf_byboundary_farm_count_yr %>% separate_wider_delim(Boundary_year,'_',names=c("Boundary_descriptor","Season"))


mydf_byboundary_farm_count_yr$Boundary_descriptor <- factor(mydf_byboundary_farm_count_yr$Boundary_descriptor,levels=c('Control farm','Bare fence','One year old','Two year old','Three year old','Old'),ordered = T)


mydf_byboundary_farm_count_yr %>%   ggplot(aes(x = Boundary_descriptor, y = count_farms_perobs, fill = `New Inseck key name`)) +
  geom_bar(position='stack',stat='identity',width=0.4) +
  facet_grid(~Season, scales = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  scale_fill_manual(values=with(brads_col_2,setNames(Brad_colours,`New Inseck key name`)))+
  ggtitle(paste('Counts by boundary'))+ylab('Average count per 100 flowers')+xlab('Boundary type')+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))  

#alpha diversity bare fence control year ....old

#one matrix of the counts
#one metadata file
head(mydf)
mydf<-mydf %>% mutate(unique_sample=paste(Boundary_descriptor,Farm_name,Season))

counts<-mydf %>% ungroup() %>% select(unique_sample,`New Inseck key name`,`Sum of Insects/100 flowers`)
#transform wide
counts_wide<-counts %>% pivot_wider(names_from=`New Inseck key name`,values_from = `Sum of Insects/100 flowers`)
counts_wide<-counts_wide %>% remove_rownames %>% column_to_rownames(var="unique_sample")


#count species per property
sppr <- specnumber(counts_wide)
sppr


#anova against variables, does mean species richness vary among treatments?
head(mydf)
meta_data<-mydf %>% ungroup() %>% select(unique_sample,Boundary_descriptor,Season,Farm_name,`Farm type`) %>% unique()
sppr_aov <-aov(sppr ~ Boundary_descriptor, data = meta_data)
summary(sppr_aov)

#impact of season and boundary
sppr_aov <-aov(sppr ~ Boundary_descriptor+Season, data = meta_data)
summary(sppr_aov)

#impact of season and boundary
sppr_aov <-aov(sppr ~ Boundary_descriptor+Season+`Farm type`, data = meta_data)
summary(sppr_aov)


#can plot that out
sppr_df <- sppr %>% 
  enframe() %>% 
  full_join(meta_data, by = c("name" = "unique_sample"))

sppr_df$Boundary_descriptor <- factor(sppr_df$Boundary_descriptor,levels=c('Control farm','Bare fence','One year old','Two year old','Three year old','Old'),ordered = T)


#just boundary
plot_sppr <- ggplot(sppr_df, aes(x = Boundary_descriptor, y = value, fill = Boundary_descriptor)) +
  geom_boxplot() +
  theme_bw()+
  labs(x = "Boundary type",
       y = "Number of species per site",
       title = "Species richness")
plot_sppr

#other significant factor was season
plot_sppr <- ggplot(sppr_df, aes(x = Boundary_descriptor, y = value, fill = Boundary_descriptor)) +
  geom_boxplot() +
facet_wrap(~Season)+
  theme_bw()+
  labs(x = "Boundary type",
       y = "Number of species per site",
       title = "Species richness")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr


#alpha diversity shannon

shannondiv<-diversity(counts_wide)
head(shannondiv)

sppr_aov <-aov(shannondiv ~ Boundary_descriptor, data = meta_data)
summary(sppr_aov)

#impact of season and boundary
sppr_aov <-aov(shannondiv ~ Boundary_descriptor+Season, data = meta_data)
summary(sppr_aov)

#impact of season and boundary
sppr_aov <-aov(shannondiv ~ Boundary_descriptor+Season+`Farm type`, data = meta_data)
summary(sppr_aov)

#can plot that out
shannondiv_df <- shannondiv %>% 
  enframe() %>% 
  full_join(meta_data, by = c("name" = "unique_sample"))

shannondiv_df$Boundary_descriptor <- factor(shannondiv_df$Boundary_descriptor,levels=c('Control farm','Bare fence','One year old','Two year old','Three year old','Old'),ordered = T)


#just boundary
plot_sppr <- ggplot(shannondiv_df, aes(x = Boundary_descriptor, y = value, fill = Boundary_descriptor)) +
  geom_boxplot() +
  theme_bw()+
  labs(x = "Boundary type",
       y = "Number of species per site",
       title = "Species richness")
plot_sppr

#other significant factor was season
plot_sppr <- ggplot(shannondiv_df, aes(x = Boundary_descriptor, y = value, fill = Boundary_descriptor)) +
  geom_boxplot() +
  facet_wrap(~Season)+
  theme_bw()+
  labs(x = "Boundary type",
       y = "Number of species per site",
       title = "Species richness")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

plot_sppr


#beta diversity
perm<-adonis2(counts_wide ~ Boundary_descriptor,data=meta_data)
perm

perm<-adonis2(counts_wide ~ Boundary_descriptor+Season,data=meta_data)
perm

perm<-adonis2(counts_wide ~ Boundary_descriptor+`Farm type`,data=meta_data)
perm

#NMDS
#NMDS is struggling to find a repeated solution. I think this is due to the rare species
#https://stats.stackexchange.com/questions/649643/best-solution-was-not-repeated-no-reliable-result-for-metamds
pk_NMDS <- metaMDS(counts_wide,trymax=200)

pk_NMDS
#stress is a little high (ideally <0.2)

stressplot(pk_NMDS)

plot(pk_NMDS)

plot_df <- scores(pk_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(meta_data, by = c("site" = "unique_sample"))

plot_df$Boundary_descriptor <- factor(plot_df$Boundary_descriptor,levels=c('Control farm','Bare fence','One year old','Two year old','Three year old','Old'),ordered = T)


plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Boundary_descriptor)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, linewidth = 0.5) +
  theme_bw()+
  labs(title = "NMDS")
plot_nmds


