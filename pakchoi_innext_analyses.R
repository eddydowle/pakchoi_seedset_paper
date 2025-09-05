#everyones favourate package.....innext
#https://github.com/AnneChao/iNEXT
install.packages('devtools')
library(devtools)
#believe 3D does both phylogenetic and nophylogenetic analyses
install_github('AnneChao/iNEXT.3D')
library(iNEXT.3D)
library(tidyverse)
library(readxl)

data<-read_excel('C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Pakchoi/Copy of Pak choi pollinators Heather analysis Brad update.xlsx',sheet='DataSheet')
#switching to fixed up dataset
brads_col_2<-read_excel('C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Pakchoi/Eddy Pie chart short.xlsx',sheet='Colours')
#tidy up some names etc
data$Boundary_descriptor<-gsub('one year old','One year old',data$Boundary_descriptor)
data$Boundary_descriptor<-gsub('two year old','Two year old',data$Boundary_descriptor)
data$Boundary_descriptor<-gsub('three year old','Three year old',data$Boundary_descriptor)
data$Boundary_descriptor<-gsub('Bare_fence','Bare fence',data$Boundary_descriptor)
data$Boundary_descriptor<-gsub('Control_farm','Control farm',data$Boundary_descriptor)


#summarising counts to clean up dataset etc
data<-data %>% select(-`Broad Insect category`,-Origin,-`Boundary age`) %>% group_by(`Farm type`,Farm_name,Season,Boundary_descriptor,`Insect key number`,`New Inseck key name`) %>% summarise(`Sum of Insects/100 flowers`=sum(`Sum of Insects/100 flowers`) )
head(data)
#make present absent
data<-data %>% mutate(unique_ob=paste(Boundary_descriptor,Farm_name,Season)) %>% mutate(presabs=case_when(`Sum of Insects/100 flowers` >0 ~1,`Sum of Insects/100 flowers`==0~0))

#gunna need to rerun metabarcoding code to see what is happening to build the list of lists

#generate a list of the unique sample locations
categories <- unique(data$Boundary_descriptor)
categories
#create empty list
split_list <- list()
#fill empty list with a presence absence OTU from each location

subset_data <- data %>% filter(Boundary_descriptor == "Old")
head(subset_data)
test<-subset_data %>% ungroup() %>% dplyr::select(unique_ob,`New Inseck key name`,`Sum of Insects/100 flowers`)
test
test<-test %>%  spread(key = unique_ob, value = `Sum of Insects/100 flowers`)
split_list[[category]] <- subset_data

for (category in categories) {
  subset_data <- data %>% filter(Boundary_descriptor == category)
 # print(sub_physeq.100)
  test<-subset_data %>% ungroup() %>% dplyr::select(unique_ob,`New Inseck key name`,`Sum of Insects/100 flowers`)
test   <- test %>%  spread(key = unique_ob, value = `Sum of Insects/100 flowers`)
test<-test %>% remove_rownames %>% column_to_rownames(var="New Inseck key name")
test<-test %>% mutate(across(everything(), ~ ifelse(. > 0, 1, 0)))
split_list[[category]]<-as(test,'matrix')
}  
matrix_list <- list(data = list())
for (category in categories) {
  otu_table <- as(split_list[[category]], "matrix")
  matrix_list[["data"]][[category]] <- otu_table
}


test<-test %>% remove_rownames %>% column_to_rownames(var="New Inseck key name")
as(test,'matrix')

for (category in categories) {
  subset_data <- data %>% filter(Boundary_descriptor == category)
  head(subset_data)
  split_list[[category]] <- subset_data
}  

out.raw <- iNEXT3D(data = matrix_list$data, diversity = 'TD', q = c(0, 1, 2), datatype = 'incidence_raw', nboot = 50)

#there is various ways to look at these plots:
#within a sample
ggiNEXT3D(out.raw, type = 1, facet.var = 'Assemblage') + facet_wrap(~Assemblage, nrow = 3)
#across samples
ggiNEXT3D(out.raw, type = 1, facet.var = "Order.q")
#sample completeness
ggiNEXT3D(out.raw, type = 2, facet.var = "Order.q", color.var = "Assemblage")

?ggiNEXT3D
col_bound<-c('burlywood1','#FDE725FF',"#5DC863FF","#21908CFF",'#3B528BFF',"#440154FF")

scales::pal_viridis(option = "D")(6)
scales::pal_viridis(option = "D")(5)




#flower number issue
#https://stats.stackexchange.com/questions/471530/what-are-the-ways-to-normalize-the-vegetation-data-when-the-plot-sizes-of-sample
#https://www.researchgate.net/post/Is_there_a_correct_way_to_standardise_data_collected_using_different_transect_sampling_dimensions
#https://onlinelibrary.wiley.com/doi/full/10.1002/aqc.3477
#do inext on individual counts
#https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210x.13292
#https://stats.stackexchange.com/questions/143541/calculating-diversity-using-normalized-species-counts



