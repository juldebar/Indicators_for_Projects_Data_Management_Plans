################################################################################################################################
rm(list=ls())
################### timevis package / https://daattali.com/shiny/timevis-demo/ ################################
# OTHER EXAMPLES HERE https://daattali.com/shiny/timevis-demo/
################################################################################################################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(timevis, timeline, dplyr,gsheet,RefManageR,rvest)
################################################################################################################################
# setwd("/tmp/bib/")
wd <- "/home/julien/Bureau/CODES/Indicators_for_Projects_Data_Management_Plans/"
wd <- setwd(wd)
# CREATE A REPOSITORY TO STORE bibtex FILES IF IT DOESN'T ALREADY EXIST 
bibtex_files_repository <- "bib"
if (file.exists(bibtex_files_repository)){
} else {
  dir.create(file.path(wd, bibtex_files_repository))
}

timeline_files_repository <- "timeline"
if (file.exists(timeline_files_repository)){
} else {
  dir.create(file.path(wd, timeline_files_repository))
}

csv_files_repository <- "csv"
if (file.exists(csv_files_repository)){
} else {
  dir.create(file.path(wd, csv_files_repository))
}

tmpdir<-getwd()
tmpdir<-file.path(wd, bibtex_files_repository)

Zotero_total_number = 4615
Zotero_projects_IOC_number = 1153+1816
Zotero_total_projects = Zotero_total_number - Zotero_projects_IOC_number
  
source(file = "/home/julien/Bureau/CODES/Indicators_for_Projects_Data_Management_Plans/R/functions.R")
source(file = "/home/julien/Bureau/CODES/Indicators_for_Projects_Data_Management_Plans/R/credentials.R")


################################################################################################################################

DataGroup_gsheet <- "https://docs.google.com/spreadsheets/d/1dQLucq5OAm1qBHPuJv_7mDEOWq9x0Cyknp6ecVtGtS4/edit?usp=sharing"
DataGroup <- as.data.frame(gsheet::gsheet2tbl(DataGroup_gsheet))
names(DataGroup)
DataGroupA = subset(DataGroup, select = c(Projet,Projet_Acronym,Budget,StartDate,EndDate,Subject,Datasets,Projet_Zotero) )
names(DataGroupA)

COI_Projects = rename(DataGroupA, Title=Projet,content=Projet_Acronym, Funding=Budget, start=StartDate, end=EndDate, group=Subject, metadata=Datasets)
COI_Projects$start <- as.Date(COI_Projects$start, format="%d-%m-%Y")
COI_Projects$end <- as.Date(COI_Projects$end, format="%d-%m-%Y")
COI_Projects$Projet_Zotero <-gsub("https://www.zotero.org/groups/303882/commission_ocean_indien/items/collectionKey/","",COI_Projects$Projet_Zotero) 
groups <- distinct(COI_Projects, group)
groups$id <-distinct(COI_Projects, group)
sapply(COI_Projects,class)

################### Create a timeline for projects ################################
setwd(file.path(wd, timeline_files_repository))
create_timeline_for_projects(df=COI_Projects)
setwd(wd)

################### Add Dynamic Columns ################################
setwd(file.path(wd, csv_files_repository))
COI_Projects <- add_column_metadata_number(df=COI_Projects)
COI_Projects <- add_column_zotero_number_of_references_from_keywords_and_full_text_search(df=COI_Projects,
                                                                                          zotero_group=zotero_group,
                                                                                          key_zotero_api=key_zotero_api,
                                                                                          tmpdir=tmpdir)
sapply(COI_Projects,class)
filename <- paste("COI_projects_list_", Sys.Date(),".csv",sep="")

write.csv(COI_Projects,file = filename)
setwd(wd)

################### Indicators ################################

indicators_files_repository <- "indicators"
if (file.exists(indicators_files_repository)){
} else {
  dir.create(file.path(wd, indicators_files_repository))
}


setwd(file.path(wd, indicators_files_repository))

bar_plot_references_projects(df = COI_Projects, type="tags", format="svg")
bar_plot_references_projects(df = COI_Projects, type="full_search", format="svg")
bar_plot_references_projects(df = COI_Projects, type="metadata", format="svg")
bar_plot_references_projects(df = COI_Projects, type="bibliographic_references", format="svg")



bar_plot_references_projects(df = COI_Projects, type="tags", format="png")
bar_plot_references_projects(df = COI_Projects, type="full_search", format="png")
bar_plot_references_projects(df = COI_Projects, type="metadata", format="png")
bar_plot_references_projects(df = COI_Projects, type="bibliographic_references", format="png")

barchart_stacked_area_references_projects(df = COI_Projects,format="png")
barchart_stacked_area_references_projects(df = COI_Projects,format="svg")
Pie_Chart_summary(df = COI_Projects,format="png")
Pie_Chart_summary(df = COI_Projects,format="svg")
Pie_Chart_references_projects(df = COI_Projects,format="png")
Pie_Chart_references_projects(df = COI_Projects,format="svg")
setwd(wd)

################### Slidify ################################
require(slidify)
library(knitrBootstrap)
library(rmarkdown)
mywd <- '/home/julien/Bureau/CODES/Indicators_for_Projects_Data_Management_Plans/Slidify/Slidify'
setwd(mywd)
slidify("index.Rmd")
# system("pandoc -s -t slidy index.md -o index.pdf")

setwd(wd)




#Specifying the url for desired website to be scrapped
# url <- "https://www.zotero.org/groups/303882/commission_ocean_indien"
# url <-"https://www.zotero.org/groups/303882/commission_ocean_indien/items"
# #Reading the HTML code from the website
# webpage <- read_html(url)
# div <-html_nodes(webpage, ".clickable")
# html_text(div)
# toto <- ReadZotero(group = zotero_group, .params = list(q="http://", qmode = "everything", key = key_zotero_api), temp.file = tempfile(fileext = ".bib", tmpdir = tmpdir), delete.file = FALSE)


# total_of_Zotero_references <- ReadZotero(user_key, .params = list(key = key_zotero_api,qmode = "everything", tag=""), temp.file = tempfile(fileext = ".bib", tmpdir = tmpdir),delete.file = FALSE)
# paste("curl -H 'Zotero-API-Version: 2' 'https://api.zotero.org/users/",user_key,"/items?format=atom'",sep="")
# paste("curl -H 'Zotero-API-Version: 2' 'https://api.zotero.org/groups/",zotero_group,"/items'",sep="")
# paste("curl -H 'Zotero-API-Version: 2' 'https://api.zotero.org/groups/",zotero_group,"/collections'",sep="")
# curl -H 'Zotero-API-Version: 2' 'https://api.zotero.org/users/475425/collections/9KH9TNSJ/items?format=atom'     
# https://api.zotero.org/users/475425/collections/9KH9TNSJ/items?format=atom