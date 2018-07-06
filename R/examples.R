################################################################################################################################
rm(list=ls())
################### timevis package / https://daattali.com/shiny/timevis-demo/ ################################
# OTHER EXAMPLES HERE https://daattali.com/shiny/timevis-demo/
################################################################################################################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(timevis, timeline, dplyr,gsheet,RefManageR)
################################################################################################################################
setwd("/tmp/bib/")
source(file = "/home/julien/Bureau/CODES/Indicators_for_Projects_Data_Management_Plans/R/functions.R")
source(file = "/home/julien/Bureau/CODES/Indicators_for_Projects_Data_Management_Plans/R/credentials.R")
################################################################################################################################

DataGroup_gsheet <- "https://docs.google.com/spreadsheets/d/1dQLucq5OAm1qBHPuJv_7mDEOWq9x0Cyknp6ecVtGtS4/edit?usp=sharing"
DataGroup <- as.data.frame(gsheet::gsheet2tbl(DataGroup_gsheet))
names(DataGroup)
DataGroupA = subset(DataGroup, select = c(Projet,Projet_Acronym,Budget,StartDate,EndDate,Subject,Datasets) )
names(DataGroupA)

COI_Projects = rename(DataGroupA, Title=Projet,content=Projet_Acronym, Funding=Budget, start=StartDate, end=EndDate, group=Subject, metadata=Datasets)
COI_Projects$start <- as.Date(COI_Projects$start, format="%d-%m-%Y")
COI_Projects$end <- as.Date(COI_Projects$end, format="%d-%m-%Y")
groups <- distinct(COI_Projects, group)
groups$id <-distinct(COI_Projects, group)
sapply(COI_Projects,class)

################### Create a timeline for projects ################################
create_timeline_for_projects(df=COI_Projects)
################### Add Dynamic Columns ################################
COI_Projects <- add_column_metadata_number(df=COI_Projects)
tmpdir<-getwd()
COI_Projects <- add_column_zotero_number_of_references_from_keywords_and_full_text_search(df=COI_Projects, zotero_group=zotero_group, zotero_key=key_zotero_api, tmpdir=tmpdir)
sapply(COI_Projects,class)
################### Add Dynamic Columns ################################
bar_plot_references_projects(df = COI_Projects, type="tags", format="svg")
bar_plot_references_projects(df = COI_Projects, type="full_search", format="svg")
bar_plot_references_projects(df = COI_Projects, type="metadata", format="svg")

bar_plot_references_projects(df = COI_Projects, type="tags", format="png")
bar_plot_references_projects(df = COI_Projects, type="full_search", format="png")
bar_plot_references_projects(df = COI_Projects, type="metadata", format="png")

