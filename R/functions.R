################################################################################################################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(timevis, timeline, dplyr,gsheet,RefManageR)
################################################################################################################################
create_timeline_for_projects <- function(df){
  number_row<-nrow(df)
  list_COI_Projects <- data.frame(id= 1:number_row,
                                  content = df$content,
                                  start   = df$start,
                                  end     = df$end,
                                  group     = df$group#,
                                  # groups = groups
  ) #%>%  setGroups(groups)
  timevis(data=list_COI_Projects,fit=TRUE, zoomFactor = 1)
  myTimeline <- timevis(data=list_COI_Projects,fit=TRUE, zoomFactor = 1)
  Timeline_file <- "IOC_Projects_Timeline.html"
  htmlwidgets::saveWidget(myTimeline, Timeline_file, selfcontained = F)
  
  return(Timeline_file)
}
######################## https://cran.r-project.org/web/packages/RefManageR/RefManageR.pdf ###############################

add_column_metadata_number <- function(df){
  number_metadata = c()
  for (metadata in df$metadata) {
    if (grepl("https://docs.google.com/spreadsheets",metadata)==TRUE) {
      cat(metadata)
      number <- nrow(as.data.frame(gsheet::gsheet2tbl(metadata)))
      } else {
        number=0
        }
    number_metadata <- append(number_metadata,number)
    cat("Number of metadata in google spreadsheet for this project\n")
    cat(number_metadata)
    cat("\n")
  }
  
  df$Metadata_number <- number_metadata
  return(df)
  }

######################## Add column Zotero Indicators ###############################
  
add_column_zotero_number_of_references_from_keywords_and_full_text_search <- function(df, zotero_group, key_zotero_api, tmpdir){
  number_publications_of_the_projects_in_Zotero_with_tags = c()
  number_publications_of_the_projects_in_Zotero_full_text = c()
  number_publications_of_the_project_in_Zotero_dedicated_collection = c()

  
  number_projects <- nrow(df)
  for(project in 1:number_projects){
    cat(paste("\n Collection Zotero  ",df$Projet_Zotero[project]," du projet",df$content[project],"\n",sep=""))

    publications_of_the_project_in_Zotero_with_tags <- ReadZotero(group = zotero_group,
                                                                  .params = list(tag=df$content[project], key = key_zotero_api),
                                                                  temp.file = tempfile(fileext = ".bib", tmpdir = tmpdir),
                                                                  delete.file = FALSE
    )
    if(df$Projet_Zotero[project]!=""){
    publications_of_the_project_in_Zotero_dedicated_collection <- ReadZotero(group = zotero_group,
                                                                  .params = list(key = key_zotero_api,collection=df$Projet_Zotero[project]),
                                                                  temp.file = tempfile(fileext = ".bib", tmpdir = tmpdir),
                                                                  delete.file = FALSE
    )
    }else{publications_of_the_project_in_Zotero_dedicated_collection<-NULL}
    publications_of_the_project_in_Zotero_full_text <- ReadZotero(group = zotero_group,
                                                                  .params = list(q=df$content[project], key = key_zotero_api),
                                                                  temp.file = tempfile(fileext = ".bib",tmpdir = tmpdir),
                                                                  delete.file = FALSE
    )
    cat("\n Number of References keywords \n")
    cat(length(publications_of_the_project_in_Zotero_with_tags))
    cat("\n Number of References full text \n")
    cat(length(publications_of_the_project_in_Zotero_full_text))
    cat("\n Number of References \n")
    cat(length(publications_of_the_project_in_Zotero_dedicated_collection))
    
    number_publications_of_the_projects_in_Zotero_with_tags <- append(number_publications_of_the_projects_in_Zotero_with_tags,length(publications_of_the_project_in_Zotero_with_tags))
    number_publications_of_the_projects_in_Zotero_full_text <- append(number_publications_of_the_projects_in_Zotero_full_text,length(publications_of_the_project_in_Zotero_full_text))
    number_publications_of_the_project_in_Zotero_dedicated_collection <- append(number_publications_of_the_project_in_Zotero_dedicated_collection,length(publications_of_the_project_in_Zotero_dedicated_collection))
    cat("\n")
    
  }
  df$Zotero_references_with_tags <- number_publications_of_the_projects_in_Zotero_with_tags
  df$Zotero_references_with_tags_full_text <- number_publications_of_the_projects_in_Zotero_full_text
  df$Zotero_references_in_Zotero_dedicated_collection <- number_publications_of_the_project_in_Zotero_dedicated_collection
  return(df)
}

######################## Plot Results ###############################
# hist(COI_Projects$Zotero_references)
# https://www.tutorialspoint.com/r/r_bar_charts.htm
# colors = c("red","yellow","green")

bar_plot_references_projects <- function(df,type,format){
  
  switch(type,
         tags={
           main=main=paste("Zotero: recherche par mots-clés@", Sys.Date()," (Total: ", sum(df$number_publications_of_the_projects_in_Zotero_with_tags), ")", sep="")
           column=df$Zotero_references_with_tags
           ylab="Number of References"
           filename <- paste("Zotero_references_per_project_search_keywords_", Sys.Date(),sep="")
         },
         full_search={
           main=paste("Zotero:  recherche plein texte (Titre, Résumé, ..) @", Sys.Date()," (Total: ", sum(df$number_publications_of_the_projects_in_Zotero_full_text), ")",sep="")
           column=df$Zotero_references_with_tags_full_text
           ylab="Number of References"
           filename <- paste("Zotero_references_per_project_search_full_text_", Sys.Date(),sep="")
         },
         bibliographic_references={
           main=paste("Zotero: bibliographic references per project @", Sys.Date()," (Total: ", sum(df$number_publications_of_the_project_in_Zotero_dedicated_collection), ")",sep="")
           column=df$Zotero_references_in_Zotero_dedicated_collection
           ylab="Number of Bibliographic references"
           filename <- paste("Bibliographic_references_number_per_project_", Sys.Date(),sep="")
         },
         metadata={
           main=paste("Geonetwork: metadata per project @", Sys.Date()," (Total: ",sum(df$Metadata)," metadata)",sep="")
           column=df$Metadata
           ylab="Number of Metadata"
           filename <- paste("Metadata_number_per_project_", Sys.Date(),sep="")
         },
         stop("Enter something that switches me!")
  )
  
  switch(format,
         svg={svg(paste(filename,".svg",sep=""), width=15, height=5)},
         png={png(paste(filename,".png",sep=""), width=1500, height=500)}
  )
  
  
  barplot(column,
          names.arg=df$content,
          xlab="Project Name",
          ylab=ylab,
          cex.names=0.8,
          las=2,
          col="blue",
          main=main,
          border="red")
  
  # http://www.sthda.com/french/wiki/fonction-abline-de-r-comment-ajouter-facilement-une-droite-a-un-graphique
  abline(h=20,col="red",lty=2)
  abline(h=30,col="red",lty=2)
  abline(h=40,col="red",lty=2)
  dev.off() # to complete the writing process and return output to your monitor
  
}



barchart_stacked_area_references_projects <- function(df,type,format){

  filename <- paste("barchart_stacked_area_references_projects_", Sys.Date(),sep="")
  switch(format,
         svg={svg(paste(filename,".svg",sep=""), width=15, height=5)},
         png={png(paste(filename,".png",sep=""), width=1500, height=500)}
  )
  # Create the input vectors.
  project <- df$content
  search <- c("Mots-Clés","Plein Texte","Collection dédiée")
  colors = c("green","orange","red")
  
  Values <- matrix(c(df$Zotero_references_with_tags,df$Zotero_references_with_tags_full_text,df$Zotero_references_in_Zotero_dedicated_collection),
                   nrow = 3,
                   ncol = nrow(df),
                   byrow = TRUE
                   )
  barplot(Values,
          main = "Références bibliographiques par projet COI selon le type de recherche",
          names.arg = project,
          xlab = "Projet",
          ylab = "Nombre de références",
          col = colors)
  
  legend("topleft", search, cex = 1.3, fill = colors)
  dev.off()
  
}
  
# https://www.statmethods.net/graphs/pie.html
Pie_Chart_references_projects <- function(df,type,format){
  
  filename <- paste("Pie_Chart_references_projects_", Sys.Date(),sep="")
  switch(format,
         svg={svg(paste(filename,".svg",sep=""), width=15, height=15)},
         png={png(paste(filename,".png",sep=""), width=500, height=500)}
  )
  
# Simple Pie Chart
slices <- df$Zotero_references_in_Zotero_dedicated_collection
project <- df$content

pie(slices,
    labels = project,
    col=rainbow(length(project)),
    main="References per project")
dev.off()

}




#####################################################################################################################################################################
################################################################ COMMENTED BELOW####################################################################
#####################################################################################################################################################################
# DataGroupB$Date <- as.Date(DataGroupA$Signature, format="%d-%m-%Y")
# 
# timeline(DataGroupA,
#          label.col = names(DataGroupA)[2],
#          start.col = names(DataGroupA)[4],
#          end.col = names(DataGroupA)[5],
#          group.col = names(DataGroupA)[2],
#          text.size=3)
# 
# timeline(DataGroupA,DataGroupB)
# timeline(DataGroupA,DataGroupB,event.col=DataGroupB$Signature)
# 
# timeline(df=DataGroupA,text.size=2)
# timeline(df=DataGroupA,start.col=DataGroupA$StartDate, end.col=DataGroupA$EndDate, text.size=4)
# timeline(df=DataGroupA,events=, start.col=DataGroupA$StartDate, end.col=DataGroupA$EndDate)
# timelineShinyDemo()
# 
# 
# 
# 
# DataGroupB <- read.csv("http://www.guxsousa.com/Downloads/DataGroup2.csv", header=F, stringsAsFactors=FALSE)
# names(DataGroupB) <- c('Event','Date','OS')
# DataGroupB$Date <- as.Date(DataGroupB$Date)
# 
# timeline(DataGroupA,DataGroupB)
# 
# draw <- function() {
#   timeline(DataGroupA,DataGroupB,
#            text.size = 4,
#            text.alpha = 3,
#            num.label.steps = 10,
#            event.spots = 10,
#            event.label.method = 1,
#            event.text.size = 3.5,
#            event.label = '',
#            event.line = FALSE,
#            event.above = FALSE)
# }
# 
# png("TimeLine.png", width = 1380, height = 880, units = "px", bg = "transparent", res = NA)
# draw()
# dev.off()