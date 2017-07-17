compute_hemispheric_graph_strength<- function (file_list, parcel_names,attendedHemifield) {
  for (file_name in file_list){
     # Load dataset and atribute cells to parcel pairs
    data<-read.table(file_name, header=FALSE, sep=";")
    
    #multiply data by new denia edge matrix
    # data<-as.data.frame(as.matrix(DEM)*as.matrix(data))
    names(data)<-as.vector(parcel_names$parcel_1)
    data$parcel_1<-parcel_names$parcel_1
    
    right_hemi_GS<-data %>%
      gather(parcel_2,wPLI,-parcel_1) %>%
      filter(grepl("_R",parcel_1) & grepl("_R",parcel_2)) %>%
      summarise(right = sum(wPLI)/2)
      
   left_hemi_GS<-data %>%
      gather(parcel_2,wPLI,-parcel_1) %>%
      filter(grepl("_L",parcel_1) & grepl("_L",parcel_2)) %>%
      summarise(left = sum(wPLI)/2)
   
   temp_dataset<-data.frame(right_hemi_GS,left_hemi_GS)
   temp_dataset$TW<-strsplit(strsplit(file_name, "T=")[[1]][2], 's.csv')[[1]][1]
   temp_dataset$subjectNb<-strsplit(strsplit(file_name, attendedHemifield)[[1]][2], '_')[[1]][1]   
    
    if (!exists('dataset')){
      dataset<-temp_dataset
      rm(data, right_hemi_GS,left_hemi_GS )
    }else
    {
      dataset<-rbind(dataset, temp_dataset)
      rm(data, right_hemi_GS,left_hemi_GS )
    }
    
  }
  return(dataset)
}