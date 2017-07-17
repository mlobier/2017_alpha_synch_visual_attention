process_amplitude_data <- function(file_list, parcel_names, system_assignements,attend_condition) {
  for (file_name in file_list)
  {
    # Load dataset and atribute cells to parcel pairs
    # Data is concatenated across parcels, then time-windows and then subject
    # The TW value refers to the middle of the 100 ms TW.
    data<-read.table(file_name, header=FALSE, sep=";")
    
    #Remove last time window which will not used (1.05-1.15 s after cue onset) 
    data<-subset(data, V2<1.1)
    
    # Identify how many TW are present in the data ()
    nbConditions<-nrow(data)/nrow(parcel_names)
    
    #Create parcel vecor to assign each value to the appropriate parcel
    parcelVector<-as.vector(parcel_names$parcel_1)
    for (i in 2:nbConditions)
    {
      parcelVector<-c(parcelVector,as.vector(parcel_names$parcel_1))
    }
    
    data<-data.frame(data, parcelVector)
    names(data)<-c('logAmp', 'TWin', 'subjectNb','parcel_1')
    
    data<-full_join(data, system_assignements,by= c("parcel_1"))
    
    data <- data %>% 
      separate(system_H, c("system", "hemisphere"), sep = "_") %>%
      mutate(hemisphere = ifelse(hemisphere == 'L',
                                 c("Left"),
                                 c("Right")))
    
    visual_system_amplitude_data <- data %>%
      filter(system=='1Vis') %>%
      group_by(TWin, hemisphere,subjectNb) %>%
      summarise(logAmp=mean(logAmp))
    
    
    visual_system_amplitude_data$frequency<-strsplit(strsplit(file_name, 
                                                              attend_condition)[[1]][2],'logAmp.csv')[[1]][1]
    visual_system_amplitude_data$condition<-strsplit(attend_condition,"_")[[1]][2]
    
    if (exists('all_freq_amp'))
    {
      all_freq_amp<-rbind(all_freq_amp, visual_system_amplitude_data)
    }
    else{
      all_freq_amp<-visual_system_amplitude_data
    }
    rm(data, visual_system_amplitude_data)
  }
  return(all_freq_amp)
}