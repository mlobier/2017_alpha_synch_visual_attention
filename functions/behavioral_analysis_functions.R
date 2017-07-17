summarise_subject_behav_data <- function (subject_dir,all_subject_sessions) {

  dataset<-load_behav_data(subject_dir,all_subject_sessions) 
  
  falseAlarm<-dataset%>%filter(contrast=="catch")%>%
    mutate(hitRate=ifelse(is.na(responseRT),0,1))%>%
    summarise(mean(hitRate))
  
  low_contrast_data_det<-dataset%>%filter(contrast=="low" & detectionAccuracy!="ERROR")%>%
    mutate(hitRate_det=ifelse(detectionAccuracy=='HIT_DET',1,0))%>%
    group_by(cueValidity,cuedHemifield)%>%
    summarise(N=n(),hitRate_det=mean(hitRate_det, na.rm = TRUE),responseRT=mean(responseRT, na.rm = TRUE))
  
  low_contrast_data_disc<-dataset%>%filter(contrast=="low" & detectionAccuracy!="ERROR" & detectionAccuracy!="MISS")%>%
    mutate(hitRate_disc=ifelse(discriminationAccuracy=='HIT_DISC',1,0))%>%
    group_by(cueValidity,cuedHemifield)%>%
    summarise(hitRate_disc=mean(hitRate_disc, na.rm = TRUE))%>%
    full_join(low_contrast_data_det)%>%
    mutate(contrast="Low")
    
 
  high_contrast_data_det<-dataset%>%filter(contrast=="high" & detectionAccuracy!="ERROR")%>%
    mutate(hitRate_det=ifelse(detectionAccuracy=='HIT_DET',1,0))%>%
    group_by(cueValidity,cuedHemifield)%>%
    summarise(N=n(),hitRate_det=mean(hitRate_det, na.rm = TRUE)) 
 
  high_contrast_data_disc<-dataset%>%filter(contrast=="high" & discriminationAccuracy!="ERROR" & discriminationAccuracy!="MISS")%>%
    mutate(hitRate_disc=ifelse(discriminationAccuracy=='HIT_DISC',1,0))%>%
    group_by(cueValidity,cuedHemifield)%>%
    summarise(hitRate_disc=mean(hitRate_disc, na.rm = TRUE),responseRT=mean(responseRT, na.rm = TRUE))%>%
    full_join(high_contrast_data_det)%>%
    mutate(contrast="High")%>%
    bind_rows(low_contrast_data_disc)
  
  subjectData<-list(all_data=high_contrast_data_disc, falseAlarm=falseAlarm)
  rm(falseAlarm, low_contrast_data_det,low_contrast_data_disc,high_contrast_data_det,high_contrast_data_disc)
  return(subjectData)
}

load_behav_data <- function (subject_folder,all_subject_sessions) {
  
  subject_nb<-strsplit(subject_folder, "/")[[1]][6]
  
  sessions<-all_subject_sessions[which(all_subject_sessions$subjectNb==subject_nb),]
  sessions<-sessions[c(-1, -2)]
  
  file_list<-list.files(subject_folder, pattern="\\_RT.csv$")
  
  for (file in file_list){
    temp_dataset<-read.table(paste(subject_folder,file,sep="/"), header=TRUE, sep=";")
    temp_dataset$setNb<-as.numeric(strsplit(strsplit(file, "set")[[1]][2], '_RT.csv')[[1]][1])
    
    if (!exists('dataset')){
      dataset<-temp_dataset
      rm(temp_dataset)
    }else
    {
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  
  dataset<-dataset[c(-8)]
  dataset<-dataset%>%
    mutate(cueValidity=recode(cueValidity, consistent="valid", inconsistent="invalid"))%>%
    mutate(contrast=recode(taskType,detection = "low", discrimination = "high"  ))
 
   dataset <- merge(dataset,sessions,by="setNb")
  dataset$subjectNb <- subject_nb
  
  return(select(dataset,-taskType))
}


compute_nb_trials_per_condition <- function (subjectNb,allSubjectSessions) {
  if (exists('dataset')){
    rm(dataset)
  }
  dataset<-load_behav_data(subjectNb,allSubjectSessions)
  
  dataset<-dataset[which(dataset$artifact=='artifactAbsent' & dataset$noise50Hz=='50HzNoiseAbsent'& ((!grepl('preStimSaccadePresent',dataset$saccade) & !grepl('preStimBlinkPresent',dataset$blinks) )| dataset$saccades=='notProsessed')),]
  
  right<-nrow(dataset[which(dataset$cuedHemifield=='right'),])
  left<-nrow(dataset[which(dataset$cuedHemifield=='left'),])
  
  nbTrialsHemifield<-data.frame(right,left)
  nbTrialsHemifield <- gather(nbTrialsHemifield) 
  names(nbTrialsHemifield) <-  c('cuedHemifield','nbTrials')
  
  
  subjectData<-nbTrialsHemifield
  rm(dataset,nbTrialsHemifield)
  rm(right, left)
  return(subjectData)
}