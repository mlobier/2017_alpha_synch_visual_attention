count_visual_connections <- function(data, system_assignments,parcel_list) {
  names(data)<-parcel_list$VertexName
  data$parcel_names<-parcel_list$VertexName
  data2<-gather(data, parcel_names_2,connection,-parcel_names)
  
  data2<-data2%>%full_join(system_assignments)%>%
    rename(parcel_names_1=parcel_names,parcel_names=parcel_names_2,system_1=system)%>%
    mutate(parcel_names=as.factor(parcel_names))%>%
    full_join(system_assignments)%>%
    rename(parcel_names_2=parcel_names,system_2=system)
  
  within_left_vis<-data2 %>% 
    filter(system_1=="1Vis_L" & system_2=="1Vis_L") %>%
    summarise(sum(connection)/2)
  
  within_right_vis<-data2 %>% 
    filter(system_1=="1Vis_R" & system_2=="1Vis_R") %>%
    summarise(sum(connection)/2)
  
  between_right_vis_DAN<-data2 %>%
    filter(system_1=="1Vis_R" & grepl("DAN",system_2)) %>%
    summarise(sum(connection))
  
  between_left_vis_DAN<-data2 %>% 
    filter(system_1=="1Vis_L" & grepl("DAN",system_2)) %>%
    summarise(sum(connection))
  
  tempConnectionNb<-data.frame(within_left_vis,within_right_vis,between_right_vis_DAN,between_left_vis_DAN)
  names(tempConnectionNb)<-c("within_left_vis","within_right_vis","between_right_vis_DAN","between_left_vis_DAN")
  
  
  return(tempConnectionNb)
}