#################
#Copy data from one database into another, updates species ID
#Select camera base files and surveys to be copied
dbfile1<-"D:/DataMerge/CameraBase1.mdb" #data will be copied here
dbfile2<-"D:/DataMerge/CameraBase2.mdb" #data will be copied from here

#select surveys in CameraBase2 to be copied to CameraBase1
surveys="1,2"

#fix issue in RODBC package for field names with spaces
f<-RODBC:::sqlwrite
f<-deparse1(f,collapse="\n")
f<-gsub("mangleColNames","",f)
f<-gsub("odbcValidChannel","RODBC:::odbcValidChannel",f)
f<-gsub("quoteColNames","RODBC:::quoteColNames",f)
f<-gsub("quoteTabNames","RODBC:::quoteTabNames",f)
f<-eval(str2lang(f))
assignInNamespace("sqlwrite", f,"RODBC")


#open databases
cb1<-openCameraBase(dbfile1)
cb2<-openCameraBase(dbfile2)


#get sites and update IDs
sites1<-getSites(cb1)
sites2<-getSites(cb2,surveys)

if(nrow(sites2)>0){
  if(nrow(sites1)>0) sites2$SiteIDNew<-sites1$SiteID[match(sites2$SiteName,sites1$SiteName)]
  sites2$NewSite<-FALSE
  sites2$NewSite[is.na(sites2$SiteIDNew)]<-TRUE
  sites2$SiteIDNew[is.na(sites2$SiteIDNew)]<-getLastID(cb1,"Site")+(1:nrow(sites2[is.na(sites2$SiteIDNew),]))
}

#get surveys and update IDs
surveys1<-getSurveys(cb1)
surveys2<-getSurveys(cb2,surveys)

if(nrow(surveys2)>0){
  if(nrow(surveys1)>0) surveys2$SurveyIDNew<-surveys1$SurveyID[match(surveys2$"Survey Name",surveys1$"Survey Name")]
  surveys2$NewSyurvey=FALSE
  surveys2$NewSyurvey[is.na(surveys2$SurveyIDNew)]<-TRUE
  surveys2$SurveyIDNew[is.na(surveys2$SurveyIDNew)]<-getLastID(cb1,"Survey")+(1:nrow(surveys2[is.na(surveys2$SurveyIDNew),]))
  
  if(nrow(sites2)>0)surveys2$SiteID<-sites2$SiteIDNew[match(surveys2$SiteID,sites2$SiteID)]
}

#get habitat and update IDs
habitat1<-getHabitat(cb1)
habitat2<-getHabitat(cb2,surveys)

if(nrow(habitat2)>0){
  if(nrow(habitat1)>0) habitat2$HabitatIDNew<-habitat1$HabitatID[match(habitat2$Habitat,habitat1$Habitat)]
  habitat2$NewHabitat<-FALSE
  habitat2$NewHabitat[is.na(habitat2$HabitatIDNew)]<-TRUE
  habitat2$HabitatIDNew[is.na(habitat2$HabitatIDNew)]<-getLastID(cb1,"Habitat")+(1:nrow(habitat2[is.na(habitat2$HabitatIDNew),]))
}

#get stations and update IDs
stations1<-getStations(cb1)
stations2<-getStations(cb2,surveys)

if(nrow(stations2)>0){
  if(nrow(surveys2)>0) stations2$SurveyID<-surveys2$SurveyIDNew[match(stations2$SurveyID,surveys2$SurveyID)]
  if(nrow(habitat2)>0) stations2$HabitatID<-habitat2$HabitatIDNew[match(stations2$HabitatID,habitat2$HabitatID)]
  
  if(nrow(stations1)) stations2$StationIDNew<-merge(stations2[,c("SurveyID","CamNumber1","CamNumber2")],stations1[,c("SurveyID","CamNumber1","CamNumber2","StationID")],all.x=T)$StationID
  stations2$NewStation<-FALSE
  stations2$NewStation[is.na(stations2$StationIDNew)]<-TRUE
  stations2$StationIDNew[is.na(stations2$StationIDNew)]<-getLastID(cb1,"Station")+(1:nrow(stations2[is.na(stations2$StationIDNew),]))
}

#get species and update IDs
species1<-getSpecies(cb1)
species2<-getSpecies(cb2)

if(nrow(species2)>0){
  if(nrow(species1)) species2$SpeciesIDNew<-species1$SpeciesID[match(species2$"Species",species1$"Species")]
  species2$NewSpecies=FALSE
  species2$NewSpecies[is.na(species2$SpeciesIDNew)]<-TRUE
  species2$SpeciesIDNew[is.na(species2$SpeciesIDNew)]<-getLastID(cb1,"Species")+(1:nrow(species2[is.na(species2$SpeciesIDNew),]))
}

#Check species
species1[!(species1$Species %in% species2$Species),] #species ony found in CameraBase1
species2[species2$NewSpecies==T,] #species ony found in CameraBase2

spall<-rbind(species2[species2$NewSpecies==T,1:(ncol(species2)-2)][,],
             species1[!(species1$Species %in% species2$Species),])
spall[order(spall$Species),] #species not present in both databases


#get station dates and update IDs
cameradays1<-getStationDates(cb1)
cameradays2<-getStationDates(cb2,surveys)

if(nrow(cameradays2)>0){
  if(nrow(stations2)) cameradays2$StationID<-stations2$StationIDNew[match(cameradays2$StationID,stations2$StationID)]
  if(nrow(cameradays1)) cameradays2$Station_DatesIDNew<-merge(cameradays2[,c("StationID","Camera","Start","End")],cameradays1[,c("StationID","Camera","Start","End","Station_DatesID")],all.x=T)$Station_DatesID
  cameradays2$NewStationDate<-FALSE
  cameradays2$NewStationDate[is.na(cameradays2$Station_DatesIDNew)]<-TRUE
  cameradays2$Station_DatesIDNew[is.na(cameradays2$Station_DatesIDNew)]<-getLastID(cb1,"Station_Dates")+(1:nrow(cameradays2[is.na(cameradays2$Station_DatesIDNew),]))
}

#get animals and update IDs
animals1<-getAnimals(cb1)
animals2<-getAnimals(cb2,surveys)

if(nrow(animals2)>0){
  if(nrow(species2)) animals2$SpeciesID<-species2$SpeciesIDNew[match(animals2$SpeciesID,species2$SpeciesID)]
  if(nrow(animals1)) animals2$AnimalIDNew<-merge(animals2[,c("SpeciesID","Code")],animals1[,c("SpeciesID","Code","AnimalID")],all.x=T)$AnimalID
  animals2$NewAnimal<-FALSE
  animals2$NewAnimal[is.na(animals2$AnimalIDNew)]<-TRUE
  animals2$AnimalIDNew[is.na(animals2$AnimalIDNew)]<-getLastID(cb1,"Animal")+(1:nrow(animals2[is.na(animals2$AnimalIDNew),]))
}

#get captures and update IDs
captures2<-getAllData(cb2,surveys) #can add specific survey ID here
captures2<-captures2[,c("CaptureID","StationID","SpeciesID","AnimalID","Sex","Individuals","Date","Time","Image1","Image2","LeftImage1","Marked","Independent","DayNight")]

if(nrow(captures2)>0){
  if(nrow(stations2)) captures2$StationID<-stations2$StationIDNew[match(captures2$StationID,stations2$StationID)]
  if(nrow(species2)) captures2$SpeciesID<-species2$SpeciesIDNew[match(captures2$SpeciesID,species2$SpeciesID)]
  if(nrow(animals2)) captures2$AnimalID<-animals2$AnimalIDNew[match(captures2$AnimalID,animals2$AnimalID)]
  captures2$CaptureID<-getLastID(cb1,"Capture")+(1:nrow(captures2))
}


#save data to database
if(nrow(sites2[sites2$NewSite==T,])>0){
  sites2$SiteID<-sites2$SiteIDNew
  saveData(cb1,sites2[sites2$NewSite==T,1:(ncol(sites2)-2)],"Site")
}

if(nrow(surveys2[surveys2$NewSyurvey==T,])>0){
  surveys2$SurveyID<-surveys2$SurveyIDNew
  saveData(cb1,surveys2[surveys2$NewSyurvey==T,1:(ncol(surveys2)-2)],"Survey")
}

if(nrow(stations2[stations2$NewStation==T,])>0){
  stations2$StationID<-stations2$StationIDNew
  saveData(cb1,stations2[stations2$NewStation==T,c(1,2,4:(ncol(stations2)-2))],"Station")
}

if(nrow(habitat2[habitat2$NewHabitat==T,])>0){
  habitat2$HabitatID<-habitat2$HabitatIDNew
  saveData(cb1,habitat2[habitat2$NewHabitat==T,1:(ncol(habitat2)-2)],"Habitat")
}

if(nrow(cameradays2[cameradays2$NewStationDate==T,])>0){
  cameradays2$Station_DatesID<-cameradays2$Station_DatesIDNew
  saveData(cb1,cameradays2[cameradays2$NewStationDate==T,1:(ncol(cameradays2)-2)],"Station_Dates")
}

if(nrow(captures2)>0){
  saveData(cb1,captures2,"Capture")
}

if(nrow(animals2[animals2$NewAnimal==T,])>0){
  animals2$AnimalID<-animals2$AnimalIDNew
  saveData(cb1,animals2[animals2$NewAnimal==T,1:(ncol(animals2)-2)],"Animal")
}


#Compare data in the two databases
sp1<-getSpeciesSummary(cb1,surveys2$SurveyID)
sp2<-getSpeciesSummary(cb2,surveys)

(sp12<-merge(sp1,sp2,by="Species"))

mean(sp12$Frequency.x==sp12$Frequency.y) #if everything worked this is 1

#close databases
closeCameraBase(cb1)
closeCameraBase(cb2)

