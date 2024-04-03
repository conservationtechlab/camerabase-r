library(RODBC)
library(ggmap)
library(terra)
library(OpenStreetMap)
library(imager)

openCameraBase<-function(dbfile){
  odbcConnectAccess2007(dbfile)
}

closeCameraBase<-function(database){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  close(database)
  rm(database)
}

getAllData<-function(database,survey=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Capture.CaptureID,Survey.SurveyID, Station.StationID, Capture.AnimalID, Survey.[Survey Name],Species.SpeciesID, Species.Common, Species.Species, Capture.Date, Capture.Time, Capture.DayNight, Station.X, Station.Y, Station.Elevation, Habitat.Habitat, Station.Group1, Station.Group2, Capture.Sex, Capture.Individuals, Station.CamNumber1, Station.CamNumber2, Capture.Image1, Capture.Image2, Capture.Independent, Capture.Marked, Capture.LeftImage1
  FROM Habitat RIGHT JOIN (Survey RIGHT JOIN (Station INNER JOIN (Species RIGHT JOIN (Animal RIGHT JOIN Capture ON Animal.AnimalID = Capture.AnimalID) ON Species.SpeciesID = Capture.SpeciesID) ON Station.StationID = Capture.StationID) ON Survey.SurveyID = Station.SurveyID) ON Habitat.HabitatID = Station.HabitatID "
  if(!is.null(survey)){
    sql<-paste0(sql,"WHERE Survey.SurveyID IN (",paste(survey,collapse=","),") ")}
  sql<-paste0(sql,"ORDER BY Survey.[Survey Name], Species.Common, Station.CamNumber1, Capture.Date, Capture.Time;")
  sqlQuery(database,sql,stringsAsFactors = FALSE)
}

getLastCaptureID<-function(database){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT max(Capture.CaptureID) as ID from Capture;"
  as.numeric(sqlQuery(database,sql,stringsAsFactors = FALSE))
}

getLastID<-function(database,table){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  if(!(table %in% c("Animal","BatchImageTmp","BatchTmp","Capture","Habitat","Site","Station",
                    "Station_Dates","Species","Survey")))stop("Please provide a valid table name.")
  switch(table,
         Animal={sql<-"SELECT max(Animal.AnimalID) as ID from Animal;"},
         BatchImageTmp={sql<-"SELECT max(BatchImageTmpID) as ID from BatchImageTmp;"},
         BatchTmp={sql<-"SELECT max(BatchID) as ID from BatchTmp;"},
         Capture={sql<-"SELECT max(Capture.CaptureID) as ID from Capture;"},
         Habitat={sql<-"SELECT max(HabitatID) as ID from Habitat;"},
         Site={sql<-"SELECT max(SiteID) as ID from Site;"},
         Station={sql<-"SELECT max(Station_DatesID) as ID from Station_Dates;"},
         Station_Dates={sql<-"SELECT max(StationID) as ID from Station;"},
         Species={sql<-"SELECT max(SpeciesID) as ID from Species;"},
         Survey={sql<-"SELECT max(SurveyID) as ID from Survey;"}
  )
  id<-sqlQuery(database,sql,stringsAsFactors = FALSE)[1,1]
  if(is.na(id))id=0
  id
}

getSites<-function(database,survey=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Site.SiteID, Site.SiteName, Site.Description 
        FROM Site INNER JOIN Survey ON Site.SiteID = Survey.SiteID "
  if(!is.null(survey)){
    sql<-paste0(sql,"WHERE Survey.SurveyID IN (",paste(survey,collapse=","),") ")}
  sql<-paste0(sql,"GROUP BY Site.SiteID, Site.SiteName, Site.Description 
              ORDER BY Site.SiteID;")
  sqlQuery(database,sql,stringsAsFactors = FALSE)
}


getSurveys<-function(database,survey=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT * FROM Survey "
  if(!is.null(survey)){
    sql<-paste0(sql,"WHERE Survey.SurveyID IN (",paste(survey,collapse=","),") ")}
  sql<-paste0(sql,"ORDER BY SurveyID;")
  sqlQuery(database,sql,stringsAsFactors = FALSE)
}

getSurveyID<-function(database,survey=NULL){
  if(is.null(survey))
    stop("Please provide values for survey.")
  surveylist<-getSurveys(database)
  surveylist[match(survey,surveylist$`Survey Name`),"SurveyID"]
}

getSurveySummary<-function(database,survey=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Survey.SurveyID,Survey.[Survey Name], Survey.StartDate, Survey.EndDate, Survey.Lat, Survey.Long, Survey.CameraDays, Count(Station.StationID) AS Stations
        FROM Survey INNER JOIN Station ON Survey.SurveyID = Station.SurveyID "
  sql<-paste0(sql,"GROUP BY Survey.SurveyID,Survey.[Survey Name], Survey.StartDate, Survey.EndDate, Survey.Lat, Survey.Long, Survey.CameraDays ")
  if(!is.null(survey)){
    sql<-paste0(sql,"HAVING Survey.SurveyID IN (",paste(survey,collapse=","),") ")}
  sql<-paste0(sql,"ORDER BY Survey.[Survey Name];")
  sqlQuery(database,sql,stringsAsFactors = FALSE)
}

getStations<-function(database,survey=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Station.StationID,Survey.SurveyID, Survey.[Survey Name], Station.HabitatID,Station.X, Station.Y, Station.Elevation, Station.Pair, Station.CamNumber1, Station.CamNumber2, Station.CamModel1, Station.CamModel2, Station.Group1, Station.Group2, Station.Comments 
              FROM Survey INNER JOIN Station ON Survey.SurveyID = Station.SurveyID "
  if(!is.null(survey)){
    sql<-paste0(sql,"WHERE Survey.SurveyID IN (",paste(survey,collapse=","),") ")}
  sql<-paste0(sql,"ORDER BY Survey.SurveyID,Station.StationID;")
  sqlQuery(database,sql,stringsAsFactors = FALSE)
}

getStationSummary<-function(database,survey=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Survey.SurveyID,  Station.StationID, Survey.[Survey Name],Station.CamNumber1, Station.CamNumber2, Station.X, Station.Y, Station.Group1, Station.Group2, Min(Capture.Date) AS MinOfDate, Max(Capture.Date) AS MaxOfDate, Count(Capture.CaptureID) AS Images
        FROM (Survey INNER JOIN Station ON Survey.SurveyID = Station.SurveyID) INNER JOIN Capture ON Station.StationID = Capture.StationID "
  sql<-paste0(sql,"GROUP BY Survey.SurveyID, Survey.[Survey Name], Station.StationID, Station.CamNumber1, Station.CamNumber2, Station.X, Station.Y, Station.Group1, Station.Group2 ")
  if(!is.null(survey)){
    sql<-paste0(sql,"HAVING Survey.SurveyID IN (",paste(survey,collapse=","),") ")}
  sql<-paste0(sql,"ORDER BY Survey.[Survey Name], Station.StationID;")
  sqlQuery(database,sql,stringsAsFactors = FALSE)
}

getStationImageDates<-function(database,survey=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Station.SurveyID, Station.StationID, 1 AS Camera, Min(Capture.Date) AS Start, Max(Capture.Date) AS End
  FROM Station INNER JOIN Capture ON Station.StationID = Capture.StationID
  WHERE (((Capture.Image1)>''))
  GROUP BY Station.SurveyID, Station.StationID"
  if(!is.null(survey)){
    sql<-paste0(sql," HAVING Station.SurveyID IN (",paste(survey,collapse=","),") ")}
  sql<-paste0(sql," UNION ALL 
  SELECT Station.SurveyID, Station.StationID, 2 as Camera, Min(Capture.Date) AS Start, Max(Capture.Date) AS End 
  FROM Station INNER JOIN Capture ON Station.StationID = Capture.StationID 
  WHERE (((Capture.Image2)>'')) 
  GROUP BY Station.SurveyID, Station.StationID ")
  if(!is.null(survey)){
    sql<-paste0(sql," HAVING Station.SurveyID IN (",paste(survey,collapse=","),") ")}
  sql<-paste0(sql," ORDER BY StationID;")
  sqlQuery(database,sql,stringsAsFactors = FALSE)
}

getStationDates<-function(database,survey=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Station_Dates.Station_DatesID, Station_Dates.StationID, Station_Dates.Camera, Station_Dates.Start, Station_Dates.End
FROM (Survey INNER JOIN Station ON Survey.SurveyID = Station.SurveyID) INNER JOIN Station_Dates ON Station.StationID = Station_Dates.StationID "
  if(!is.null(survey)){
    sql<-paste0(sql,"WHERE Survey.SurveyID IN (",paste(survey,collapse=","),") ")}
  sql<-paste0(sql,"ORDER BY Station_Dates.Station_DatesID;")
  sqlQuery(database,sql,stringsAsFactors = FALSE)
}

getSpecies<-function(database){
  sqlQuery(database,
           paste0("SELECT * FROM Species;"),
           stringsAsFactors = FALSE)
}

getSpeciesID<-function(database,species=NULL,common=NULL){
  if(!is.null(species) && !is.null(common))
    stop("Please provide values for species or common but not both.")
  if(is.null(species) && is.null(common))
    stop("Please provide values for species or common.")
  splist<-getSpecies(database)
  if(!is.null(species)){
    splist[match(species,splist$Species),"SpeciesID"]
  }else if(!is.null(common)){
    splist[match(common,splist$Common),"SpeciesID"]
  }
}

getSpeciesSummary<-function(database,survey,species=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")  
  sql<-"SELECT Survey.[Survey Name], Species.Common, Species.Species, Count(Capture.Date) AS Events,Avg(SpeciesCamera2.CountOfStationID) AS Cameras, Round(Count(Capture.Date)/Max(Survey.CameraDays)*1000,2) AS Frequency
  FROM Survey INNER JOIN (Station INNER JOIN ((Species INNER JOIN Capture ON Species.SpeciesID = Capture.SpeciesID) INNER JOIN SpeciesCamera2 ON Species.SpeciesID = SpeciesCamera2.SpeciesID) ON (Station.SurveyID = SpeciesCamera2.SurveyID) AND (Station.StationID = Capture.StationID)) ON Survey.SurveyID = Station.SurveyID
  WHERE (((Capture.Independent)=True) AND ((Capture.Date)>=survey.startdate) AND ((Capture.Date)<=survey.enddate)) "
  if(!is.null(survey)){
    sql<-paste0(sql,"AND Survey.SurveyID IN (",paste(survey,collapse=","),") ")}
  if(!is.null(species)){  
    sql<-paste0(sql,"AND Species.Common IN (",paste(sQuote(species,F),collapse=","),")")
  }
  sql<-paste0(sql,"GROUP BY Survey.[Survey Name], Species.Common, Species.Species
  ORDER BY Survey.[Survey Name], Species.Common;")
  sqlQuery(database,sql,stringsAsFactors = FALSE)
}

getIndividualDetections<-function(database,survey=NULL,species=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Survey.SurveyID, Station.StationID, Capture.CaptureID, Animal.AnimalID, Species.SpeciesID, Species.Common, Species.Species, Station.X, Station.Y, Animal.Code, Animal.Sex, Animal.Age, Capture.Date, Capture.Time, Capture.Image1, Capture.Image2, Capture.LeftImage1 
              FROM (Animal INNER JOIN ((Survey INNER JOIN Station ON Survey.SurveyID = Station.SurveyID) INNER JOIN Capture ON Station.StationID = Capture.StationID) ON Animal.AnimalID = Capture.AnimalID) INNER JOIN Species ON Capture.SpeciesID = Species.SpeciesID "
  if(!is.null(survey) | !is.null(species)){
    sql<-paste0(sql,"WHERE")
  }
  if(!is.null(survey)){
    sql<-paste0(sql," Survey.SurveyID IN (",paste(survey,collapse=","),")")
    add=T
  }
  if(!is.null(species)){  
    if(add)sql<-paste(sql,"AND")
    sql<-paste0(sql," Species.Common IN (",paste(sQuote(species,F),collapse=","),")")
  }
  sqlQuery(database, sql, stringsAsFactors = FALSE)
}

getAnimals<-function(database,survey=NULL,species=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Animal.AnimalID, Animal.SpeciesID, Animal.Code, Animal.Sex, Animal.Age, Animal.Description, Animal.Comments
        FROM Animal INNER JOIN ((Survey INNER JOIN Station ON Survey.SurveyID = Station.SurveyID) INNER JOIN Capture ON Station.StationID = Capture.StationID) ON Animal.AnimalID = Capture.AnimalID "
  if(!is.null(survey) | !is.null(species)){
    sql<-paste0(sql,"WHERE")
  }
  if(!is.null(survey)){
    sql<-paste0(sql," Survey.SurveyID IN (",paste(survey,collapse=","),")")
    add=T
  }
  if(!is.null(species)){  
    if(add)sql<-paste(sql,"AND")
    sql<-paste0(sql," Species.Common IN (",paste(sQuote(species,F),collapse=","),")")
  }
  sql<-paste0(sql,"GROUP BY Animal.AnimalID, Animal.SpeciesID, Animal.Code, Animal.Sex, Animal.Age, Animal.Description, Animal.Comments
              ORDER BY Animal.AnimalID;")
  sqlQuery(database, sql, stringsAsFactors = FALSE)
}

getHabitat<-function(database,survey=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Habitat.HabitatID, Habitat.Habitat
        FROM Habitat INNER JOIN Station ON Habitat.HabitatID = Station.HabitatID "
  if(!is.null(survey)){
    sql<-paste0(sql,"WHERE Station.SurveyID IN (",paste(survey,collapse=","),") ")}
  sql<-paste0(sql,"GROUP BY Habitat.HabitatID, Habitat.Habitat 
              ORDER BY Habitat.HabitatID;")
  sqlQuery(database,sql,stringsAsFactors = FALSE)
}

getBatches<-function(database){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT Survey.SurveyID, Station.CamNumber1, Station.CamNumber2, BatchTmp.BatchID FROM Survey 
        INNER JOIN (BatchTmp INNER JOIN Station ON BatchTmp.StationID = Station.StationID) ON Survey.SurveyID = Station.SurveyID;"
  sqlQuery(database, sql, stringsAsFactors = FALSE)  
}

getBatcheImages<-function(database,batch=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT * FROM BatchImageTmp "
  if(!is.null(batch)){
    sql<-paste0(sql,"WHERE BatchImageTmp.BatchID IN (",paste(batch,collapse=","),")")
  }
  sql<-paste0(sql," ORDER BY BatchImageTmpID;")
  sqlQuery(database, sql, stringsAsFactors = FALSE) 
}

getSettings<-function(database){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sql<-"SELECT TOP 1 Settings.ImageDir, Settings.BinDir , Settings.renamefiles FROM Settings;"
  sqlQuery(database, sql, stringsAsFactors = FALSE)  
}

saveBatch<-function(database,batch,batchimages){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  batchid<-as.integer(sqlQuery(database, "SELECT Max(BatchTmp.BatchID) AS MaxOfBatchID FROM BatchTmp;",stringsAsFactors = FALSE))
  if(is.na(batchid))batchid<-0
  batch$BatchID<-batch$BatchID+batchid
  batchimages$BatchID<-batchimages$BatchID+batchid
  sqlSave(database,batch,tablename="BatchTmp",append=T,rownames = F)
  sqlSave(database,batchimages,tablename="BatchImageTmp",append=T,rownames = F,fast=F)
}

saveCaptures<-function(database,data){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sqlSave(database,data,tablename="Capture",append=T,rownames = F,fast=F)
}

saveData<-function(database,data,table){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  if(!(table %in% c("Animal","BatchImageTmp","BatchTmp","Capture","Habitat","Site","Station",
                    "Station_Dates","Species","Survey")))stop("Please provide a valid table name.")
    sqlSave(database,data,tablename=table,append=T,rownames = F, colnames = FALSE ,fast=F)
}


updateSpeciesID<-function(database,data){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sqlUpdate(database,data[,c("CaptureID","SpeciesID")],tablename="Capture",index="CaptureID")
}

updateSpeciesIDBatch<-function(database,data){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  sqlUpdate(database,data[,c("BatchImageTmpID","BatchID","SpeciesID")],tablename="BatchImageTmp",index=c("BatchImageTmpID","BatchID"))
}

getCameraDayMatrix<-function(database,survey=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  if(is.null(survey))
    stop("Please provide values for survey.")
  data<-sqlQuery(database,
                 paste0("SELECT Survey.SurveyID, Survey.[Survey Name], Station.StationID, Station.CamNumber1, Station.CamNumber2, Station_Dates.Camera, Station_Dates.Start, Station_Dates.End, Survey.StartDate, Survey.EndDate, DateDiff('d',[Survey].[StartDate],[Station_Dates].[Start])+1 AS StartInt, DateDiff('d',[Survey].[StartDate],[Station_Dates].[End])+1 AS EndInt
                  FROM (Survey INNER JOIN Station ON Survey.SurveyID = Station.SurveyID) INNER JOIN Station_Dates ON Station.StationID = Station_Dates.StationID
                  WHERE Survey.SurveyID IN (",paste(survey,collapse=","),")
                  ORDER BY Survey.SurveyID,Station.StationID;"),
                 stringsAsFactors = FALSE)
  
  data$StartInt[data$StartInt<1]<-1
  data$EndInt[data$EndInt<1]<-1
  
  surveysum<-getSurveySummary(database,survey)
  totalday<-round(surveysum$EndDate-surveysum$StartDate+1,0)
  
  camdays<-matrix(0,length(unique(data$StationID)),max(totalday))
  
  i=1
  for(s in unique(data$StationID)){
    datasel<-data[data$StationID==s,]
    for(n in 1:nrow(datasel)){
      camdays[i,datasel[n,"StartInt"]:datasel[n,"EndInt"]]<-1
    }
    i=i+1
  }
  colnames(camdays)<-1:ncol(camdays)
  row.names(camdays)<-unique(data$StationID)
  stationdata<-getStations(database,survey)
  stationdata<-stationdata[stationdata$StationID %in% unique(data$StationID),]
  list(summary=tapply(apply(camdays,1,sum),stationdata$`Survey Name`,sum),stationdata=stationdata,cameradays=camdays)
}


getDetectionyMatrix<-function(database,survey=NULL,species=NULL){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  specieslist<-getSpecies(database)
  if(!is.null(species))
    specieslist<-merge(data.frame(SpeciesID=species),specieslist)
  specieslist<-specieslist[order(specieslist$Species),]
  species<-specieslist$SpeciesID
  add=F
  
  sql<-"SELECT Survey.SurveyID, Station.StationID, Capture.CaptureID, Capture.SpeciesID, Survey.[Survey Name], Species.Species, Species.Common, DateDiff('d',[Survey].[StartDate],[Capture].[Date])+1 AS DateInt
              FROM ((Survey INNER JOIN Station ON Survey.SurveyID = Station.SurveyID) INNER JOIN Capture ON Station.StationID = Capture.StationID) INNER JOIN Species ON Capture.SpeciesID = Species.SpeciesID
              WHERE Capture.Date >= Survey.StartDate AND Capture.Date<=Survey.EndDate
              GROUP BY Survey.SurveyID, Station.StationID, Capture.CaptureID, Capture.SpeciesID, Survey.[Survey Name], Species.Species, Species.Common, DateDiff('d',[Survey].[StartDate],[Capture].[Date])+1, Species.SpeciesID "
  if(!is.null(survey) | !is.null(species)){
    sql<-paste0(sql,"HAVING")
  }
  if(!is.null(survey)){
    sql<-paste0(sql," Survey.SurveyID IN (",paste(survey,collapse=","),") ")
    add=T
  }
  if(!is.null(species)){  
    if(add)sql<-paste(sql,"AND")
    sql<-paste0(sql," Species.SpeciesID IN (",paste(species,collapse=","),") ")
  }
  sql<-paste0(sql,"ORDER BY Survey.SurveyID, Station.StationID;")   
  
  data<-sqlQuery(database,sql,stringsAsFactors = FALSE)
  
  camdays<-getCameraDayMatrix(database,survey)
  
  detections<-array(0,c(length(species),dim(camdays$cameradays)))
  
  i=1
  for(s in species){
    datasel<-data[data$SpeciesID==s,]
    detections[i,,][!camdays$cameradays]<-NA
    for(m in unique(datasel$StationID)){
      msel<-which(camdays$stationdata$StationID==m)
      detections[i,msel,datasel[datasel$StationID==m,"DateInt"]]<-1
    }
    
    i=i+1
  }
  
  dimnames(detections)<-list(specieslist$Species,row.names(camdays$cameradays),colnames(camdays$cameradays))
  
  #list(summary=tapply(apply(t$cameradays,1,sum),t$stationdata$`Survey Name`,sum),stationdata=stationdata,cameradays=camdays)
  detections
}

plotStations<-function(database,survey=NULL,species=NULL,type="osm",buffer=0.25,size=3,crs="+proj=utm +zone=19 +south +ellps=WGS84 +datum=WGS84"){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  if(is.null(survey))
    stop("Please provide values for survey.")
  
  stations<-getStations(database,survey)
  utm<-vect(stations,geom=c("X","Y"),crs=crs)
  latlong<-project(utm,"+proj=longlat +ellps=WGS84 +datum=WGS84")
  stations$Lat<-geom(latlong)[,4]
  stations$Long<-geom(latlong)[,3]
  stations<-stations[!is.na(stations$Lat),]
  buffer=max((max(stations$Lat)-min(stations$Lat)),max(stations$Long-min(stations$Long)))*buffer
  #bbox<-c(min(stations$Long,na.rm=T)-buffer, min(stations$Lat,na.rm=T)-buffer,max(stations$Long,na.rm=T)+buffer,max(stations$Lat,na.rm=T)+buffer)
  #pdx.map <- get_stamenmap(bbox = bbox)
  #ggmap(pdx.map)+geom_point(data=stations, aes(x=Long, y=Lat), size=3, color="magenta")
  map <- openmap(c(max(stations$Lat,na.rm=T)+buffer,min(stations$Long,na.rm=T)-buffer), c(min(stations$Lat,na.rm=T)-buffer,max(stations$Long,na.rm=T)+buffer),type=type)
  map_longlat <- openproj(map, projection = "+proj=longlat")
  mapplot<-OpenStreetMap::autoplot.OpenStreetMap(map_longlat)+
    xlab("Longitude") + ylab("Latitude")
  if(!is.null(species)){
    sql<-"SELECT Station.StationID, Count(Capture.CaptureID) AS CountOfCaptureID
          FROM (Station INNER JOIN Survey ON Station.SurveyID = Survey.SurveyID) INNER JOIN Capture ON Station.StationID = Capture.StationID "
    sql<-paste0(sql,"WHERE (Survey.SurveyID IN (",paste(survey,collapse=","),") ")
    sql<-paste0(sql," AND Capture.SpeciesID = ",species)
    sql<-paste0(sql," AND Capture.Independent=True)")
    sql<-paste0(sql," GROUP BY Station.StationID;")
    spcount<-sqlQuery(database,sql,stringsAsFactors = FALSE)
    spcount<-merge(spcount,stations)
    mapplot+geom_point(data=stations, aes(x=Long, y=Lat), size=2, color="black") +
      geom_point(data=spcount, aes(x=Long, y=Lat), size=size, color="magenta")
  }else{
    mapplot+geom_point(data=stations, aes(x=Long, y=Lat), size=size, color="magenta")
  }
  #plot(map_longlat,raster=TRUE)
  #plot(latlong,add=T,col="magenta",cex=1.2)
}


saveImage<-function(database,file,camera,datetime,survey){
  if(!inherits(database,"RODBC"))stop("Please provide a valid RODBC database connection.")
  if(!file.exists(file))stop("The image file does not exist.")
  
  #get image directory
  settings<-getSettings(database)
  imagedir<-settings$ImageDir
  if(!dir.exists(imagedir))stop("The image directory does not exist.")
  
  #remove non-standared characters from camera name
  camera<-gsub("[^0-9A-Za-z]","_" , camera ,ignore.case = TRUE)
  
  #copy and rename image file
  filenew<-paste0(camera,"_",format(datetime,"%Y%m%d"),"_",format(datetime,"%H%M%S"),".",tools::file_ext(file))
  
  sql<-paste0("SELECT Capture.Image1 as image FROM Capture where Capture.Image1 = '",filenew, "' UNION ALL ",
              "SELECT Capture.Image2 as image FROM Capture where Capture.Image2 = '", filenew, "';")
  
  imagerows<-  sqlQuery(database,sql,stringsAsFactors = FALSE)
  
  n=1
  while(nrow(imagerows)>0){
    filenew<-paste0(camera,"_",format(datetime,"%Y%m%d"),"_",format(datetime,"%H%M%S"),"_",sprintf("%03d",n),".",tools::file_ext(file))
    sql<-paste0("SELECT Capture.Image1 as image FROM Capture where Capture.Image1 = '",filenew, "' UNION ALL ",
                "SELECT Capture.Image2 as image FROM Capture where Capture.Image2 = '", filenew, "';")
    
    imagerows<-  sqlQuery(database,sql,stringsAsFactors = FALSE)
    n=n+1
  }
  
  outdir<-paste0(imagedir,"\\",survey,"\\")
  outdirsmall<-paste0(imagedir,"\\",survey,"\\small\\")
  
  dir.create(outdir,showWarnings = F)
  dir.create(outdirsmall,showWarnings = F)
  
  file.copy(file,paste0(outdir,filenew))
  
  img<-load.image(file)
  img<-imresize(img,scale=800/dim(img)[1])
  save.image(img,paste0(outdirsmall,filenew))
  filenew
}



matchPairs<-function(images,stations,tolerance=180,offset1=0,offset2=0,offset_video=0,interval=15){
  bti=1 #BatchImageTempID
  bi=1 #BatchID
  
  batchimage<-data.frame(BatchImageTmpID=numeric(),BatchID=numeric(),StationID=numeric(),Img1=character(),Img2=character(),
                         DateImg1=as.POSIXct(character()),DateImg2=as.POSIXct(character()),SpeciesID=numeric(),Sex=character(),
                         Individuals=numeric(),LeftImage1=numeric(),Marked=numeric(),Remove=numeric())
  
  batch<-data.frame(BatchID=numeric(),StationID=numeric(),Dir1=character(),Dir2=character(),Tolerance=numeric(),
                    OffsetCam1=numeric(),OffsetCam2=numeric(),OffsetVideo=numeric(),Interval=numeric())
  
  opb <- pbapply::pboptions(char = "=")
  for(s in 1:nrow(stations)){
    
    print(paste("Processing Station", stations$StationID[s],stations$CamNumber1[s],stations$CamNumber2[s]))
    
    if(!is.na(stations$CamNumber1[s])){
      images1<-images[images$Camera==stations$CamNumber1[s],c("Camera","FilePath","DateTime","ImageDirectory")]
      images1$DateTime<-as.POSIXct(images1$DateTime)
      images1<-images1[order(images1$DateTime),]
    }else{
      images1<-images[1,c("Camera","FilePath","DateTime","ImageDirectory")][-1,]
    }
    
    if(!is.na(stations$CamNumber2[s])){
      images2<-images[images$Camera==stations$CamNumber2[s],c("Camera","FilePath","DateTime","ImageDirectory")]
      images2$DateTime<-as.POSIXct(images2$DateTime) 
      images2<-images2[order(images2$DateTime),]
    }else{
      images2<-images[1,c("Camera","FilePath","DateTime","ImageDirectory")][-1,]
    }
    
    i1=1
    i2=1
    ni1<-nrow(images1)
    ni2<-nrow(images2)
    
    batchimagetmp<-list()
    bit=1 #batch image temp index for list
    
    if(ni1>0 | ni2>0){
      batch<-rbind(batch,data.frame(BatchID=bi,StationID=stations$StationID[s],Dir1=images1$ImageDirectory[1],Dir2=images2$ImageDirectory[1],Tolerance=tolerance,
                                    OffsetCam1=offset1,OffsetCam2=offset2,OffsetVideo=offset_video,Interval=interval))
      
      pb <- pbapply::startpb(1, ni1+ni2)
      c=(ni1+ni2)/100
      while(i1<ni1 || i2<ni2){
        if(ni1>0 && ni2>0 && i1<=ni1 && i2<=ni2 && abs(difftime(images1[i1,]$DateTime+offset1,images2[i2,]$DateTime+offset2,units="secs"))<=tolerance){
          batchimagetmp[[bit]]<-data.frame(BatchImageTmpID=bti,BatchID=bi,StationID=stations$StationID[s],
                                           Img1=images1[i1,]$FilePath,Img2=images2[i2,]$FilePath,
                                           DateImg1=images1[i1,]$DateTime,DateImg2=images2[i2,]$DateTime,SpeciesID=NA,Sex="unknown",
                                           Individuals=1,LeftImage1=1,Marked=0,Remove=0)
          #if(i1<ni1)i1=i1+1
          #if(i2<ni2)i2=i2+1
          i1=i1+1
          i2=i2+1
        } else if((ni1>0 && ni2==0) || (i1<ni1 && i2>=ni2) || ((images1[i1,]$DateTime+offset1<images2[i2,]$DateTime+offset2 && i1<ni1))) {
          batchimagetmp[[bit]]<-data.frame(BatchImageTmpID=bti,BatchID=bi,StationID=stations$StationID[s],
                                           Img1=images1[i1,]$FilePath,Img2="",
                                           DateImg1=images1[i1,]$DateTime,DateImg2=as.POSIXct(NA),SpeciesID=NA,Sex="unknown",
                                           Individuals=1,LeftImage1=1,Marked=0,Remove=0)
          i1=i1+1
        } else if((ni1==0 && ni2>0) || (i2<ni2 && i1>=ni1) || ((images1[i1,]$DateTime+offset1>images2[i2,]$DateTime+offset2 && i2<ni2))) {
          batchimagetmp[[bit]]<-data.frame(BatchImageTmpID=bti,BatchID=bi,StationID=stations$StationID[s],
                                           Img1="",Img2=images2[i2,]$FilePath,
                                           DateImg1=as.POSIXct(NA),DateImg2=images2[i2,]$DateTime,SpeciesID=NA,Sex="unknown",
                                           Individuals=1,LeftImage1=1,Marked=0,Remove=0)   
          i2=i2+1    
          
        }
        #cat(i1,"/",i2,"/n")
        bti=bti+1
        bit=bit+1
        if((i1+i2)>c){
          pbapply::setpb(pb, i1+i2)
          c<-c+(ni1+ni2)/100
        }
      }
      batchimage<-rbind(batchimage,do.call(rbind,batchimagetmp))
      pbapply::setpb(pb, ni1+ni2)
      pbapply::closepb(pb)
      
      bi=bi+1
    }
  }
  list(batch=batch,batchimage=batchimage)
}



