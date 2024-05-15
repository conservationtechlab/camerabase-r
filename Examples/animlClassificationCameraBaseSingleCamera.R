##############################################################################
# This script takes images organized in folders by camera (folder name
# corresponds to camera name), classifies images with the animl package
# and imports images and capture data into Camera Base. This script only
# works for surveys with a single camera at each station. 
#
# It is assumed that survey and station data exist in Camera Base and that
# folder names match the codes of Camera 1 in the station data.
#

library(reticulate)
use_condaenv("animl-env")

library(animl)

#load camera base functions
source("CameraBaseFunctions.R")

####################
#parameters to be set
#
imagedir<-"" #directory where the raw images are stored
datadir<-"" #directory where temporary data files will be stored
dbpath <- "D:/CameraBase.mdb" #path to camera base
cbsurvey<-1 #survey we want to import data for

#machine learning models, get from animl repo
mdmodelfile<-"models/md_v5a.0.0_saved_model"  #MegaDetector model file
classmodelfile <- "models/Kenya/EfficientNetB5_456_Unfrozen_05_0.59_0.89_Kenya.h5" #Species classifier
classfile<-"models/Kenya/EfficientNetB5_456_Unfrozen_05_0.59_0.89_Kenya.txt" #class file

timezoneoffset=0 #time to add or subtract from video files
basedepth=length(strsplit(imagedir,split="/")[[1]])+1 #number of levels until we get to directories for each camera
removeempty=FALSE #remove empty images from import? TRUE or FALSE
ncores=16 #number of cores for parallel processing



#create global variable file and directory names
setupDirectory(datadir,globalenv())

#Open Camera Base
cb<-openCameraBase(dbpath)

#read in image data
images<-buildFileManifest(imagedir,exif=T,offset=timezoneoffset)

#get camera name from folder name
#adapt this to folder naming
images$Station<-sapply(images$Directory,function(x)strsplit(x,"/")[[1]][basedepth])
images$Camera<-images$Station
images$ImageDirectory<-sapply(images$Directory,function(x)paste(c(strsplit(x,"/")[[1]][1:(basedepth+1)],""),collapse="//"))
images$FileExtension<-tools::file_ext(images$FileName)
images$NewName=paste(images$Camera,format(images$DateTime,format="%Y%m%d_%H%M%S"),images$FileName,sep="_")

#get StationID from Camera Base
stations<-getStations(cb,cbsurvey)
images$StationID<-stations[match(images$Camera,stations$CamNumber1),]$StationID

if(sum(is.na(images$StationID))>0)stop(paste("Warning:",sum(is.na(imagesall$StationID)),"images don't have a matching StationID"))


####################
# save point
write.csv(images,file=filemanifest,row.names = F,quote = F)


####################
#check start and end dates for all cameras
camdates<-data.frame(unique(images[,c("Station","Camera")])[order(paste(unique(images[,c("Station","Camera")])$Station,unique(images[,c("Station","Camera")])$Camera,sep="")),],
                     StartDate=as.POSIXct(tapply(images$DateTime,paste(images$Station,images$Camera,sep="_"),min),origin='1970-01-01'),
                     EndDate=as.POSIXct(tapply(images$DateTime,paste(images$Station,images$Camera,sep="_"),max),origin='1970-01-01'))

#copy to clipboard to paste into Excel
write.table(camdates,file="clipboard",sep="\t",row.names = F)


####################
#Resume point
#read image file
images<-read.csv(file=filemanifest)
images$DateTime<-as.POSIXct(images$DateTime)

####################
# Process videos, extract frames for ID
imagesall<-imagesFromVideos(images,outdir=vidfdir,outfile=imageframes,frames=5,parallel=T,workers=ncores)

####################
#Resume point
imagesall<-read.csv(imageframes)
imagesall$DateTime<-as.POSIXct(imagesall$DateTime)

####################
#Detect animals with Megadetector
mdmodel<-loadMDModel(mdmodelfile)
mdres<-detectObjectBatch(mdmodel,imagesall$Frame,min_conf=0.1,mdversion=5,batch=6,outfile=mdresults,checkpoint = 2500)

imagesall <- parseMD(mdres,imagesall,outfile=cropfile)

####################
#Resume point
#read images with MD classification
imagesall<-read.csv(cropfile)
imagesall$DateTime<-as.POSIXct(imagesall$DateTime)

#########
#plot some images
plotBoxes(imagesall[sample(1:nrow(imagesall),1),],minconf=0.1)
plotBoxes(mdres[[sample(1:length(mdres),1)]],minconf=0.1)


####################
#Species classification
#

#select animal crops for classification
animals <- getAnimals(imagesall)
empty <- getEmpty(imagesall)

classes<-read.table(classfile,stringsAsFactors = F,sep="\t",quote = "\"",header=T)$Species

#use on-the-fly image crop generator, no need to save crop files
pred <- predictSpecies(animals,classmodelfile,resize=456,standardize=FALSE,batch = 48,workers=ncores)

#safe prediction output
write.csv(pred,predresults,row.names=F,quote = F)


####################
#Resume point
#read prediction matrix
pred<-as.matrix(read.csv(predresults))

#######################
#process species results
#
imagesall<-sequenceClassification(animals,empty,pred,classes,emptyclass = "Empty",stationcolumn="Station",sort=c("Station","Camera","DateTime"),maxdiff=60)

#safe classification
write.csv(imagesall,resultsfile,row.names = F,quote = F)


####################
#Resume point
#load saved data
imagesall<-read.csv(resultsfile)
imagesall$DateTime<-as.POSIXct(imagesall$DateTime)


####################
#export for import into Camera Base
#

#add species information, adapt for your data
#species_list<-read.csv(speciesfile,stringsAsFactors = F)
species_database<-getSpecies(cb)

imagesall$prediction[imagesall$prediction=="empty"]<-"Empty"
imagesall$prediction[imagesall$prediction=="human"]<-"Homo sapiens"
imagesall$prediction[imagesall$prediction=="vehicle"]<-"Vehicle"

#match to Camera Base species list
#imagesall$Species<-species_list[match(imagesall$prediction ,species_list$Common),"Species"]
imagesall$Species<-imagesall$prediction
imagesall$SpeciesID<-species_database[match(imagesall$Species ,species_database$Species),"SpeciesID"]
imagesall$Common<-species_database[match(imagesall$Species ,species_database$Species),"Common"]

#check if all species have a match
if(sum(is.na(imagesall$SpeciesID))>0)stop(paste("Warning:",sum(is.na(imagesall$SpeciesID)),"images don't have a matching SpeciesID"))
unique(imagesall[is.na(imagesall$SpeciesID),]$Species)


#summarize data for each image and prepare for import
exportdata<-imagesall[imagesall$Common!="Empty",][!duplicated(imagesall$FilePath[imagesall$Common!="Empty"]),]
exportdata<-rbind(exportdata,imagesall[!(imagesall$FilePath %in% exportdata$FilePath),][!duplicated(imagesall$FilePath[!(imagesall$FilePath %in% exportdata$FilePath)]),])

#filter out empty images if chosen
if(removeempty){
  exportdata<-exportdata[exportdata$Species!="Empty",]
}

cid<-getLastID(cb,"Capture")

export<-data.frame(CaptureID=(cid+1):(cid+nrow(exportdata)),
                   StationID=exportdata$StationID,
                   SpeciesID=exportdata$SpeciesID,
                   AnimalID=0,
                   Sex="unknown",
                   Individuals=1,
                   Date=as.Date(exportdata$DateTime),
                   Time=strftime(exportdata$DateTime, format="%H:%M:%S"),
                   Image1=exportdata$NewName,
                   Image2="",
                   LeftImgage1=0,
                   Marked=0,
                   Independent=0,
                   DayNight="",
                   stringsAsFactors = F)

saveData(cb,export,"Capture")

#copy images to Camera Base
settings<-getSettings(cb)
cbimagedir<-settings$ImageDir
if(!dir.exists(imagedir))stop("The image directory does not exist.")

#create Camera Base image directories if they don't exist
outdir<-paste0(cbimagedir,"\\",cbsurvey,"\\")
outdirsmall<-paste0(cbimagedir,"\\",cbsurvey,"\\small\\")
dir.create(outdir,showWarnings = F)
dir.create(outdirsmall,showWarnings = F)

file.copy(exportdata$FilePath,paste0(outdir,exportdata$NewName))




