library(here)
library(ecRutils)
library(rgdal)
library(raster)
library(dplyr)
library(gdalUtils)


### set AOI dimension

#indonesia: 
aoi<-"indonesia"
north<-c("10N","00N","10S")
west<-c(
  paste0("0",c(90:90),"E"),
  paste0("",seq(100,140,10),"E")
)
x<-expand.grid(north=north,west=west)
x<-paste0(x[,1],"_",x[,2])
dim<-c(x)

#global oil palm suitable area (latitudes up to +/- 30 degree from equator, all longitudes except for large parts of the pacific)
aoi<-"world"
north<-c("30N", "20N", "10N", "00N", "10S","20S")
west<-c(sprintf("%02.0f0W", 12:1), sprintf("%02.0f0E", 0:17))
x<-expand.grid(north=north,west=west)
x<-paste0(x[,1],"_",x[,2])
dim<-c(x)


############################
### download forest loss ###
############################

nowrun=1

dset = "lossyear" 

if (nowrun==1){
    
      ###hansen version
      # url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2016-v1.4/"
      # url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/"
      url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2018-v1.6/"
    
      ##file list
      pre<-paste0("Hansen_GFC-2018-v1.6_",dset)
      flist<-paste0(pre,"_",dim,".tif")
    
      ##download tile raster
      a<-lapply(
        flist,
        function(x){
          print(x)
          if (file.exists(file.path("data","tmp","lossyear",paste0("h.",x)))==F){
            download.file(paste0(url,x),file.path("data","tmp","lossyear",paste0("h.",x)), mode="wb")
          }
        }
      )
      
      ##create file index for virtual raster
      input_files=paste0(
        list.files(file.path(c(here()), "data","tmp","lossyear"),paste0("h.Hansen"),full.names=T))
      #modify paths for use with windows
      if(Sys.info()["sysname"] == "Windows") {
        input_files<-gsub('/', '\\\\', input_files)
      }
      #save file paths to .txt
      index<-file(paste0(file.path("data","tmp","lossyear",paste0("h.2018.",dset,".index.txt"))))
      writeLines(input_files, con=index)
      
      ##create virtual raster
      outfile<-paste0(file.path(c(here()), "data","tmp","lossyear",paste0("h.2018.",dset,".raster.vrt")))
      infile<-file.path(c(here()), "data","tmp","lossyear",paste0("h.2018.",dset,".index.txt"))
      #modify paths for use with windows
      if(Sys.info()["sysname"] == "Windows") {
        infile<-gsub('/', '\\\\', infile)
        outfile<-gsub('/', '\\\\', outfile)
      }
      gdalbuildvrt(output.vrt = c(outfile), 
                   input_file_list = c(infile), 
                   resolution = "average", 
                   r = "nearest")
      
      ##translate virtual raster to compressed TIFF and store in output folder
      destfile<-paste0(file.path(c(here()),"data","store",paste0("h.2018.",aoi,".",dset,".tif")))
      #modify paths for use with windows
      if(Sys.info()["sysname"] == "Windows") {
        infile<-gsub('/', '\\\\', infile)
        outfile<-gsub('/', '\\\\', outfile)
      }
      if (file.exists(destfile)==F){
        gdal_translate(outfile, 
                       destfile,
                       co = c("COMPRESS=LZW", "NUM_THREADS=15", "BIGTIFF=YES"), #num_threads for parallel compression, bigtiff for >4GB tiff
                       ot = "Byte")
      }
    
      ##delete temp files
      file.remove(
        c(
          paste0(file.path("data","tmp","lossyear",paste0("h.",flist))),
          paste0(file.path("data","tmp","lossyear",paste0("h.2018.",dset,".index.txt"))),
          paste0(file.path("data","tmp","lossyear",paste0("h.2018.",dset,".raster.vrt")))
        )
      )
}


##########################
### download data mask ###
##########################

dset = "datamask" 

if (nowrun==1){
  
  ###hansen version
  # url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2016-v1.4/"
  # url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/"
  url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2018-v1.6/"
  
  ##file list
  pre<-paste0("Hansen_GFC-2018-v1.6_",dset)
  flist<-paste0(pre,"_",dim,".tif")
  
  ##download tile raster
  a<-lapply(
    flist,
    function(x){
      print(x)
      if (file.exists(file.path("data","tmp","datamask",paste0("h.",x)))==F){
        download.file(paste0(url,x),file.path("data","tmp","datamask",paste0("h.",x)), mode="wb")
      }
    }
  )
  
  ##create file index for virtual raster
  input_files=paste0(
    list.files(file.path(c(here()), "data","tmp","datamask"),paste0("h.Hansen"),full.names=T))
  #modify paths for use with windows
  if(Sys.info()["sysname"] == "Windows") {
    input_files<-gsub('/', '\\\\', input_files)
  }
  #save file paths to .txt
  index<-file(paste0(file.path("data","tmp","datamask",paste0("h.2018.",dset,".index.txt"))))
  writeLines(input_files, con=index)
  
  ##create virtual raster
  infile<-file.path(c(here()), "data","tmp","datamask",paste0("h.2018.",dset,".index.txt"))
  outfile<-paste0(file.path(c(here()), "data","tmp","datamask",paste0("h.2018.",dset,".raster.vrt")))
  #modify paths for use with windows
  if(Sys.info()["sysname"] == "Windows") {
    infile<-gsub('/', '\\\\', infile)
    outfile<-gsub('/', '\\\\', outfile)
  }
  gdalbuildvrt(output.vrt = c(outfile), 
               input_file_list = c(infile), 
               resolution = "average", 
               r = "nearest")
  
  ##translate virtual raster to compressed TIFF and store in output folder
  destfile<-paste0(file.path(c(here()),"data","store",paste0("h.2018.",aoi,".",dset,".tif")))
  #modify paths for use with windows
  if(Sys.info()["sysname"] == "Windows") {
    infile<-gsub('/', '\\\\', infile)
    outfile<-gsub('/', '\\\\', outfile)
  }
  if (file.exists(destfile)==F){
    gdal_translate(outfile, 
                   destfile,
                   co = c("COMPRESS=LZW", "NUM_THREADS=15", "BIGTIFF=YES"), #num_threads for parallel compression, bigtiff for >4GB tiff
                   ot = "Byte")
  }
  
  ##delete temp files
  file.remove(
    c(
      paste0(file.path("data","tmp","datamask",paste0("h.",flist))),
      paste0(file.path("data","tmp","datamask",paste0("h.2018.",dset,".index.txt"))),
      paste0(file.path("data","tmp","datamask",paste0("h.2018.",dset,".raster.vrt")))
    )
  )
}


###########################
### download tree cover ###
###########################

dset = "treecover2000"  

if (nowrun==1){
    
    ###hansen version
    # url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2016-v1.4/"
    # url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/"
    url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2018-v1.6/"
    
    ##file list
    pre<-paste0("Hansen_GFC-2018-v1.6_",dset)
    flist<-paste0(pre,"_",dim,".tif")
    
    ##download tile raster
    a<-lapply(
      flist,
      function(x){
        print(x)
        if (file.exists(file.path("data","tmp","treecover2000",paste0("h.",x)))==F){
          download.file(paste0(url,x),file.path("data","tmp","treecover2000",paste0("h.",x)), mode="wb")
        }
      }
    )
    
    ##create file index for virtual raster
    input_files=paste0(
      list.files(file.path(c(here()), "data","tmp","treecover2000"),paste0("h.Hansen"),full.names=T))
    #modify paths for use with windows
    if(Sys.info()["sysname"] == "Windows") {
      input_files<-gsub('/', '\\\\', input_files)
    }
    #save file paths to .txt
    index<-file(paste0(file.path("data","tmp","treecover2000",paste0("h.2018.",dset,".index.txt"))))
    writeLines(input_files, con=index)
    
    ##create virtual raster
    infile<-file.path(c(here()), "data","tmp","treecover2000",paste0("h.2018.",dset,".index.txt"))
    outfile<-paste0(file.path(c(here()), "data","tmp","treecover2000",paste0("h.2018.",dset,".raster.vrt")))
    #modify paths for use with windows
    if(Sys.info()["sysname"] == "Windows") {
      infile<-gsub('/', '\\\\', infile)
      outfile<-gsub('/', '\\\\', outfile)
    }
    gdalbuildvrt(output.vrt = c(outfile), 
                 input_file_list = c(infile), 
                 resolution = "average", 
                 r = "nearest")
    
    ##translate virtual raster to compressed TIFF and store in output folder
    destfile<-paste0(file.path(c(here()),"data","store",paste0("h.2018.",aoi,".",dset,".tif")))
    #modify paths for use with windows
    if(Sys.info()["sysname"] == "Windows") {
      infile<-gsub('/', '\\\\', infile)
      outfile<-gsub('/', '\\\\', outfile)
    }
    if (file.exists(destfile)==F){
      gdal_translate(outfile, 
                     destfile,
                     co = c("COMPRESS=LZW", "NUM_THREADS=15", "BIGTIFF=YES"), #num_threads for parallel compression, bigtiff for >4GB tiff
                     ot = "Byte")
    }
    
    ##delete temp files
    file.remove(
      c(
        paste0(file.path("data","tmp","treecover2000",paste0("h.",flist))),
        paste0(file.path("data","tmp","treecover2000",paste0("h.2018.",dset,".index.txt"))),
        paste0(file.path("data","tmp","treecover2000",paste0("h.2018.",dset,".raster.vrt")))
      )
    )
}
