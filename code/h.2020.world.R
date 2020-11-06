library(here)
library(ecRutils)
library(rgdal)
library(raster)
library(dplyr)
library(gdalUtils)


########################################################
### download forest loss, forest cover and data mask ###
########################################################

nowrun=1

###data sets
dlist<-c(
  "treecover2000",
  "lossyear",
  "datamask"
)

if (nowrun==1){

  t<-lapply(dlist, function(dset){
      
      # dset = "treecover2000"  
    
      ###hansen version
      # url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2016-v1.4/"
      # url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/"
      url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2018-v1.6/"
    
      ###dimensions
      #global oil palm suitable area (latitudes up to +/- 30 degree from equator, all longitudes except for large parts of the pacific)
      north<-c("30N", "20N", "10N", "00N", "10S","20S")
      west<-c(sprintf("%02.0f0W", 12:1), sprintf("%02.0f0E", 0:17))
      x<-expand.grid(north=north,west=west)
      x<-paste0(x[,1],"_",x[,2])
      dim<-c(x)
    
      ##file list
      pre<-paste0("Hansen_GFC-2018-v1.6_",dset)
      flist<-paste0(pre,"_",dim,".tif")
    
      ##download tile raster
      a<-lapply(
        flist,
        function(x){
          print(x)
          if (file.exists(file.path("data","tmp",paste0("h.",x)))==F){
            download.file(paste0(url,x),file.path("data","tmp",paste0("h.",x)), mode="wb")
          }
        }
      )
      
      ##create file index for virtual raster
      input_files=paste0(
        list.files(file.path(c(here()), "data","tmp"),paste0("h.Hansen"),full.names=T))
      #modify paths for use with windows
      if(Sys.info()["sysname"] == "Windows") {
        input_files<-gsub('/', '\\\\', input_files)
      }
      #save file paths to .txt
      index<-file(paste0(file.path("data","tmp",paste0("h.2018.",dset,".index.txt"))))
      writeLines(input_files, con=index)
      
      ##create virtual raster
      infile<-file.path(c(here()), "data","tmp",paste0("h.2018.",dset,".index.txt"))
      outfile<-paste0(file.path(c(here()), "data","tmp",paste0("h.2018.",dset,".raster.vrt")))
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
      destfile<-paste0(file.path(c(here()),"data","store",paste0("h.2018.",dset,".world.tif")))
      #modify paths for use with windows
      if(Sys.info()["sysname"] == "Windows") {
        infile<-gsub('/', '\\\\', infile)
        outfile<-gsub('/', '\\\\', outfile)
      }
      if (file.exists(destfile)==F){
        gdal_translate(outfile, 
                       destfile,
                       co = c("COMPRESS=LZW", "NUM_THREADS=15"), #num_threads for parallelization of LZW compression
                       ot = "Byte")
      }
    
      ##delete temp files
      file.remove(
        c(
          paste0(file.path("data","tmp",paste0("h.",flist))),
          paste0(file.path("data","tmp",paste0("h.2018.",dset,".index.txt"))),
          paste0(file.path("data","tmp",paste0("h.2018.",dset,".raster.vrt")))
        )
      )
    }
  )
}

