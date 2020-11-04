library(here)
library(ecRutils)
library(rgdal)
library(raster)
library(dplyr)

nowrun=0
nowrun.download=0

###################
### forest loss ###
###################

if (nowrun==1){
  
  ###hansen version
  url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2016-v1.4/"
  url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/"
  url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2018-v1.6/"

  ###dimensions
  #brazil: 
  north<-c("10N","00N","10S","20S","30S")
  west<-c("080W","070W","060W","050W","040W")
  x<-expand.grid(north=north,west=west)
  x<-paste0(x[,1],"_",x[,2])
  extra<-"00N_080W"
  dim<-c(x,extra)
  #indonesia: 
  north<-c("10N","00N","10S")
  west<-c(
    paste0("0",c(90:90),"E"),
    paste0("",seq(100,140,10),"E")
  )
  x<-expand.grid(north=north,west=west)
  x<-paste0(x[,1],"_",x[,2])
  dim<-c(x)
  
  ###data sets
  dlist<-c(
    "treecover2000",
    # "loss",
    # "gain",
    "lossyear"#,
    # "datamask"#,
    # "first"#,
    # "last"
  )

  ###set round
  dset<-"lossyear"

  ## download complete filelist
  # url<-paste0(url2013,paste0(dset,".txt"))
  # download.file(url,file.path(datafolder,"tmp",paste0("h.",dset,".txt")))
  # l<-readLines(file.path(datafolder,"tmp",paste0("h.",dset,".txt")))
  # l[1]

  ##file list
  pre<-paste0("Hansen_GFC-2018-v1.6_",dset)
  flist<-paste0(pre,"_",dim,".tif")

  ##download tile raster
  a<-lapply(
    flist,
    function(x){
      # x<- flist[[1]]
      print(x)
      if (file.exists(file.path(databasefolder,"deforestation","hansen",paste0("h.",x)))==F){
        download.file(paste0(url,x),file.path(databasefolder,"deforestation","hansen",paste0("h.",x)))
      }
    }
  )

  #merge raster tiles with gdal
  cmd<-f.gdal.merge(
    o=paste0(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".merge.tif"))),
    # ot="Int16",
    # input_files=
    #   paste(
    #     paste0(file.path(datafolder,"tmp"),"/","h.",flist),
    #     collapse=" "
    #   )
    input_files=paste(
        list.files(file.path(databasefolder,"deforestation","hansen"),paste0("h.Hansen"),full.names=T),
      collapse=" "
    ),
    add.something.at.the.end="--config CHECK_DISK_FREE_SPACE NO"
  )
  writeClipboard(cmd)

  ##crop raster to spatial extent of country
  #read
  # m<-readOGR(file.path(databasefolder,"admin_borders/brazil/municip2017"),"RG2017_regioesgeograficas2017")
  # m<-readOGR(file.path(databasefolder,"admin_borders/brazil/brazil"),"brazil")
  m<-readOGR(file.path(databasefolder,"admin_borders/indonesia/kap2015idm"),"kap2015idm.corrected")
  crs(m)
  r<-raster(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".merge.tif")))
  # r<-raster(file.path("/Volumes/pululahua",paste0("h.2018.",dset,".merge.tif")))
  crs(r)
  # equal projections
  #crop
  cmd<-f.gdal.warp(
    t_srs=paste0('"',crs(m),'"'),
    te= paste0(extent(m)[c(1,3,2,4)],collapse=" "),
    # tr= paste0(res(m),collapse=" "),
    # r = "average",
    # overwrite="",
    srcfile=r@file@name,
    dstfile=file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".crop.tif"))

  )
  writeClipboard(cmd)

  ##compress tif
  r<-raster(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".crop.tif")))
  crs(r)
  cmd<-f.gdal.translate(
    # ot="UInt16", # Byte, UInt16, Int16, UInt32, Int32, Float32, Float64, CInt16, CInt32, CFloat32, CFloat64
    co="Compress=LZW", # COMPRESS=[JPEG/LZW/PACKBITS/DEFLATE/CCITTRLE/CCITTFAX3/CCITTFAX4/LZMA/NONE]:
    src_dataset=r@file@name,
    dst_dataset=file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".compr.tif"))
  )
  writeClipboard(cmd)

  ##save file
  file.copy(
    file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".compr.tif")),
    file.path(databasefolder,"deforestation","hansen",paste0("h.2018.indonesia.",dset,".tif")),
    overwrite=T
  )


  ##resample data
  #data load
  h16<-raster(file.path("h.2016.lossyear.tif"))
  h17<-raster(file.path("h.2018.lossyear.tif")))
  h18<-raster(file.path("h.2018.indonesia.lossyear.tif"))
  #projection
  crs(h16) #equal
  crs(h17) #equal
  #extent
  extent(h17)
  extent(h16)  #equal
  res(h17)
  res(h16) #not equal
  dim(h17)
  dim(h16) #not equal
  ncell(h17)
  ncell(h16) #not equal

  ##compare one tile
  # if (nowrun==1){
  #   url2013<-"http://commondatastorage.googleapis.com/earthenginepartners-hansen/GFC2013/Hansen_GFC2013_lossyear_00N_100E.tif"
  #   url2016<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2016-v1.4/Hansen_GFC-2016-v1.4_lossyear_00N_100E.tif"
  #   download.file(url2013,file.path(datafolder,"tmp",paste0("h.2013.00N100E.tif")))
  #   download.file(url2016,file.path(datafolder,"tmp",paste0("h.2016.00N100E.tif")))
  #   h13<-raster(file.path(datafolder,"tmp",paste0("h.2013.00N100E.tif")))
  #   h16<-raster(file.path(datafolder,"tmp",paste0("h.2016.00N100E.tif")))
  #   ncell(h13)/1000
  #   ncell(h16)/1000
  #   dim(h13)
  #   dim(h16)
  #   file.remove(file.path(datafolder,"tmp",paste0("h.2013.00N100E.tif")))
  #   file.remove(file.path(datafolder,"tmp",paste0("h.2016.00N100E.tif")))
  # }

  ##delete files
  file.remove(
    c(
      paste0(file.path(databasefolder,"deforestation","hansen",paste0("h.",flist))),
      paste0(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".merge.tif"))),
      paste0(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".crop.tif"))),
      paste0(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".compr.tif")))
    )
  )
}

#######################
### tree cover 2000 ###
#######################
if (nowrun==1){
  
  ###hansen version
  url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2016-v1.4/"
  url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/"
  url<-"https://storage.googleapis.com/earthenginepartners-hansen/GFC-2018-v1.6/"

  ###dimensions
  #brazil: 
  north<-c("10N","00N","10S","20S","30S")
  west<-c("080W","070W","060W","050W","040W")
  x<-expand.grid(north=north,west=west)
  x<-paste0(x[,1],"_",x[,2])
  extra<-"00N_080W"
  dim<-c(x,extra)
  #indonesia: 
  north<-c("10N","00N","10S")
  west<-c(
    paste0("0",c(90:90),"E"),
    paste0("",seq(100,140,10),"E")
  )
  x<-expand.grid(north=north,west=west)
  x<-paste0(x[,1],"_",x[,2])
  dim<-c(x)
  
  ###data sets
  dlist<-c(
    "treecover2000",
    # "loss",
    # "gain",
    "lossyear"#,
    # "datamask"#,
    # "first"#,
    # "last"
  )

  ###set round
  dset<-"treecover2000"

  ## download complete filelist
  # url<-paste0(url2013,paste0(dset,".txt"))
  # download.file(url,file.path(datafolder,"tmp",paste0("h.",dset,".txt")))
  # l<-readLines(file.path(datafolder,"tmp",paste0("h.",dset,".txt")))
  # l[1]

  ##file list
  pre<-paste0("Hansen_GFC-2018-v1.6_",dset)
  flist<-paste0(pre,"_",dim,".tif")

  ##download tile raster
  a<-lapply(
    flist,
    function(x){
      # x<- flist[[1]]
      print(x)
      if (file.exists(file.path(databasefolder,"deforestation","hansen",paste0("h.",x)))==F){
        download.file(paste0(url,x),file.path(databasefolder,"deforestation","hansen",paste0("h.",x)))
      }
    }
  )

  #merge raster tiles with gdal
  cmd<-f.gdal.merge(
    o=paste0(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".merge.tif"))),
    # ot="Int16",
    # input_files=
    #   paste(
    #     paste0(file.path(datafolder,"tmp"),"/","h.",flist),
    #     collapse=" "
    #   )
    input_files=paste(
        list.files(file.path(databasefolder,"deforestation","hansen"),paste0("h.Hansen"),full.names=T),
      collapse=" "
    ),
    add.something.at.the.end="--config CHECK_DISK_FREE_SPACE NO"
  )
  writeClipboard(cmd)

  ##crop raster to spatial extent of country
  #read
  # m<-readOGR(file.path(databasefolder,"admin_borders/brazil/municip2017"),"RG2017_regioesgeograficas2017")
  # m<-readOGR(file.path(databasefolder,"admin_borders/brazil/brazil"),"brazil")
  m<-readOGR(file.path(databasefolder,"admin_borders/indonesia/kap2015idm"),"kap2015idm.corrected")
  crs(m)
  r<-raster(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".merge.tif")))
  # r<-raster(file.path("/Volumes/pululahua",paste0("h.2018.",dset,".merge.tif")))
  crs(r)
  # equal projections
  #crop
  cmd<-f.gdal.warp(
    t_srs=paste0('"',crs(m),'"'),
    te= paste0(extent(m)[c(1,3,2,4)],collapse=" "),
    # tr= paste0(res(m),collapse=" "),
    # r = "average",
    # overwrite="",
    srcfile=r@file@name,
    dstfile=file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".crop.tif"))

  )
  writeClipboard(cmd)

  ##compress tif
  r<-raster(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".crop.tif")))
  crs(r)
  cmd<-f.gdal.translate(
    # ot="UInt16", # Byte, UInt16, Int16, UInt32, Int32, Float32, Float64, CInt16, CInt32, CFloat32, CFloat64
    co="Compress=LZW", # COMPRESS=[JPEG/LZW/PACKBITS/DEFLATE/CCITTRLE/CCITTFAX3/CCITTFAX4/LZMA/NONE]:
    src_dataset=r@file@name,
    dst_dataset=file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".compr.tif"))
  )
  writeClipboard(cmd)

  ##save file
  file.copy(
    file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".compr.tif")),
    file.path(databasefolder,"deforestation","hansen",paste0("h.2018.indonesia.",dset,".tif")),
    overwrite=T
  )

  ##delete files
  file.remove(
    c(
      paste0(file.path(databasefolder,"deforestation","hansen",paste0("h.",flist))),
      paste0(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".merge.tif"))),
      paste0(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".crop.tif"))),
      paste0(file.path(databasefolder,"deforestation","hansen",paste0("h.2018.",dset,".compr.tif")))
    )
  )
}


########################
### forest maps 2000 ###
########################
### 2000 forest maps
if (nowrun==T){
  ##data
  r<-raster(file.path(databasefolder,"deforestation","hansen","h.2018.indonesia.treecover2000.tif"))
  r@file@name
  
  #treshholds forest densities
  t<-c(30,50,75,90)
  a<-lapply(
    t,
    function(t){
      #filter values
      # t<-30
      new.file.name1<-sub("treecover2000.tif",paste0("forest.",t,"p.2000.heavy.tif"),r@file@name)
      cmd.f<-f.gdal.calc(
        calc=paste0('"(A>=',t,')*1"'),
        A = r@file@name,
        outfile=new.file.name1,
        overwrite=""
      )
      #compress
      new.file.name2<-sub("treecover2000.tif",paste0("forest.",t,"p.2000.tif"),r@file@name)
      cmd.c<-f.gdal.translate(
        ot="Byte",
        co="Compress=LZW",
        src_dataset=new.file.name1,
        dst_dataset=new.file.name2
      )
      #clean
      cmd.d<-paste0("rm ",new.file.name1)
      #out
      cmd<-c(cmd.f,cmd.c,cmd.d,"")
      return(cmd)
    }
  )
  cmd<-unlist(a)
  sfile<-tempfile()
  cat(cmd,file=sfile,sep="\n")
  writeClipboard(paste("bash",sfile))
  # system(cmd)
}


###############################
### forest maps 2001 - 20XX ###
###############################
if (nowrun==T){
  
  r<-raster(file.path(databasefolder,"deforestation","hansen",paste0("h.",hyear,".indonesia.lossyear.tif")))
  table(r[])
  
  ##specs
  hyear<-2018
  timeframe<-c(2001:2018)
  timeframe<-c(2001,2018)
  t<-30 #treashold
  
  a<-lapply(
    timeframe,
    function(y){
      # y<-2016
      cmd.f<-f.gdal.calc(
        # calc=paste0('"(A-(logical_and(B>0,B<=',y-2000,')))>0','"'),
        calc=paste0('"(A-( (B>0) * (B<=',y-2000,')))>0','"'),
        A = file.path(databasefolder,"deforestation","hansen",paste0("h.",hyear,".indonesia.forest.",t,"p.2000.tif")),
        B = file.path(databasefolder,"deforestation","hansen",paste0("h.",hyear,".indonesia.lossyear.tif")),
        outfile=file.path(databasefolder,"deforestation","hansen",paste0("h.",hyear,".indonesia.forest.",t,"p.",y,".heavy.tif")),
        overwrite=""
      )
      #compress
      cmd.c<-f.gdal.translate(
        ot="Byte",
        co="Compress=LZW",
        src_dataset=file.path(databasefolder,"deforestation","hansen",paste0("h.",hyear,".indonesia.forest.",t,"p.",y,".heavy.tif")),
        dst_dataset=file.path(databasefolder,"deforestation","hansen",paste0("h.",hyear,".indonesia.forest.",t,"p.",y,".tif"))
      )
      #clean
      cmd.d<-paste0("rm ",file.path(databasefolder,"deforestation","hansen",paste0("h.",hyear,".indonesia.forest.",t,"p.",y,".heavy.tif")))
      #out
      cmd<-c(cmd.f,cmd.c,cmd.d,"")
      # writeClipboard(cmd)
      return(cmd)
    }
  )
  cmd<-unlist(a)
  sfile<-file.path(databasefolder,"deforestation","hansen","bash3.sh")
  cat(cmd,file=sfile,sep="\n")
  writeClipboard(paste("bash",sfile))
  
  file.remove(
    file.path(databasefolder,"deforestation","hansen","bash3.sh")
  )
  
}
