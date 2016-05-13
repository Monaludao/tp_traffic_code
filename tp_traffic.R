tp_traffic_go<-function(){
    repeat{
        print(paste(format(Sys.time(),"%H:%M"),"start"))
        tp_traffic_dl()
        print(paste(format(Sys.time(),"%H:%M"),"end"))
        Sys.sleep(290)
    }
}

tp_traffic_dl <- function () {
    library(bitops)
    library(RCurl)
    library(XML)
    library(httr)
    library(RJSONIO)
    
    options(digits=22)
    options(stringsAsFactors=FALSE)
    setInternet2(TRUE)
    
    json.meta.url<-getURL("http://data.taipei/opendata/datalist/apiAccess?scope=datasetMetadataSearch&q=id:38f2d8e0-07ce-4201-87cb-3462be39d8aa",encoding = "utf-8")
    json.data.url<-getURL("http://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=5aacba65-afda-4ad5-88f5-6026934140e6",encoding = "utf-8")
    json.meta<-fromJSON(json.meta.url)
    json.data<-fromJSON(json.data.url)
    
    ##process json.meta
    record.time<-as.POSIXct(strptime(json.meta[[1]][[5]][[1]][[5]],"%Y-%m-%d %H:%M:%S","Asia/Taipei"))
    
    ##process json.data
    json.df<-data.frame(id = integer(),SectionID = character(),AvgSpd = double(),AvgOcc = double(),
                          TotalVol = double(),MOELevel = integer(),StartWgsX = double(),StartWgsY = double(),
                          EndWgsX = double(),EndWgsY = double())
    for (i in 1:length(json.data[[1]][[5]])) {
        json.df<-rbind(json.df, json.data[[1]][[5]][[i]])
    }
    colnames(json.df)<-names(json.data[[1]][[5]][[1]])
    
    json.df$'_id'<-as.integer(json.df$`_id`)
    json.df$AvgSpd<-as.double(json.df$AvgSpd)
    json.df$AvgOcc<-as.double(json.df$AvgOcc)
    json.df$TotalVol<-as.double(json.df$TotalVol)
    json.df$MOELevel<-as.integer(json.df$MOELevel)
    json.df$StartWgsX<-as.double(json.df$StartWgsX)
    json.df$StartWgsY<-as.double(json.df$StartWgsY)
    json.df$EndWgsX<-as.double(json.df$EndWgsX)
    json.df$EndWgsY<-as.double(json.df$EndWgsY)
    
    json.df$date<-as.Date(record.time,tz="Asia/Taipei")
    json.df$time<-format(record.time,"%H_%M")
    
    data.path<-paste0("../tp_traffic_data/",json.df$date[1],"_",json.df$time[1],".csv")
    write.csv(json.df,data.path)
    print(data.path)
}