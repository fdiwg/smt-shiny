runCmsy <- function (region,subregion,stock,group,name,englishName,scientificName,source,minOfYear,maxOfYear,startYear,endYear,flim,fpa,blim,bpa,bmsy,fmsy,msy,msyBTrigger,b40,m,fofl,last_f,resiliance,r.low,r.hi,stb.low,stb.hi,int.yr,intb.low,intb.hi,endb.low,endb.hi,q.start,q.end,btype,force.cmsy,comments, inputCsvFile, WPS)  {

  data<-read.csv(inputCsvFile, header =T, sep=",")
  
  keeps <- c("Stock",	"yr",	"ct",	"bt")
  data <- data[keeps]
  
  dimnames(data)[2][[1]]<-tolower(gsub(" ", "_", dimnames(data)[2][[1]]))
  dffile<-paste(tempdir(),"/","cmsy_data_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".csv",sep="")
  dffile<-gsub(" ", "_", dffile)
  
  write.csv(data,file=dffile, quote = FALSE, eol = "\n", row.names = FALSE,  fileEncoding = "UTF-8")

  #TAKE THE INPUT TABLE CONTENT FROM THE CSV FILE AS A STRING#  
  body<-readChar(dffile, file.info(dffile)$size)
  body<-gsub("\r\n", "\n", body)
  body<-gsub("\n$", "", body)
  print(body)

  #SEND THE REQUEST#  

  exec = WPS$execute(
    identifier ="org.gcube.dataanalysis.wps.statisticalmanager.synchserver.mappedclasses.transducerers.CMSY_2",status=T,
    dataInputs = list(
      catch_file = WPSComplexData$new(value = body ,mimeType = "application/d4science"),
      Region = WPSLiteralData$new(value = ifelse(is.null(region),"",region)),
      Subregion = WPSLiteralData$new(value = ifelse(is.null(subregion),"",subregion)),
      Stock = WPSLiteralData$new(value = ifelse(is.null(stock),"",stock)),
      Group = WPSLiteralData$new(value = ifelse(is.null(group),"",group)), 
      Name = WPSLiteralData$new(value = ifelse(is.null(name),"",name)),
      EnglishName = WPSLiteralData$new(value = ifelse(is.null(englishName),"",englishName)),
      ScientificName = WPSLiteralData$new(value = ifelse(is.null(scientificName),"",scientificName)),
      Source = WPSLiteralData$new(value = as.character(source)),
      MinOfYear = WPSLiteralData$new(value = as.integer(minOfYear)),
      MaxOfYear = WPSLiteralData$new(value = as.integer(maxOfYear)),
      StartYear = WPSLiteralData$new(value = as.integer(startYear)),
      EndYear = WPSLiteralData$new(value = as.integer(endYear)),
      Flim = WPSLiteralData$new(value = as.character(flim)),
      Fpa = WPSLiteralData$new(value = as.character(fpa)),
      Blim = WPSLiteralData$new(value = as.character(blim)),
      Bpa = WPSLiteralData$new(value = as.character(bpa)),
      Bmsy = WPSLiteralData$new(value = as.character(bmsy)),
      FMSY = WPSLiteralData$new(value = as.character(fmsy)),
      MSY = WPSLiteralData$new(value = as.character(msy)),
      MSYBtrigger = WPSLiteralData$new(value = as.character(msyBTrigger)),
      B40 = WPSLiteralData$new(value = as.character(b40)),
      M = WPSLiteralData$new(value = as.character(m)),
      Fofl = WPSLiteralData$new(value = as.character(fofl)),
      last_F = WPSLiteralData$new(value = as.character(last_f)), 
      Resilience = WPSLiteralData$new(value = resiliance),
      r.low = WPSLiteralData$new(value = as.character(r.low)),
      r.hi = WPSLiteralData$new(value = as.character(r.hi)),
      stb.low = WPSLiteralData$new(value = as.double(stb.low)),
      stb.hi = WPSLiteralData$new(value = as.double(stb.hi)),
      int.yr = WPSLiteralData$new(value = as.character(int.yr)),
      intb.low = WPSLiteralData$new(value = as.character(intb.low)),
      intb.hi = WPSLiteralData$new(value = as.character(intb.hi)),
      endb.low = WPSLiteralData$new(value = as.double(endb.low)),
      endb.hi = WPSLiteralData$new(value = as.double(endb.hi)),
      q.start = WPSLiteralData$new(value = as.character(q.start)),
      q.end = WPSLiteralData$new(value = as.character(q.end)),
      btype = WPSLiteralData$new(value = "None"),
      force.cmsy = WPSLiteralData$new(value = FALSE),
      Comment = WPSLiteralData$new(value ="comments")
    )
  )
  
  Status<-exec$getStatus()$getValue()
  out<- exec$getProcessOutputs()[[1]]$getData()$getFeatures()
  
  flog.info("Got from CMSY: %s", out)
  print(out)
  
  file.remove(dffile)
  options(warn=0)
  
  #if(all(out$fid%in%sprintf("F%s",0:3))){
  if(Status=="ProcessSucceeded"){
    flog.warn("CMSY SUCCESS")
    print("CMSY SUCCESS")
    out <- data.frame(lapply(out, as.character), stringsAsFactors=FALSE)
    return (out)}
  else{
    flog.warn("CMSY FAIL")
    print("CMSY FAIL")
    return (NULL)
    stop("WPS call failed.")
  }
}
