#####  LANDTRAIN (LANDslide Triggering RAINfall): a code to identify rainfall-induced landslide triggering condition #####


### --------------------- Checking & installing packages --------------------- ###
if ("gWidgets" %in% row.names(installed.packages())==FALSE) install.packages("gWidgets") 
if ("gWidgetsRGtk2" %in% row.names(installed.packages())==FALSE) install.packages("gWidgetsRGtk2") 
if ("gWidgetstcltk" %in% row.names(installed.packages())==FALSE) install.packages("gWidgetstcltk") 
if ("tcltk" %in% row.names(installed.packages())==FALSE) install.packages("tcltk") 


pars <-commandArgs(trailingOnly=TRUE)


### --------------------- Variable definition --------------------- ###

if (length(table(pars == "-wd"))==2)
  {
  wd_selected<-pars[which(pars=="-wd")+1]
  } else
  {
  require(tcltk)
  wd_selected<-tk_choose.dir(getwd(), caption="Choose a suitable folder")
  }
setwd(wd_selected)

getwd()

if (length(table(pars == "-f"))==2)
  {
  file_selected<-pars[which(pars=="-f")+1]
  } else
  {
  require(tcltk)
  file_selected<-tk_choose.files(caption="Choose the .csv file containing rainfall data")
  }

nomefile_est<-rev(unlist(strsplit(file_selected,"/")))[1]
nomefile<-unlist(strsplit(nomefile_est,"\\."))[1]
estenzionefile<-rev(unlist(strsplit(nomefile_est,"\\.")))[1]

if (estenzionefile=="csv") {txtfile<-FALSE}
if (estenzionefile=="txt") {txtfile<-TRUE}


resultdir<-paste("Results",sep="")


dir.create(resultdir, showWarnings = TRUE, recursive = FALSE)
resultdirplot<-paste("Results/Graphic_output",sep="")
dir.create(resultdirplot, showWarnings = TRUE, recursive = FALSE)
resultdirtext<-paste("Results/Text_output",sep="")
dir.create(resultdirtext, showWarnings = TRUE, recursive = FALSE)


### --------------------- Retrive input variables from dropdown menu in the GUI) --------------------- ###

if (length(table(pars == "-ti"))==2 || length(table(pars == "-sp"))==2)
  {
  gui_user_inputs<-list(time_interval=as.numeric(pars[which(pars=="-ti")+1]),sel_period=as.numeric(pars[which(pars=="-sp")+1]))
  } else
  {
  require(gWidgets)
  options(guiToolkit="tcltk") ## or RGtk2 or Qt
  w <- gwindow("Parameters")
  tbl <- glayout(cont=w, horizontal=FALSE)
  dryperiodseries <- c("48", "72")
  timeperiodseries <- c("3", "6","8","12")
  tbl[1,1] <- "Select the dry period length"
  tbl[1,2] <- (cb1 <- gcombobox(dryperiodseries, cont=tbl))
  tbl[2,1] <- "Select the time window length"
  tbl[2,2] <- (cb2 <- gcombobox(timeperiodseries, cont=tbl))
  tbl[3,2] <- (b <- gbutton("Ok", cont=tbl))
  addHandlerClicked(b, handler = function(h,...)
  {
    time_interval_val <- as.numeric(svalue(cb1)) # Whitout preassigning value assign doesn't work
    sel_period_val <- as.numeric(svalue(cb2)) # Whitout preassigning value assign doesn't work
    assign(x="gui_user_inputs",value=list(time_interval=time_interval_val,sel_period=sel_period_val),envir=.GlobalEnv)
    dispose(.GlobalEnv$w)
    rm(w,envir=.GlobalEnv)
  })
  while(exists("w",.GlobalEnv)) {Sys.sleep(5)}
 
  }

sprintf(as.character(gui_user_inputs$time_interval))
sprintf(as.character(gui_user_inputs$sel_period))


### --------------------- Data reading --------------------- ###
separator="."
if (txtfile==TRUE)
  {
  data<-read.table(paste(file_selected,sep=""),dec=separator,sep="\t",skip=1,header=FALSE,stringsAsFactors=FALSE)
  data<-data[,-6]
  colnames(data)<-c("pluviometro","data","intervallo","quantita","intensita")
  } else
  {
  data<-read.table(paste(file_selected,sep=""),dec=separator,sep=";",skip=1,header=FALSE,stringsAsFactors=FALSE)
  data<-data[,-6]
  colnames(data)<-c("pluviometro","data","intervallo","quantita","intensita")
  }

data_split<-unlist(strsplit(data$data," "))
dates<-data_split[seq(1,length(data$data)*2,2)]
dates<-paste(substr(dates,7,10),"-",substr(dates,4,5),"-",substr(dates,1,2),sep="")
hours<-data_split[seq(2,length(data$data)*2,2)]
hours<-gsub("\\.",":",hours)
hours<-substr(hours,1,nchar(hours)-6)
data_testo<-paste(dates,hours,sep=" ")

Sys.setenv(TZ = "UTC")
Sys.timezone()

data$data<-sort(as.POSIXct(strftime(data_testo, format="%Y/%m/%d %H:%M:%S",tz="UTC",usetz=FALSE)))


### --------------------- First check on the selected portion of the Rainfall series  --------------------- ###

intervalli<-diff(data$data)

gaps<-table(intervalli)
ind_gap<-which(as.numeric(names(gaps))>1)
names(gaps)<-paste(names(gaps),"ore")

if (max(intervalli)>1) {print("Beware: in the series, the following data miss: ");print(gaps[ind_gap])}

time_sintetico<-seq(min(data$data),max(data$data),as.difftime(1,units="hours"))

quantita<-rep(NA,length(time_sintetico))
data_nuovi<-as.data.frame(matrix(NA,nrow=length(time_sintetico),ncol=5))
colnames(data_nuovi)<-c("pluviometro","data","intervallo","quantita","intensita")
data_nuovi[,1]<-rep(names(table(data[,1])),length(time_sintetico))
data_nuovi[,2]<-time_sintetico

index_nobuchi<-which(time_sintetico %in% data$data)
if(length(index_nobuchi)<length(time_sintetico))
  {
  lunghezza_serie_continua<-as.difftime(max(time_sintetico)-max(time_sintetico[-index_nobuchi]),units="days")
  data_nuovi[index_nobuchi,c(3:5)]<-data[c(3:5)]
  } else
  {
  lunghezza_serie_continua<-as.difftime(max(time_sintetico)-min(time_sintetico),units="days")
  data_nuovi[,c(3:5)]<-data[c(3:5)]
  }

if (Sys.info()[1] == "Linux") {x11()}
if (Sys.info()[1] == "Windows") {windows()}
if (Sys.info()[1] == "Darwin") {quartz()}

plot(data_nuovi$data,data_nuovi$quantita,type="h",col="blue",main=paste("Raingauge number ",names(table(data[,1])),sep=""),xlab="Date (day)",ylab="Rainfall height (mm)")
if(length(index_nobuchi)<length(time_sintetico))
  {
  lines(data_nuovi$data[-index_nobuchi],rep(max(na.omit(data_nuovi$quantita)),length(data_nuovi$data[-index_nobuchi])),type="h",lty="dashed",col="darkred")

  }
mtext(paste("Length of the continuous time period: ",round(lunghezza_serie_continua,0)," days",sep=""),padj=-0.5,col="darkred",cex=0.8)
savePlot(filename=paste(resultdirplot,"/Discontinuous_Rainfall_Series_",nomefile,".pdf",sep=""),type="pdf",device=dev.list()[1])

if(length(index_nobuchi)<length(time_sintetico))
  {
  if (!length(table(pars == "-silent"))==2)
    {
    require(tcltk)
    ReturnVal_continue<-tkmessageBox(message = "The rainfall data series is not continuous over the selected time period. Do you want to continue? Yes will continue the analysis, No will quit the program.",icon = "question", type = "yesno", default = "yes")
    if(tclvalue(ReturnVal_continue)=="no") {q(save="no")}
    }
  }

### --------------------- Aggregation of rainfall measures--------------------- ###
# Selection of the continuous series

if(length(index_nobuchi)<length(time_sintetico))
  {
  print(paste("Selection of the continuous series from ",max(time_sintetico[-index_nobuchi])," to ",max(time_sintetico),sep=""))
  inzio_serie_continua<-max(time_sintetico[-index_nobuchi])
  fine_serie_continua<-max(time_sintetico)
  data_selezione<-data_nuovi[which(data_nuovi$data>max(time_sintetico[-index_nobuchi])),]
  if (!length(table(pars == "-silent"))==2)
    {
    require(tcltk)
    ReturnVal <- tkmessageBox(title="Data selection",message=paste("Selection of the continuous series from ",max(time_sintetico[-index_nobuchi])," to ",max(time_sintetico),sep=""),icon="info",type="ok")
    }
  } else
  {
  data_selezione<-data_nuovi
  inzio_serie_continua<-min(time_sintetico)
  fine_serie_continua<-max(time_sintetico)
  if (!length(table(pars == "-silent"))==2)
    {
    require(tcltk)
    ReturnVal<-tkmessageBox(title="Data selection",message=paste("Selection of the continuous series from ",min(time_sintetico)," to ",max(time_sintetico),sep=""),icon="info",type="ok")
    }
  }

finestre_vect<-c(3,6,8,12)

### Beware! time_rain/finestre_vect must be integer. To do this, the variable time_rain shall be the lowest common multiple! ###

time_rain<-gui_user_inputs$time_interval

final_dataframe<-NULL
final_comparison_dataframe<-NULL
for (count in finestre_vect)
  {

  print(paste("Aggregation period : ",count,sep=""))
  period.aggregation<-count*60*60 #in seconds
  
  range_sel<-range(seq(max(data_selezione$data),min(data_selezione$data),-as.difftime(period.aggregation,units="secs")))
  index_sel<-which(data_selezione$data>=range_sel[1]+as.numeric(min(diff(data_selezione$data)),units="secs"))
  
  dati_aggr_sel<-data_selezione[index_sel,]
  
  time.series.step<-as.numeric(min(diff(dati_aggr_sel$data)),units="secs")
  step.aggregation<-period.aggregation/time.series.step
  number.step.aggregation<-floor(length(dati_aggr_sel$data)/step.aggregation)
  array.rainfall<-array(dati_aggr_sel$quantita,dim=c(step.aggregation,number.step.aggregation))
  
  aggregated.rainfall<-t(array.rainfall)%*%rep(1,step.aggregation)
  index.selection.start.date<-seq(1,length(dati_aggr_sel$data),step.aggregation)
  index.selection.final.date<-seq(step.aggregation,length(dati_aggr_sel$data),step.aggregation)
  time.series.aggregated.rainfall<-dati_aggr_sel$data[index.selection.final.date]
  
  aggregated_data_frame<-data.frame(list(start_date=dati_aggr_sel$data[index.selection.start.date],end_date=dati_aggr_sel$data[index.selection.final.date],rainfall=aggregated.rainfall,mean_intensity=aggregated.rainfall/count,cum_intensity=cumsum(aggregated.rainfall/count),inv_cum_intensity=rev(cumsum(rev(aggregated.rainfall/count))),interval=as.difftime(rep(count,length(aggregated.rainfall)),units="hours")))
  str(aggregated_data_frame)
  final_dataframe<-rbind(aggregated_data_frame,final_dataframe)
  
  comparison_dataframe<-data.frame(list(inv_cum_intensity_time_rain=rev(final_dataframe[which(final_dataframe$interval==count),c(6)])[((time_rain/count)+1):dim(aggregated_data_frame)[1]],inv_cum_intensity_0=rev(final_dataframe[which(final_dataframe$interval==count),c(6)])[1:(dim(aggregated_data_frame)[1]-(time_rain/count))],inv_cum_intensity_diff=rev(final_dataframe[which(final_dataframe$interval==count),c(6)])[((time_rain/count)+1):dim(aggregated_data_frame)[1]]-rev(final_dataframe[which(final_dataframe$interval==count),c(6)])[1:(dim(aggregated_data_frame)[1]-(time_rain/count))],interval=as.difftime(rep(count,(length(aggregated.rainfall)-(time_rain/count))),units="hours")))

  final_comparison_dataframe<-rbind(comparison_dataframe,final_comparison_dataframe)
  }

###### Selection of time window Wi from GUI. Wi is assigned to "selected_period" ######

selected_period<-gui_user_inputs$sel_period


#### Temporal rainfall series (h) plotted backward on the abscissa ####
if (Sys.info()[1] == "Linux") {x11()}
if (Sys.info()[1] == "Windows") {windows()}
if (Sys.info()[1] == "Darwin") {quartz()}

if(selected_period==3)
{
plot(seq(3,length(which(final_dataframe$interval==3))*3,3),rep(1000,length(which(final_dataframe$interval==3))),type="h",col="grey",ylim=round(range(final_dataframe[,c(6)]),1),xlab="Antecedent period (h)",ylab="Cumulated intensity (mm/h)",main="Cumulated rainfall intensity curve")
lines(seq(3,length(which(final_dataframe$interval==3))*3,3),rev(final_dataframe[which(final_dataframe$interval==3),c(6)]),type="l",lwd=1.5,col="blue")
points(seq(3,length(which(final_dataframe$interval==3))*3,3),rev(final_dataframe[which(final_dataframe$interval==3),c(6)]),pch=19,cex=0.7,col="blue")
legend("topleft",paste("Time window:",selected_period,"h",sep=""),pch=c(19),pt.cex=c(0.7),lty=c(1),lwd=c(1.5),col=c("blue"),box.col="transparent",bg="transparent")
}

if(selected_period==6)
{
plot(seq(6,length(which(final_dataframe$interval==6))*6,6),rep(1000,length(which(final_dataframe$interval==6))),type="h",col="grey",ylim=round(range(final_dataframe[,c(6)]),1),xlab="Antecedent period (h)",ylab="Cumulated intensity (mm/h)",main="Cumulated rainfall intensity curve")
lines(seq(6,length(which(final_dataframe$interval==6))*6,6),rev(final_dataframe[which(final_dataframe$interval==6),c(6)]),type="l",lwd=1.5,col="darkred")
points(seq(6,length(which(final_dataframe$interval==6))*6,6),rev(final_dataframe[which(final_dataframe$interval==6),c(6)]),pch=19,cex=0.7,col="darkred")
legend("topleft",paste("Time window:",selected_period,"h",sep=""),pch=c(19),pt.cex=c(0.7),lty=c(1),lwd=c(1.5),col=c("darkred"),box.col="transparent",bg="transparent")
}

if(selected_period==8)
{
plot(seq(8,length(which(final_dataframe$interval==8))*8,8),rep(1000,length(which(final_dataframe$interval==8))),type="h",col="grey",ylim=round(range(final_dataframe[,c(6)]),1),xlab="Antecedent period (h)",ylab="Cumulated intensity (mm/h)",main="Cumulated rainfall intensity curve")
lines(seq(8,length(which(final_dataframe$interval==8))*8,8),rev(final_dataframe[which(final_dataframe$interval==8),c(6)]),type="l",lwd=1.5,col="darkgreen")
points(seq(8,length(which(final_dataframe$interval==8))*8,8),rev(final_dataframe[which(final_dataframe$interval==8),c(6)]),pch=19,cex=0.7,col="darkgreen")
legend("topleft",paste("Time window:",selected_period,"h",sep=""),pch=c(19),pt.cex=c(0.7),lty=c(1),lwd=c(1.5),col=c("darkgreen"),box.col="transparent",bg="transparent")
}

if(selected_period==12)
{
plot(seq(12,length(which(final_dataframe$interval==12))*12,12),rep(1000,length(which(final_dataframe$interval==12))),type="h",col="grey",ylim=round(range(final_dataframe[,c(6)]),1),xlab="Antecedent period (h)",ylab="Cumulated intensity (mm/h)",main="Cumulated rainfall intensity curve")
lines(seq(12,length(which(final_dataframe$interval==12))*12,12),rev(final_dataframe[which(final_dataframe$interval==12),c(6)]),type="l",lwd=1.5,col="violet")
points(seq(12,length(which(final_dataframe$interval==12))*12,12),rev(final_dataframe[which(final_dataframe$interval==12),c(6)]),pch=19,cex=0.7,col="violet")
legend("topleft",paste("Time window:",selected_period,"h",sep=""),pch=c(19),pt.cex=c(0.7),lty=c(1),lwd=c(1.5),col=c("violet"),box.col="transparent",bg="transparent")
}

savePlot(filename=paste(resultdirplot,"/Intensity_Cumulated rainfall_",nomefile,"_",selected_period,"x", time_rain,".pdf",sep=""),type="pdf",device=dev.list()[2])

###### Calculation of rainfall event starting time Ts ########



#### Automatic identification of the starting time for the rainfall period to be analyzed

analysis_dataframe<-final_comparison_dataframe[which(final_comparison_dataframe$interval==selected_period),]

### Threshold  value e is assigned. It is used in the test performed on the difference of the cumulated intensities to look for the
#tempting Ts* ###  
rain_threshold<-0.4  
intensity_threshold<-rain_threshold/2

min_pos_vect<-which(analysis_dataframe$inv_cum_intensity_diff<=intensity_threshold)
counter_min_pos<-0

  counter_min_pos<-counter_min_pos+1
  min_pos<-min_pos_vect[counter_min_pos]
  time_flat_hour<-as.difftime(min_pos*selected_period,units="hours")

#### the variable time_flat is the time period between the rainfall event starting time Ts and its end Te. This Te corresponds to
#### the shallow landslide triggering time. #####

  time_flat<-max(data_selezione$data)-time_flat_hour 
  data_selezione_analysis<-data_selezione[which(data_selezione$data>=time_flat),]
  
  
#### It must check whether at Ts*, that is tempting starting time of the rainfall event, there is a nought value of rainfall height. If so, the Ts shall moved ahead
#### up to a new Ts corresponding to a not null rainfall height. Doing this the time_flat must be changed.   ######
  
#### Here 0 enables to enter in the section for the window search ###  
  
value_thresholds<-0 
  
  if (min(which(data_selezione_analysis$quantita>value_thresholds))>1)
    {
    time_flat_hour_old<-time_flat_hour
    time_flat_old<-time_flat
    data_selezione_analysis_old<-data_selezione_analysis
   
    time_flat_hour<-time_flat_hour-(as.difftime(min(which(data_selezione_analysis$quantita>value_thresholds)),units="hours")-1)
    time_flat<-max(data_selezione$data)-time_flat_hour
    data_selezione_analysis<-data_selezione[which(data_selezione$data>=time_flat),]
    write.table(t(c("raingauge","date","interval","quantity","intensity","selected")),file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=FALSE)
    
    write.table(data.frame(data_selezione_analysis_old,selezionati=data_selezione_analysis_old$data %in% data_selezione_analysis$data,stringsAsFactors=FALSE),file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=FALSE,append=TRUE)
    write.table(NULL,file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE,append=TRUE)
    write.table(data.frame(cumulated_rainfall=sum(data_selezione_analysis$quantita),duration=sum(data_selezione_analysis$intervallo),intensity=sum(data_selezione_analysis$quantita)/sum(data_selezione_analysis$intervallo),stringsAsFactors=FALSE),file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE,append=TRUE)      
      } else
    {
    time_flat_hour_old<-time_flat_hour
    time_flat_old<-time_flat
    data_selezione_analysis_old<-data_selezione_analysis
    
    time_flat_hour<-time_flat_hour+(as.difftime(selected_period,units="hours"))
    time_flat<-max(data_selezione$data)-time_flat_hour
    data_selezione_analysis_export<-data_selezione[which(data_selezione$data>=time_flat),]
    
    if(length(which(data_selezione[which(data_selezione$data>=time_flat & data_selezione$data<time_flat_old),]$quantita>value_thresholds))==0)
      {
      write.table(t(c("raingauge","date","interval","quantity","intensity","selected")),file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=FALSE)
    
      write.table(data.frame(data_selezione_analysis,selezionati=rep(TRUE,dim(data_selezione_analysis)[1]),stringsAsFactors=FALSE),file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=FALSE,append=TRUE)
      write.table(NULL,file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE,append=TRUE)
      write.table(data.frame(cumulated_rainfall=sum(data_selezione_analysis$quantita),duration=sum(data_selezione_analysis$intervallo),intensity=sum(data_selezione_analysis$quantita)/sum(data_selezione_analysis$intervallo),stringsAsFactors=FALSE),file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE,append=TRUE)         
      } else
      {
      max_pos<-max(which(data_selezione[which(data_selezione$data>=time_flat & data_selezione$data<time_flat_old),]$quantita<=value_thresholds))
      time_flat_hour<-as.difftime(selected_period-max_pos,units="hours")
      time_flat<-time_flat_old-time_flat_hour
      data_selezione_analysis<-data_selezione[which(data_selezione$data>=time_flat),]
      }
    write.table(t(c("raingauge","date","interval","quantity","intensity","selected")),file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=FALSE)

    write.table(data.frame(data_selezione_analysis_export,selezionati=data_selezione_analysis_export$data %in% data_selezione_analysis$data,stringsAsFactors=FALSE),file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=FALSE,append=TRUE)
    write.table(NULL,file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE,append=TRUE)
    write.table(data.frame(cumulated_rainfall=sum(data_selezione_analysis$quantita),duration=sum(data_selezione_analysis$intervallo),intensity=sum(data_selezione_analysis$quantita)/sum(data_selezione_analysis$intervallo),stringsAsFactors=FALSE),file=paste(resultdirtext,"/SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE,append=TRUE)   
    }

name_file_aggregated_output<-"Aggregated_results.txt"

if(file.exists(paste(resultdirtext,"/",name_file_aggregated_output,sep=""))==FALSE)
  {
  write.table(data.frame(filename=paste("SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),time_interval=time_rain,selected_analysis_period=selected_period,cumulated_rainfall=sum(data_selezione_analysis$quantita),duration=sum(data_selezione_analysis$intervallo),intensity=sum(data_selezione_analysis$quantita)/sum(data_selezione_analysis$intervallo),days_continuos_series=round(lunghezza_serie_continua,0),start_series=inzio_serie_continua,end_series=fine_serie_continua,stringsAsFactors=FALSE),file=paste(resultdirtext,"/",name_file_aggregated_output,sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE,append=FALSE)
  } else
  {
  write.table(data.frame(filename=paste("SelectedSeries_",selected_period,"+",time_rain,"hour_",nomefile,".txt",sep=""),time_interval=time_rain,selected_analysis_period=selected_period,cumulated_rainfall=sum(data_selezione_analysis$quantita),duration=sum(data_selezione_analysis$intervallo),intensity=sum(data_selezione_analysis$quantita)/sum(data_selezione_analysis$intervallo),days_continuos_series=round(lunghezza_serie_continua,0),start_series=inzio_serie_continua,end_series=fine_serie_continua,stringsAsFactors=FALSE),file=paste(resultdirtext,"/",name_file_aggregated_output,sep=""),quote=FALSE,sep="\t",row.names=FALSE,col.names=FALSE,append=TRUE)
  
  }


q(save="no")
