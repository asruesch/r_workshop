# Vilas County lakes TP:Chl analysis
# July 2017 R Workshop

library(RODBC)
library(stringr)

# Enter SWIMS credentials
username = readline(prompt="What is your SWIMS username?")
pwd = readline(prompt="What is your SWIMS password?")

# Connect to SWIMS
con = odbcConnect("ora_secprd_64",
                  uid=username,
                  pwd=pwd,
                  rows_at_time=1000,
                  believeNRows=FALSE)

counties = read.csv("~/r_workshop/data/WI_counties.csv", as.is=TRUE)
mod_data = read.csv("~/r_workshop/data/TP_CHL_models.csv")

for (c in 1:72) {
  # Create SWIMS query
  query = paste(readLines("code/tp_chl.sql"), collapse=" ")
  query = str_replace_all(query, "\\t|\\n|\\r", "")
  query = str_replace(query, "%county%", counties$NAME[c])
  
  # query = paste("SELECT W07510_WT_SWIMS_MONIT_STATION.STATION_ID, W07510_WT_SWIMS_MONIT_STATION.PRIMARY_STATION_NAME, W07510_WT_SWIMS_FIELDWORK.START_DATETIME, W07510_WT_SWIMS_SAMPLE_RESULT.DNR_PARAMETER_CODE, W07510_WT_SWIMS_SAMPLE_RESULT.RESULT_AMT, W07510_WT_SWIMS_SAMPLE_RESULT.RESULT_UNITS_TEXT, W07510_WT_SWIMS_VERTICAL_MEASURE.START_AMT, W07510_WT_SWIMS_VERTICAL_MEASURE.UNIT_CODE FROM (((((W07510_WT_SWIMS_MONIT_STATION INNER JOIN W07510_WT_SWIMS_FIELDWORK ON W07510_WT_SWIMS_MONIT_STATION.MONIT_STATION_SEQ_NO = W07510_WT_SWIMS_FIELDWORK.MONIT_STATION_SEQ_NO) INNER JOIN W07510_WT_SWIMS_SAMPLE_HEADER ON W07510_WT_SWIMS_FIELDWORK.FIELDWORK_SEQ_NO = W07510_WT_SWIMS_SAMPLE_HEADER.FIELDWORK_SEQ_NO) INNER JOIN W07510_WT_SWIMS_SAMPLE_ANALYSIS ON W07510_WT_SWIMS_SAMPLE_HEADER.SAMPLE_HEADER_SEQ_NO = W07510_WT_SWIMS_SAMPLE_ANALYSIS.SAMPLE_HEADER_SEQ_NO) INNER JOIN W07510_WT_SWIMS_SAMPLE_RESULT ON W07510_WT_SWIMS_SAMPLE_ANALYSIS.SAMPLE_ANALYSIS_SEQ_NO = W07510_WT_SWIMS_SAMPLE_RESULT.SAMPLE_ANALYSIS_SEQ_NO) INNER JOIN W07510_WT_SWIMS_VERTICAL_MEASURE ON W07510_WT_SWIMS_SAMPLE_HEADER.SAMPLE_HEADER_SEQ_NO = W07510_WT_SWIMS_VERTICAL_MEASURE.SAMPLE_HEADER_SEQ_NO) INNER JOIN W07510_WT_SWIMS_MONIT_STATION_LOC_V ON W07510_WT_SWIMS_FIELDWORK.MONIT_STATION_SEQ_NO = W07510_WT_SWIMS_MONIT_STATION_LOC_V.MONIT_STATION_SEQ_NO WHERE (((W07510_WT_SWIMS_SAMPLE_RESULT.DNR_PARAMETER_CODE)=46380 Or (W07510_WT_SWIMS_SAMPLE_RESULT.DNR_PARAMETER_CODE)=99717 Or (W07510_WT_SWIMS_SAMPLE_RESULT.DNR_PARAMETER_CODE)=665) AND ((W07510_WT_SWIMS_MONIT_STATION.STATION_TYPE_CODE)='LAKE' Or (W07510_WT_SWIMS_MONIT_STATION.STATION_TYPE_CODE)='RESERVOIR' Or (W07510_WT_SWIMS_MONIT_STATION.STATION_TYPE_CODE)='RIVERINE IMPOUNDMENT') AND ((W07510_WT_SWIMS_MONIT_STATION_LOC_V.COUNTY_NAME)=", "'", counties$NAME[c], "'", "))", sep="")
  # query = str_replace_all(str_replace_all(query,"\n"," "),"\\s+"," ")
  # query = str_replace_all(query,"W07510_","W07510.")
  
  # Execute SWIMS query
  rawdata = sqlQuery(con, query, believeNRows=FALSE, as.is=TRUE)
  if(class(rawdata)=="character") {next}
  if(nrow(rawdata)==0) {next}
  
  # Format fields
  rawdata$START_DATETIME = as.Date(rawdata$START_DATETIME)
  rawdata$RESULT_AMT = as.numeric(rawdata$RESULT_AMT)
  rawdata$START_AMT = as.numeric(rawdata$START_AMT)
  
  # Remove data >2 m deep
  rawdata = rawdata[
    c((rawdata$UNIT_CODE=="FEET" & rawdata$START_AMT<6.6) |
        (rawdata$UNIT_CODE=="METERS" & rawdata$START_AMT<=2) |
        (rawdata$UNIT_CODE=="IN" & rawdata$START_AMT<80) | 
        (rawdata$UNIT_CODE=="CM" & rawdata$START_AMT<=200)),]
  
  # Remove data with zero, negative, or NA results
  rawdata = rawdata[!is.na(rawdata$RESULT_AMT),]
  rawdata = rawdata[rawdata$RESULT_AMT>0,]
  
  # Select and standardize units
  rawdata$RESULT_UGL = rawdata$RESULT_AMT
  rawdata$RESULT_UGL[
    rawdata$RESULT_UNITS_TEXT %in% c("MG/L","MG/L AS P")] = 
    rawdata$RESULT_AMT[rawdata$RESULT_UNITS_TEXT %in% c("MG/L","MG/L AS P")]*1000
  
  # Merge CHL and TP data
  CHL = rawdata[rawdata$DNR_PARAMETER_CODE %in% c(99717,46380),
                c("STATION_ID","PRIMARY_STATION_NAME","START_DATETIME","RESULT_UGL")]
  TP = rawdata[rawdata$DNR_PARAMETER_CODE %in% c(665),
               c("STATION_ID","PRIMARY_STATION_NAME","START_DATETIME","RESULT_UGL")]
  data = merge(CHL, TP, by=c("STATION_ID","PRIMARY_STATION_NAME","START_DATETIME"), suffixes=c("_CHL","_TP"))
  if(nrow(data)==0) {next}
  colnames(data) = c("ID","NAME","DATE","CHL","TP")
  data = aggregate(data[,c("CHL","TP")], by=list(data$ID,data$NAME,data$DATE), FUN=mean)
  colnames(data) = c("ID","NAME","DATE","CHL","TP")
  
  # Summarize stations
  stations = aggregate(data$DATE, by=list(data$ID,data$NAME), FUN=length)
  colnames(stations) = c("ID","NAME","COUNT")
  stations = stations[stations$COUNT>=6,]
  if(nrow(stations)==0) {next}
  stations = stations[order(stations$NAME),]
  
  # Parameters for all plots
  date_lims = c(min(data$DATE), max(data$DATE))
  TP_lims = exp(range(mod_data$logTP))
  CHL_lims = exp(range(mod_data$lwr, mod_data$upr))
  labs = c(0.1,0.2,0.5,1,2,5,10,20,50,100,200,500,1000)
  grids = c(0.1,0.2,0.5,1:9,1:9*10,1:9*100)
  min_year = as.numeric(min(format(range(data$DATE), format="%Y")))
  max_year = as.numeric(max(format(range(data$DATE), format="%Y")))
  date_grids = as.Date(paste(min_year:max_year, "-01-01", sep=""))
  polyx = c(exp(mod_data$logTP), exp(rev(mod_data$logTP)))
  polyy = c(exp(mod_data$lwr), exp(rev(mod_data$upr)))
  cols = c("darkorange","blue","darkgreen")
  tcols = adjustcolor(cols, alpha.f=0.3)
  
  # Make pdf of plots for all stations
  plotname = paste("~/r_workshop/plots/", 
    counties$NAME[c], "_lake_TP_CHL_station_plots.pdf", sep="")
  pdf(plotname, width = 8.5, height = 11)
  par(mfrow=c(2,1), mar=c(5,6,3,4), oma=c(2,2,2,2))
  for (s in 1:nrow(stations)) {
    # Subset data for station
    sdata = data[data$ID==stations$ID[s],]
  
    # Station plot parameters
    plot_title = paste(stations$ID[s], stations$NAME[s], sep=" - ")
    TP_CHL_lims = c(min(sdata$CHL,sdata$TP), max(sdata$CHL,sdata$TP))
    
    # Fit TP:CHL regression
    model = lm(log(CHL)~log(TP), sdata)
    fit = data.frame(TP=seq(min(sdata$TP), max(sdata$TP), length.out=100))
    fit = cbind(fit, exp(predict(model, fit, interval="confidence", level=0.9)))
    
    # Plot TP:CHL data and regression line
    plot(CHL~TP, sdata, type="n", log="xy", yaxt="n",
         xlim=TP_lims, ylim=CHL_lims, 
         xlab=expression("TP ("*mu*g/L*")"),
         ylab=expression("Chl ("*mu*g/L*")"),
         main=plot_title)
    axis(side=2, at=labs, labels=labs, las=1)
    abline(h=grids, v=grids, col="lightgray", lwd=0.5)
    polygon(polyx, polyy, col="lightgray", border=NA)
    lines(exp(mod_data$logTP), exp(mod_data$fit))
    polygon(c(fit$TP,rev(fit$TP)), c(fit$lwr,rev(fit$upr)),
            col=tcols[1], border=NA)
    lines(fit~TP, fit, col=cols[1])
    points(CHL~TP, sdata, pch=21, col=cols[1], bg=tcols[1])
    box()
    
    # Fit TP and CHL trends
    nyrs = length(unique(format(sdata$DATE,"%Y")))
    if(nyrs>=5) {
      fit = data.frame(DATE=seq(min(sdata$DATE), max(sdata$DATE), length.out=100))
      model = lm(log(TP)~DATE, sdata)
      TPfit = cbind(fit,exp(predict(model, fit, interval="confidence", level=0.9)))
      model = lm(log(CHL)~DATE, sdata)
      CHLfit = cbind(fit,exp(predict(model, fit, interval="confidence", level=0.9)))
    }
    
    # Plot TP and CHL time series
    plot(TP~DATE, sdata, type="n", log="y", yaxt="n",
         xlab="", ylab=expression("TP/Chl ("*mu*g/L*")"),
         xlim=date_lims, ylim=TP_CHL_lims, main="")
    axis(side=2, at=labs, labels=labs, las=1)
    abline(h=grids, v=date_grids, col="lightgray", lwd=0.5)
    if(nyrs>=5) {
      polygon(c(fit$DATE,rev(fit$DATE)), c(TPfit$lwr,rev(TPfit$upr)), 
              col=tcols[2], border=NA)
      polygon(c(fit$DATE,rev(fit$DATE)), c(CHLfit$lwr,rev(CHLfit$upr)),
              col=tcols[3], border=NA)
      lines(fit~DATE, TPfit, col=cols[2])
      lines(fit~DATE, CHLfit, col=cols[3])
    }
    points(TP~DATE, sdata, pch=21, col=cols[2], bg=tcols[2])
    points(CHL~DATE, sdata, pch=21, col=cols[3], bg=tcols[3])
    legend("top", xpd=TRUE, inset=c(0,-0.15), legend=c("TP","CHL"), 
           pch=21, col=cols[2:3], pt.bg=tcols[2:3], bty="n", horiz=TRUE)
    box()
  }
  dev.off()

}



