rm(list=ls())

library(readxl)
library(lubridate)

for(i in 1:5){
    assign(paste("s",i,sep=""),as.data.frame(read_excel("MR_Double_Sample_Predicted_full.xls",sheet=i)))
    print(dim(get(paste("s",i,sep=""))))
}

d = rbind(s1,s2,s3,s4,s5)
rownames(d) = NULL
d = unique(d)

d$MILK_WEIGHING_DTM = ymd_hms(d$MILK_WEIGHING_DTM)
d = d[-which(is.na(d$MILK_WEIGHING_DTM)),]

cond1 = d$MILK_YIELD < 2 | d$MILK_YIELD >20
rmidx = which(cond1)

cond2 = d$ACT_FAT < 1 | d$ACT_FAT >10
rmidx = c(rmidx,which(cond2))

cond3 = d$ACT_PRO < 1 | d$ACT_PRO >10
rmidx = c(rmidx,which(cond3))

hr = hour(d$MILK_WEIGHING_DTM)
cond4 = hr < 5 | hr >= 23
rmidx = c(rmidx,which(cond4))

rmidx = unique(rmidx)

d = d[-rmidx,]

# prepare new variables in data.frame d
d$hr = hour(d$MILK_WEIGHING_DTM)
d$ampm = "AM"
d$ampm[d$hr >= 12] = "PM"

uq = unique(d$TAG)

AMPM = NULL
PMAM = NULL

count = 0

g2meas = list() # greater than 2 measurements per day (number of days per cow where > 2 measurements)

for(i in 1:length(uq)){
    cid = uq[i]
    dsub = d[d$TAG==cid,]

    if(length(table(dsub$ampm))==1){ # only AM or only PM
        next
    }

    dt = date(dsub$MILK_WEIGHING_DTM)
    tb = table(dt) # number of observations per day
    idx = which(tb > 2)
    if(length(idx)>0){ # removing days where there are more than 2 milkings
        g2meas[[i]] = tb
    
        dts = names(tb)[idx]
        rmidx = NULL
        for(j in 1:length(dts)){
            rmidx = c(rmidx,which(dt==dts[j]))
        }
        dsub = dsub[-rmidx,]
    }

    if(nrow(dsub)==0){ # if there are now no rows left, move on to next cow
        next
    }
    
    print(i)
    #print(tb)
    n = nrow(dsub)
    #plot(dsub$MILK_WEIGHING_DTM,rep(0,n))

    dsub = dsub[order(dsub$MILK_WEIGHING_DTM),]
    

    for(j in 1:(n-1)){
        r1 = dsub[j,]
        r2 = dsub[j+1,]
        t1 = r1$MILK_WEIGHING_DTM
        t2 = r2$MILK_WEIGHING_DTM
        tdiff = as.numeric(difftime(t2,t1,units="hours")) # time difference in hours
        if(tdiff>=24){ # there is a gap of over a day between readings ... move on to next reading
            next
        }
        ap1 = r1$ampm
        ap2 = r2$ampm
        if(ap1==ap2){ # if both AM or both PM, exclude
            next
        }

        if(ap1=="AM"){
            colnames(r1) = paste(colnames(r1),"AM",sep="")
            colnames(r2) = paste(colnames(r1),"PM",sep="")
            AMPM = rbind(AMPM,cbind(r1,r2))
        }
        else{
            colnames(r1) = paste(colnames(r1),"PM",sep="")
            colnames(r2) = paste(colnames(r1),"AM",sep="")
            PMAM = rbind(PMAM,cbind(r1,r2))
        }
    }

    #print(AMPM)
    #print(PMAM)

    #readline("Press a key to continue ...\n")
}

write.csv(AMPM,file="AMPM.csv",row.names=FALSE)
write.csv(PMAM,file="PMAM.csv",row.names=FALSE)

save(list=ls(),file="finaldata.RData")

