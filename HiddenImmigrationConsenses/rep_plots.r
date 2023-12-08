# replication code for plots 
# (run rep_estimates.do first)

rm(list=ls())
library(foreign)
library("ggplot2")

# function to prepare data
prepdata <- function(d){
  
  # prep estimates
  d$var <- rownames(d)
  colnames(d) <- c("pe","se","var")
  d$order <- 1:nrow(d)
  # compute Cis
  d$upper <-d$pe + 1.96*d$se
  d$lower <-d$pe - 1.96*d$se
  
  # define group
  d$group <- NA
  d$group[d$var %in% paste(c("1b",2),".FeatGender",sep="")]         <- "Gender"
  d$group[d$var %in% paste(c("1b",2:7),".FeatEd",sep="")]           <- "Education"
  d$group[d$var %in% paste(c("1b",2:11),".FeatJob",sep="")]         <- "Job"
  d$group[d$var %in% paste(c(1:5,"6b",7:10),".FeatCountry",sep="")] <- "Origin"
  d$group[d$var %in% paste(c("1b",2:3),".FeatReason",sep="")]       <- "Reason for Application"
  d$group[d$var %in% paste(c("1b",2:4),".FeatExp",sep="")]          <- "Job Experience"
  d$group[d$var %in% paste(c("1b",2:5),".FeatTrips",sep="")]        <- "Prior Entry"
  d$group[d$var %in% paste(c(1:2,"3b",4),".FeatPlans",sep="")]      <- "Job Plans"
  d$group[d$var %in% paste(c("1b",2:4),".FeatLang",sep="")]         <- "Language Skills"
  
  # order 
  d <- d[order(factor(d$group,levels=unique(d$group)[c(1,2,4,5,3,7,8,6,9)])),]
  d$order <- 1:nrow(d)
  
  # label attributes
  offset <- c("   ")
  
  d$var[d$group=="Gender"] <- paste(offset,c("female","male"))
  d$var[d$group=="Education"] <- paste(offset,c("no formal","4th grade",
                                                "8th grade","high school",
                                                "two-year college","college degree",
                                                "graduate degree"))
  
  d$var[d$group=="Language Skills"] <- paste(offset,c("fluent English",
                                                      "broken English",
                                                      "tried English but unable",
                                                      "used interpreter"))
  
  d$var[d$group=="Origin"] <- paste(offset,c("Germany","France","Mexico",
                                             "Philippines","Poland","India",
                                             "China","Sudan","Somalia","Iraq"))
  
  d$var[d$group=="Job"] <- paste(offset,c("janitor","waiter","child care provider",
                                          "gardener","financial analyst",
                                          "construction worker","teacher",
                                          "computer programmer","nurse",
                                          "research scientist","doctor"))
  
  d$var[d$group=="Reason for Application"] <- paste(offset,c("reunite with family",
                                                             "seek better job",
                                                             "escape persecution"))
  
  
  d$var[d$group=="Job Experience"] <- paste(offset,c("none","1-2 years",
                                                     "3-5 years","5+ years"))
  
  
  d$var[d$group=="Job Plans"] <- paste(offset,c("contract with employer",
                                                "interviews with employer",
                                                "will look for work",
                                                "no plans to look for work"))
  
  d$var[d$group=="Prior Entry"] <- paste(offset,c("never","once as tourist",
                                                  "many times as tourist","six months with family",
                                                  "once w/o authorization"))            
  # sub in group labels
  dd <- data.frame(var= c("Gender:",
                          " ",
                          "Education:",
                          "  ",
                          "Language:",
                          "   ",
                          "Origin:",
                          "    ",
                          "Profession:",
                          "     ",
                          "Job experience:",
                          "      ",
                          "Job plans:",
                          "       ",
                          "Application reason:",
                          "        ",
                          "Prior trips to U.S.:"
  ),order=c(.5,2.1,2.5,9.1,9.5,13.1,13.5,23.1,23.5,34.1,34.5,38.1,38.5,42.1, 42.5,45.1,45.5),
                   pe=1,se=1,upper=1,lower=1,group=NA)
  d <- rbind(d,dd)
  d <-d[order(d$order),]
  d$var <- factor(d$var,levels=unique(d$var)[length(d$var):1])
  return(d)
}

# theme for figures
theme_bw1 <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size , colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_line(colour = "grey50"),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "none"
    )
}


## Figure 2: Effects of Immigrant Attributes on Probability of Being Preferred for Admission
ffilename <- "chosen"
d <- read.table(paste(ffilename,".txt",sep=""))
d <- prepdata(d)

yylab  <- c("Effect on Pr(Immigrant Preferred for Admission)")

p = ggplot(d,aes(y=pe,x=var))#,colour=group))
p = p + coord_flip(ylim = c(-.3, .3))  
p = p + geom_hline(yintercept = 0,size=.5,colour="darkgrey",linetype="solid") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.4,.4,.2),1),labels=c("-.4","-.2","0",".2",".4"))
p = p + scale_x_discrete(name="")
print(p)
dev.off()

pdf(paste("1",ffilename,".pdf",sep=""),width=10,height=12.5) 
p = p  + theme_bw1()
print(p)
dev.off()
write.csv(d[,c("pe","se","var","upper","lower","group")],
          file=paste("1",ffilename,".csv",sep=""))

## Figure C.1: Effects of Immigrant Attributes on Support for Admission
ffilename <- "support"
d <- read.table(paste(ffilename,".txt",sep=""))
d <- prepdata(d)

yylab <- c("Change in Pr(Immigrant Supported for Admission to U.S.)")

p = ggplot(d,aes(y=pe,x=var))#,colour=group))
p = p + coord_flip(ylim = c(-.3, .3))  
p = p + geom_hline(yintercept = 0,size=.5,colour="darkgrey",linetype="solid") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.4,.4,.2),1),labels=c("-.4","-.2","0",".2",".4"))
p = p + scale_x_discrete(name="")
print(p)
dev.off()

pdf(paste("1",ffilename,".pdf",sep=""),width=10,height=12.5) 
p = p  + theme_bw1()
print(p)
dev.off()
write.csv(d[,c("pe","se","var","upper","lower","group")],
          file=paste("1",ffilename,".csv",sep=""))

## Figures for subsets 

# theme
theme_bw1 <- function(base_size = 10.1, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size , colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_line(colour = "grey50"),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "none"
    )
}

yylab  <- c("Effect on Pr(Immigrant Preferred for Admission)")

# list of subsets
dl <- list()

dl[[1]] <- list(subfilename="ppeducat",
                subsetnlabel="Educational attainment:",
                slevels=1:2,
                slabels=c("No College","Some College or More"))

dl[[2]] <- list(subfilename="ethnocentrism",
                subsetnlabel="",
                slevels=1:2,
                slabels=c("Low Ethnocentrism","High Ethnocentrism"))

dl[[3]] <- list(subfilename="whiteornot",
                subsetnlabel="",
                slevels=1:2,
                slabels=c("Non-White","White"))

dl[[4]] <- list(subfilename="partyid",
                subsetnlabel="",
                slevels=1:2,
                slabels=c("Republican","Democrat"))

dl[[5]] <- list(subfilename="industryfb",
                subsetnlabel="",
                slevels=1:2,
                slabels=c("Works in Industry with few Immigrants",
                          "Works in Industry with many Immigrants"))

dl[[6]] <- list(subfilename="income",
                subsetnlabel="",
                slevels=1:2,
                slabels=c("Low Income",
                          "High Income"))

dl[[7]] <- list(subfilename="fiscalexp2",
                subsetnlabel="Fiscal Exposure to Immigration:",
                slevels=1:2,
                slabels=c("Low",
                          "High"))

dl[[8]] <- list(subfilename="zipcodediversity",
                subsetnlabel="ZIP:",
                slevels=1:3,
                slabels=c("Many Immigrants, Majority Not Hispanic",
                          "Many Immigrants, Majority Hispanic",
                          "Few Immigrants"))

dl[[9]] <- list(subfilename="ideology",
                subsetnlabel="",
                slevels=1:2,
                slabels=c("Liberal","Conservative"))

dl[[10]] <- list(subfilename="opposeimmig",
                subsetnlabel="",
                slevels=1:2,
                slabels=c("Does not Support Reducing Immigration",
                          "Supports Reducing Immigration"))

dl[[11]] <- list(subfilename="gender",
                 subsetnlabel="",
                 slevels=1:2,
                 slabels=c("Male",
                           "Female"))

dl[[12]] <- list(subfilename="age",
                 subsetnlabel="",
                 slevels=1:2,
                 slabels=c("Young","Old"))


dl[[13]] <- list(subfilename="paneltenure",
                 subsetnlabel="",
                 slevels=1:2,
                 slabels=c("Short","Long"))


dl[[14]] <- list(subfilename="pairingno",
                 subsetnlabel="Pairing No:",
                 slevels=1:5,
                 slabels=1:5)


dl[[15]] <- list(subfilename="selfmonitor",
                 subsetnlabel="",
                 slevels=1:2,
                 slabels=c("Low Self-Monitor",
                           "High Self-Monitor"))

dl[[16]] <- list(subfilename="countertypical",
                 subsetnlabel="# of atypical profiles:",
                 slevels=1:3,
                 slabels=c("0-3","4-5","6-9"))


dl[[17]] <- list(subfilename="hispanicornot",
                subsetnlabel="",
                slevels=1:2,
                slabels=c("Non-Hispanic","Hispanic"))

# do the plots
for(kk in 1:length(dl)){

filenames <- paste(paste(dl[[kk]]$subfilename,
                         dl[[kk]]$slevels,sep=""),
                   ".txt",sep="")

alldata <- list()
for(i in 1:length(filenames)){
  d <- read.table(filenames[i])
  alldata[[i]] <- prepdata(d)
  alldata[[i]]$subset      <- dl[[kk]]$slevels[i]
  alldata[[i]]$subsetlabel <- paste(dl[[kk]]$subsetnlabel,
                                    dl[[kk]]$slabels[i],sep=" ")
}

d <- alldata[[1]]
for(i in 2:length(filenames)){
d <- rbind(d,alldata[[i]])
}

d$subsetlabel <- factor(d$subsetlabel,levels=unique(d$subsetlabel))

p = ggplot(d ,aes(y=pe,x=var))#,colour=group))
p = p + facet_grid(.~subsetlabel)
p = p + coord_flip(ylim = c(-.3, .3))
p = p + geom_hline(yintercept = 0,size=.5,colour="darkgrey",linetype="solid") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=seq(-.2,.2,.1)) 
p = p + scale_x_discrete(name="")
p = p + theme_bw1()
print(p)

dev.off()
pdf(paste("1",dl[[kk]]$subfilename,".pdf",sep=""),width=14,height=10)
print(p)
dev.off()

write.csv(d[,c("pe","se","var","upper","lower","group","subset","subsetlabel")],
          file=paste("1",dl[[kk]]$subfilename,".csv",sep=""))

}

# additional robustness checks

## Respondent Fixed Effects 
ffilename <- "resfixedeffects"
d <- read.table(paste(ffilename,".txt",sep=""))
d <- prepdata(d)

yylab <- c("Effect on Pr(Immigrant Preferred for Admission)")

p = ggplot(d,aes(y=pe,x=var))#,colour=group))
p = p + coord_flip(ylim = c(-.3, .3))  
p = p + geom_hline(yintercept = 0,size=.5,colour="darkgrey",linetype="solid") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.4,.4,.2),1),labels=c("-.4","-.2","0",".2",".4"))
p = p + scale_x_discrete(name="")
print(p)
dev.off()

pdf(paste("1",ffilename,".pdf",sep=""),width=10,height=12.5)
p = p  + theme_bw1()
print(p)
dev.off()
write.csv(d[,c("pe","se","var","upper","lower","group")],
          file=paste("1",ffilename,".csv",sep=""))

## Respondent Random Effects 
ffilename <- "resrandomeffects"
d <- read.table(paste(ffilename,".txt",sep=""))
d <- prepdata(d)

yylab <- c("Effect on Pr(Immigrant Preferred for Admission)")

p = ggplot(d,aes(y=pe,x=var))#,colour=group))
p = p + coord_flip(ylim = c(-.3, .3))  
p = p + geom_hline(yintercept = 0,size=.5,colour="darkgrey",linetype="solid") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.4,.4,.2),1),labels=c("-.4","-.2","0",".2",".4"))
p = p + scale_x_discrete(name="")
print(p)
dev.off()

pdf(paste("1",ffilename,".pdf",sep=""),width=10,height=12.5)
p = p  + theme_bw1()
print(p)
dev.off()
write.csv(d[,c("pe","se","var","upper","lower","group")],
          file=paste("1",ffilename,".csv",sep=""))


## Figure 3: Estimated Probability of Being Chosen for Admission for Selected Immigrant Profiles
rm(list=ls())
library(foreign)

# function to get labels
getlabels <- function(vector){
  
  label <- c("")
  for(i in 1:length(vector)){
    
    if(i==1){ # Gender_Immigrant
      if(vector[i]==2){lab <- c("Male")}
      if(vector[i]==1){lab <- c("Female")}
      lab <- paste("Gender: ",lab,sep="")
      label = paste(label,lab,sep="")
    }
    
    if(i==2){ # Ed_Immigrant
      if(vector[i]==1){lab <- c("no formal")}
      if(vector[i]==2){lab <- c("4th grade")}
      if(vector[i]==3){lab <- c("8th grade")}
      if(vector[i]==4){lab <- c("high school")}
      if(vector[i]==5){lab <- c("two-year college")}
      if(vector[i]==6){lab <- c("college degree")}
      if(vector[i]==7){lab <- c("graduate degree")}
      lab <- paste("Education: ",lab,sep="")
      label = paste(label,lab,sep="; ")
    }
    
    if(i==3){ # Language_Immigrant
      if(vector[i]==1){lab <- c("fluent English")}
      if(vector[i]==2){lab <- c("broken English")}
      if(vector[i]==3){lab <- c("tried English but unable")}
      if(vector[i]==4){lab <- c("used interpreter")}
      lab <- paste("Language: ",lab,sep="")
      label = paste(label,lab,sep="; ")
    }
    
    if(i==4){ # Country_Immigrant
      if(vector[i]==1){lab <- c("Germany")}
      if(vector[i]==2){lab <- c("France")}
      if(vector[i]==3){lab <- c("Mexico")}
      if(vector[i]==4){lab <- c("Philippines")}  
      if(vector[i]==5){lab <- c("Poland")}
      if(vector[i]==6){lab <- c("India")}  
      if(vector[i]==7){lab <- c("China")}
      if(vector[i]==8){lab <- c("Sudan")}
      if(vector[i]==9){lab <- c("Somalia")}
      if(vector[i]==10){lab <- c("Iraq")}
      lab <- paste("Origin: ",lab,sep="")
      label = paste(label,lab,sep="; ")
    }    
    
    if(i==5){ # Job_Immigrant
      if(vector[i]==1){lab <- c("janitor")}
      if(vector[i]==2){lab <- c("waiter")}
      if(vector[i]==3){lab <- c("child care provider")}
      if(vector[i]==4){lab <- c("gardener")}      
      if(vector[i]==5){lab <- c("financial analyst")}      
      if(vector[i]==6){lab <- c("construction worker")}
      if(vector[i]==7){lab <- c("teacher")}
      if(vector[i]==8){lab <- c("computer programmer")}      
      if(vector[i]==9){lab <- c("nurse")}
      if(vector[i]==10){lab <- c("research scientist")}
      if(vector[i]==11){lab <- c("doctor")}
      lab <- paste("Profession: ",lab,sep="")
      label = paste(label,lab,sep="; ")
    }
    
    if(i==6){ # JobExp_Immigrant
      if(vector[i]==1){lab <- c("none")}
      if(vector[i]==2){lab <- c("1-2 years")}
      if(vector[i]==3){lab <- c("3-5 years")}
      if(vector[i]==4){lab <- c("5+years")}
      lab <- paste("Job experience: ",lab,sep="")
      label = paste(label,lab,sep="; ")
    }
    
    if(i==7){ # JobPlans_Immigrant
      if(vector[i]==1){lab <- c("contract with employer")}
      if(vector[i]==2){lab <- c("interviews with employer")}
      if(vector[i]==3){lab <- c("will look for work")}
      if(vector[i]==4){lab <- c("no plans to look for work")}
      lab <- paste("Job plans: ",lab,sep="")
      label = paste(label,lab,sep="; ")
    }
    if(i==8){ # Reason_Immigrant
      if(vector[i]==1){lab <- c("reunite with family")}
      if(vector[i]==2){lab <- c("seek better job")}
      if(vector[i]==3){lab <- c("escape persecution")}
      lab <- paste("Application reason: ",lab,sep="")
      label = paste(label,lab,sep="; ")
    }
    if(i==9){ # PriorTrips_Immigrant
      if(vector[i]==1){lab <- c("never")}
      if(vector[i]==2){lab <- c("once as tourist")}
      if(vector[i]==3){lab <- c("many times as tourist")}
      if(vector[i]==4){lab <- c("six months with family")}
      if(vector[i]==5){lab <- c("once w/o authorization")}
      lab <- paste("Prior trips to U.S.: ",lab,sep="")
      label = paste(label,lab,sep="; ")
    }
    
  }
  return(label) 
}

d <- read.table("chosenPRs.txt")
d$ub <- d$pe+1.645*d$se
d$lb <- d$pe-1.645*d$se

d$index <- seq(1,10,2)

d$labels <- NA
for(i in 1:nrow(d)){
  d$labels[i] <- getlabels(d[i,colnames(d)[3:11]])
}
howmanylabels <- length(colnames(d)[3:11])

pdf("1prbyprofile.pdf",width=12,height=8)
plot(x=d$pe,y=d$index,
     ylim=c(0.3,max(d$index)+.6),
     xlim=c(-.45,1.15),
     xlab="                         Pr(Immigrant Preferred for Admission to U.S.)",
     ylab="",
     xaxt="n",
     yaxt="n",
     pch=19     
)
axis(side=2,at=d$index,labels=NA)
axis(side=1,at=seq(.0,1,.1),labels=seq(0,1,.1))

arrows(x0=d$lb, y0=d$index, x1=d$ub,y1=d$index,angle=90,code=3,length=.2) 
abline(v=.5,lty=3)

for(i in 1:nrow(d)){
  text(x=rep(-.25,howmanylabels),y=d$index[i]+seq(.8,-.8,length.out=howmanylabels)
       ,labels=strsplit(d$labels[i],"; ")[[1]]
       ,cex=.9)
}

text(y=d$index,x=1.07,labels=paste("percentile: ",c(1,25,50,75,99),sep=""),cex=.9)
dev.off()






