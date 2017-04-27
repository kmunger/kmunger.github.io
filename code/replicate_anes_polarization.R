setwd("C:/Users/kevin/Dropbox/Dissertation")
library(foreign)
library(readstata13)
library(dplyr)
anes<-read.dta13("anes_timeseries_cdf_stata12.dta")

anes16<-read.dta("anes_timeseries_2016_Stata12.dta")


##only FTF

anes<-filter(anes, VCF0017 == "0. All personal")


##only face to face

anes16<-filter(anes16,V160501== "1. FTF/CASI")

summary(anes16$V160501)

##generate age variable
anes16$age<-anes16$V161267
anes16<-anes16[is.na(anes16$age)==F,]

anes16<-filter(anes16, age > 1 )



anes$age<-anes$VCF0101
anes<-anes[is.na(anes$age)==F,]

anes<-filter(anes, age > 1 )


##generate internet variable

summary(anes16$V161326)

anes16$internet<-as.numeric(anes16$V161326)

table(anes16$internet)

anes16$internet[anes16$internet<3]<-NA

anes16$internet[anes16$internet==3]<-1
anes16$internet[anes16$internet==4]<-0




summary(anes$VCF0744)

anes$internet<-as.numeric(anes$VCF0744)

table(anes$internet)

anes$internet[anes$internet==3]<-NA


##define year var

anes16$year<-2016


anes$year<-anes$VCF0004

#anes<-filter(anes, year >1995)


##this is the ftf weight
anes$weight<-(anes$VCF0009x)

##define perceptsions of the parties

table(anes$VCF0503)

anes$dem_placement<-as.numeric(anes$VCF0503)
anes$rep_placement<-as.numeric(anes$VCF0504)

anes$dem_placement[anes$dem_placement ==1 | anes$dem_placement == 9]<-NA

anes$rep_placement[anes$rep_placement ==1 | anes$rep_placement == 9]<-NA

##shift down 4 per paper; negative is liberal
anes$dem_placement<-(anes$dem_placement)-5
anes$rep_placement<-(anes$rep_placement)-5

##for 2016

table(anes16$V161130)

anes16$dem_placement<-as.numeric(anes16$V161130)
anes16$rep_placement<-as.numeric(anes16$V161131)

anes16$dem_placement[anes16$dem_placement ==1 | anes16$dem_placement == 2]<-NA

anes16$rep_placement[anes16$rep_placement ==1 | anes16$rep_placement == 2]<-NA

##shift down 4 per paper; negative is liberal
anes16$dem_placement<-(anes16$dem_placement)-6
anes16$rep_placement<-(anes16$rep_placement)-6


##define ideological affect 
table(anes$VCF0211)


##the top codes used to be for missing data
anes$VCF0211[anes$VCF0211==99 |anes$VCF0211==98 ]<-NA
anes$lib_therm<-anes$VCF0211



anes$VCF0212[anes$VCF0212==99 |anes$VCF0212==98 ]<-NA
anes$cons_therm<-anes$VCF0212

table(anes16$V162101)

anes16$V162097[anes16$V162097==998 |anes16$V162097==999| anes16$V162097==-6 |anes16$V162097==-9]<-NA
anes16$lib_therm<-anes16$V162097



anes16$V162101[anes16$V162101==998 |anes16$V162101==999 |anes16$V162101==-6 |anes16$V162101==-9]<-NA
anes16$cons_therm<-anes16$V162101



##define partisan affect 

table(anes$VCF0224)

anes$VCF0218[anes$VCF0218==99 |anes$VCF0218==98 ]<-NA
anes$dem_therm<-anes$VCF0218

anes$VCF0224[anes$VCF0224==99 |anes$VCF0224==98 ]<-NA
anes$rep_therm<-anes$VCF0224

table(anes16$V161095)


anes16$V161095[anes16$V161095==-99 |anes16$V161095==-89 | anes16$V161095==-88 ]<-NA
anes16$dem_therm<-anes16$V161095

anes16$V161096[anes16$V161096==-99 |anes16$V161096==-88 |anes16$V161096==-89]<-NA
anes16$rep_therm<-anes16$V161096


##turn PID variable into numeric, create leaners and full groups
summary(anes$VCF0301)

anes$pid_num<-as.numeric(anes$VCF0301)

table(anes$pid_num)

##drop missing

anes$pid_num[anes$pid_num ==1]<-NA
####


table(anes16$V161158x)
anes16$pid_num<-as.numeric(anes16$V161158x)

table(anes16$pid_num)


##get it on the same scale, drop missing

anes16$pid_num[anes16$pid_num < 0 ]<-NA

anes16$pid_num<-anes16$pid_num + 1

##turn ideology into numeric, filter


summary(anes$VCF0803)


anes$ideology_num<-as.numeric(anes$VCF0803)
anes$ideology_num[anes$ideology_num == 1] <-NA
anes$ideology_num[anes$ideology_num == 9] <-NA
anes$ideology_num[anes$ideology_num == 10] <-NA


table(anes$ideology_num)


summary(anes16$V161126)


anes16$ideology_num<-as.numeric(anes16$V161126)

anes16$ideology_num[anes16$ideology_num == 1] <-NA
anes16$ideology_num[anes16$ideology_num == 2] <-NA
anes16$ideology_num[anes16$ideology_num == 10] <-NA



anes16$ideology_num<-anes16$ideology_num - 1 
hist(anes16$ideology_num)
hist(anes$ideology_num)


##merge together--the weights you use depend on the variables, if pre/post

##for PRE
anes16$weight <- anes16$V160101f

##create minisets

anes_short<-select(anes, weight, year, lib_therm, dem_therm, rep_therm, cons_therm, 
                   dem_placement, rep_placement,ideology_num, pid_num, age, internet )
anes16_short<-select(anes16, weight, year, lib_therm, dem_therm, rep_therm, cons_therm,
                     dem_placement, rep_placement,ideology_num, pid_num, age, internet )

anes_pre<-rbind(anes_short, anes16_short)



#for POST---the codebook says to use this if you're using both pre and post data
anes16$weight <- anes16$V160102f


##create minisets

anes_short<-select(anes,dem_placement, rep_placement,  weight, year, lib_therm, dem_therm, rep_therm, cons_therm, 
                   ideology_num, pid_num, age, internet )
anes16_short<-select(anes16,dem_placement, rep_placement,
                      weight, year, lib_therm, dem_therm, rep_therm, cons_therm, ideology_num, pid_num, age, internet )

anes_post<-rbind(anes_short, anes16_short)


##initialize--MAP INCLUDE LEANERS; PRE-election weights

years<-c(1996, 1998, 2000, 2004, 2008, 2016)

MAP<-vector()
MAP75<-vector()
MAP65<-vector()
MAP1839<-vector()

MAP75i<-vector()
MAP65i<-vector()
MAP1839i<-vector()

MAP75ni<-vector()
MAP65ni<-vector()
MAP1839ni<-vector()


for(i in 1:length(years)){
  apropos<-filter(anes_pre, year ==years[i])
  
  dem_leaners<-filter(apropos, pid_num < 5  & pid_num > 1  )
  rep_leaners<-filter(apropos, pid_num > 5   )
  
###################STRAIGHT UP  
MAP[i]<-  sum(dem_leaners$weight * (dem_leaners$dem_therm - dem_leaners$rep_therm), na.rm=T) /sum(dem_leaners$weight, na.rm=T) +
  
sum(rep_leaners$weight * (rep_leaners$rep_therm - rep_leaners$dem_therm), na.rm=T) /sum(rep_leaners$weight, na.rm=T)

###################by age

dem_leaners75<-filter(apropos, pid_num < 5  & pid_num > 1 & age>74 )
rep_leaners75<-filter(apropos, pid_num > 5  & age>74 )
MAP75[i]<-  sum(dem_leaners75$weight * (dem_leaners75$dem_therm - dem_leaners75$rep_therm), na.rm=T) /sum(dem_leaners75$weight, na.rm=T) +
  
  sum(rep_leaners75$weight * (rep_leaners75$rep_therm - rep_leaners75$dem_therm), na.rm=T) /sum(rep_leaners75$weight, na.rm=T)

dem_leaners65<-filter(apropos, pid_num < 5  & pid_num > 1 & age>64 )
rep_leaners65<-filter(apropos, pid_num > 5  & age>64 )

MAP65[i]<-  sum(dem_leaners65$weight * (dem_leaners65$dem_therm - dem_leaners65$rep_therm), na.rm=T) /sum(dem_leaners65$weight, na.rm=T) +
  
  sum(rep_leaners65$weight * (rep_leaners65$rep_therm - rep_leaners65$dem_therm), na.rm=T) /sum(rep_leaners65$weight, na.rm=T)

dem_leaners1839<-filter(apropos, pid_num < 5  & pid_num > 1 & age < 40 )
rep_leaners1839<-filter(apropos, pid_num > 5  & age < 40)

MAP1839[i]<-  sum(dem_leaners1839$weight * (dem_leaners1839$dem_therm - dem_leaners1839$rep_therm), na.rm=T) /sum(dem_leaners1839$weight, na.rm=T) +
  sum(rep_leaners1839$weight * (rep_leaners1839$rep_therm - rep_leaners1839$dem_therm), na.rm=T) /sum(rep_leaners1839$weight, na.rm=T)


###################by age--internet 
dem_leaners75<-filter(apropos, pid_num < 5  & pid_num > 1 & age>74  & internet ==1)
rep_leaners75<-filter(apropos, pid_num > 5  & age>74 & internet ==1)
MAP75i[i]<-  sum(dem_leaners75$weight * (dem_leaners75$dem_therm - dem_leaners75$rep_therm), na.rm=T) /sum(dem_leaners75$weight, na.rm=T) +
  
  sum(rep_leaners75$weight * (rep_leaners75$rep_therm - rep_leaners75$dem_therm), na.rm=T) /sum(rep_leaners75$weight, na.rm=T)

dem_leaners65<-filter(apropos, pid_num < 5  & pid_num > 1 & age>64 & internet ==1 )
rep_leaners65<-filter(apropos, pid_num > 5  & age>64 & internet ==1 )

MAP65i[i]<-  sum(dem_leaners65$weight * (dem_leaners65$dem_therm - dem_leaners65$rep_therm), na.rm=T) /sum(dem_leaners65$weight, na.rm=T) +
  
  sum(rep_leaners65$weight * (rep_leaners65$rep_therm - rep_leaners65$dem_therm), na.rm=T) /sum(rep_leaners65$weight, na.rm=T)

dem_leaners1839<-filter(apropos, pid_num < 5  & pid_num > 1 & age < 40  & internet ==1)
rep_leaners1839<-filter(apropos, pid_num > 5  & age < 40 & internet ==1)

MAP1839i[i]<-  sum(dem_leaners1839$weight * (dem_leaners1839$dem_therm - dem_leaners1839$rep_therm), na.rm=T) /sum(dem_leaners1839$weight, na.rm=T) +
  sum(rep_leaners1839$weight * (rep_leaners1839$rep_therm - rep_leaners1839$dem_therm), na.rm=T) /sum(rep_leaners1839$weight, na.rm=T)


###################by age-- no internet 
dem_leaners75<-filter(apropos, pid_num < 5  & pid_num > 1 & age>74  & (internet==0 | internet ==2))
rep_leaners75<-filter(apropos, pid_num > 5  & age>74 & (internet==0 | internet ==2))
MAP75ni[i]<-  sum(dem_leaners75$weight * (dem_leaners75$dem_therm - dem_leaners75$rep_therm), na.rm=T) /sum(dem_leaners75$weight, na.rm=T) +
  
  sum(rep_leaners75$weight * (rep_leaners75$rep_therm - rep_leaners75$dem_therm), na.rm=T) /sum(rep_leaners75$weight, na.rm=T)

dem_leaners65<-filter(apropos, pid_num < 5  & pid_num > 1 & age>64 & (internet==0 | internet ==2))
rep_leaners65<-filter(apropos, pid_num > 5  & age>64 & (internet==0 | internet ==2) )

MAP65ni[i]<-  sum(dem_leaners65$weight * (dem_leaners65$dem_therm - dem_leaners65$rep_therm), na.rm=T) /sum(dem_leaners65$weight, na.rm=T) +
  
  sum(rep_leaners65$weight * (rep_leaners65$rep_therm - rep_leaners65$dem_therm), na.rm=T) /sum(rep_leaners65$weight, na.rm=T)

dem_leaners1839<-filter(apropos, pid_num < 5  & pid_num > 1 & age < 40  & (internet==0 | internet ==2))
rep_leaners1839<-filter(apropos, pid_num > 5  & age < 40 & (internet==0 | internet ==2))

MAP1839ni[i]<-  sum(dem_leaners1839$weight * (dem_leaners1839$dem_therm - dem_leaners1839$rep_therm), na.rm=T) /sum(dem_leaners1839$weight, na.rm=T) +
  sum(rep_leaners1839$weight * (rep_leaners1839$rep_therm - rep_leaners1839$dem_therm), na.rm=T) /sum(rep_leaners1839$weight, na.rm=T)


}


r<-data.frame( MAP1839i, MAP1839ni, 
               MAP75i, MAP75ni,
               MAP65i, MAP65ni, years)




require(ggplot2)
require(reshape2)

alldens = melt(r, id.vars ="years")

levels(alldens$variable)[levels(alldens$variable)=="MAP1839i"] <- "18-39, Internet"
levels(alldens$variable)[levels(alldens$variable)=="MAP1839ni"] <- "18-39, no Internet"
levels(alldens$variable)[levels(alldens$variable)=="MAP65i"] <- "65+, Internet"
levels(alldens$variable)[levels(alldens$variable)=="MAP65ni"] <- "65+, no Internet"
levels(alldens$variable)[levels(alldens$variable)=="MAP75i"] <- "75+, Internet"
levels(alldens$variable)[levels(alldens$variable)=="MAP75ni"] <- "75+, no Internet"


colors=c("hotpink1", "skyblue1", "orangered1", "gray1", "seagreen4", "tan4")

pdf("MAP_ftf.pdf", 7, 5)

ggplot(alldens, aes(y=value, x=years, colour=variable)) +

   geom_point() + 
  coord_cartesian(ylim =c(10,100)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
 
        axis.line = element_line(colour = "black")) + ylab("Mean Affect Polarization") +
  geom_line(size=2) + 
  scale_colour_manual(values = colors) +xlab("")

dev.off()

###############################################################

###############################################################
#
#         this is for partisan-ideology polarization: NO LEANERS, PRE


##############################################

##

##initialize--MPI
MPI<-vector()
MPI75<-vector()
MPI65<-vector()
MPI1839<-vector()

MPI75i<-vector()
MPI65i<-vector()
MPI1839i<-vector()

MPI75ni<-vector()
MPI65ni<-vector()
MPI1839ni<-vector()





for(i in 1:length(years)){
  apropos<-filter(anes_pre, year ==years[i])
  


  ###################by age--internet 
  dem75<-filter(apropos, pid_num < 4  & pid_num > 1 & age>74  & internet ==1)
  rep75<-filter(apropos, pid_num > 6  & age>74 & internet ==1)
  MPI75i[i]<-   sum(rep75$weight * rep75$ideology_num , na.rm=T) /sum(rep75$weight, na.rm=T) -
    
    sum(dem75$weight * dem75$ideology_num , na.rm=T) /sum(dem75$weight, na.rm=T) 
    
   
  
  dem65<-filter(apropos, pid_num < 4  & pid_num > 1 & age>64 & internet ==1 )
  rep65<-filter(apropos, pid_num > 6  & age>64 & internet ==1 )
  
  MPI65i[i]<-  sum(rep65$weight * rep65$ideology_num , na.rm=T) /sum(rep65$weight, na.rm=T) -
    
    sum(dem65$weight * dem65$ideology_num , na.rm=T) /sum(dem65$weight, na.rm=T) 
  
  dem1839<-filter(apropos, pid_num < 4  & pid_num > 1 & age < 40  & internet ==1)
  rep1839<-filter(apropos, pid_num > 6  & age < 40 & internet ==1)
  
  MPI1839i[i]<-  sum(rep1839$weight * rep1839$ideology_num , na.rm=T) /sum(rep1839$weight, na.rm=T) -
    
    sum(dem1839$weight * dem1839$ideology_num , na.rm=T) /sum(dem1839$weight, na.rm=T) 
  
  ###################by age-- no internet 
  dem75<-filter(apropos, pid_num < 4  & pid_num > 1 & age>74  & (internet==0 | internet ==2))
  rep75<-filter(apropos, pid_num > 6  & age>74 & (internet==0 | internet ==2))
  MPI75ni[i]<-  sum(rep75$weight * rep75$ideology_num , na.rm=T) /sum(rep75$weight, na.rm=T) -
    
    sum(dem75$weight * dem75$ideology_num , na.rm=T) /sum(dem75$weight, na.rm=T) 
  
  dem65<-filter(apropos, pid_num < 4  & pid_num > 1 & age>64 & (internet==0 | internet ==2))
  rep65<-filter(apropos, pid_num > 6  & age>64 & (internet==0 | internet ==2) )
  
  MPI65ni[i]<-  sum(rep65$weight * rep65$ideology_num , na.rm=T) /sum(rep65$weight, na.rm=T) -
    
    sum(dem65$weight * dem65$ideology_num , na.rm=T) /sum(dem65$weight, na.rm=T) 
  
  dem1839<-filter(apropos, pid_num < 4  & pid_num > 1 & age < 40  & (internet==0 | internet ==2))
  rep1839<-filter(apropos, pid_num > 6  & age < 40 & (internet==0 | internet ==2))
  
  MPI1839ni[i]<-  sum(rep1839$weight * rep1839$ideology_num , na.rm=T) /sum(rep1839$weight, na.rm=T) -
    
    sum(dem1839$weight * dem1839$ideology_num , na.rm=T) /sum(dem1839$weight, na.rm=T) 
  
}





r<-data.frame( MPI1839i, MPI1839ni, 
               MPI75i, MPI75ni,
               MPI65i, MPI65ni, years)




require(ggplot2)
require(reshape2)

alldens = melt(r, id.vars ="years")

levels(alldens$variable)[levels(alldens$variable)=="MPI1839i"] <- "18-39, Internet"
levels(alldens$variable)[levels(alldens$variable)=="MPI1839ni"] <- "18-39, no Internet"
levels(alldens$variable)[levels(alldens$variable)=="MPI65i"] <- "65+, Internet"
levels(alldens$variable)[levels(alldens$variable)=="MPI65ni"] <- "65+, no Internet"
levels(alldens$variable)[levels(alldens$variable)=="MPI75i"] <- "75+, Internet"
levels(alldens$variable)[levels(alldens$variable)=="MPI75ni"] <- "75+, no Internet"




pdf("MPI_ftf.pdf", 7, 5)

ggplot(alldens, aes(y=value, x=years, colour=variable)) +
  
  geom_point() + 
  coord_cartesian(ylim =c(-5,5)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        
        axis.line = element_line(colour = "black")) + ylab("Mean Partisan-Ideology Polarization") +
  geom_line(size=2) + 
  scale_colour_manual(values = colors) +xlab("")

dev.off()

###############################################################

###############################################################
#
#         this is for percieved partisan-ideology polarization--PRE


##############################################

##

##initialize--PPI
PPI75i<-vector()
PPI65i<-vector()
PPI1839i<-vector()

PPI75ni<-vector()
PPI65ni<-vector()
PPI1839ni<-vector()





for(i in 1:length(years)){
  apropos<-filter(anes_pre, year ==years[i])
  
  
  
  ###################by age--internet 
  ppl75<-filter(apropos, age>74  & internet ==1)
  PPI75i[i]<-   sum(ppl75$weight * (ppl75$rep_placement- ppl75$dem_placement) , na.rm=T) /sum(ppl75$weight, na.rm=T) 
  
  
  
  ppl65<-filter(apropos, age>64 & internet ==1 )

  PPI65i[i]<-  sum(ppl65$weight * (ppl65$rep_placement- ppl65$dem_placement) , na.rm=T) /sum(ppl65$weight, na.rm=T) 
  
  
  ppl1839<-filter(apropos,  age < 40  & internet ==1)

  PPI1839i[i]<-  sum(ppl1839$weight * (ppl1839$rep_placement- ppl1839$dem_placement) , na.rm=T) /sum(ppl1839$weight, na.rm=T) 
  
  
  ###################by age-- no internet 
  ppl75<-filter(apropos, age>74  & (internet ==0 | internet ==2))
  PPI75ni[i]<-   sum(ppl75$weight * (ppl75$rep_placement- ppl75$dem_placement) , na.rm=T) /sum(ppl75$weight, na.rm=T) 
  
  
  
  ppl65<-filter(apropos, age>64 & (internet ==0 | internet ==2) )
  
  PPI65ni[i]<-  sum(ppl65$weight * (ppl65$rep_placement- ppl65$dem_placement) , na.rm=T) /sum(ppl65$weight, na.rm=T) 
  
  
  ppl1839<-filter(apropos,  age < 40  & (internet ==0 | internet ==2))
  
  PPI1839ni[i]<-  sum(ppl1839$weight * (ppl1839$rep_placement- ppl1839$dem_placement) , na.rm=T) /sum(ppl1839$weight, na.rm=T) 
  
}





require(ggplot2)

r<-data.frame( PPI1839i, PPI1839ni, 
               PPI75i, PPI75ni,
               PPI65i, PPI65ni, years)



require(reshape2)

alldens = melt(r, id.vars ="years")

levels(alldens$variable)[levels(alldens$variable)=="PPI1839i"] <- "18-39, Internet"
levels(alldens$variable)[levels(alldens$variable)=="PPI1839ni"] <- "18-39, no Internet"
levels(alldens$variable)[levels(alldens$variable)=="PPI65i"] <- "65+, Internet"
levels(alldens$variable)[levels(alldens$variable)=="PPI65ni"] <- "65+, no Internet"
levels(alldens$variable)[levels(alldens$variable)=="PPI75i"] <- "75+, Internet"
levels(alldens$variable)[levels(alldens$variable)=="PPI75ni"] <- "75+, no Internet"




pdf("PPI_ftf.pdf", 7, 5)

ggplot(alldens, aes(y=value, x=years, colour=variable)) +
  
  geom_point() + 
  coord_cartesian(ylim =c(0,4)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        
        axis.line = element_line(colour = "black")) + ylab("Perceived Partisan-Ideology Polarization") +
  geom_line(size=2) + 
  scale_colour_manual(values = colors) +xlab("")

dev.off()

###############################################################

###############################################################
#
#         this is for mean ideological polarization--YES LEANERS, POST


##############################################

MIP<-vector()
MIP75<-vector()
MIP65<-vector()
MIP1839<-vector()

MIP75i<-vector()
MIP65i<-vector()
MIP1839i<-vector()

MIP75ni<-vector()
MIP65ni<-vector()
MIP1839ni<-vector()

for(i in 1:length(years)){
  apropos<-filter(anes_post, year ==years[i])
  
  lib_leaners<-filter(apropos, ideology_num < 5  & ideology_num > 1  )
  cons_leaners<-filter(apropos, ideology_num > 5 & ideology_num <9   )
  
  ###################STRAIGHT UP  
  MIP[i]<-  sum(lib_leaners$weight * (lib_leaners$lib_therm - lib_leaners$cons_therm), na.rm=T) /sum(lib_leaners$weight, na.rm=T) +
    
    sum(cons_leaners$weight * (cons_leaners$cons_therm - cons_leaners$lib_therm), na.rm=T) /sum(cons_leaners$weight, na.rm=T)
  
  ###################by age
  
  lib_leaners75<-filter(apropos, ideology_num < 5  & ideology_num > 1 & age>74 )
  cons_leaners75<-filter(apropos, ideology_num > 5 & ideology_num <9  & age>74 )
  MIP75[i]<-  sum(lib_leaners75$weight * (lib_leaners75$lib_therm - lib_leaners75$cons_therm), na.rm=T) /sum(lib_leaners75$weight, na.rm=T) +
    
    sum(cons_leaners75$weight * (cons_leaners75$cons_therm - cons_leaners75$lib_therm), na.rm=T) /sum(cons_leaners75$weight, na.rm=T)
  
  lib_leaners65<-filter(apropos, ideology_num < 5  & ideology_num > 1 & age>64 )
  cons_leaners65<-filter(apropos, ideology_num > 5 & ideology_num <9  & age>64 )
  
  MIP65[i]<-  sum(lib_leaners65$weight * (lib_leaners65$lib_therm - lib_leaners65$cons_therm), na.rm=T) /sum(lib_leaners65$weight, na.rm=T) +
    
    sum(cons_leaners65$weight * (cons_leaners65$cons_therm - cons_leaners65$lib_therm), na.rm=T) /sum(cons_leaners65$weight, na.rm=T)
  
  lib_leaners1839<-filter(apropos, ideology_num < 5  & ideology_num > 1 & age < 40 )
  cons_leaners1839<-filter(apropos, ideology_num > 5 & ideology_num <9  & age < 40)
  
  MIP1839[i]<-  sum(lib_leaners1839$weight * (lib_leaners1839$lib_therm - lib_leaners1839$cons_therm), na.rm=T) /sum(lib_leaners1839$weight, na.rm=T) +
    sum(cons_leaners1839$weight * (cons_leaners1839$cons_therm - cons_leaners1839$lib_therm), na.rm=T) /sum(cons_leaners1839$weight, na.rm=T)
  
  
  ###################by age--internet 
  lib_leaners75<-filter(apropos, ideology_num < 5  & ideology_num > 1 & age>74  & internet ==1)
  
  lib_leaners75$lib_therm - lib_leaners75$cons_therm
  cons_leaners75$cons_therm - cons_leaners75$lib_therm
  
  cons_leaners75<-filter(apropos, ideology_num > 5 & ideology_num <9  & age>74 & internet ==1)
  MIP75i[i]<-  sum(lib_leaners75$weight * (lib_leaners75$lib_therm - lib_leaners75$cons_therm), na.rm=T) /sum(lib_leaners75$weight, na.rm=T) +
    
    sum(cons_leaners75$weight * (cons_leaners75$cons_therm - cons_leaners75$lib_therm), na.rm=T) /sum(cons_leaners75$weight, na.rm=T)
  
  lib_leaners65<-filter(apropos, ideology_num < 5  & ideology_num > 1 & age>64 & internet ==1 )
  cons_leaners65<-filter(apropos, ideology_num > 5 & ideology_num <9  & age>64 & internet ==1 )
  
  MIP65i[i]<-  sum(lib_leaners65$weight * (lib_leaners65$lib_therm - lib_leaners65$cons_therm), na.rm=T) /sum(lib_leaners65$weight, na.rm=T) +
    
    sum(cons_leaners65$weight * (cons_leaners65$cons_therm - cons_leaners65$lib_therm), na.rm=T) /sum(cons_leaners65$weight, na.rm=T)
  
  lib_leaners1839<-filter(apropos, ideology_num < 5  & ideology_num > 1 & age < 40  & internet ==1)
  cons_leaners1839<-filter(apropos, ideology_num > 5 & ideology_num <9  & age < 40 & internet ==1)
  
  MIP1839i[i]<-  sum(lib_leaners1839$weight * (lib_leaners1839$lib_therm - lib_leaners1839$cons_therm), na.rm=T) /sum(lib_leaners1839$weight, na.rm=T) +
    sum(cons_leaners1839$weight * (cons_leaners1839$cons_therm - cons_leaners1839$lib_therm), na.rm=T) /sum(cons_leaners1839$weight, na.rm=T)
  
  
  ###################by age-- no internet 
  lib_leaners75<-filter(apropos, ideology_num < 5  & ideology_num > 1 & age>74  &( internet ==2 | internet ==0))
  cons_leaners75<-filter(apropos, ideology_num > 5 & ideology_num <9  & age>74 &( internet ==2 | internet ==0))
  MIP75ni[i]<-  sum(lib_leaners75$weight * (lib_leaners75$lib_therm - lib_leaners75$cons_therm), na.rm=T) /sum(lib_leaners75$weight, na.rm=T) +
    
    sum(cons_leaners75$weight * (cons_leaners75$cons_therm - cons_leaners75$lib_therm), na.rm=T) /sum(cons_leaners75$weight, na.rm=T)
  
  lib_leaners65<-filter(apropos, ideology_num < 5  & ideology_num > 1 & age>64 &( internet ==2 | internet ==0))
  cons_leaners65<-filter(apropos, ideology_num > 5 & ideology_num <9  & age>64 &( internet ==2 | internet ==0) )
  
  MIP65ni[i]<-  sum(lib_leaners65$weight * (lib_leaners65$lib_therm - lib_leaners65$cons_therm), na.rm=T) /sum(lib_leaners65$weight, na.rm=T) +
    
    sum(cons_leaners65$weight * (cons_leaners65$cons_therm - cons_leaners65$lib_therm), na.rm=T) /sum(cons_leaners65$weight, na.rm=T)
  
  lib_leaners1839<-filter(apropos, ideology_num < 5  & ideology_num > 1 & age < 40  &( internet ==2 | internet ==0))
  cons_leaners1839<-filter(apropos, ideology_num > 5 & ideology_num <9  & age < 40 &( internet ==2 | internet ==0))
  
  MIP1839ni[i]<-  sum(lib_leaners1839$weight * (lib_leaners1839$lib_therm - lib_leaners1839$cons_therm), na.rm=T) /sum(lib_leaners1839$weight, na.rm=T) +
    sum(cons_leaners1839$weight * (cons_leaners1839$cons_therm - cons_leaners1839$lib_therm), na.rm=T) /sum(cons_leaners1839$weight, na.rm=T)
  
  

  
  cons_leaners<-filter(apropos, ideology_num > 5 & ideology_num <9)
  lib_leaners<-filter(apropos, ideology_num < 5  & ideology_num > 1 )
  
  
  MIP[i]<-  sum(lib_leaners$weight * (lib_leaners$lib_therm - lib_leaners$cons_therm), na.rm=T) /sum(lib_leaners$weight, na.rm=T) +
    
    sum(cons_leaners$weight * (cons_leaners$cons_therm - cons_leaners$lib_therm), na.rm=T) /sum(cons_leaners$weight, na.rm=T)
}

##plot MIP
r<-data.frame( MIP1839i, MIP1839ni, 
               MIP75i, MIP75ni,
               MIP65i, MIP65ni, years)




require(ggplot2)
require(reshape2)

alldens = melt(r, id.vars ="years")

levels(alldens$variable)[levels(alldens$variable)=="MIP1839i"] <- "18-39, Internet"
levels(alldens$variable)[levels(alldens$variable)=="MIP1839ni"] <- "18-39, no Internet"
levels(alldens$variable)[levels(alldens$variable)=="MIP65i"] <- "65+, Internet"
levels(alldens$variable)[levels(alldens$variable)=="MIP65ni"] <- "65+, no Internet"
levels(alldens$variable)[levels(alldens$variable)=="MIP75i"] <- "75+, Internet"
levels(alldens$variable)[levels(alldens$variable)=="MIP75ni"] <- "75+, no Internet"



pdf("MIP_ftf.pdf", 7, 5)

ggplot(alldens, aes(y=value, x=years, colour=variable)) +
  
  geom_point() + 
  coord_cartesian(ylim =c(10,100)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        
        axis.line = element_line(colour = "black")) + ylab("Mean Ideological Polarization") +
  geom_line(size=2) + 
  scale_colour_manual(values = colors) +xlab("")

dev.off()



####overall party thermometers

##initialize--PPI
rep_therm<-vector()
dem_therm<-vector()
rep_therm_reps<-vector()
dem_therm_reps<-vector()
rep_therm_dems<-vector()
dem_therm_dems<-vector()




years<-unique(anes_pre$year)


for(i in 1:length(years)){
  apropos<-filter(anes_pre, year ==years[i])
   
  dem_leaners<-filter(apropos, pid_num < 5  & pid_num > 1  )
  rep_leaners<-filter(apropos, pid_num > 5 )
  
  
  rep_therm[i]<- sum(apropos$weight* apropos$rep_therm, na.rm = T)/sum(apropos$weight, na.rm = T)
  dem_therm[i]<- sum(apropos$weight* apropos$dem_therm, na.rm = T)/sum(apropos$weight, na.rm = T)
  
  rep_therm_reps[i]<- sum(rep_leaners$weight* rep_leaners$rep_therm, na.rm = T)/sum(rep_leaners$weight, na.rm = T)
  dem_therm_reps[i]<- sum(rep_leaners$weight* rep_leaners$dem_therm, na.rm = T)/sum(rep_leaners$weight, na.rm = T)
  rep_therm_dems[i]<- sum(dem_leaners$weight* dem_leaners$rep_therm, na.rm = T)/sum(dem_leaners$weight, na.rm = T)
  dem_therm_dems[i]<- sum(dem_leaners$weight* dem_leaners$dem_therm, na.rm = T)/sum(dem_leaners$weight, na.rm = T)
  
  
  }

r<-data.frame( rep_therm, dem_therm, years)

r<-data.frame( rep_therm_reps, dem_therm_reps, rep_therm_dems,dem_therm_dems, years)




r<-filter(r, years > 1977)

require(ggplot2)
require(reshape2)

alldens = melt(r, id.vars ="years")

levels(alldens$variable)[levels(alldens$variable)=="rep_therm_reps"] <- "Republicans In-Group"
levels(alldens$variable)[levels(alldens$variable)=="dem_therm_reps"] <- "Republicans Out-Group"
levels(alldens$variable)[levels(alldens$variable)=="rep_therm_dems"] <- "Democrats Out-Group"
levels(alldens$variable)[levels(alldens$variable)=="dem_therm_dems"] <- "Democrats In-Group"




png("weighted_party_thermometers_group.png")

ggplot(alldens, aes(y=value, x=years, colour=variable)) +
  
  geom_point() + 
  coord_cartesian(ylim =c(30,80))  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        
        axis.line = element_line(colour = "black")) + ylab("Weighted Mean Feeling Thermometer") +
  geom_line(size=2)   +xlab("") + scale_colour_manual(values = c("red", "pink", "lightblue", "blue"))

dev.off()
