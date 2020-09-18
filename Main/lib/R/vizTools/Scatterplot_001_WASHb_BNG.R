rm(list=ls())
library(ggplot2)


#############################################################
# set working directory

setwd("~/Documents/GitHub/ClinEpiWorkflow/Main/lib/R/vizTools")


#############################################################
# load data 

d <- read.csv("./Data/2020_09_17_WASHb_BNG_bulk_download_merged.csv", as.is=T)


#############################################################
#rename variables of interest because default lables are too long/messy

d$pid <- d$Participant_Id
d$lns <- d$Percent.LNS.consumed..caregiver.report..EUPATH_0035031.
d$hfias <- d$Household.Food.Insecurity.Access.Scale..HFIAS...EUPATH_0011145.
d$hfias_score <- d$Household.Food.Insecurity.Access.Scale..HFIAS..score..EUPATH_0011151.
d$diar <- d$Diarrhea.case.during.the.last.7.days..caregiver.report..EUPATH_0035097.
d$weight_for_age <- d$Weight.for.age.z.score..using.median.weight..EUPATH_0035073.
d$circ_for_age <- d$Head.circumference.for.age.z.score..using.median.circumference..EUPATH_0035075.
d$height_for_age <- d$Length..or.height.for.age.z.score..using.median.stature..EUPATH_0035067.
d$svy <- d$Study.timepoint..OBI_0001508.
d$hh_svy <- d$Household.study.timepoint..EUPATH_0044122.


#############################################################
# limit data to target kids & renamed variables of interest

table(d$Target.child.or.sibling.neighbor..EUPATH_0035112., useNA="ifany")
names(d)

d <- d[d$Target.child.or.sibling.neighbor..EUPATH_0035112.=="Target child",110:119]


#############################################################
#############################################################
# clean up data --> general issue: household observation data is in a different row than participant observation data,
# even when the study timepoint is the same
#############################################################
#############################################################

#############################################################
# pull out household data
# specific issue with households:
# there are household data (hh) with timepoint: svy==NA and household observation (hh_obs) data with svy==c(0,1,2)
# need to fill in the hh data at every timepoint of hh_obs & remove rows where svy==NA

hh <- d[,c("pid", "lns", "hfias", "hfias_score", "hh_svy")]

#hfias data is measured 1x not at the observation level --> fill in for each participant
for(i in hh$pid){
  if(length(unique(hh$hfias[!is.na(hh$hfias) & hh$pid==i]))>0){
    hh$hfias[hh$pid==i] <- unique(hh$hfias[!is.na(hh$hfias) & hh$pid==i])
  }
}


# remove rows where hh_svy==NA
hh <- hh[!is.na(hh$hh_svy),]

# rename hh_svy to svy
names(hh)[names(hh)=="hh_svy"] <- "svy"


#############################################################
# pull out participant observation data & clean up

p <- d[!is.na(d$svy),c("pid", "svy", "diar", "weight_for_age", "circ_for_age","height_for_age")]
head(p)


#############################################################
#merge household observation and participant observation data by svy & pid

d <- merge(p, hh, all.x=T, all.y=T, by=c("pid", "svy"))


#############################################################
# plot

diar.labs <- c("Diarrhea", "No diarrhea")
names(diar.labs) <- c("Yes", "No")

ggplot(d[!is.na(d$diar),], aes(weight_for_age, height_for_age, color=as.character(svy))) +
  geom_point(alpha=0.6, shape=1, size=1) + 
  geom_smooth () +
  xlab("Weight-for-age z-score") +
  ylab("Height-for-age z-score") +
  labs(color="Timepoint") +
  facet_grid(cols = vars(diar), labeller=labeller(.cols=diar.labs))



