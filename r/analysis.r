# script to analyze data from Matthew Baggott's mystical experiences survey
# 

# packages
require(ggplot2)
require(stringr)
require(lubridate)
require(scales)
require(reshape2)
require(fastR)
require(plyr)

username <- strsplit(getwd(), split='/')[[1]][3]
baselocation <- paste0('/Users/',username,'/Documents')
projectname <- "mystical"

# project locations
datadir <- paste0(baselocation,"/coding/",projectname,"/data/")
rdir <- paste0(baselocation,"/coding/",projectname,"/r/")
resultsdir <- paste0(baselocation,"/coding/",projectname,"/results/")

setwd(datadir)

# define functions

# make names of data.frame nicer
makeNicerNames <- function(dfnames){
  require(stringr)
  str_replace_all(tolower(str_replace_all(make.names(dfnames),"\\.+","_")),"_$","")
}

# Get lower triangle of the correlation matrix
getLowerTriangle <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
getUpperTriangle <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# read data and improve names

daters <- read.csv("mystical_experience_data.csv",stringsAsFactors=FALSE,na.strings=c("NA",""))
names(daters) <- makeNicerNames(names(daters))

ids <- c("user_id", "time_utc", "publisher_category", "gender", "age", 
         "geography", "urban_density", "income", "parental_status", "weight")
resps <- c("question_1_answer", "question_2_answer_1_using_sacred_plants_or_other_drugs", 
         "question_2_answer_2_fasting", "question_2_answer_3_being_outdoors_in_nature", 
         "question_2_answer_4_prayer_meditation_or_contemplation", "question_2_answer_5_religious_ceremony_practice_or_ritual", 
         "question_2_answer_none_of_the_above", "response_time_1", "response_time_2")
resps_short <- c("had_relig", "drugs", 
           "fasting", "nature", "prayer", "ceremony", 
           "other", "rt_1", "rt_2")

# shorten response names
names(daters)[names(daters) %in% resps]  <- 
  resps_short[match(names(daters)[names(daters) %in% resps],resps)]

# make rt numeric
daters$rt_1 <- as.numeric(str_replace(daters$rt_1,"ms",""))
daters$rt_2 <- as.numeric(str_replace(daters$rt_2,"ms",""))

# make responses boolean
daters[,names(daters) %in% setdiff(resps_short,c("rt_1","rt_2"))] <- 
!is.na(daters[,names(daters) %in% setdiff(resps_short,c("rt_1","rt_2"))])

summary(daters)

# count behaviors ('triggers') other than 'other'
triggers <- c("drugs", "fasting", "nature", "prayer", "ceremony")
daters$trigger_count  <- rowSums(daters[,names(daters) %in% triggers])
head(daters)

# distribution of situations/behaviors endorsed
ggplot(daters,aes(x=trigger_count))+geom_histogram(fill="grey")+
  coord_flip()+xlab("Number of behaviors endorsed\n")+
  ylab("Count of respondents")+theme_bw()

#get percents of number endorsed
table(daters$trigger_count)/dim(daters)[1]

#get common pairs
for(i in 1:(length(triggers)-1)){
  for(j in (i+1):length(triggers)){
    #cat(paste(i, j,"\n"))
    cat(paste0(triggers[i]," and ",triggers[j]," occurs in ", 100*
                 round(sum(daters[daters$trigger_count==2,triggers[i]] &
                             daters[daters$trigger_count==2,triggers[j]])/dim(daters)[1],3),"%\n"))
 }
}


paste0("Among those who reported prayer, meditation, or contemplation before a mystical-religious experience ",
       round(100*mean(daters$nature[daters$prayer==TRUE]),1),"% also being outside in nature; ",
       round(100*mean(daters$ceremony[daters$prayer==TRUE]),1),"% also reported religious ceremony, practice, or ritual; ",
       round(100*mean(daters$fasting[daters$prayer==TRUE]),1),"% also reported fasting, and ",
       round(100*mean(daters$drugs[daters$prayer==TRUE]),1),"% also reported using drugs.")

paste0("Among those who reported religious ceremony, practice, or ritual before a mystical-religious experience ",
       round(100*mean(daters$nature[daters$ceremony==TRUE]),1),"% also being outside in nature; ",
       round(100*mean(daters$prayer[daters$ceremony==TRUE]),1),"% also reported prayer, meditation, or contemplation; ",
       round(100*mean(daters$fasting[daters$ceremony==TRUE]),1),"% also reported fasting, and ",
       round(100*mean(daters$drugs[daters$ceremony==TRUE]),1),"% also reported using drugs.")

paste0("Among those who reported being outside in nature before a mystical-religious experience ",
       round(100*mean(daters$prayer[daters$nature==TRUE]),1),"% also prayer, meditation, or contemplation; ",
       round(100*mean(daters$ceremony[daters$nature==TRUE]),1),"% also reported religious ceremony, practice, or ritual; ",
       round(100*mean(daters$fasting[daters$nature==TRUE]),1),"% also reported fasting, and ",
       round(100*mean(daters$drugs[daters$nature==TRUE]),1),"% also reported using drugs.")

paste0("Among those who reported drug-taking before a mystical-religious experience ",
       round(100*mean(daters$prayer[daters$drugs==TRUE]),1),"% also reported prayer, meditation, or contemplation; ",
       round(100*mean(daters$ceremony[daters$drugs==TRUE]),1),"% also reported religious ceremony, practice, or ritual; ",
       round(100*mean(daters$fasting[daters$drugs==TRUE]),1),"% also reported fasting, and ",
       round(100*mean(daters$nature[daters$drugs==TRUE]),1),"% also reported being outdoors in nature.")

# how much drug use could be formal religious practice? 
either  <- daters$prayer | daters$ceremony
paste0("Among those who reported fasting before a mystical-religious experience: ",
     round(100*mean(either[daters$drugs==TRUE]),2),"% reported either prayers or religious ceremony")

paste0("Among those who reported fasting before a mystical-religious experience: ",
       round(100*mean(daters$prayer[daters$fasting==TRUE]),1),"% also reported prayer, meditation, or contemplation; ",
       round(100*mean(daters$ceremony[daters$fasting==TRUE]),1),"% also reported religious ceremony, practice, or ritual; ",
       round(100*mean(daters$drugs[daters$fasting==TRUE]),1),"% also reported drug use, and ",
       round(100*mean(daters$nature[daters$fasting==TRUE]),1),"% also reported being outdoors in nature.")

# prepare to plot distribution of behaviors

daters_tall  <- melt(daters[,c(ids, triggers, "other")], id.vars=ids,measure.vars=c(triggers,"other"),
                     variable.name="behavior",value.name="value")
total_values <- length(unique(daters_tall$user_id))

to_plot <- ddply(daters_tall,.(behavior),summarise, 
      m =      100*round(sum(value,na.rm=TRUE)/total_values,3),
      lower = 100*round(wilson.ci(sum(value), n = total_values, conf.level = 0.95)[1],3),
      upper = 100*round(wilson.ci(sum(value), n = total_values, conf.level = 0.95)[2],3))      
to_plot$behavior <- factor(to_plot$behavior, levels=to_plot$behavior[order(to_plot$m)])

ggplot(to_plot,aes(x=behavior,y=m/100,ymax=upper/100,ymin=lower/100))+
  geom_bar(stat="identity",fill="grey",colour="black")+geom_errorbar(fill="grey",width=0.5)+
  coord_flip()+ylab("\nPercent of respondents endorsing\n")+theme_bw(base_size=16)+xlab("")+
  scale_y_continuous(labels=percent)

setwd(resultsdir)
basic_filename <- paste0("Figure_1_mystical_activities_")
output_filename <- paste0(basic_filename,as.Date(format(Sys.time(), "%m/%d/%Y"),"%m/%d/%Y"),".png")
ggsave(output_filename,width= 2500/300, height = 1200/300, units="in",bg = "transparent", dpi=330)
output_filename <- paste0(basic_filename,as.Date(format(Sys.time(), "%m/%d/%Y"),"%m/%d/%Y"),".pdf")
ggsave(output_filename,width= 2500/300, height = 1200/300, units="in",bg = "transparent", dpi=330)



# prepare to plot chi-sq matrix
temp <- as.matrix(daters[,names(daters) %in% triggers])
actual_chisq <- matrix(ncol=length(triggers),nrow = length(triggers), 
                       dimnames=list(triggers,triggers))
actual_p <- matrix(ncol=length(triggers),nrow = length(triggers), 
                  dimnames=list(triggers,triggers))
for(i in seq_along(triggers)){
  for(j in seq_along(triggers)){
    actual_chisq[i,j]  <- summary(table(as.numeric(temp[,colnames(temp)==triggers[i]]),
                                   as.numeric(temp[,colnames(temp)==triggers[j]])))[3]$statistic
    actual_p[i,j]  <- summary(table(as.numeric(temp[,colnames(temp)==triggers[i]]),
                                        as.numeric(temp[,colnames(temp)==triggers[j]])))[6]$p.value
  }
}
rm(temp)
actual_chisq
actual_p<0.05

upper_chi <- getUpperTriangle(actual_chisq)
tall_chi <- na.omit(melt(upper_chi))

# make the diagonal self-comparisons grey NAs on plot
tall_chi$value[tall_chi$value>1000]  <- NA

melted_p <- melt(actual_p)
melted_p$pvalue <- melted_p$value
melted_p$value <- NULL

tall_chi <- merge(tall_chi,melted_p,all.x=T)
tall_chi$sig <- tall_chi$pvalue<=0.05

# threshold so nonsig are not shown
tall_chi$value[!tall_chi$sig] <- 0


ggplot(tall_chi, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = muted("red"),
                       name="Chi\nSquare") +
  theme_minimal()+ # minimal theme
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 14, hjust = 1))+
  coord_fixed()+xlab("")+ylab("")

setwd(resultsdir)
basic_filename <- paste0("Figure_2_independence_of_activities_")
output_filename <- paste0(basic_filename,as.Date(format(Sys.time(), "%m/%d/%Y"),"%m/%d/%Y"),".png")
ggsave(output_filename,width= 1200/300, height = 1200/300, units="in",bg = "transparent", dpi=330)
output_filename <- paste0(basic_filename,as.Date(format(Sys.time(), "%m/%d/%Y"),"%m/%d/%Y"),".pdf")
ggsave(output_filename,width= 1200/300, height = 1200/300, units="in",bg = "transparent", dpi=330)

