library(stringr)

setwd('/Users/treny/Dropbox/git_election_data/demographics')

files <- list.files()[grep('demo_',list.files())]
df <- read.csv(files[1],stringsAsFactors = F)
df$county <- tolower(df$county)
year <- gsub('demo_|.csv','',files[1])
names(df)[3:length(names(df))] <- paste0(names(df)[3:length(names(df))],'_',year)

for(i in 2:length(files)){
  dat <- read.csv(files[i],stringsAsFactors = F)
  print(files[i])
  dat$county <- tolower(dat$county)
  year <- gsub('demo_|.csv','',files[i])
  names(dat)[3:length(names(dat))] <- paste0(names(dat)[3:length(names(dat))],'_',year)
  df <- merge(df,dat,by=c('state','county'))
}

write.csv(df,row.names = F,'demographics_1990-2016.csv')

