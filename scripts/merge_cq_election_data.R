setwd('/Users/treny/Downloads')
files <- list.files()[grep('export',list.files())]
year <- '1992-2000'
filename <- paste0('house_',year)

dat <- read.csv(files[1],skip = 2)
dat <- dat[grep('District',dat$Area),]
head(dat)
if(length(files) > 1){
  for(i in 2:length(files)){
  temp <- read.csv(files[i],skip = 2)
  temp <- temp[grep('District',temp$Area),]
  dat <- rbind(dat,temp)
  } 
  write.csv(dat,row.names=F,paste0(filename,'.csv'))
} else {
  write.csv(dat,row.names=F,paste0(filename,'.csv'))
}
file.remove(files)

# files <- list.files()[grep('house_',list.files())]
# dat <- read.csv(files[1],stringsAsFactors = F)
# 
# for(i in 2:length(files)){
#   temp <- read.csv(files[i],stringsAsFactors = F)
#   dat <- rbind(dat,temp)
# }
# head(dat)
# write.csv(dat,row.names=F,'/Users/treny/Dropbox/git_election_data/house/full_house_data_1992-2014.csv')
