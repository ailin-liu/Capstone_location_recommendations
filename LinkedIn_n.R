# proposed project
setwd("F:/Ailin/TDI/temp_datalab_records_linkedin_company")
rm(list=ls())
file="temp_datalab_records_linkedin_company.csv"

Linkd=read.table(file, header = TRUE,as.is=TRUE,sep = ",", quote = "\"",
              dec = ".", fill = TRUE, comment.char = "")
Linkd$date.added=as.Date(Linkd$date_added)
Linkd$date.updated=as.Date(Linkd$date_updated)
Linkd$as_of_date=as.Date(Linkd$as_of_date)
Linkd$dataset_id=as.integer(Linkd$dataset_id)
Linkd$followers_count=as.integer(Linkd$followers_count)
Linkd$employees_on_platform=as.integer(Linkd$employees_on_platform)


test=Linkd[which(Linkd$description !=""),] 
# 600918 records has description

test=Linkd[which(Linkd$entity_id !=""),]
test=Linkd[which(Linkd$cusip !=""),]
test=Linkd[which(Linkd$isin !=""),]
# no record in entity_id, cusip,isin

# replace "&amp;" with "&"
library(dplyr)
Linkd$industry=gsub('&amp;','&',Linkd$industry)
temp=group_by(Linkd,industry,company_name)
company_list=summarise(temp,Num_of_records=n())
industry_list=summarise(company_list,Num_of_Company=n())
# total 124 industries

record_thread=200


# find manufacturing industry
# category 1: infrastructure
# "Glass, Ceramics & Concrete"   7*
# "Building Materials"  47 *
# "Railroad Manufacture" 6
# "Transportation/Trucking/Railroad"  55
# "Civil Engineering"  8

Cat1=c("Glass, Ceramics & Concrete","Building Materials","Civil Engineering",
       "Railroad Manufacture","Transportation/Trucking/Railroad")
company_Cat1=company_list[which(company_list$industry %in% Cat1 & 
                                  company_list$Num_of_records>record_thread),]

# category 2: machinery
# "Machinery"  70*
# "Mechanical or Industrial Engineering"  68 ???

Cat2=c("Machinery","Mechanical or Industrial Engineering")
company_Cat2=company_list[which(company_list$industry %in% Cat2 & 
                                  company_list$Num_of_records>record_thread),]


# Category 3: automotive
# "Automotive"  93
Cat3=c("Automotive")
company_Cat3=company_list[which(company_list$industry %in% Cat3 & 
                                  company_list$Num_of_records>record_thread),]

# category 4: electrical related
# "Electrical/Electronic Manufacturing"  116*
# "Semiconductors" 108
# "consumer electronics"  23

Cat4=c("Electrical/Electronic Manufacturing","Semiconductors","Consumer Electronics")
company_Cat4=company_list[which(company_list$industry %in% Cat4 & 
                                  company_list$Num_of_records>record_thread),]

# category 5: medical & bio
# "Medical Devices"  127
# "biotechnology" 337
Cat5=c("Medical Devices","Biotechnology")
company_Cat5=company_list[which(company_list$industry %in% Cat5 & 
                                  company_list$Num_of_records>record_thread),]


# category 6: chemicals
# "Chemicals" 70

Cat6=c("Chemicals")
company_Cat6=company_list[which(company_list$industry %in% Cat6 & 
                                  company_list$Num_of_records>record_thread),]
                          
# category 7: others
# "Mining & Metals"  93
# "Packaging and Containers"  24*
# "Paper & Forest Products"  15
# "Plastics"  9
# "Printing"  17

Cat7=c("Mining & Metals","Packaging and Containers","Paper & Forest Products",
       "Plastics", "Printing")
company_Cat7=company_list[which(company_list$industry %in% Cat7 & 
                                  company_list$Num_of_records>record_thread),]

# find fast growing companies and industries
Cat=c(Cat1,Cat2,Cat3,Cat4,Cat5,Cat6,Cat7)

comp_Cat=company_list[which(company_list$industry %in% Cat & 
                                  company_list$Num_of_records>record_thread),]

record_Q=as.Date(c("2015-07-01","2015-10-01",
                   "2016-01-01","2016-04-01","2016-07-01","2016-10-01",
                   "2017-01-01","2017-04-01","2017-07-01","2017-10-01",
                   "2018-01-01","2018-04-01","2018-07-01","2018-10-01"))

for (i in 2:length(record_Q)){
  temp=Linkd[which(Linkd$company_name %in% comp_Cat$company_name &
                     Linkd$as_of_date >= record_Q[i-1] &
                     Linkd$as_of_date < record_Q[i]),]
  if (substr(record_Q[i-1],6,7)=="01") Quar="Q1"
  if (substr(record_Q[i-1],6,7)=="04") Quar="Q2"
  if (substr(record_Q[i-1],6,7)=="07") Quar="Q3"
  if (substr(record_Q[i-1],6,7)=="10") Quar="Q4"
  
  name1=paste0("I",substr(record_Q[i-1],1,4),Quar)

  tgroup=group_by(temp,company_name)
  follower_num=summarise(tgroup,name=mean(followers_count))
  colnames(follower_num)[colnames(follower_num)=="name"]=name1
  comp_Cat=merge(comp_Cat,follower_num,by=c("company_name"),all=T)
  
}
num_n=dim(comp_Cat)[2]+1


comp_Cat$R2015Q4=comp_Cat$I2015Q4/comp_Cat$I2015Q3
comp_Cat$R2016Q1=comp_Cat$I2016Q1/comp_Cat$I2015Q4
comp_Cat$R2016Q2=comp_Cat$I2016Q2/comp_Cat$I2016Q1
comp_Cat$R2016Q3=comp_Cat$I2016Q3/comp_Cat$I2016Q2
comp_Cat$R2016Q4=comp_Cat$I2016Q4/comp_Cat$I2016Q3
comp_Cat$R2017Q1=comp_Cat$I2017Q1/comp_Cat$I2016Q4
comp_Cat$R2017Q2=comp_Cat$I2017Q2/comp_Cat$I2017Q1
comp_Cat$R2017Q3=comp_Cat$I2017Q3/comp_Cat$I2017Q2
comp_Cat$R2017Q4=comp_Cat$I2017Q4/comp_Cat$I2017Q3
comp_Cat$R2018Q1=comp_Cat$I2018Q1/comp_Cat$I2017Q4
comp_Cat$R2018Q2=comp_Cat$I2018Q2/comp_Cat$I2018Q1
comp_Cat$R2018Q3=comp_Cat$I2018Q3/comp_Cat$I2018Q2

rate_n=dim(comp_Cat)[2]
comp_Cat$Avg_exp_rate=0

for (i in 1:dim(comp_Cat)[1]){
  comp_Cat$Avg_exp_rate[i]=rowMeans(comp_Cat[i,num_n:rate_n],na.rm=TRUE)-1
}

comp_Cat=comp_Cat[order(-comp_Cat$Avg_exp_rate),]

# only consider companies with data in each quarter
comp_Cat_big=na.omit(comp_Cat)

comp_Cat_big$category=""

comp_Cat_big[which(comp_Cat_big$industry %in% Cat1),]$category="Cat1"
comp_Cat_big[which(comp_Cat_big$industry %in% Cat2),]$category="Cat2"
comp_Cat_big[which(comp_Cat_big$industry %in% Cat3),]$category="Cat3"
comp_Cat_big[which(comp_Cat_big$industry %in% Cat4),]$category="Cat4"
comp_Cat_big[which(comp_Cat_big$industry %in% Cat5),]$category="Cat5"
comp_Cat_big[which(comp_Cat_big$industry %in% Cat6),]$category="Cat6"
comp_Cat_big[which(comp_Cat_big$industry %in% Cat7),]$category="Cat7"

# by category
par(mfrow=c(1,1))
comp_Cat_big$category=with(comp_Cat_big,reorder(category,Avg_exp_rate))
title(main="Increase rate of followers in LinkedIn", ylab="Increase rate")
boxplot(Avg_exp_rate ~ category,data=comp_Cat_big)




# by industry

comp_Cat_big$industry=as.factor(comp_Cat_big$industry)
comp_Cat_big$industry=with(comp_Cat_big,reorder(industry,Avg_exp_rate))

boxplot(Avg_exp_rate ~ industry,data=comp_Cat_big,las=2)
#barplot(comp_Cat_big$Avg_exp_rate)


# sample of plots

bluebird=Linkd[which(Linkd$company_name=="bluebird bio"),]
par(mfrow=c(1,1))
plot(bluebird$as_of_date,bluebird$followers_count/bluebird$followers_count[1],
     xlab="Date",ylab="Percentage",main="Percentage of followers increase in LinkedIn for Bluebird Bio")


Copart=Linkd[which(Linkd$company_name=="Copart"),]
par(mfrow=c(1,1))
plot(Copart$as_of_date,Copart$followers_count/Copart$followers_count[1],
     xlab="Date",ylab="Percentage",main="Percentage of followers increase in LinkedIn for Copart")


