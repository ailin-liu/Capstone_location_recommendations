# proposed project
setwd("F:/Ailin/project/temp_datalab_records_linkedin_company")
rm(list=ls())
file="temp_datalab_records_linkedin_company.csv"

# load dataset
Linkd=read.table(file, header = TRUE,as.is=TRUE,sep = ",", quote = "\"",
              dec = ".", fill = TRUE, comment.char = "")
names(Linkd)
Linkd$date.added=as.Date(Linkd$date_added)
Linkd$date.updated=as.Date(Linkd$date_updated)
Linkd$as_of_date=as.Date(Linkd$as_of_date)
Linkd$dataset_id=as.integer(Linkd$dataset_id)
Linkd$followers_count=as.integer(Linkd$followers_count)
Linkd$employees_on_platform=as.integer(Linkd$employees_on_platform)

# explore the dataset
test=Linkd[which(Linkd$description !=""),] 
# 600918 out of 2426196 records has description

test=Linkd[which(Linkd$entity_id !=""),]
test=Linkd[which(Linkd$cusip !=""),]
test=Linkd[which(Linkd$isin !=""),]
# no record in entity_id, cusip,isin


library(dplyr)
dataset_id_list=as.data.frame(unique(Linkd$dataset_id,incomparables=FALSE))
colnames(dataset_id_list)[1]="dataset_id"
# 4610 distinct dataset_id

company_list=as.data.frame(unique(Linkd$company_name,incomparables=FALSE))
colnames(company_list)[1]="company_name"
# 5028 distinct company_name

industry_list=as.data.frame(unique(Linkd$industry,incomparables=FALSE))
colnames(industry_list)[1]="industry"
# 141 industry,some industry names use "&amp;" instead of "&"

# replace "&amp;" with "&" in industry and company_name
Linkd$industry=gsub('&amp;','&',Linkd$industry)
Linkd$company_name=gsub('&amp;','&',Linkd$company_name)

company_list=as.data.frame(unique(Linkd$company_name,incomparables=FALSE))
colnames(company_list)[1]="company_name"
industry_list=as.data.frame(unique(Linkd$industry,incomparables=FALSE))
colnames(industry_list)[1]="industry"
# updated company #:4950, updated industry #: 124 


library(sqldf)

data_id_list=sqldf("Select distinct dataset_id, company_name, industry
                   from Linkd
                   Join dataset_id_list using(dataset_id)")
# 5825-4610 = 1215 duplicate records due to imcomplete information
data_id_list[order(data_id_list$dataset_id),]



dup_comp=data_id_list[duplicated(data_id_list$dataset_id),]
colnames(dup_comp)[3]="old_industry"
colnames(dup_comp)[2]="old_compname"
unq_comp=data_id_list[!duplicated(data_id_list$dataset_id),]

dup_comp1$company_name=""
dup_comp1$industry=""
dup_comp1 = sqldf("Select dataset_id,old_compname,old_industry,company_name,industry
                 from dup_comp
                 join unq_comp using (dataset_id)")
# 1215 companies has either duplicate names or industry categories

dup_comp_name=sqldf("select *
                from dup_comp1
                where old_compname != company_name")
# 380 companies with two different names

dup_ind_name=sqldf("select *
                from dup_comp1
                    where old_industry != industry")
# 894 companies with two different industry category

# do some easy fix here
# for companies without industry, assign to the same as the one with industry category
# duplicate industry, pick the one shown up in the dataset first
# for duplicate names, assign to the one shown up first



for (i in 1:length(dup_comp1[,1])){
  id=dup_comp1[i,1]
  if(dup_comp1$industry[i]==""){
    ind_name=dup_comp1$old_industry[i]
  }else{
    ind_name=dup_comp1$industry[i]
  }
  comp_name=dup_comp1$company_name[i]
  unq_comp[which(unq_comp$dataset_id==id),]$industry=ind_name
}
colnames(unq_comp)[2]="CompName"
colnames(unq_comp)[3]="IndName"

# add updated company name and industry catgory into dataset
Linkd=sqldf("Select *
                   from Linkd
                   Join unq_comp using(dataset_id)")

# sample plots of followers_count and employee_on_platform
Ford=Linkd[which(Linkd$company_name=="Ford Motor Company"),]
par(mfrow=c(2,1))
plot(Ford$as_of_date,Ford$followers_count)
plot(Ford$as_of_date,Ford$employees_on_platform)  

GE=Linkd[which(Linkd$company_name=="GE"),]
par(mfrow=c(2,1))
plot(GE$as_of_date,GE$followers_count)
plot(GE$as_of_date,GE$employees_on_platform)  

# sudden changes in employee_on_platform for some companies
# choose followers_count as features
library(dplyr)
temp=group_by(Linkd,dataset_id)
Num_of_records=summarise(temp,Num_of_records=n())
data_id_list=sqldf("Select *
                   from unq_comp
                   Join Num_of_records using(dataset_id)")
par(mfrow=c(1,1))
plot(data_id_list$Num_of_records)

# choose record_thread=400
record_thread=400

# define 7 category of manufacturing industry

# find manufacturing industry
# category 1: infrastructure
# "Glass, Ceramics & Concrete"   7*
# "Building Materials"  47 *
# "Railroad Manufacture" 6
# "Transportation/Trucking/Railroad"  55
# "Civil Engineering"  8
Cat1=c("Glass, Ceramics & Concrete","Building Materials","Civil Engineering",
       "Railroad Manufacture","Transportation/Trucking/Railroad")

# category 2: machinery
# "Machinery"  70*
# "Mechanical or Industrial Engineering"  68 ???
Cat2=c("Machinery","Mechanical or Industrial Engineering")

# Category 3: automotive
# "Automotive"  93
Cat3=c("Automotive")

# category 4: electrical related
# "Electrical/Electronic Manufacturing"  116*
# "Semiconductors" 108
# "consumer electronics"  23
Cat4=c("Electrical/Electronic Manufacturing","Semiconductors","Consumer Electronics")

# category 5: medical & bio
# "Medical Devices"  127
# "biotechnology" 337
Cat5=c("Medical Devices","Biotechnology")


# category 6: chemicals
# "Chemicals" 70
Cat6=c("Chemicals")

# category 7: others
# "Mining & Metals"  93
# "Packaging and Containers"  24*
# "Paper & Forest Products"  15
# "Plastics"  9
# "Printing"  17
Cat7=c("Mining & Metals","Packaging and Containers","Paper & Forest Products",
       "Plastics", "Printing")

# find fast growing companies and industries
# calculate followers in each quarter
Cat=c(Cat1,Cat2,Cat3,Cat4,Cat5,Cat6,Cat7)

MF_comp=data_id_list[which(data_id_list$IndName %in% Cat & 
                             data_id_list$Num_of_records>record_thread),]

record_Q=as.Date(c("2015-07-01","2015-10-01",
                   "2016-01-01","2016-04-01","2016-07-01","2016-10-01",
                   "2017-01-01","2017-04-01","2017-07-01","2017-10-01",
                   "2018-01-01","2018-04-01","2018-07-01","2018-10-01"))

for (i in 2:length(record_Q)){
  temp=Linkd[which(Linkd$CompName %in% MF_comp$CompName &
                     Linkd$as_of_date >= record_Q[i-1] &
                     Linkd$as_of_date < record_Q[i]),]
  tgroup=group_by(temp,CompName)
  if (substr(record_Q[i-1],6,7)=="01") Quar="Q1"
  if (substr(record_Q[i-1],6,7)=="04") Quar="Q2"
  if (substr(record_Q[i-1],6,7)=="07") Quar="Q3"
  if (substr(record_Q[i-1],6,7)=="10") Quar="Q4"
  
  name1=paste0("I",substr(record_Q[i-1],1,4),Quar)

  tgroup=group_by(temp,dataset_id)
  follower_num=summarise(tgroup,name=mean(followers_count))
  colnames(follower_num)[colnames(follower_num)=="name"]=name1
  MF_comp=merge(MF_comp,follower_num,by=c("dataset_id"),all=T)
  
}
num_n=dim(MF_comp)[2]+1

# calculate followers increase rate in each quater
MF_comp$R2015Q4=MF_comp$I2015Q4/MF_comp$I2015Q3
MF_comp$R2016Q1=MF_comp$I2016Q1/MF_comp$I2015Q4
MF_comp$R2016Q2=MF_comp$I2016Q2/MF_comp$I2016Q1
MF_comp$R2016Q3=MF_comp$I2016Q3/MF_comp$I2016Q2
MF_comp$R2016Q4=MF_comp$I2016Q4/MF_comp$I2016Q3
MF_comp$R2017Q1=MF_comp$I2017Q1/MF_comp$I2016Q4
MF_comp$R2017Q2=MF_comp$I2017Q2/MF_comp$I2017Q1
MF_comp$R2017Q3=MF_comp$I2017Q3/MF_comp$I2017Q2
MF_comp$R2017Q4=MF_comp$I2017Q4/MF_comp$I2017Q3
MF_comp$R2018Q1=MF_comp$I2018Q1/MF_comp$I2017Q4
MF_comp$R2018Q2=MF_comp$I2018Q2/MF_comp$I2018Q1
MF_comp$R2018Q3=MF_comp$I2018Q3/MF_comp$I2018Q2

# calculate average expansion rate on followers
rate_n=dim(MF_comp)[2]
MF_comp$Avg_exp_rate=0
for (i in 1:dim(MF_comp)[1]){
  MF_comp$Avg_exp_rate[i]=rowMeans(MF_comp[i,num_n:rate_n],na.rm=TRUE)-1
}

MF_comp=MF_comp[order(-MF_comp$Avg_exp_rate),]

# only consider companies with data in each quarter
MF_comp_big=na.omit(MF_comp)
MF_comp_big$category=""

MF_comp_big[which(MF_comp_big$IndName %in% Cat1),]$category="Cat1"
MF_comp_big[which(MF_comp_big$IndName %in% Cat2),]$category="Cat2"
MF_comp_big[which(MF_comp_big$IndName %in% Cat3),]$category="Cat3"
MF_comp_big[which(MF_comp_big$IndName %in% Cat4),]$category="Cat4"
MF_comp_big[which(MF_comp_big$IndName %in% Cat5),]$category="Cat5"
MF_comp_big[which(MF_comp_big$IndName %in% Cat6),]$category="Cat6"
MF_comp_big[which(MF_comp_big$IndName %in% Cat7),]$category="Cat7"

# by category
par(mfrow=c(1,1))
MF_comp_big$category=with(MF_comp_big,reorder(category,Avg_exp_rate))
title(main="Increase rate of followers in LinkedIn", ylab="Increase rate")
boxplot(Avg_exp_rate ~ category,data=MF_comp_big)


# by IndName

MF_comp_big$IndName=as.factor(MF_comp_big$IndName)
MF_comp_big$IndName=with(MF_comp_big,reorder(IndName,Avg_exp_rate))
boxplot(Avg_exp_rate ~ IndName,data=MF_comp_big,las=2)


Corning=Linkd[which(Linkd$CompName=="Corning Incorporated"),]
par(mfrow=c(1,1))
plot(Corning$as_of_date,Corning$followers_count/Corning$followers_count[1],
     xlab="Date",ylab="Percentage",main="Percentage of followers increase in LinkedIn for Corning")

Carvana=Linkd[which(Linkd$CompName=="Carvana"),]
plot(Carvana$as_of_date,Carvana$followers_count/Carvana$followers_count[1],
     xlab="Date",ylab="Percentage",main="Percentage of followers increase in LinkedIn for Carvana")

Tesla=Linkd[which(Linkd$CompName=="Tesla Motors"),]
plot(Tesla$as_of_date,Tesla$followers_count/Tesla$followers_count[1],
     xlab="Date",ylab="Percentage",main="Percentage of followers increase in LinkedIn for Tesla")

