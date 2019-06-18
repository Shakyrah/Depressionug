library(data.table)
library(magrittr)
library(ggplot2)
library(readxl)
library(stringr)


### Reading all files in a list
path <- "/Users/kyra/Documents/Dk/sm/sm/data/raw_data/"
file_names <- list.files(path) 
file_names <- file_names[file_names != "KEY.xlsx"]
file_names %<>% .[!str_detect(.,"~")]
post_assess_names <- file_names[str_detect(tolower(file_names),"post asses")]
functionality_names<- file_names[str_detect(tolower(file_names),"functionality")]
phq_names <- file_names[str_detect(tolower(file_names),"phq")]

sheet_list <- sapply(file_names,function(x) excel_sheets(paste0(path,x)) %>% as.data.table,simplify = F,USE.NAMES = T)
skip_v <- rep(0,length(file_names))
ds_list <- sapply(file_names,function(x) read_excel(paste0(path,x)) %>% as.data.table,simplify = F,USE.NAMES = T)

# Different format for first file
file_temp <- "All cycles 2016 to 2017 PHQ9 DATA.xlsx"
ds_list[[1]] <- read_excel(paste0(path,file_temp),skip = 1,col_types = "text") %>% as.data.table

# what columns does each file has?
sapply(post_assess_names,function(x) ds_list[[x]] %>% names,simplify = F,USE.NAMES = T)
sapply(post_assess_names,function(x) sheet_list[[x]] ,simplify = F,USE.NAMES = T)


# After the first presentation, it was agreed that only dataset with 2016-2017 observations was going to be considered for the analysis

# Reading and preprocessing
temp <- read_excel(paste0(path,file_temp),skip = 1,col_types = "text") %>% as.data.table
temp[temp=="891"] <- NA
temp[,year_of_birth:=as.Date(as.numeric(year_of_birth), origin = "1899-12-30")]
temp[,year:=year(year_of_birth)]
temp[,decade_born:=round(as.numeric(substr(year,3,4))-4.999,-1)]
temp[year<1920 | year>2010,decade_born:=NA]
totals <- names(temp) %>% .[str_detect(.,"Total")]
for (j in totals) set(temp,j=j,value = as.numeric(temp[[j]]))
temp[,dif:=Total__2-Total__4]
temp[,year_cycle:=paste(`Year of treatemnt`,`Cycle of treatment`,sep = "_")]
temp$year_cycle
nrow(temp)
names(temp)


### Check final depression levels
temp2 <- temp[!is.na(Total__4)]
temp2[,dep_level:="moderate or more"]
temp2[Total__4<9,dep_level:="mild"]
temp2[Total__4<5,dep_level:="minimal"]
temp2[,.N,dep_level][,perc:=N/sum(N) %>% round(1)] %>% print


# Variables for model
vars <- names(temp)[c(2,102,7,8,9,10,11,100,13,14,15,16,17,18)]
vars_hist <- names(temp)[c(2,7,8,13,14,15,16,17,18,102)]


# Generate series of how depression scores evolved
melted <- temp[,c("patient_id",totals),with=F] %>% melt("patient_id",variable.name="time",value.name="score")
melted$time %<>% str_replace("Total__|Total","")
melted[time=="",time:="0"]
melted[,time_str:=time]
melted[,time:=as.integer(time)]
melted <- merge(temp[,c("patient_id","dif",vars),with=F],melted,"patient_id")
ggplot(melted[patient_id %in% unique(patient_id)[1:1000]],aes(time,score,group=patient_id))+geom_line(size=.05)
ggplot(melted,aes(time,score,group=patient_id))+geom_line(size=.05)


# histogram of difference, and final depression scores
hist(temp$dif)
quantile(temp$dif,c(0.05,.1,.3,.5,.7,.9,.95),na.rm=T)
quantile(temp$Total__4,c(0.05,.1,.3,.5,.7,.9,.95),na.rm=T)
hist(temp[,Total__4])
hist(temp[Total__4>=9,Total__4])

# only 3% of the cases end with depression score of 10 or more (Note that this ignores cases with missing values)
sum(temp$Total__4>=10,na.rm=T)/sum(!is.na(temp$Total__4))

# Comparing the marital status composition of two groups
# group 1: depressed women (socre of 10 or above)
# group 2: women with final score below or equal to 3
temp_dep <- temp[Total__4>=10]
depressed <- temp_dep[,.N,marital_status] 
depressed[,depressed:=round(100*N/sum(N),1)]
below <- temp[Total__4<=3.4,.N,marital_status]
below[,below:=round(100*N/sum(N),1)]
res <- merge(below,depressed,"marital_status")
res[,.(marital_status,below,depressed)] %>% View

ggplot(melted[dif<=4],aes(time,score,group=patient_id))+geom_line(size=.05)

# boxplots in time by family position
ggplot(melted[dif<=4],aes(time_str,score))+geom_boxplot()+facet_wrap(~family_position)

# Linear regression model shows 
formula <- paste0("dif~",paste(vars,collapse = "+"),"+Total__2") %>% as.formula
model <- lm(formula,data = temp);summary(model)
summary(model)$coefficients %>% View
model %>% summary




#### Basic Statistics

# 1/3 of the data has no final score
temp[is.na(Total__4),.N]/temp[,.N]
# mostly women
temp[,.N,.(gender)]


# For variables of interest the code computes it's median first score and it's score improvement and generates a plot
# this code also generates other inputs that can be found in the eda shiny app
for(var in vars){
  count <- temp[,.N,var];count
  temp_red <- temp[temp[[var]] %in% count[N>100][[var]]]
  median_temp <- merge(count,temp_red[,lapply(.SD,median,na.rm=T),var,.SDcols=c(totals,"dif")],var)[N>100][order(dif,decreasing = T)]
  write.csv(median_temp,paste0("outputs/depression_improvement/tables/",var,".csv"),row.names = F)
  median_temp[,vari:=median_temp[[var]]]
  png(filename = paste0("outputs/depression_improvement/plots/starting_dif/",var,".png"),width = 10,height = 10,units = "in",res = 300)
  print({
    ggplot(median_temp[order(Total__2,decreasing = T)],aes(Total__2,dif))+geom_point()+geom_text(aes(label=substr(vari,1,3)),position="jitter")+labs(x="Score First Session",y="Score Improvement")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20))#+ggtitle(label = var)
  })
  dev.off()
  
  temp_red2 <- transform(temp_red,vari=factor(temp_red[[var]],levels=median_temp[[var]]))
  png(filename = paste0("outputs/depression_improvement/plots/dif_density",var,".png"),width = 10,height = 10,units = "in",res = 300)
  print({
    ggplot(temp_red2, aes(dif)) + 
      geom_density(alpha = 0.2)+
      geom_vline(data=transform(temp_red[,.(dif=median(dif,na.rm=T)),var],vari=factor(temp_red[,.(dif=median(dif,na.rm=T)),var][[var]],levels=median_temp[[var]])),aes(xintercept=dif))+
      facet_wrap(~vari)+
      ggtitle(var)
  })
  dev.off()
  
  melted2 <- transform(melted,vari=factor(count[[var]][!is.na(count[[var]])],levels=median_temp[[var]]))
  melted2 <- melted2[melted2[[var]] %in% count[N>100][[var]]]
  melted2[,vari:=melted2[[var]]]
  
  png(filename = paste0("outputs/depression_improvement/plots/time_series/",var,".png"),width = 10,height = 10,units = "in",res = 300)
  print({
    ggplot(melted2,aes(time,score,group=patient_id))+geom_line(size=.05)+facet_wrap(~vari)+ggtitle(label = var)
  })
  dev.off()
  png(filename = paste0("outputs/depression_improvement/plots/boxplot_series/",var,".png"),width = 10,height = 10,units = "in",res = 300)
  print({
    ggplot(melted2,aes(time_str,score))+geom_boxplot()+facet_wrap(~vari)+ggtitle(label = var)
  })
  dev.off()
}


# weird cases SK
red <- temp[mhf_name %in% c("LA","SK")]
red[,c("mhf_name","dif",totals),with=F]

ggplot(red, aes(dif, fill = mhf_name)) + geom_density(alpha = 0.2)
ggplot(red, aes(dif, fill = mhf_name)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',binwidth = 2)
