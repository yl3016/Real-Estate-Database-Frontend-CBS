Real-Estate-Database-Frontend-CBS
=================================

library(foreign)
library(plyr)
library(stringdist)
library(doMC)
library(stringr)

registerDoMC(2)

#Set Data of County
COUNTY_NAME = 'Montgomery'
DATA_FILE = paste(COUNTY_NAME,'.dta',sep='')
ADDED_ROWS_FILE = paste('Added_Rows_',COUNTY_NAME,'.Rdata',sep='')
SCORE_FILE = paste('Score_Output_',COUNTY_NAME,'.Rdata',sep='')
ADDRESS_FILE = paste('Address_',COUNTY_NAME,'.Rdata',sep='')
print(DATA_FILE)
setwd('/Desktop/School/Agile Design/')
x = read.dta(DATA_FILE)
#Take out blank and 0 property ids
data =x[x$sr_property_id!=0 & x$sr_property_id!='',]
#Sample 300,000 properties or less if there arent at least 300,000
property_sample = sample(unique(data$sr_property_id),min(length(unique(data$sr_property_id)),300000),replace=FALSE)
data = data[data$sr_property_id %in% property_sample,]
#Set dates to date type, and min and max dates
data$sr_date_transfer = with(data, as.Date(paste(substr(sr_date_transfer,1,4), substr(sr_date_transfer,5,6), 
                                                 substr(sr_date_transfer,7,8), sep='-')))
data$min_date = rep(min(data$sr_date_transfer),dim(data)[1])
data$max_date = rep(max(data$sr_date_transfer),dim(data)[1])
#Get rid of duplicate rows
ID = with(data,paste(sr_property_id,sr_buyer,sr_date_transfer,sr_tran_type))
data = data[!duplicated(ID),]
#Filter after year 2000
data = data[data$sr_date_transfer > as.Date("2000-01-01"),]


#I.  Scoring FUNCTIONS
#I.a. Find max(counter-party interacted) for each unique party
get_max_pair<-function(df){
    max = max(df$freq)
    freq = df[which(df$freq==max),'freq']
    party2 = df[which(df$freq==max),'party2']
    c(freq = as.numeric(freq[1]), party2=as.character(party2[1]))
}
# Score sum of transactions with different parties
Score_Sum_LLC = function(vec){
    score = ifelse(vec==0,NA,
                   ifelse(vec==1,0,
                          ifelse(vec==2,log(2)/log(7),
                                 ifelse(vec >= 3 & vec <= 4,log(3)/log(7),
                                        ifelse(vec >= 5 & vec <= 8,log(4)/log(7),1)))))
    return(score)
}
# Score max party transactions
Score_Max_LLC = function(vec){
    score = ifelse(vec==0,NA,
                   ifelse(vec==1,0,
                          ifelse(vec==2,log(2)/log(8),
                                 ifelse(vec >= 3 & vec <= 4,log(3)/log(8),
                                        ifelse(vec >= 5 & vec <= 6,log(6)/log(8),1)))))
    return(score)
}
# Weight the LLC scores
Score_LLC = function(vec1,vec2){
    score = ifelse(vec1 <0.5 & vec2 <0.5,0.45 * (vec1+vec2),
                   ifelse(vec1 >= 0.5 & vec2 < 0.5, 0.3 * vec1 + 0.7 * vec2,
                          ifelse(vec1 < 0.5 & vec2 >= 0.5, 0.3 * vec1 + 0.7 * vec2,
                                 ifelse(vec1 >= 0.5 & vec2 >= 0.5 & (0.55 * (vec1+vec2)) < 1, 0.55 * (vec1+vec2), 1))))
    return(score)
}

#I.b. Calculate holding time and max_house 
# Too calculate holding time and max houses at once, we need to first add missing rows
buy_sell_cycle <- function(df){
    print(df$sr_property_id[1])
    df = df[order(df$sr_date_transfer, decreasing=FALSE),]
    dfR = df[df$sr_tran_type %in% c('R','S'),]
    min_date = df$min_date[1]
    max_date = df$max_date[1]
    property = df[1,'sr_property_id']
    state_county_vec = c(df[1,3],as.character(df[1,4]),
                         as.character(df[1,5]), df[1,6],
                         df[1,7], as.character(df[1,8]))
    names = unique(c(as.character(df$sr_buyer),as.character(df$sr_seller)))
    names = names[names!='']
    returned = vector()
    max_seller = ''
    seller_table = 0
    prop = 0
    if(dim(dfR)[1]>0){
        seller_table = table(dfR$sr_seller)
        max_seller = names(seller_table)[which(seller_table==max(seller_table))]
        prop = prop.table(seller_table)
    }
    
    for(name in names){
        
        sell.ind = which(dfR$sr_seller==name)
        buy.ind = which(dfR$sr_buyer==name)
        sell.full= which(df$sr_seller==name)
        buy.full = which(df$sr_buyer==name)
        
        #IF THERE IS NO SELL OR NO BUY ACTION OF TYPE 'S' or 'R'
        if(xor(length(sell.ind)==0,length(buy.ind)==0)){
            
            #IF NO BUY BEFORE FIRST SELL AND SELL EXITS
            if(length(sell.ind)>0 & sum(buy.ind<min(sell.ind))==0){
                #IF NO L or R TRAN WITH NAME IN BUYER FIELD  OR  FIRST BUY IS AFTER FIRST SELL  
                if(length(buy.full)==0 | min(buy.full) > min(sell.full)){
                    
                    buydate =  ifelse(min(sell.full)!=1,df[min(sell.full)-1,'sr_date_transfer'],min_date)
                    seller = ''
                    returned = rbind(returned, c(1,property,state_county_vec,rep('',27),
                                                 name,seller, rep('',4), buydate,rep('',3),
                                                 'R','',1,rep('',39),min_date,max_date))
                    #IF THERE IS A BUY ACTION WITH 'L' TRAN_TYPE BEFORE FIRST SELL   
                }else{
                    buydate = df[min(buy.full),'sr_date_transfer']
                    seller = ''
                    returned = rbind(returned, c(1,property,state_county_vec,rep('',27),
                                                 name,seller, rep('',4), buydate,rep('',3),
                                                 'R','',1,rep('',39),min_date,max_date))  
                }   
            }# END OF SELLER TEST
            #IF BUY EXISTS AND NO SELL AFTER BUY
            if(length(buy.ind)>0 & sum(sell.ind>max(buy.ind))==0){
                print('########################################################')
                
                #HAPPENS ALOT WITH APPARTMENT SO CONTROL FOR THAT
                if(max_seller!= '' & max(seller_table)>4 & max(prop)>.6 & df[min(buy.ind),'sr_seller']%in%max_seller){
                    
                    selldate = max_date ####MAX DATE POSSIBLE
                    buyer = ''
                    returned = rbind(returned, c(1,property,state_county_vec,rep('',27),
                                                 buyer,name, rep('',4), selldate,rep('',3),
                                                 'R','',1,rep('',39),min_date,max_date))
                    #IF NO SELL ACTIONS OR LATEST BUY IS AFTER LATEST SALE   
                }else if(length(sell.full)==0 | max(buy.full)>max(sell.full)){
                    selldate = ifelse(max(buy.full)!=dim(df)[1],df[max(buy.full)+1,'sr_date_transfer'],max_date)
                    buyer = ''
                    returned = rbind(returned, c(1,property,state_county_vec,rep('',27),
                                                 buyer,name, rep('',4), selldate,rep('',3),
                                                 'R','',1,rep('',39),min_date,max_date))
                    #IF THERE IS A SELL ACTION AFTER BUY OF TYPE 'L'
                }else{
                    selldate = df[max(sell.full),'sr_date_transfer']
                    buyer = ''
                    returned = rbind(returned, c(1,property,state_county_vec,rep('',27),
                                                 buyer,name, rep('',4), selldate,rep('',3),
                                                 'R','',1,rep('',39),min_date,max_date))
                }
            }
            #ELSE IF ONLY BUY ACTIONS OF TYPE 'L': ADD A BUY AND SELL AT MIN AND MAX DATES
        }else if(length(buy.full)>0 & length(buy.ind)==0 & length(sell.full)==0){
            buydate = ifelse(min(buy.full)-1>0, df[min(buy.full)-1,"sr_date_transfer"], min_date)
            seller = ''
            returned = rbind(returned, c(1,property,state_county_vec,rep('',27),
                                         name,seller, rep('',4), buydate,rep('',3),
                                         'R','',1,rep('',39),min_date,max_date))
            selldate = ifelse(max(buy.full)!=dim(df)[1],df[max(buy.full)+1,"sr_date_transfer"],max_date)
            buyer = ''
            returned = rbind(returned, c(1,property,state_county_vec,rep('',27),
                                         buyer,name, rep('',4), selldate,rep('',3),
                                         'R','',1,rep('',39),min_date,max_date))
            
        }else if(length(sell.full)>0 & length(sell.ind)==0 & length(buy.full)==0){
            buydate = ifelse(min(sell.full)-1>0, df[min(sell.full)-1,"sr_date_transfer"], min_date)
            seller = ''
            returned = rbind(returned, c(1,property,state_county_vec,rep('',27),
                                         name,seller, rep('',4), buydate,rep('',3),
                                         'R','',1,rep('',39),min_date,max_date))
            selldate = df[min(sell.full),"sr_date_transfer"]
            buyer = ''
            returned = rbind(returned, c(1,property,state_county_vec,rep('',27),
                                         buyer,name, rep('',4), selldate,rep('',3),
                                         'R','',1,rep('',39),min_date,max_date))
            #IF ALOT OF SELLS AFTER A BUY ()
        }else if(max_seller!='' & max(seller_table)>4 & max(prop)>.6 & name%in%max_seller & length(buy.ind)<3 | 'S'%in%df[df$sr_seller==name,'sr_tran_type']){
            
            buydate = df[min(buy.ind), 'sr_date_transfer'] ####MAX DATE POSSIBLE
            seller = ''
            for(i in length(sell.ind)-length(buy.ind)){
                returned = rbind(returned, c(1,property,state_county_vec,rep('',27),
                                             name,seller, rep('',4), buydate ,rep('',3),
                                             'R','',1,rep('',39),min_date,max_date))
            }
        }else{}
        
    }
    returned = as.data.frame(returned)
    #if(dim(returned)[1]>0){
    #names(returned)<-names(df)
    #returned$sr_date_transfer = as.Date(as.numeric.f(returned$sr_date_transfer), origin='1970-01-01')
    #}
    return(returned)
}
# Max number of houses owned at once and duration function
overlap_date<-function(df){
    if(substr(df$sr_buyer[1],2,2) == 'A'){
        print(df$sr_buyer[1])
    }
    start = as.numeric(df$sr_date_transfer - rep(min(df$sr_date_transfer)-1, dim(df)[1]))
    end = as.numeric(df$sell_date - rep(min(df$sr_date_transfer)-1, dim(df)[1]))
    maxsell = as.numeric(max(df$sell_date) - min(df$sr_date_transfer)+1)
    days = rep(0, maxsell+1)
    for(i in 1:length(start)){
        if(start[i] == 1){
            days = days + c(rep(1,end[i]), rep(0,maxsell+1-end[i]))
        }else{
            days = days + c(rep(0,start[i]-1), rep(1, end[i]-start[i]+1), rep(0, maxsell+1-end[i]))
        }
    }
    return(c(Max = max(days), AvgLength = mean(end-start)/365))
}
Score_MaxHouse = function(vec){
    score = ifelse(vec==0,NA,
                   ifelse(vec==1,0,
                          ifelse(vec==2,log(2)/log(7),
                                 ifelse(vec >= 3 & vec <= 5,log(4)/log(7),
                                        ifelse(vec >= 6 & vec <= 8,log(6)/log(7),1)))))
    return(score)
}

Score_HoldTime<-function(vec){
    score = ifelse(vec<2,0,
                   ifelse(vec<5, .5, 1))
    return(score)
}

#I.c. STRING SIMILARITY
stringSim = function(str1, str2){
    return(1 - (stringdist(str1, str2, method = 'lv')/max(nchar(str1), nchar(str2))))
}
Score_Address<-function(df){  ####RUN ON RESALES?
    if(substr(df$sr_buyer[1],2,2) == 'A'){
        print(df$sr_buyer[1])
    }
    return(c(Addr = mean(df$score)))
}


#FROM FACTOR TO NUMERIC/INTEGER
as.numeric.f<- function(x){as.numeric(levels(x))[x]}
as.integer.f<- function(x){as.integer(levels(x))[x]}
#####################################################################
#II. Preparing data to score
# II.a. To compute LLC Scores
# delete rows with NULL or ' ' values in each column
Related_Transaction = data[which(data$sr_buyer != ' '& data$sr_buyer != '' & data$sr_seller != ' '& data$sr_seller != ''),]
# find pairs with only one party from institutions: LLC|TURST|TRUSTEE|HOLDINGS
# can add more from company list#
Related_Transaction = Related_Transaction[xor(grepl('LLC',Related_Transaction$sr_buyer) |grepl('TRUSTEE',Related_Transaction$sr_buyer) | grepl('HOLDINGS',Related_Transaction$sr_buyer) | grepl('TRUST',Related_Transaction$sr_buyer) , grepl('LLC',Related_Transaction$sr_seller) | grepl('TRUSTEE',Related_Transaction$sr_buyer) |grepl('HOLDINGS',Related_Transaction$sr_seller) | grepl('TRUST',Related_Transaction$sr_seller)),]
buyer_seller = Related_Transaction[,c('sr_buyer','sr_seller')]
rownames(buyer_seller) = NULL
# sort by rows
buyer_seller = t(apply(buyer_seller, 1, sort))
# combine buyer&seller names
buyer_seller = paste(buyer_seller[,1], buyer_seller[,2], sep="_")
# count transaction frequency
buyer_seller_list = table(buyer_seller)
buyer_seller_list = data.frame(names = row.names(buyer_seller_list),freq = buyer_seller_list[order(buyer_seller_list, decreasing=T)])
rownames(buyer_seller_list) = NULL

#Prepare buyer seller lists for aggregating 
split = str_split_fixed(buyer_seller_list[,1],'_',2)
parties = c(split[,1], split[,2])
parties2 = c(split[,2], split[,1])
b_s_list = data.frame(parties = parties,freq = rep(buyer_seller_list[,2],2), party2 =parties2)
#Aggregate and get max transactions and matching 2nd party
inter_Max = ddply(b_s_list,.(parties), get_max_pair, .parallel=TRUE)
inter_Max = inter_Max[order(inter_Max$parties),]
# Sum frequency for each unique party
b_s_list[] = lapply(b_s_list, function(x) type.convert(as.character(x)))
b_s_score_object = aggregate(b_s_list$freq ~ b_s_list$parties, b_s_list, 'sum')
colnames(b_s_score_object) = c('party','sum_freq')
b_s_score_object = b_s_score_object[order(b_s_score_object$party),]
# Combine Tables
b_s = cbind(b_s_score_object, inter_Max[,2:3])
colnames(b_s) = c('party','sum_freq','max_freq','party2')
#Calculate Scores
Sumscore = as.vector(Score_Sum_LLC(b_s[,2]))
Maxscore = as.vector(Score_Max_LLC(b_s[,3]))
LLCscore = Score_LLC(Sumscore, Maxscore)
b_s_score_result = data.frame(sr_buyer = as.character(b_s$party), LLCscore = LLCscore, Related_Party = b_s$party2)

#####################################################################
#II.b. To comput Max House, Holding date Scores 
#ADD ROWS
a = unique(data$sr_property_id)
num = round(length(a)/4)

Added_Rows = vector()
#Put in for loop so we can see progress
for(i in 1:4){
    if(i<4){
        Added_Rows = rbind(Added_Rows, ddply(data[data$sr_property_id %in% a[((i-1)*num+1):(i*num)],],
                                              .(sr_property_id),buy_sell_cycle,.parallel = TRUE))
        print(i)
    }
    else{
        Added_Rows = rbind(Added_Rows, ddply(data[data$sr_property_id%in%a[((i-1)*num+1):length(a)],],
                                                  .(sr_property_id),buy_sell_cycle,.parallel = TRUE))
    }
}
save(Added_Rows, file=ADDED_ROWS_FILE)

#Clean up added rows
Added_Rows = Added_Rows[,-91]
Added_Rows = Added_Rows[,-1]
names(Added_Rows) = names(data)
Added_Rows$sr_date_transfer = as.Date(as.numeric.f(Added_Rows$sr_date_transfer), origin='1970-01-01')
Added_Rows$min_date = as.Date(as.numeric.f(Added_Rows$min_date), origin='1970-01-01')
Added_Rows$max_date = as.Date(as.numeric.f(Added_Rows$max_date), origin='1970-01-01')
for(i in 1:89){
    if(class(data[,i])=='integer'|class(data[,i])=='numeric'){
        Added_Rows[,i] = as.numeric.f(Added_Rows[,i])
    }else if(class(data[,i])=='character'){
        Added_Rows[,i] = as.character(Added_Rows[,i])
    }
}

#Match (property, buyer) to (property, seller)
DATA = rbind(Added_Rows, data)
Buy = DATA[DATA$sr_tran_type %in% c('R','S'),]
Buy$addr_empty_both = rep(dim(subset(Buy, sr_site_addr_raw=='' & sr_mail_addr_raw==''))[1]/dim(Buy)[1],dim(Buy)[1])
Buy$addr_empty_site= rep(dim(subset(Buy, sr_site_addr_raw=='' & sr_mail_addr_raw!=''))[1]/dim(Buy)[1],dim(Buy)[1])
Buy$addr_empty_mail= rep(dim(subset(Buy, sr_site_addr_raw!='' & sr_mail_addr_raw==''))[1]/dim(Buy)[1],dim(Buy)[1])
Sales = Buy[,c('sr_seller','sr_property_id','sr_date_transfer')]
names(Sales) = c('sr_buyer','sr_property_id','sell_date')
M = merge(Buy, Sales, by = c('sr_buyer', 'sr_property_id'), all=TRUE)
M = M[M$sr_buyer!='',]
M = M[!is.na(M$sr_buyer),]
M = M[M$sr_date_transfer <= M$sell_date,]
# Calculate max number of houses owned at once & duration scores
Score_Output = ddply(M, .(sr_buyer), overlap_date)
save(Score_Output,file=SCORE_FILE)
Score_Output$Max_Score = Score_MaxHouse(Score_Output$Max)
Score_Output$Duration_Score = Score_HoldTime(Score_Output$AvgLength)

# II.c. Calculate address similarity scores
Addr = data[data$sr_tran_type %in% c('R','S'),c('sr_buyer','sr_site_addr_raw','sr_mail_addr_raw')]
Addr$sim = stringSim(Addr$sr_site_addr_raw,Addr$sr_mail_addr_raw)
Addr$score = ifelse(Addr$sr_site_addr_raw!='' & Addr$sr_mail_addr_raw!='' & Addr$sr_site_addr_raw!='*'& Addr$sr_mail_addr_raw!='*',
                    ifelse(Addr$sim>.89,0,ifelse(Addr$sim >.86,.5,1)),.5)
Address = ddply(Addr, .(sr_buyer), Score_Address)
Seller_NotIncl = data.frame(sr_buyer = unique(DATA$sr_buyer[!DATA$sr_buyer %in% data[data$sr_tran_type %in% c('R','S'),]$sr_buyer]))
Seller_NotIncl$Addr = rep(.5,length(Seller_NotIncl))
AddressFull = rbind(Address, Seller_NotIncl)
# Collect and format score dataframe
# Add LLC Scores to Score output frame
NonBSLCC = Score_Output[!Score_Output$sr_buyer%in%b_s_score_result$sr_buyer,]$sr_buyer
LLCframe = rbind(b_s_score_result, data.frame(sr_buyer = NonBSLCC, 
                                              LLCscore=rep(0,length(NonBSLCC)),
                                              Related_Party=rep('NONE',length(NonBSLCC))))
Score_Output = merge(Score_Output,LLCframe, by='sr_buyer', all.x = TRUE)
Score_OutputFull = merge(Score_Output, AddressFull, by = 'sr_buyer', all=FALSE)
Score_OutputFull = Score_OutputFull[,c('sr_buyer','Max_Score','Duration_Score','Addr','LLCscore','Related_Party')]
save(Score_OutputFull, file = paste('Final_',COUNTY_NAME,'.Rdata', sep=''))

Prob = with(Score_OutputFull, .5*Max_Score + ifelse(Max>0,1,0)*Duration_Score*.334 + (1/6)*Addr)




#######################################################################################################
company = c('LLC', 'INC', 'TRUST', 'HOMES', 'CORP', 'BANK', 'BK', 'LP', 'HOUSING','DEV',
            'TRUSTEE', 'NATIONAL', 'HOLDINGS', 'GROUP','LC','CENTER','INVESTMENTS',
            'FEDERAL', 'OPPERTUNITIES', 'COMMUNITY', 'ASSOCS', 'FINANCIAL', 'BUILDERS','SITE',
            'COMMUNITIES', 'ESTATES','CONSTRUCTION', 'EQUITY','PROPERTY', 'REALTY','CAPITAL')`
