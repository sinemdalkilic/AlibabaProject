# clear memory
rm(list=ls())
gc()
# load packages
require(data.table)
require(ggplot2)

products<-readRDS("C://Users//Acerr//Desktop//Courses//Current//IE 492//CAT 42-20180218T104859Z-001//CAT 42//cat42_products.rds")
tr<-readRDS("C://Users//Acerr//Desktop//Courses//Current//IE 492//CAT 42-20180218T104859Z-001//CAT 42//cat42_tr.rds")
inventory<-readRDS("C://Users//Acerr//Desktop//Courses//Current//IE 492//CAT 42-20180218T104859Z-001//CAT 42//cat42_inventory.rds")
sellers<-readRDS("C://Users//Acerr//Desktop//Courses//Current//IE 492//CAT 42-20180218T104859Z-001//CAT 42//cat42_sellers.rds")

tr[,price:=amount/quantity]
setnames(tr,"day", "date")
setnames(inventory,"day", "date")
products[,pvs:=app_pv+pc_pv]
products[,uvs:=app_uv+pc_uv]
tr_sub_cat=merge(tr[,.(if_cainiao, merchant_id, date, item_id, quantity, amount, price),], 
                 products[, .(date, item_id, sub_category_id, pvs, uvs, brand_id)], by=c("date", "item_id"))


inventory_availability=inventory[, .(availability=sum(beg_inv)+sum(replen_in)), by=c("item_id", "date")]
# inventory_availability=inventory_availability[availability==0, if_avail:=0 ]
# inventory_availability=inventory_availability[availability!=0, if_avail:=1 ]
#tr_sc=merge(tr_sub_cat,inventory_availability[,.(item_id,date,if_avail=if_avail)],by=c("date", "item_id"))
tr_sc=merge(tr_sub_cat,inventory_availability[,.(item_id,date,availability)],by=c("date", "item_id"))
#tr_sc[, view:=uv/pv,]
#tr_sc[,viewmult:=uv*pv,]

setnames(sellers, "subcategory_id","sub_category_id")
setnames(sellers, "day", "date")
sellers = transform(sellers, date=as.Date(as.character(date), "%Y%m%d"))
merch_brand=merge(sellers[, .(avg_logistic_review_score, avg_order_quality_score, avg_service_quality_score, merchant_id, date, sub_category_id)], 
                  products[, .(date, merchant_id, item_id, brand_id, sub_category_id)], by=c("date", "merchant_id","sub_category_id"))

scores_sc_brand=merch_brand[,.(logisticscore=mean(avg_logistic_review_score,na.rm = TRUE),orderscore=mean(avg_order_quality_score,na.rm = TRUE),servicescore=mean(avg_service_quality_score,na.rm = TRUE)),by=c("date", "brand_id", "sub_category_id")]
tr_sc=merge(scores_sc_brand, tr_sc, by=c("date", "brand_id", "sub_category_id"))

noofsellers_subcat= merch_brand[,.(noofsellerssc=length(unique(merchant_id))),by=c("date", "sub_category_id")]
noofsellers_brand= merch_brand[,.(noofsellersbr=length(unique(merchant_id))),by=c("date", "brand_id")]

tr_sc=merge(tr_sc,noofsellers_subcat,by=c("date", "sub_category_id"))
tr_sc=merge(tr_sc,noofsellers_brand,by=c("date", "brand_id"))

tr_sc[availability<950,dummy:=1,by=c("date", "sub_category_id", "brand_id")]
tr_sc[availability>=950,dummy:=0,by=c("date", "sub_category_id", "brand_id")]
tr_sc[,avgavailability:=mean(availability),by=c("date", "sub_category_id")]
x=tr_sc[, sum(dummy), by=.(date, brand_id, sub_category_id, item_id)]
x[V1>0, dumx:=1,] 
x[V1==0, dumx:=0,] 
x[, itemnumless950:=sum(as.integer(dumx)), by=.(date, sub_category_id, brand_id)] 
tr_sc=merge(tr_sc, x[, .(itemnumless950, date, brand_id, sub_category_id, item_id),], by=c("date", "brand_id", "sub_category_id", "item_id"))
tr_sc<-unique(tr_sc)
tr_sc[,day:=weekdays(tr_sc$date, abbreviate = TRUE)]
dummy=tr_sc[,.(quantity=sum(quantity)),by=c("date","day")]
ggplot(tr_sc[,.(quantity=sum(quantity)),by=c("date","day")],aes(x=date,y=quantity, colour=day, group=day))+geom_line()+geom_point()
ggplot(dummy[quantity<10000],aes(x=date,y=quantity, colour=day, group=day))+geom_line()+geom_point()


tr_sc_brand=tr_sc[,.(quantity=sum(quantity),price=mean(price),logprice=log(mean(price)),view=(sum(pvs)/sum(uvs)),logisticscore=mean(logisticscore),
                     orderscore=mean(orderscore),servicescore=mean(servicescore), availability=sum(availability),avgavailability,itemnumless950,noofsellersbr,noofsellerssc,day,uv=sum(uvs),pv=sum(pvs)),by=.(date, brand_id, sub_category_id)]

tr_sc_brand=unique(tr_sc_brand)

#tr_sc_brand[day=="Sun", if_Sun:=1]
#tr_sc_brand[day!="Sun", if_Sun:=0]

tr_sc_brand[day=="Paz", if_Sun:=1]
tr_sc_brand[day!="Paz", if_Sun:=0]

#tr_sc_brand[day=="Mon", if_Mon:=1]
#tr_sc_brand[day!="Mon", if_Mon:=0]

tr_sc_brand[day=="Pzt", if_Mon:=1]
tr_sc_brand[day!="Pzt", if_Mon:=0]

#tr_sc_brand[day=="Tue", if_Tue:=1]
#tr_sc_brand[day!="Tue", if_Tue:=0]

tr_sc_brand[day=="Sal", if_Tue:=1]
tr_sc_brand[day!="Sal", if_Tue:=0]
 
#tr_sc_brand[day=="Wed", if_Wed:=1]
#tr_sc_brand[day!="Wed", if_Wed:=0]
 
tr_sc_brand[day=="Çar", if_Wed:=1]
tr_sc_brand[day!="Çar", if_Wed:=0]
 
#tr_sc_brand[day=="Thu", if_Thu:=1]
#tr_sc_brand[day!="Thu", if_Thu:=0]
 
tr_sc_brand[day=="Per", if_Thu:=1]
tr_sc_brand[day!="Per", if_Thu:=0]
 
#tr_sc_brand[day=="Fri", if_Fri:=1]
#tr_sc_brand[day!="Fri", if_Fri:=0]
 
tr_sc_brand[day=="Cum", if_Fri:=1]
tr_sc_brand[day!="Cum", if_Fri:=0]
 
#tr_sc_brand[day=="Sat", if_Sat:=1]
#tr_sc_brand[day!="Sat", if_Sat:=0]
 
tr_sc_brand[day=="Cmt", if_Sat:=1]
tr_sc_brand[day!="Cmt", if_Sat:=0]


#dummy=tr_sc_brand[,list(brand_id,sub_category_id),by="date"]
#unique(dummy[,list(brand_id,sub_category_id)])

#brands: 232 345 609 773 850 659 285
#
#sub_cat: 250 365 9 44 117 376 69 285

#dummy=tr_sc_brand[brand_id==232 & sub_category_id==250]
#dummy=dummy[order(date)]
#dummy[,cumlogisticscore:=cumsum(logisticscore)/(1:.N)

tr_sc_brand=unique(tr_sc_brand[order(date)])

tr_sc_brand[brand_id==232 & sub_category_id==250, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==345 & sub_category_id==365, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==9, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==44, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==117, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==376, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==69, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==117, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==250, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==376, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==850 & sub_category_id==117, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==659 & sub_category_id==376, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==285, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]
tr_sc_brand[brand_id==285 & sub_category_id==250, cumlogisticscore:=cumsum(logisticscore)/(1:.N)]

tr_sc_brand[brand_id==232 & sub_category_id==250, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==345 & sub_category_id==365, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==9, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==44, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==117, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==376, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==69, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==117, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==250, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==376, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==850 & sub_category_id==117, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==659 & sub_category_id==376, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==285, cumorderscore:=cumsum(orderscore)/(1:.N)]
tr_sc_brand[brand_id==285 & sub_category_id==250, cumorderscore:=cumsum(orderscore)/(1:.N)]

tr_sc_brand[brand_id==232 & sub_category_id==250, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==345 & sub_category_id==365, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==9, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==44, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==117, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==376, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==69, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==117, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==250, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==773 & sub_category_id==376, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==850 & sub_category_id==117, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==659 & sub_category_id==376, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==609 & sub_category_id==285, cumservicescore:=cumsum(servicescore)/(1:.N)]
tr_sc_brand[brand_id==285 & sub_category_id==250, cumservicescore:=cumsum(servicescore)/(1:.N)]

tr_sc_brand[brand_id!=232 & sub_category_id==250,maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=345 & sub_category_id==365, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==9, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==44, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==117, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==376, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==69, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==117, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==250, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==376, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=850 & sub_category_id==117, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=659 & sub_category_id==376, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==285, maxprice:=max(price) ,by=.(date)]
tr_sc_brand[brand_id!=285 & sub_category_id==250, maxprice:=max(price) ,by=.(date)]

tr_sc_brand[brand_id!=232 & sub_category_id==250,minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=345 & sub_category_id==365, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==9, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==44, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==117, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==376, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==69, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==117, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==250, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==376, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=850 & sub_category_id==117, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=659 & sub_category_id==376, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==285, minprice:=min(price) ,by=.(date)]
tr_sc_brand[brand_id!=285 & sub_category_id==250, minprice:=min(price) ,by=.(date)]

tr_sc_brand[brand_id!=232 & sub_category_id==250,avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=345 & sub_category_id==365, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==9, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==44, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==117, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==376, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==69, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==117, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==250, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=773 & sub_category_id==376, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=850 & sub_category_id==117, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=659 & sub_category_id==376, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=609 & sub_category_id==285, avgprice:=mean(price) ,by=.(date)]
tr_sc_brand[brand_id!=285 & sub_category_id==250, avgprice:=mean(price) ,by=.(date)]

#adding june 18
tr_sc_brand[date=="2017-06-18", if_june18:=1]
tr_sc_brand[date!="2017-06-18", if_june18:=0]

#Chinese New year week
tr_sc_brand[date=='2017-01-27' | date=='2017-01-28'|date=='2017-01-29' | date=='2017-01-30' | date=='2017-01-31' | date=='2017-02-01' |date=='2017-02-02', if_chinese:=1]
tr_sc_brand[date!='2017-01-27' & date!='2017-01-28'& date!='2017-01-29' & date!='2017-01-30' & date!='2017-01-31' & date!='2017-02-01' & date!='2017-02-02', if_chinese:=0]

#specify sub_category_id and brand_id inside the regression model
#lagged data

data<-unique(tr_sc_brand[sub_category_id==250 & brand_id==232,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+maxprice+minprice+avgprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula= quantity~logprice+maxprice+cumservicescore+avgavailability+itemnumless950+noofsellerssc+if_june18+pv,data=data)
summary(fit2)
anova(fit1,fit2) #0.1361 fail to reject
#fit2 wins
acf(fit2$residuals)# normality assumption :)
plot(fit2)
#pairs(quantity~logprice+maxprice+cumservicescore+avgavailability+itemnumless950+noofsellerssc+if_june18+pv,data=data)

# WATCH OUT FORECAST #

temp=data[date>="2017-01-01"& date<="2017-06-30",]
templeft=data[date>"2017-06-30",]
dummy=data[date>="2017-01-01"& date<="2017-06-30",quantity]

#forecast

require("forecast")
predicted<-predict(fit2,templeft)
forecasted<-forecast::forecast(fit2,templeft,prediction.interval = TRUE)
plot(predicted)
plot(data$date[182:212],forecasted$mean,xlab="date",ylab="quantity",main="Forecasts for July Brand 232 Subcategory 250",ylim=c(0,200))
points(data$date[182:212],data$quantity[182:212],col="blue")
lines(data$date[182:212],data$quantity[182:212],col="blue")
lines(data$date[182:212],forecasted$mean,col="black")
#lines(forecasted$lower[,2],col="red")
#lines(forecasted$upper[,2],col="red")
legend("topleft",lty=1, col = c("blue","black"), c("true values", "forecasted"),seg.len=1)
forecast::accuracy(forecasted,data$quantity[182:212])
#####################

data<-unique(tr_sc_brand[sub_category_id==365 & brand_id==345,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula= quantity~logprice+cumservicescore+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_chinese+view+pv, data =data)
summary(fit2)
anova(fit1,fit2) #0.1675 fail to reject

fit3=lm(formula= quantity~logprice+cumservicescore+itemnumless950+if_Mon+if_Tue+if_Wed+if_Thu+if_chinese+view+pv, data =data)
summary(fit3)
anova(fit2,fit3) #0.4103

fit4=lm(formula= quantity~logprice+cumservicescore+itemnumless950+if_Mon+if_Tue+if_Wed+if_chinese+view+pv, data =data)
summary(fit4)
anova(fit3,fit4) #0.4463

#fit4 wins
acf(fit4$residuals)# normality assumption :)
plot(fit4)

data<-unique(tr_sc_brand[sub_category_id==9 & brand_id==609,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula = quantity~logprice+itemnumless950+if_Thu+noofsellersbr+if_june18+view+uv+pv, data =data)
summary(fit2)
anova(fit1,fit2) #0.1606

#fit2 wins
acf(fit2$residuals)# normality assumption :)
plot(fit2)

data<-unique(tr_sc_brand[sub_category_id==44 & brand_id==609,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula = quantity~logprice+itemnumless950+if_june18+uv, data =data)
summary(fit2)
anova(fit1,fit2) #0.1501
#fit2 wins

acf(fit2$residuals)
plot(fit2)

data<-unique(tr_sc_brand[sub_category_id==117 & brand_id==609,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+maxprice+minprice+avgprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula = quantity~logprice+maxprice+cumorderscore+cumservicescore+itemnumless950+noofsellersbr+if_Wed+if_june18+if_chinese+pv, data =data)
summary(fit2)
anova(fit1,fit2) #0.1586

fit3=lm(formula = quantity~logprice+maxprice+cumorderscore+cumservicescore+itemnumless950+noofsellersbr+if_june18+if_chinese+pv, data =data)
summary(fit3)
anova(fit2,fit3) #0.1131
#fit3 wins

data<-unique(tr_sc_brand[sub_category_id==376 & brand_id==609,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+maxprice+minprice+avgprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula = quantity~logprice+maxprice+itemnumless950+if_Sun+if_Thu+if_june18+uv, data =data)
summary(fit2)
anova(fit1,fit2) #0.1498

fit3=lm(formula = quantity~logprice+maxprice+itemnumless950+if_Sun+if_june18+uv, data =data)
summary(fit3)
anova(fit2,fit3)

fit4=lm(formula = quantity~logprice+maxprice+itemnumless950+if_june18+uv, data =data)
summary(fit4)
anova(fit3,fit4) #0.1705

fit5=lm(formula = quantity~logprice+itemnumless950+if_june18+uv, data =data)
summary(fit5)
anova(fit4,fit5) #0.1515

#fit5 wins.

data<-unique(tr_sc_brand[sub_category_id==69 & brand_id==773,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula = quantity~logprice+avgavailability+itemnumless950+if_Tue+pv, data =data)
summary(fit2)
anova(fit1,fit2) #0.1668

fit3=lm(formula = quantity~logprice+itemnumless950+if_Tue+pv, data =data)
summary(fit3)
anova(fit2,fit3) #0.1542

#fit3 wins

data<-unique(tr_sc_brand[sub_category_id==117 & brand_id==773,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+maxprice+minprice+avgprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)
#fit1 wins

data<-unique(tr_sc_brand[sub_category_id==250 & brand_id==773,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+maxprice+minprice+avgprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula = quantity~minprice+cumlogisticscore+cumservicescore+itemnumless950+if_Sun+if_Mon+if_Tue+noofsellerssc+if_june18+uv+pv, data =data)
summary(fit2)
anova(fit1,fit2) #0.1661

fit3=lm(formula = quantity~minprice+cumlogisticscore+cumservicescore+itemnumless950+if_Sun+if_Mon+if_Tue+if_june18+uv+pv, data =data)
summary(fit3)
anova(fit2,fit3) #0.1424

fit4=lm(formula = quantity~minprice+cumlogisticscore+itemnumless950+if_Sun+if_Mon+if_Tue+if_june18+uv+pv, data =data)
summary(fit4)
anova(fit3,fit4) #0.1962

fit5=lm(formula = quantity~minprice+itemnumless950+if_Sun+if_Mon+if_Tue+if_june18+uv+pv, data =data)
summary(fit5)
anova(fit4,fit5) #0.3554

#fit5 wins

data<-unique(tr_sc_brand[sub_category_id==376 & brand_id==773,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+maxprice+minprice+avgprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula = quantity~logprice+avgavailability+noofsellersbr+if_june18+pv, data =data)
summary(fit2)
anova(fit1,fit2) #2.046e-07
#fit1 wins

data<-unique(tr_sc_brand[sub_category_id==117 & brand_id==850,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+maxprice+minprice+avgprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula = quantity~maxprice+cumlogisticscore+cumorderscore+itemnumless950+if_Wed+if_june18+if_chinese+view, data =data)
summary(fit2)
anova(fit1,fit2)

fit3=lm(formula = quantity~maxprice+cumlogisticscore+cumorderscore+itemnumless950+if_Wed+if_chinese+view, data =data)
summary(fit3)
anova(fit2,fit3)
#fit3 wins

data<-unique(tr_sc_brand[sub_category_id==376 & brand_id==659,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

#NAs are problematic even when not in model so I don't use step I perform the analysis manually
#Let's see Silmeye gönlüm el vermedi :)
#na.fail(data$logprice) #:)
#na.fail(data$view[2:88]) #ok
#na.fail(data$avgavailability[2:88]) #ok
#na.fail(data$itemnumless950[2:88]) #ok
#na.fail(data$noofsellersbr[2:88]) #ok
#na.fail(data$noofsellerssc[2:88]) #ok
#na.fail(data$uv[2:88])
#na.fail(data$pv[2:88])
#na.fail(data$maxprice[2:88])
#na.fail(data$minprice[2:88])
#na.fail(data$avgprice[2:88])
#na.fail(data$if_june18[2:88])
#na.fail(data$if_chinese[2:88])
#na.fail(data$if_Mon[2:88])
#na.fail(data$if_Tue[2:88])
#na.fail(data$if_Wed[2:88])
#na.fail(data$if_Thu[2:88])
#na.fail(data$if_Fri[2:88])
#na.fail(data$if_Sat[2:88])
#na.fail(data$if_Sun[2:88])
data$cumlogisticscore<-NULL
data$cumservicescore<-NULL
data$cumorderscore<-NULL
data$logisticscore<-NULL
data$servicescore<-NULL
data$orderscore<-NULL
data$if_june18<-NULL
data$if_chinese<-NULL
data$noofsellersbr<-NULL
data<-na.omit(data) #Omit ettik

fit1=step(lm(formula = quantity~logprice+maxprice+minprice+avgprice+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+view+uv+pv, data =data))
summary(fit1)
#fit1 wins

data<-unique(tr_sc_brand[sub_category_id==285 & brand_id==609,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

#anova çalışsın diye 
data$cumlogisticscore<-NULL
data$cumservicescore<-NULL
data$cumorderscore<-NULL
data$logisticscore<-NULL
data$servicescore<-NULL
data$orderscore<-NULL
data$maxprice<-NULL
data$minprice<-NULL
data$avgprice<-NULL
data<-na.omit(data)

fit1=step(lm(formula = quantity~logprice+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula = quantity~logprice+if_Thu+noofsellerssc+view, data =data)
summary(fit2)
anova(fit1,fit2) #0.1075

fit3=lm(formula = quantity~logprice+if_Thu+noofsellerssc, data =data)
summary(fit3)
anova(fit2,fit3) #0.1314

#fit3 wins

data<-unique(tr_sc_brand[sub_category_id==250 & brand_id==285,])
data[,view := shift(view, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]

fit1=step(lm(formula = quantity~logprice+maxprice+minprice+avgprice+cumlogisticscore+cumorderscore+cumservicescore+avgavailability+itemnumless950+if_Sun+if_Mon+if_Tue+if_Wed+if_Thu+if_Fri+if_Sat+noofsellerssc+noofsellersbr+if_june18+if_chinese+view+uv+pv, data =data))
summary(fit1)

fit2=lm(formula = quantity~logprice+minprice+cumorderscore+avgavailability+if_Tue+if_Fri+if_june18+uv, data =data)
summary(fit2)
anova(fit1,fit2)

fit3=lm(formula = quantity~logprice+minprice+cumorderscore+avgavailability+if_Tue+if_june18+uv, data =data)
summary(fit3)
anova(fit2,fit3)

#fit3 wins
