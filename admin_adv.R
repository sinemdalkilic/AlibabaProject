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
tr_sc<-unique(tr_sc[,merchnumless950:=sum(dummy),by=c("brand_id","date","sub_category_id")])


tr_sc[,day:=weekdays(tr_sc$date, abbreviate = TRUE)]
dummy=tr_sc[,.(quantity=sum(quantity)),by=c("date","day")]
ggplot(tr_sc[,.(quantity=sum(quantity)),by=c("date","day")],aes(x=date,y=quantity, colour=day, group=day))+geom_line()+geom_point()
ggplot(dummy[quantity<10000],aes(x=date,y=quantity, colour=day, group=day))+geom_line()+geom_point()


tr_sc_brand=tr_sc[,.(quantity=sum(quantity),price=mean(price),view=(sum(pvs)/sum(uvs)),logisticscore=mean(logisticscore),
                     orderscore=mean(orderscore),servicescore=mean(servicescore), availability=sum(availability),merchnumless950,noofsellersbr,noofsellerssc,day,uv=sum(uvs),pv=sum(pvs)),by=.(date, brand_id, sub_category_id)]

tr_sc_brand=unique(tr_sc_brand)


#tr_sc_brand[day=="Sun", if_Sun:=1]
#tr_sc_brand[day!="Sun", if_Sun:=0]

#tr_sc_brand[day=="Mon", if_Mon:=1]
#tr_sc_brand[day!="Mon", if_Mon:=0]


#tr_sc_brand[day=="Tue", if_Tue:=1]
#tr_sc_brand[day!="Tue", if_Tue:=0]

#tr_sc_brand[day=="Wed", if_Wed:=1]
#tr_sc_brand[day!="Wed", if_Wed:=0]


#tr_sc_brand[day=="Thu", if_Thu:=1]
#tr_sc_brand[day!="Thu", if_Thu:=0]

#tr_sc_brand[day=="Fri", if_Fri:=1]
#tr_sc_brand[day!="Fri", if_Fri:=0]

#tr_sc_brand[day=="Sat", if_Sat:=1]
#tr_sc_brand[day!="Sat", if_Sat:=0]
tr_sc_brand[day=="Sun", if_Sun:=1]
tr_sc_brand[day!="Sun", if_Sun:=0]

tr_sc_brand[day=="Mon", if_Mon:=1]
tr_sc_brand[day!="Mon", if_Mon:=0]

tr_sc_brand[day=="Tue", if_Tue:=1]
tr_sc_brand[day!="Tue", if_Tue:=0]
tr_sc_brand[day=="Wed", if_Wed:=1]
tr_sc_brand[day!="Wed", if_Wed:=0]
tr_sc_brand[day=="Thu", if_Thu:=1]
tr_sc_brand[day!="Thu", if_Thu:=0]
tr_sc_brand[day=="Fri", if_Fri:=1]
tr_sc_brand[day!="Fri", if_Fri:=0]
tr_sc_brand[day=="Sat", if_Sat:=1]
tr_sc_brand[day!="Sat", if_Sat:=0]


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
tr_sc_brand[date!='2017-01-27' && date!='2017-01-28'&& date!='2017-01-29' && date!='2017-01-30' && date!='2017-01-31' && date!='2017-02-01' && date!='2017-02-02', if_chinese:=0]

#specify sub_category_id and brand_id inside the regression model
#lagged data

data<-unique(tr_sc_brand[sub_category_id==250 & brand_id==232,])
data[,view := shift(view, 1, type="lag")]
data[,price := shift(price, 1, type="lag")]
data[,cumlogisticscore := shift(cumlogisticscore, 1, type="lag")]
data[,cumorderscore := shift(cumorderscore, 1, type="lag")]
data[,cumservicescore := shift(cumservicescore, 1, type="lag")]
data[,merchnumless950 := shift(merchnumless950, 1, type="lag")]
data[,availability := shift(availability, 1, type="lag")]
data[,uv := shift(uv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]
data[,pv := shift(pv, 1, type="lag")]
data[,maxprice := shift(maxprice, 1, type="lag")]
data[,minprice := shift(minprice, 1, type="lag")]
data[,avgprice := shift(avgprice, 1, type="lag")]
data[,noofsellerssc := shift(noofsellerssc, 1, type="lag")]
data[,noofsellersbr := shift(noofsellersbr, 1, type="lag")]

#Stepwise Regression

fit=step(lm(formula = quantity~price+uv, data =data))
# summary(sc_250_fit_step)
# 
# #to scale the beg_inv
#tr_sc_brand[,beg_inv_norm:=scale(beg_inv, center=TRUE, scale=TRUE), by=c("brand_id", "sub_category_id")]
# 
# #plot between price and normalized beg_inv
# ggplot(tr_sc_brand[sub_category_id==250 & brand_id==773,],)+geom_line(aes(date, beg_inv))+geom_line(aes(date, price), color='red')
# ggplot(tr_sc_brand[sub_category_id==250 & brand_id==773,],)+geom_line(aes(date, beg_inv))



# sc_250_fit_=lm(formula = quantity~logisticscore+orderscore+servicescore+price+view+beg_inv+NoOfSellersBr, data = tr_sc_brand[sub_category_id==250 & brand_id==232,])
# summary(sc_250_fit_)

temp=tr_sc_brand[sub_category_id==250 & brand_id==232,]


tr_sc_brand[, mrktpriceSC:= mean(price), by=c("date", "sub_category_id")]


inventory[, beg_inv+replen_in+trans_in==0, ]
