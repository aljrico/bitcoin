
require("quantmod")
require("nortest")
library(tidyverse)
library(reshape2)
library(data.table)
source("btc-functions.R")

# Fetch Data --------------------------------------------------------------
btc.usd <- new.env()
getSymbols("BTC-USD", env = btc.usd, src = "yahoo", from = as.Date("1950-01-01"), to = as.Date("2018-12-31"))

btc.usd<- btc.usd$`BTC-USD`
btc <- as_tibble(btc.usd[,"BTC-USD.Adjusted"])
btc$date <- as.Date(as.character(index(btc.usd)), origin = "1975-01-01")

colnames(btc) <- c("price","date")



# Historical Plot ---------------------------------------------------------

btc %>%
	filter(date > "2016-01-01") %>%
	ggplot(aes(x = date, y = (price))) +
	geom_line(size = 1.3, colour = "#4a6978") +
	scale_x_date(date_label ="%Y-%m-%d") +
	xlab("") +
	ylab("") +
	theme_minimal() +
	theme(text = element_text(family = 'Open Sans'),
				axis.title = element_text(family = 'Open Sans')) +
	scale_y_continuous(labels = scales::dollar)



# Volatility --------------------------------------------------------------

btc <- btc %>%
	mutate(logprice = log(price))

btc$volbtc <- vol(df=btc)

btc %>%
	filter(date > "2010-01-01") %>%
	ggplot(aes(x = date)) +
	geom_line(aes(y = volbtc),size = 1.3, colour = "#3c7591") +
	geom_line(aes(y = price), size = 1, colour = "#4a6978") +
	scale_x_date(date_label ="%Y-%m-%d") +
	xlab("") +
	ylab("") +
	theme_minimal() +
	theme(text = element_text(family = 'Open Sans'),
				axis.title = element_text(family = 'Open Sans'))



btc %>%
	melt(id.vars = c("date")) %>%
	filter(variable %in% c("price","volbtc")) %>%
	filter(date > "2010-01-01") %>%
	ggplot(aes(x = date, colour = variable)) +
	geom_line(aes(y = value),size = 1) +
	scale_x_date(date_label ="%Y-%m-%d") +
	xlab("") +
	ylab("") +
	theme_minimal() +
	theme(text = element_text(family = 'Open Sans'),
				axis.title = element_text(family = 'Open Sans')) +
	facet_grid(variable ~., scales = "free") +
	scale_color_manual(values=c("#4a6978","#694a78"))


# Other currencies --------------------------------------------------------

# EUR
eur.usd <- new.env()
getSymbols("EUR=X", env = eur.usd, src = "yahoo", from = as.Date("1950-01-01"), to = as.Date("2018-12-31"))

eur.usd<- eur.usd$`EUR=X`
eur <- as_tibble(1/eur.usd[,"EUR=X.Adjusted"])
eur$date <- as.Date(as.character(index(eur.usd)), origin = "1975-01-01")
colnames(eur) <- c("price","date")

eur <- eur %>%
	mutate(logprice = log(price))

eur$voleur <- vol(df = eur)

melt(merge(eur[,c("date","voleur")], btc[,c("date","volbtc")]),id.vars = c("date")) %>%
	mutate(variable = ifelse(variable == "voleur", "eur", "btc")) %>%
	filter(date > "2016-01-01") %>%
	ggplot(aes(x = date, colour = variable)) +
	geom_line(aes(y = (value)),size = 1) +
	scale_x_date(date_label ="%Y-%m-%d") +
	xlab("") +
	ylab("") +
	theme_minimal() +
	theme(text = element_text(family = 'Open Sans'),
				axis.title = element_text(family = 'Open Sans')) +
	scale_color_manual(values=c("#4a6978","#694a78"))



# gbp
gbp.usd <- new.env()
getSymbols("GBP=X", env = gbp.usd, src = "yahoo", from = as.Date("1950-01-01"), to = as.Date("2018-12-31"))

gbp.usd<- gbp.usd$`GBP=X`
gbp <- as_tibble(1/gbp.usd[,"GBP=X.Adjusted"])
gbp$date <- as.Date(as.character(index(gbp.usd)), origin = "1975-01-01")
colnames(gbp) <- c("price","date")

gbp <- gbp %>%
	mutate(logprice = log(price))

gbp$volgbp <- vol(df = gbp)

# jpy
jpy.usd <- new.env()
getSymbols("JPY=X", env = jpy.usd, src = "yahoo", from = as.Date("1950-01-01"), to = as.Date("2018-12-31"))

jpy.usd<- jpy.usd$`JPY=X`
jpy <- as_tibble(1/jpy.usd[,"JPY=X.Adjusted"])
jpy$date <- as.Date(as.character(index(jpy.usd)), origin = "1975-01-01")
colnames(jpy) <- c("price","date")

jpy <- jpy %>%
	mutate(logprice = log(price))

jpy$voljpy <- vol(df = jpy)



btc[,c("date","volbtc")] %>%
	merge(eur[,c("date","voleur")]) %>%
	merge(gbp[,c("date","volgbp")]) %>%
	merge(jpy[,c("date","voljpy")]) %>%
	melt(id.vars = c("date")) %>%
	filter(date > "2014-07-01") %>%
	ggplot(aes(x = date, colour = variable)) +
	geom_line(aes(y = (value)),size = 1) +
	scale_x_date(date_label ="%Y-%m-%d") +
	xlab("") +
	ylab("") +
	theme_minimal() +
	theme(text = element_text(family = 'Open Sans'),
				axis.title = element_text(family = 'Open Sans')) +
	scale_color_manual(values=c("#4a6978","#694a78","#59784a","#78594a"), labels = c("btc","eur","gbp","jpy")) +
	labs(colour = "Volatility")



# Google trends -----------------------------------------------------------


gtrends <- fread("gtrends.csv") %>%
	mutate(term = as.numeric(ifelse(term == "<1", 0.25,term))/100) %>%
	mutate(date = as.Date(as.Date(paste(Month,"-01",sep=""))), format = "%Y-%m-%d") %>%
	select(date,term)

btc[,c("date","volbtc")] %>%
	merge(eur[,c("date","voleur")]) %>%
	merge(gbp[,c("date","volgbp")]) %>%
	merge(jpy[,c("date","voljpy")]) %>%
	merge(gtrends) %>%
	melt(id.vars = c("date")) %>%
	filter(date > "2014-07-01") %>%
	ggplot(aes(x = date, colour = variable)) +
	geom_line(aes(y = (value)),size = 1) +
	scale_x_date(date_label ="%Y-%m-%d") +
	xlab("") +
	ylab("") +
	theme_minimal() +
	theme(text = element_text(family = 'Open Sans'),
				axis.title = element_text(family = 'Open Sans')) +
	scale_color_manual(values=c("#4a6978","#694a78","#59784a","#78594a","red"), labels = c("btc","eur","gbp","jpy","google_trends")) +
	labs(colour = "Volatility")
