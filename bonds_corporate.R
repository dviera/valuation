library(rvest)
library(tidyverse)

get_bonds <- function(country = 18, currency = 333, type = "6%2c7%2c8%2c19"){
  read_url <- function(page = 1){
    url <- paste0("https://markets.businessinsider.com/bonds/finder?p=",
                  page,
                  "&borrower=&maturity=&yield=&bondtype=",
                  type,
                  "&coupon=&currency=",
                  currency,
                  "&rating=&country=",
                  country)
    web_page <- read_html(url)
    web_page
  }

  web_page <- read_url()
  nr_results <- html_text(html_nodes(web_page, '.box-headline'))[2]
  nr_results <- parse_number(nr_results)
  nr_pages <- ceiling(nr_results/20)
  
  for(page in 1:nr_pages){
    cat("Accesing page", page, "out of", nr_pages, "...", "\n")
    if(page == 1){
      web_page <- read_url()
      bonds <- html_text(html_nodes(web_page, 'td'))
      bonds <- matrix(bonds, ncol = 8, byrow = TRUE)
      colnam = c("ISSUER",	"CURRENCY",	"COUPON",	"YIELD",	"MOODY_RATING",	"MATURITY",
                 "BID",	"ASK")
      bonds_df <- as.data.frame(bonds)
      colnames(bonds_df) <- colnam
    } else {
      web_page <- read_url(page = page)
      temp <- html_text(html_nodes(web_page, 'td'))
      temp <- matrix(temp, ncol = 8, byrow = TRUE)
      colnam = c("ISSUER",	"CURRENCY",	"COUPON",	"YIELD",	"MOODY_RATING",	"MATURITY",
                 "BID",	"ASK")
      temp_df <- as.data.frame(temp)
      colnames(temp_df) <- colnam
      bonds_df <- rbind(bonds_df, temp_df)
    }
  }
  bonds_df
}

bdf <- get_bonds()
bdf_clean <- bdf
bdf_clean$ISSUER <- gsub('\r\n\t\t\t\t\t\t\t\t\t', '', bdf_clean $ISSUER)
bdf_clean$ISSUER <- gsub('\r\n\t\t\t\t\t\t\t\t', '', bdf_clean $ISSUER)
bdf_clean$CURRENCY <- gsub('\r\n\t\t\t\t\t\t\t\t\t', '', bdf_clean $CURRENCY)
bdf_clean$CURRENCY <- gsub('\r\n\t\t\t\t\t\t\t\t', '', bdf_clean $CURRENCY)
bdf_clean$COUPON <- gsub('\r\n\t\t\t\t\t\t\t\t\t', '', bdf_clean $COUPON)
bdf_clean$COUPON <- gsub('\r\n\t\t\t\t\t\t\t\t', '', bdf_clean $COUPON)
bdf_clean$COUPON <- as.numeric(gsub('%', '', bdf_clean $COUPON))
bdf_clean$YIELD <- gsub('\r\n\t\t\t\t\t\t\t\t\t', '', bdf_clean $YIELD)
bdf_clean$YIELD <- gsub('\r\n\t\t\t\t\t\t\t\t', '', bdf_clean $YIELD)
bdf_clean$YIELD <- as.numeric(gsub('%', '', bdf_clean $YIELD))
bdf_clean$MOODY_RATING <- gsub('\r\n\t\t\t\t\t\t\t\t\t', '', bdf_clean $MOODY_RATING)
bdf_clean$MOODY_RATING <- gsub('\r\n\t\t\t\t\t\t\t\t', '', bdf_clean $MOODY_RATING)
bdf_clean$MATURITY <- gsub('\r\n\t\t\t\t\t\t\t\t\t', '', bdf_clean $MATURITY)
bdf_clean$MATURITY <- gsub('\r\n\t\t\t\t\t\t\t\t', '', bdf_clean $MATURITY)
bdf_clean$MATURITY <- parse_datetime(bdf_clean$MATURITY, "%m/%d/%Y")
bdf_clean$BID <- gsub('\r\n\t\t\t\t\t\t\t\t\t', '', bdf_clean $BID)
bdf_clean$BID <- gsub('\r\n\t\t\t\t\t\t\t\t', '', bdf_clean $BID)
bdf_clean$BID <- as.numeric(bdf_clean$BID)
bdf_clean$ASK <- gsub('\r\n\t\t\t\t\t\t\t\t\t', '', bdf_clean $ASK)
bdf_clean$ASK <- gsub('\r\n\t\t\t\t\t\t\t\t', '', bdf_clean $ASK)
bdf_clean$ASK <- as.numeric(bdf_clean$ASK)

# example
Baa3 <- bdf_clean %>% filter(MOODY_RATING == "Baa3", CURRENCY == "USD")
x = difftime(Baa3$MATURITY, Sys.time(), units = "weeks")/52.25
y = Baa3$YIELD
plot(x, y, col = "darkred")
xx <- seq(0, 90, by = 0.1)
lines(xx, predict(lm(y~poly(x,3,raw=TRUE)), data.frame(x=xx)), col="skyblue", lwd = 2)
mean(na.omit(y))
mean(na.omit(y[x>10]))
predict(lm(y~poly(x,3,raw=TRUE)), data.frame(x = 10))
mean(predict(lm(y~poly(x,3,raw=TRUE)), data.frame(x = seq(10, 80, by = 0.1))))
