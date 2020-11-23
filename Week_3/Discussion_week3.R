# check installed packages
installed.packages()
install.packages('sqldf')
library('sqldf')

# create dummy data
# https://abicky.net/2011/05/29/200710/
set.seed(1000)
product <- data.frame(id = 101:110, name = sample(LETTERS, 10), price = sample(5:20 * 100, 10))
n <- 100
makeOrder <- function(term, n) {
  data.frame(order_id = sort(sample(seq(1001, len = n * 2), n)),
             date = sort(sample(paste("2011-05-", term, sep = ""), n, replace = TRUE)),
             customer_id = sample(10001:10100, n, replace = TRUE),
             product_id = sample(product$id, n, replace = TRUE),
             number = sample(1:5, n, replace = TRUE))
}

order1 <- makeOrder(21:24, n)
order2 <- makeOrder(25:28, n)

# use sqldf
sqldf('select * from order1')
sqldf('select order_id, customer_id from order1 where order_id = 1009')
sqldf('select * from order1 a inner join order2 b on a.customer_id = b.customer_id')
