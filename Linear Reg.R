library(tibble)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lme4)




df <- tibble(cafe_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2,
                         2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3),
             timestamp = structure(
               c(1596283511, 1596283511, 1596287730, 1596287730, 1596287730,
                 1596370505, 1596378076, 1596386672, 1596386672, 1596283999,
                 1596283999, 1596283999, 1596291727, 1596368783, 1596368783,
                 1596369685, 1596285196, 1596285327, 1596285327, 1596286869,
                 1596291836, 1596293731, 1596293731, 1596370648, 1596370648),
               class = c("POSIXct", "POSIXt"), tzone = "UTC"),
             invoice_id = c(1, 1, 2, 2, 2, 3, 4, 5, 5, 1, 1, 1, 2,
                            3, 3, 4, 1, 2, 2, 3, 4, 5, 5, 6, 6),
             item = c("coffee", "latte macchiato", "cake", "cappuchino",
                      "tea", "coffee", "cake", "espresso", "orange juice",
                      "tea", "bagel", "espresso", "coffee", "ice cream",
                      "espresso", "cake", "latte macchiato", "cake",
                      "cappuchino", "bagel", "espresso", "tea", "cake",
                      "coffee", "bagel"),
             quantity= c(1, 1, 2, 2, 1, 2, 1, 2, 1, 2, 3, 1, 1,
                         2, 3, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2),
             item_price = c(2, 3, 3, 2.5, 2.5, 2, 3, 1.5, 2.6,
                            2.5, 3, 1.5, 2, 2.3, 1.5, 3, 3, 3,
                            2.5, 3, 1.5, 2.5, 3, 2, 3),
             total = c(2, 3, 6, 5, 2.5, 4, 3, 3, 2.6, 5, 9, 1.5, 2,
                       4.6, 4.5, 3, 3, 3, 2.5, 3, 3, 2.5, 3, 4, 6))

head(df)

cor(df$total, df$item_price, method = c("pearson")) # I got 0.2886805 no so far at least!

# linear model of total according to bevarage.

li <- lm(total ~ item_price, data=df)
li
summary(li)

# data frame with the variable you want to plot against item_price
predic <- data.frame(yourprediction = predict(li, df), itemprice=df$item_price)

# The predicted line of linear regression
ggplot(data = df, aes(x = total, y = item_price)) +
  geom_point(position = "jitter", aes(color = item))+
  geom_line(color='red',data = predic, aes(x=yourprediction, y=itemprice))+
  ylab("This is your item price")+
  xlab("This is how your Total increments")+
  ggtitle("Red Regression Line", subtitle = "item_price is red line, that increments")+
  theme_economist()

# In conclusion, the total will increment, if you increment the price, but your total don´t increment too much as x_axis shows


lmer(quantity ~ (item_price | item), data = df) %>% coef()