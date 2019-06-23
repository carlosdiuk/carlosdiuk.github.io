library(ggplot2)
library(randomForest)

data <- read.csv("~/Downloads/FIFA_womens_corrs.csv")

# Goal diff by GDP per capita
fit <- lm(data=data, formula = goal_diff ~ GDPpc)
ggplot(data=data, aes(x = GDPpc, y = goal_diff)) +
  ggtitle("2019 FIFA Women's World Cup") +
  geom_point() +
  geom_text(label=data$country, nudge_x = 0.05, nudge_y = 0.5, check_overlap = F) +
  geom_smooth(method="lm") +
  scale_y_continuous(name="Overall Goal Difference") +
  scale_x_continuous("GDP per capita")+ 
  annotate("text", x=15000, y=20, label = paste0("R^2 == ",signif(summary(fit)$adj.r.squared, 5)), parse=T) +
  annotate("text", x=15000, y=17, label = paste0("y == ", signif(fit$coef[[1]],5), " + " ,signif(fit$coef[[2]], 5), "*x"), parse=T)

# Remove US and Thailand and redo
out<-which(data$country==c("Thailand", "United States"))
data_no_outliers<-data[-out,]
fit_no_out <- lm(data=data_no_outliers, formula = goal_diff ~ GDPpc)
ggplot(data=data_no_outliers, aes(x = GDPpc, y = goal_diff)) +
  ggtitle("2019 FIFA Women's World Cup") +
  geom_point() +
  geom_text(label=data_no_outliers$country, nudge_x = 0.05, nudge_y = 0.5, check_overlap = F) +
  geom_smooth(method="lm") +
  scale_y_continuous(name="Overall Goal Difference") +
  scale_x_continuous("GDP per capita")+ 
  annotate("text", x=15000, y=10, label = paste0("R^2 == ",signif(summary(fit_no_out)$adj.r.squared, 5)), parse=T) +
  annotate("text", x=15000, y=8, label = paste0("y == ", signif(fit_no_out$coef[[1]],5), " + " ,signif(fit_no_out$coef[[2]], 5), "*x"), parse=T)

# By GEI
data$negGEI <- 1-data$GEI
fit <- lm(data=data, formula = goal_diff ~ negGEI)
ggplot(data=data, aes(x = negGEI, y = goal_diff)) +
  ggtitle("2019 FIFA Women's World Cup") +
  geom_point() +
  geom_text(label=data$country, nudge_x = 0.05, nudge_y = 0.5, check_overlap = F) +
  geom_smooth(method="lm") +
  scale_y_continuous(name="Overall Goal Difference") +
  scale_x_continuous("Gender Equality Index")+ 
  annotate("text", x=0.5, y=20, label = paste0("R^2 == ",signif(summary(fit)$adj.r.squared, 5)), parse=T) +
  annotate("text", x=0.5, y=17, label = paste0("y == ", signif(fit$coef[[1]],5), " + " ,signif(fit$coef[[2]], 5), "*x"), parse=T)

# Full regression
fit_full <- lm(data=data, formula = goal_diff ~ negGEI + GDPpc + log(population) )
summary(fit_full)

# Bin GEI high v low
data$GEIlevel <- data$negGEI <= median(data$negGEI)
data$GEIlevel <- as.factor(data$GEIlevel)
levels(data$GEIlevel) <- c("low", "high")

fit_full_bin <- lm(data=data, formula = goal_diff ~ GEIlevel + GDPpc + log(population) )
summary(fit_full_bin)

