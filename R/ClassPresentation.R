getwd()
library(data.table)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(ggcorrplot)
library(car)

kc_houses<-fread('Data/kc_house_data.csv')

# Rows and columns
dim(kc_houses)
head(kc_houses)

# No missing values
summary(kc_houses)

# Convert Dates
kc_houses$date<-as.IDate(kc_houses$date, format = "%m/%d/%Y")
summary(kc_houses)

# Univariate model
ggplot(kc_houses, aes(x=seq_along(id), y=price))+
  geom_point(col = "#1f83b4")+
  geom_hline(aes(yintercept = mean(kc_houses$price,na.rm = T)), col = "#ff7f0e")+ 
  scale_color_tableau() +
  labs(x="Id", y="Price")+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "grey"), plot.title = element_text(hjust = 0.5))

kc_houses[,err:=price-mean(price,na.rm = T)]
kc_houses[,err_sq:=err^2]
sse <- sum(kc_houses$err_sq)
options(scipen = 999)
sse

ggplot(kc_houses, aes(x=price)) + geom_density()

ggplot(kc_houses, aes(x=price))+geom_histogram()

#Corr Plot
ncol(kc_houses)
cor_mat <- kc_houses[,3:21]
corr <- cor(cor_mat, use = "pairwise.complete.obs")

ggcorrplot(corr, hc.order = FALSE, type = "lower",
           ggtheme = ggthemes::theme_gdocs,
           colors = c("#ff7f0e", "white", "#1f83b4"),
           lab = TRUE)+
           theme(panel.grid.major=element_blank())

# Scatter Plot
theme_blank<- theme(axis.line=element_blank(),axis.text.x=element_blank(),
      axis.text.y=element_blank(),axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),legend.position="none",
      panel.background=element_rect(fill = "transparent"),panel.border=element_blank(),panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),plot.background=element_rect(fill = "transparent", color = NA))


hist_top<-ggplot(kc_houses, aes(x=sqft_living)) + 
  geom_histogram(aes(y =..density..), 
                 col="#ff7f0e", 
                 fill="#ffaa0e", 
                 alpha = .2,
                 bins = 35) + 
  geom_density(col="#ff7f0e") + 
  scale_color_tableau() +
  theme_blank

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme_blank

scatter<-ggplot(kc_houses, aes(x=sqft_living, y=price))+
  geom_point(col="#1f83b4")+
  labs(x="Sq. Ft. Living", y="Price")+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "grey"), plot.title = element_text(hjust = 0.5))

hist_right <- ggplot(kc_houses, aes(x=price)) + 
  geom_histogram(aes(y =..density..), 
                 col="#ff7f0e", 
                 fill="#ffaa0e", 
                 alpha = .2,
                 bins = 35) + 
  geom_density(col="#ff7f0e") + 
  coord_flip() +
  theme_blank

grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))




# Scatter Plot - 2

hist_top<-ggplot(kc_houses, aes(x=grade)) + 
  geom_histogram(aes(y =..density..), 
                 col="#ff7f0e", 
                 fill="#ffaa0e", 
                 alpha = .2,
                 bins = 35) + 
  geom_density(col="#ff7f0e") + 
  scale_color_tableau() +
  theme_blank

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme_blank

scatter<-ggplot(kc_houses, aes(x=grade, y=price))+
  geom_point(col="#1f83b4")+
  labs(x="Grade", y="Price")+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "grey"), plot.title = element_text(hjust = 0.5))

hist_right <- ggplot(kc_houses, aes(x=price)) + 
  geom_histogram(aes(y =..density..), 
                 col="#ff7f0e", 
                 fill="#ffaa0e", 
                 alpha = .2,
                 bins = 35) + 
  geom_density(col="#ff7f0e") + 
  coord_flip() +
  theme_blank

grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))



# Scatter Plot - 3
names(kc_houses)
hist_top<-ggplot(kc_houses, aes(x=sqft_above)) + 
  geom_histogram(aes(y =..density..), 
                 col="#ff7f0e", 
                 fill="#ffaa0e", 
                 alpha = .2,
                 bins = 35) + 
  geom_density(col="#ff7f0e") + 
  scale_color_tableau() +
  theme_blank

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme_blank

scatter<-ggplot(kc_houses, aes(x=sqft_above, y=price))+
  geom_point(col="#1f83b4")+
  labs(x="Sq. Ft. Above", y="Price")+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "grey"), plot.title = element_text(hjust = 0.5))

hist_right <- ggplot(kc_houses, aes(x=price)) + 
  geom_histogram(aes(y =..density..), 
                 col="#ff7f0e", 
                 fill="#ffaa0e", 
                 alpha = .2,
                 bins = 35) + 
  geom_density(col="#ff7f0e") + 
  coord_flip() +
  theme_blank

grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))


# Scatter Plot - 4

ggplot_scatter_hist_combo(dat = kc_houses, a=kc_houses$sqft_above, b=kc_houses$sqft_living, lab_a = "Sq. Ft. Above", lab_b = "Sq. Ft. Living")

names(kc_houses$sqft_above)
colnames(kc_houses$sqft_above)
ggplot_scatter_hist_combo <- function(dat, a, b, lab_a, lab_b){
  # if(is.null(lab_a))
  #   lab_a<-a
  # if(is.null(lab_b))
  #   lab_b<-b

  theme_blank<- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),legend.position="none",
                      panel.background=element_rect(fill = "transparent"),panel.border=element_blank(),panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank(),plot.background=element_rect(fill = "transparent", color = NA))
  
  hist_top<-ggplot(dat, aes(x=a)) + 
    geom_histogram(aes(y =..density..), 
                   col="#ff7f0e", 
                   fill="#ffaa0e", 
                   alpha = .2,
                   bins = 35) + 
    geom_density(col="#ff7f0e") + 
    scale_color_tableau() +
    theme_blank
  
  empty <- ggplot()+geom_point(aes(1,1), colour="white")+
    theme_blank
  
  scatter<-ggplot(dat, aes(x=a, y=b))+
    geom_point(col="#1f83b4")+
    labs(x=lab_a, y=lab_b)+
    theme(panel.background = element_blank(), axis.line = element_line(colour = "grey"), plot.title = element_text(hjust = 0.5))
  
  hist_right <- ggplot(dat, aes(x=b)) + 
    geom_histogram(aes(y =..density..), 
                   col="#ff7f0e", 
                   fill="#ffaa0e", 
                   alpha = .2,
                   bins = 35) + 
    geom_density(col="#ff7f0e") + 
    coord_flip() +
    theme_blank
  
  grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
}

#Scatter Plot -5

ggplot_scatter_hist_combo(dat = kc_houses, a=kc_houses$sqft_above, b=kc_houses$grade, lab_a = "Sq. Ft. Above", lab_b = "Grade")


#Scatter Plot -5

ggplot_scatter_hist_combo(dat = kc_houses, a=kc_houses$sqft_living, b=kc_houses$grade, lab_a = "Sq. Ft. Living", lab_b = "Grade")


unique(kc_houses$grade)
?sample()

set.seed(1)
kc_houses[, is_training:=sample(c(TRUE,FALSE), nrow(kc_houses), replace = T, prob = c(.7,.3))]
nrow(kc_houses)
kc_houses[,.N*100/nrow(kc_houses),by=is_training]

kc_houses_train <- kc_houses[is_training==T]
kc_houses_test <- kc_houses[is_training==F]


#bivariate

?exp
ggplot(kc_houses_test, aes(x=sqft_living, y=price))+
  geom_point()
kc_houses_train$grade <- as.factor(kc_houses_train$grade)
model<-lm(price~sqft_living+grade, data = kc_houses_train)
summary(model)
anova(model)
vif(model)
ggplot(wine2, aes(x=wine2$Cult)) + geom_density()