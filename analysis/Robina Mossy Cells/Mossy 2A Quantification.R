getwd()
read.csv("5HT2ACreCounted_03).xlsb.csv")
read.csv("5HT2ACreCounted_04.csv")
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("reshape2")
library(reshape2)
install.packages("mosaic")
library(mosaic)


df <- data.frame (type = c("GluR2/3", "2A/GluR2/3", "2A"), average = c("42.5","12","0.4375"))

df$average<- as.numeric(df$average)
head(df)

sd(X5HT2ACreCounted_03_xlsb$`number of colocalization (yellow)`)
sd(X5HT2ACreCounted_03_xlsb$`# of GluR2/3 cells (geen)`)    
sd(X5HT2ACreCounted_03_xlsb$`# of 5HT2A positive mossy cells (red)`)
sd(X5HT2ACreCounted_03_xlsb$`Colocalization %`)

df2 <- data.frame (type = c("GluR2/3", "2A/GluR2/3", "2A"), average = c("42.5","12","0.4375"), sd = c("15.6205", ".8007053", "7.951466"))
df2$average<- as.numeric(df2$average)
df2$sd<- as.numeric(df2$sd)
df2


p<- ggplot(data = df2, aes(x=type,y=average, color=type)) +
  geom_bar(stat="identity", fill="white") + 
  geom_errorbar(aes(ymin = average - sd, ymax = average + sd),
                width = 0.6) +
  xlab("Cell Type") + 
  ylab("Average Number of Cells per Brain Slice") +
  ggtitle("Amount of Protein Expressing Cells") 
   

p<- p+scale_color_manual(values=c("#f51818", "#E69F00", "#18f527")) + theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust=0.5))
p 

df3 <- data.frame (type3= c("2A/GluR2/3"), average3= c("27.91506"), sd3 = c("15.16302"))
df3$average3<- as.numeric(df3$average3)
df3$sd3<- as.numeric(df3$sd3)
df3

p3<- ggplot(data = df3, aes(x=type3,y=average3, color=type3)) +
  geom_col(width = ) +
  geom_bar(stat="identity", position = "dodge", fill="white") + 
  geom_errorbar(aes(ymin = average3 - sd3, ymax = average3 + sd3),
                width = 0.7) +
  xlab("Cell Type ") + 
  ylab("Average % of GluR2/3 Colocalizing w/ 2A") +
  ggtitle(" Colocalization")
 

p3<- p3+scale_color_manual(values=c("#E69F00")) + theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust=0.5)) + theme(plot.margin = unit(c(1,4,1,4), "cm"))
p3

 data <- X5HT2ACreCounted_03_xlsb [, c("Ap Positions", "Green Cells","Red Cells","Yellow Cells")]

data_long <- melt(data, id.vars=c("Ap Positions"))
data_long <- data_long %>% na.omit()
data_long

favstats(X5HT2ACreCounted_03_xlsb$`Green Cells`)
favstats(X5HT2ACreCounted_03_xlsb$`Red Cells`)
favstats(X5HT2ACreCounted_03_xlsb$`Yellow Cells`)

d10<- data.frame (data_long, sd = c("9.440199", "0.4364737", "4.558055"))
d10$sd<- as.numeric(d10$sd)


p10<- ggplot(data = d10, aes(x = Ap.Positions, y = value, color = variable)) +
  geom_line(stat="identity") +
  scale_color_manual(labels = c("GluR2/3", "2A", "2A/GluR2/3"), values=c("#18f527", "#f51818", "#E69F00")) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_point(size = 2, shape = 21) +
  xlab("AP Positions") + 
  ylab("Average Number of Cells per Brain Slice") +
  ggtitle("Protein Expression Across AP Axis") +
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd))

p10 <- p10 + scale_x_reverse()
p10


data2 <- X5HT2ACreCounted_03_xlsb [, c("Ap Positions", "Green Cells","Yellow Cells")]

data_long2 <- melt(data2, id.vars=c("Ap Positions"))
data_long2 <- data_long2 %>% na.omit()
data_long2

favstats(X5HT2ACreCounted_03_xlsb$`Green Cells`)
favstats(X5HT2ACreCounted_03_xlsb$`Yellow Cells`)

d11<- data.frame (data_long2, sd = c("9.440199", "4.558055"))
d11$sd<- as.numeric(d11$sd)


p11<- ggplot(data = d11, aes(x = Ap.Positions, y = value, color = variable)) +
  geom_line(stat="identity") +
  scale_color_manual(labels = c("GluR2/3", "2A/GluR2/3"), values=c("#18f527", "#E69F00")) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_point(size = 2, shape = 21) +
  xlab("AP Positions") + 
  ylab("Average Number of Cells per Brain Slice") +
  ggtitle("Protein Expression Across AP Axis") +
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd))

p11 <- p11 + scale_x_reverse()
p11


