library(tidyverse)
library(haven)
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(geomtextpath)
#convert data format from .dta to .csv
data2018 <- read_dta('/Users/jinyang/Downloads/Chinese\ General\ Social\ Survey/2018/cgss2018.dta')
data2017 <- read_dta('/Users/jinyang/Downloads/Chinese\ General\ Social\ Survey/2017/cgss2017.dta')
data2015 <- read_dta('/Users/jinyang/Downloads/Chinese\ General\ Social\ Survey/2015/cgss2015_14.dta')
data2013 <- read_dta('/Users/jinyang/Downloads/Chinese\ General\ Social\ Survey/2013/cgss2013_14.dta')
data2012 <- read_dta('/Users/jinyang/Downloads/Chinese\ General\ Social\ Survey/2012/cgss2012_14.dta')
data2011 <- read_dta('/Users/jinyang/Downloads/Chinese\ General\ Social\ Survey/2011/cgss2011_14.dta')
data2010 <- read_dta('/Users/jinyang/Downloads/Chinese\ General\ Social\ Survey/2010/cgss2010_14.dta')
data2005 <- read_dta('/Users/jinyang/Downloads/Chinese\ General\ Social\ Survey/2005/cgss2005_14.dta')
write.csv(data2018, file = "data2018.csv")
write.csv(data2017, file = "data2017.csv")
write.csv(data2015, file = "data2015.csv")
write.csv(data2013, file = "data2013.csv")
write.csv(data2012, file = "data2012.csv")
write.csv(data2011, file = "data2011.csv")
write.csv(data2010, file = "data2010.csv")
write.csv(data2005, file = "data2005.csv")
#data describing
data2010$a40
data2005$qe15b
table(data2010$a40)
table(data2005$qe15b)
#data importing
data=read_csv('/Users/jinyang/Documents/Term_1/SOCI 502/Stylized Fact/data/CGSS_2005_2018.csv')
head(data)
#data describing
n05=nrow(data[!is.na(data$"2005"),])
n10=nrow(data[!is.na(data$"2010"),])
n12=nrow(data[!is.na(data$"2012"),])
n13=nrow(data[!is.na(data$"2013"),])
n15=nrow(data[!is.na(data$"2015"),])
n17=nrow(data[!is.na(data$"2017"),])
n18=nrow(data[!is.na(data$"2018"),])
t05=table(data$"2005")
t10=table(data$"2010")
t12=table(data$"2012")
t13=table(data$"2013")
t15=table(data$"2015")
t17=table(data$"2017")
t18=table(data$"2018")
#tables of proportion and count
df_proportion <- data.frame (num_attitude=c(1,2,3,4,5,0),
                  "y2005"= c(t05[names(t05)==1]/n05, t05[names(t05)==2]/n05, t05[names(t05)==3]/n05, t05[names(t05)==4]/n05, t05[names(t05)==5]/n05, 0),
                  "y2010"= c(t10[names(t10)==1]/n10, t10[names(t10)==2]/n10, t10[names(t10)==3]/n10, t10[names(t10)==4]/n10, t10[names(t10)==5]/n10, t10[names(t10)==6]/n10),
                  "y2012"= c(t12[names(t12)==1]/n12, t12[names(t12)==2]/n12, t12[names(t12)==3]/n12, t12[names(t12)==4]/n12, t12[names(t12)==5]/n12, t12[names(t12)==6]/n12),
                  "y2013"= c(t13[names(t13)==1]/n13, t13[names(t13)==2]/n13, t13[names(t13)==3]/n13, t13[names(t13)==4]/n13, t13[names(t13)==5]/n13, t13[names(t13)==6]/n13),
                  "y2015"= c(t15[names(t15)==1]/n15, t15[names(t15)==2]/n15, t15[names(t15)==3]/n15, t15[names(t15)==4]/n15, t15[names(t15)==5]/n15, t15[names(t15)==6]/n15),
                  "y2017"= c(t17[names(t17)==1]/n17, t17[names(t17)==2]/n17, t17[names(t17)==3]/n17, t17[names(t17)==4]/n17, t17[names(t17)==5]/n17, t17[names(t17)==6]/n17),
                  "y2018"= c(t18[names(t18)==1]/n18, t18[names(t18)==2]/n18, t18[names(t18)==3]/n18, t18[names(t18)==4]/n18, t18[names(t18)==5]/n18, t18[names(t18)==6]/n18)
)
df_count <- data.frame (num_attitude=c(1,2,3,4,5,0),
                             "y2005"= c(t05[names(t05)==1], t05[names(t05)==2], t05[names(t05)==3], t05[names(t05)==4], t05[names(t05)==5], 0),
                             "y2010"= c(t10[names(t10)==1], t10[names(t10)==2], t10[names(t10)==3], t10[names(t10)==4], t10[names(t10)==5], t10[names(t10)==6]),
                             "y2012"= c(t12[names(t12)==1], t12[names(t12)==2], t12[names(t12)==3], t12[names(t12)==4], t12[names(t12)==5], t12[names(t12)==6]),
                             "y2013"= c(t13[names(t13)==1], t13[names(t13)==2], t13[names(t13)==3], t13[names(t13)==4], t13[names(t13)==5], t13[names(t13)==6]),
                             "y2015"= c(t15[names(t15)==1], t15[names(t15)==2], t15[names(t15)==3], t15[names(t15)==4], t15[names(t15)==5], t15[names(t15)==6]),
                             "y2017"= c(t17[names(t17)==1], t17[names(t17)==2], t17[names(t17)==3], t17[names(t17)==4], t17[names(t17)==5], t17[names(t17)==6]),
                             "y2018"= c(t18[names(t18)==1], t18[names(t18)==2], t18[names(t18)==3], t18[names(t18)==4], t18[names(t18)==5], t18[names(t18)==6])
)
#tidy data
tidy_df=data |> 
  pivot_longer('2005':'2018', names_to = "year", values_to = "num_attitude")
tidy_df_propotion=df_proportion |> 
  pivot_longer('y2005':'y2018', names_to = "year", values_to = "proportion")
tidy_df_count=df_count |> 
  pivot_longer('y2005':'y2018', names_to = "year", values_to = "count")
#mutate
tidy_data=tidy_df|>mutate(attitude=ifelse(num_attitude==1, "always wrong",ifelse(num_attitude==2, "mostly wrong", ifelse(num_attitude==3, 'cannot say right or wrong',ifelse(num_attitude==4, "sometimes right", ifelse(num_attitude==5, "always right", 'donot know/refuse to answer')) ))))
tidy_data_proportion=tidy_df_propotion|>mutate(attitude=ifelse(num_attitude==1, "always wrong",ifelse(num_attitude==2, "mostly wrong", ifelse(num_attitude==3, 'cannot say right or wrong',ifelse(num_attitude==4, "sometimes right", ifelse(num_attitude==5, "always right", 'donot know/refuse to answer')) ))))
tidy_data_count=tidy_df_count|>mutate(attitude=ifelse(num_attitude==1, "always wrong",ifelse(num_attitude==2, "mostly wrong", ifelse(num_attitude==3, 'cannot say right or wrong',ifelse(num_attitude==4, "sometimes right", ifelse(num_attitude==5, "always right", 'donot know/refuse to answer')) ))))
##plotting
#count plot
factor_levels=c("donot know/refuse to answer","always wrong","mostly wrong","cannot say right or wrong", "sometimes right","always right")
tidy_data_count$attitude=factor(tidy_data_count$attitude, levels=factor_levels)
tidy_data_count|>filter(attitude!='donot know/refuse to answer')|>
  ggplot(aes(x=year, y=count,fill=attitude))+ 
  geom_col(position = "stack")+#facet_grid(.~year)+
  #theme(axis.text.x=element_blank(),
        #axis.ticks.x=element_blank())+
  #geom_text(aes(attitude, attitude_count,label=attitude_count, size=3)+
  ggtitle("attitudes toward same-sex sexual behavior")+
  scale_fill_brewer(palette = "Blues")+
  theme_clean()+
  annotate("text", x="y2005", y=11000, label="Do you agree that\nHomosexuality is\na personal behavior,\nothers should\nnot criticize")+
  annotate("text", x="y2005", y=8800, label="strongly\ndisagree")+
  annotate("text", x="y2005", y=5000, label="disagree")+
  annotate("text", x="y2005", y=2500, label="neutral")+
  annotate("text", x="y2005", y=800, label="agree")+
  annotate("text", x="y2005", y=130, color="white",label="strongly agree")+
  annotate("text", x="y2013", y=11500, label="Do you think sexual behavior between people of the same sex is...",size=5)
#proportion plot
tidy_data_proportion$attitude=factor(tidy_data_proportion$attitude, levels=factor_levels)
tidy_data_proportion|>filter(attitude!='donot know/refuse to answer')|>filter(year!="y2005")|>
  ggplot(aes(year,proportion, group=attitude, col=attitude, label=attitude))+ 
  geom_point(size=1.5)+ 
  ggtitle("attitudes toward same-sex sexual behavior")+
  geom_smooth(method=lm, fill='lightblue',level=0.95)+
  scale_colour_discrete()+
  theme_clean()




