# plot of birth rate by month
#
# data from UNData
# https://data.un.org/Default.aspx
# found at
# UNData Live births by month of birth
# https://data.un.org/Data.aspx?d=POP&f=tableCode:55
# by clicking "Download" as semicolon separated
# need to manually add # characters to footnote lines
#

library(ggplot2)
library(dplyr)
library(gridExtra)


# read data here
#birthrate_file = "~/git/birth_rate_by_month/data/UNdata_Export_20210419_155726210.txt.gz"
#birthrate_file = "~/git/birth_rate_by_month/data/UNdata_Export_20230630_084956534.txt.gz"
birthrate_file = "~/git/birth_rate_by_month/data/UNdata_Export_20250419_103054264.txt.gz"
birthdata = read.table(birthrate_file, header=TRUE, sep=";", stringsAsFactors = FALSE )

#test_data = filter(birthdata, Country.or.Area=="Philippines")
#aggregate(birthdata$Reliability, by=list(birthdata$Country.or.Area), table )

summary(birthdata)

# months in order, to keep numerical order
months = c("January", "February", "March", "April", "May", "June",
           "July", "August", "September", "October", "November", "December")
# adjusted number of days per month
d_per_month = (365/12) / c(31,28,31,30,31,30,31,31,30,31,30,31)

#
# big loop to make plots for all countries
country_list = unique(birthdata[["Country.or.Area"]])
#
i = 0
plot_list = list()
mo_yr_country_list = list()
for (country in country_list){
    i = i+1
    country_final_only = filter(birthdata, Reliability == "Final figure, complete",
                             Month %in% months,
                             Country.or.Area==country) %>%
        select( Country.or.Area, Month, Year, Value ) %>%
        arrange(Country.or.Area, Year, match(Month,months))
    months_by_year_by_country = c(table(country_final_only[["Year"]],country_final_only[["Country.or.Area"]]))
    months_by_year_by_country = months_by_year_by_country[months_by_year_by_country>0]
    #mo_yr_country_list[[i]] = months_by_year_by_country
    yearly_averages = country_final_only %>% group_by(Country.or.Area, Year) %>% summarise( yearly_mean = mean(Value) )
    yearly_averages_c = rep(yearly_averages$yearly_mean, months_by_year_by_country )
    d_per_month_c = d_per_month[match(country_final_only[["Month"]],months)]
    
    number_of_years = length(yearly_averages$Year)
    yearly_mean_range = round(range(yearly_averages$yearly_mean))
    year_range = range(country_final_only$Year)
    subtitle_text = paste("Data from UN Demographic Statistics Database, including",number_of_years,"years from",year_range[1],"to",year_range[2])
    #subtitle_text = paste(number_of_years, "years from",year_range[1],"to",year_range[2])
    caption_text = paste("Averages range from", yearly_mean_range[1] ,"to",yearly_mean_range[2])
    cgg = ggplot(country_final_only, aes(x=match(Month,months), 
                                                y=Value/yearly_averages_c*d_per_month_c, 
                                                group=interaction(Country.or.Area,Year) ) ) + 
        theme(axis.text.y=element_text(size=13),
              axis.title.y=element_text(size=16),
              legend.position=c(1,0.8),
              legend.justification = "right",
              legend.title = element_text(size=16),
              legend.key.size = unit(1, 'cm'),
              plot.title = element_text(size=25)) +
        coord_cartesian(xlim=c(1,12),ylim=c(0.75,1.25)) +
        scale_x_continuous(breaks=c(1:12), labels=months, minor_breaks = NULL) +
        scale_color_gradient(low="#00220b", high="#66e284") +
        labs(x=NULL, y = "Births relative to monthly average of that year",
             title=country, subtitle=subtitle_text,
             caption=caption_text) +
        geom_line(aes(colour=Year), alpha=0.3, size=3, lineend = "round")
    country_w_underscores = gsub(" ","_",country)
    if (yearly_mean_range[2] > 100) {
        plot_list[[i]] = cgg
        #outputfilename = paste0("~/git/birth_rate_by_month/countries/", country_w_underscores, ".UNdata_20210419.pdf")
        #outputfilename = paste0("~/git/birth_rate_by_month/countries/", country_w_underscores, ".UNdata_20230630.pdf")
        outputfilename = paste0("~/git/birth_rate_by_month/countries/", country_w_underscores, ".UNdata_20250419.pdf")
        ggsave(outputfilename, cgg, device="pdf", width=8, height=6)
        outputfilename = gsub("pdf","png", outputfilename)
        ggsave(outputfilename, cgg, device="png", width=8, height=6, dpi = 90)
    }
} # end for loop

#pdf("~/git/birth_rate_by_month/all_countries_tiled.UNdata_20210419.pdf", paper="a4", width=8, height=10)
#pdf("~/git/birth_rate_by_month/all_countries_tiled.UNdata_20250419.pdf", paper="a4", width=8, height=10)
#for (i in seq(1, length(plot_list), 6)) {
#    grid.arrange(grobs=plot_list[i:(i+5)], ncol=2)
#}
#dev.off()


################################################################################
# vector of countries by region, arbitrarily
is_M_europe = c("Germany", "Austria")

# begin analysis
#
# for Austria and Germany
ATDE_final_only = filter(birthdata, Reliability == "Final figure, complete",
                                 Month %in% months,
                                 Country.or.Area %in% is_M_europe) %>%
                      select( Country.or.Area, Month, Year, Value ) %>%
                      arrange(Country.or.Area, Year, match(Month,months))
#ATDE_final_only
#table(ATDE_final_only[["Year"]],ATDE_final_only[["Country.or.Area"]])

months_by_year_by_country = c(table(ATDE_final_only[["Year"]],ATDE_final_only[["Country.or.Area"]]))
months_by_year_by_country = months_by_year_by_country[months_by_year_by_country>0]

#table(table(ATDE_final_only[["Country.or.Area"]],ATDE_final_only[["Year"]]))
#sum(table(ATDE_final_only[["Country.or.Area"]],ATDE_final_only[["Year"]]))
#
#table(ATDE_final_only[["Year"]])
#sum(table(ATDE_final_only[["Year"]]))
#head(ATDE_final_only)

yearly_averages = ATDE_final_only %>% group_by(Country.or.Area, Year) %>% summarise( yearly_mean = mean(Value) )
yearly_averages_c = rep(yearly_averages$yearly_mean, months_by_year_by_country )
d_per_month_c = d_per_month[match(ATDE_final_only[["Month"]],months)]

atdegg = ggplot(ATDE_final_only, aes(x=match(Month,months), 
                                y=Value/yearly_averages_c*d_per_month_c, 
                                group=interaction(Country.or.Area,Year) ) ) + 
    theme(axis.text.y=element_text(size=13),
          axis.title.y=element_text(size=16),
          legend.position=c(0.25,0.15),
          legend.title = element_text(size=14),
          legend.key.size = unit(1, 'cm')) +
    coord_cartesian(xlim=c(1,12),ylim=c(0.8,1.2)) +
    scale_x_continuous(breaks=c(1:12), labels=months, minor_breaks = NULL) +
    labs(caption="Data from UN Demographic Statistics Database, using data on AT and DE from 1973 to 2018",
         x=NULL, y = "Births relative to monthly average of that year",
         color = "Country") +
    scale_color_manual(values=c("#b10026", "#fec44f")) +
    geom_line(aes(colour=Country.or.Area), alpha=0.3, size=3, lineend = "round") +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    annotate( geom="text", x=10, y=1.18, label="Oktoberfest", fontface="bold" ) +
    annotate( geom="text", x=7, y=1.19, label="9 months\nafter Oktoberfest", fontface="bold" ) +
    annotate( geom="segment", x=10,y=1.15, xend=10, yend = 1.12, arrow = arrow(length = unit(3, "mm")) ) +
    annotate( geom="segment", x=7,y=1.17, xend=7, yend = 1.14, arrow = arrow(length = unit(3, "mm")) )
atdegg

ggsave("~/git/birth_rate_by_month/images/UNdata_Export_20210419_AT_plus_DE.pdf", atdegg, device="pdf", width=8, height=6)

#
# for Sweden, Norway, and Finland
is_N_europe = c("Sweden", "Norway", "Finland")

SENOFI_final_only = filter(birthdata, Reliability == "Final figure, complete",
                             Month %in% months,
                             Country.or.Area %in% is_N_europe) %>%
    select( Country.or.Area, Month, Year, Value ) %>%
    arrange(Country.or.Area, Year, match(Month,months))

months_by_year_by_country = c(table(SENOFI_final_only[["Year"]],SENOFI_final_only[["Country.or.Area"]]))
months_by_year_by_country = months_by_year_by_country[months_by_year_by_country>0]

yearly_averages = SENOFI_final_only %>% group_by(Country.or.Area, Year) %>% summarise( yearly_mean = mean(Value) )
yearly_averages_c = rep(yearly_averages$yearly_mean, months_by_year_by_country )
d_per_month_c = rep(d_per_month, length(months_by_year_by_country) )

senofigg = ggplot(SENOFI_final_only, aes(x=match(Month,months), 
                                     y=Value/yearly_averages_c*d_per_month_c, 
                                     group=interaction(Country.or.Area,Year) ) ) + 
    theme(axis.text.y=element_text(size=13),
          axis.title.y=element_text(size=16),
          legend.position=c(0.25,0.17),
          legend.title = element_text(size=14),
          legend.key.size = unit(1, 'cm')) +
    coord_cartesian(xlim=c(1,12),ylim=c(0.75,1.25)) +
    scale_x_continuous(breaks=c(1:12), labels=months, minor_breaks = NULL) +
    labs(x=NULL, y = "Births relative to monthly average of that year",
         caption="Data from UN Demographic Statistics Database, using data for Norway, Sweden, and Finland from 1971 to 2018",
         color = "Country") +
    scale_color_manual(values=c("#3690c0", "#99000d", "#fec44f" )) +
    geom_line(aes(colour=Country.or.Area), alpha=0.3, size=3, lineend = "round") +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
senofigg
ggsave("~/git/birth_rate_by_month/images/UNdata_Export_20210419_SE_NO_FI.pdf", senofigg, device="pdf", width=8, height=6)


#summary(SENOFI_final_only)
# plot by year, rather than country
scandyeargg = ggplot(SENOFI_final_only, aes(x=match(Month,months), 
                                y=Value/yearly_averages_c*d_per_month_c, 
                                group=interaction(Country.or.Area,Year) ) ) + 
    theme(axis.text.y=element_text(size=13),
          axis.title.y=element_text(size=16),
          legend.position=c(1,0.8),
          legend.justification = "right",
          legend.title = element_text(size=16),
          legend.key.size = unit(1, 'cm')) +
    coord_cartesian(xlim=c(1,12),ylim=c(0.8,1.25)) +
    scale_x_continuous(breaks=c(1:12), labels=months, minor_breaks = NULL) +
    scale_color_gradient(low="#00220b", high="#66e284") +
    labs(x=NULL, y = "Births relative to monthly average of that year",
         title="Norway, Sweden, and Finland",
         caption="Data from UN Demographic Statistics Database, using data for Norway, Sweden, and Finland from 1971 to 2018") +
    geom_line(aes(colour=Year), alpha=0.3, size=3, lineend = "round")
scandyeargg
ggsave("~/git/birth_rate_by_month/images/UNdata_Export_20210419_scand_by_year.pdf", scandyeargg, device="pdf", width=8, height=6)


#
# for Mediterranean europe
#is_S_europe = c("Spain", "Italy", "Greece", "Croatia")
is_S_europe = c("Spain", "Italy" )

GRITSP_final_only = filter(birthdata, Reliability == "Final figure, complete",
                           Month %in% months,
                           Country.or.Area %in% is_S_europe) %>%
    select( Country.or.Area, Month, Year, Value ) %>%
    arrange(Country.or.Area, Year, match(Month,months))

months_by_year_by_country = c(table(GRITSP_final_only[["Year"]],GRITSP_final_only[["Country.or.Area"]]))
months_by_year_by_country = months_by_year_by_country[months_by_year_by_country>0]
yearly_averages = GRITSP_final_only %>% group_by(Country.or.Area, Year) %>% summarise( yearly_mean = mean(Value) )
yearly_averages_c = rep(yearly_averages$yearly_mean, months_by_year_by_country )
d_per_month_c = d_per_month[match(GRITSP_final_only[["Month"]],months)]

gritspgg = ggplot(GRITSP_final_only, aes(x=match(Month,months), 
                                         y=Value/yearly_averages_c*d_per_month_c, 
                                         group=interaction(Country.or.Area,Year) ) ) + 
    theme(axis.text.y=element_text(size=13),
          axis.title.y=element_text(size=16),
          legend.position=c(0.65,0.17),
          legend.title = element_text(size=14),
          legend.key.size = unit(1, 'cm')) +
    coord_cartesian(xlim=c(1,12),ylim=c(0.75,1.25)) +
    scale_x_continuous(breaks=c(1:12), labels=months, minor_breaks = NULL) +
    labs(x=NULL, y = "Births relative to monthly average of that year",
         caption="Data from UN Demographic Statistics Database, using data for Spain, Italy, Croatia and Greece from 1970 to 2018",
         color = "Country") +
#    scale_color_manual(values=c("#cb181d", "#08519c", "#006d2c", "#ffcf3d" )) +
    scale_color_manual(values=c("#006d2c", "#ffcf3d" )) +
    geom_line(aes(colour=Country.or.Area), alpha=0.3, size=3, lineend = "round") +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
gritspgg
ggsave("~/git/birth_rate_by_month/images/UNdata_Export_20210419_medeurope.pdf", gritspgg, device="pdf", width=8, height=6)

################################################################################
################################################################################


# wood fire earth metal water, X4 and X5 of each decade are wood
#                        wood       fire       earth     metal      water
element_colors = rep(c("#12934d","#a72c01", "#bca626", "#8a78c1", "#2689bc"), each=2 )

# 1960 metal rat   rat   ox  tiger rabbit dragon snake
zodiac_symbols = c("鼠", "牛", "虎", "兔", "龍", "蛇", 
                   "馬", "羊", "猴", "雞", "狗", "豬" )
#                 horse  goat monkey chicken dog pig


i = 0
plot_list = list()
mo_yr_country_list = list()
country_list = c( "China, Hong Kong SAR", "China, Macao SAR", "Japan", "Malaysia", 
                  "Philippines", "Republic of Korea", "Singapore" )

country = "Malaysia"
print(country)
country_final_only = filter(birthdata, Reliability %in% c("Final figure, complete","Provisional figure"),
                            Month %in% months,
                            Country.or.Area==country) %>%
  select( Country.or.Area, Month, Year, Value ) %>%
  mutate( mi = match(Month,months) ) %>%
  arrange(Country.or.Area, Year, mi)

months_by_year_by_country = c(table(country_final_only[["Year"]],country_final_only[["Country.or.Area"]]))
months_by_year_by_country = months_by_year_by_country[months_by_year_by_country>0]

yearly_averages = country_final_only %>% group_by(Country.or.Area, Year) %>% summarise( yearly_mean = mean(Value) )
yearmonth_index = country_final_only$Year + (0.0833 * (match(country_final_only$Month, months )-1))
yearmonth_index
number_of_years = length(yearly_averages$Year)
yearly_mean_range = round(range(yearly_averages$yearly_mean))
year_range = range(country_final_only$Year)
subtitle_text = paste("Data from UN Demographic Statistics Database, including",number_of_years,"years from",year_range[1],"to",year_range[2])
caption_text = paste("Monthly range from", min(country_final_only$Value) ,"to", max(country_final_only$Value))
#zodiac_year = zodiac_symbols[(((unique(country_final_only$Year)-1960)%%12)+1)]
zodiac_year = zodiac_symbols[(((seq(year_range[1],year_range[2],1)-1960)%%12)+1)]
#zodiac_color = element_colors[(((unique(country_final_only$Year)-1964)%%10)+1)]
zodiac_color = element_colors[(((seq(year_range[1],year_range[2],1)-1964)%%10)+1)]

cgg = ggplot(country_final_only, aes(x=yearmonth_index,y=Value)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y=element_text(size=13),
        axis.title.y=element_text(size=16),
        legend.position=c(1,0.8),
        legend.justification = "right",
        legend.title = element_text(size=16),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size=25)) +
  coord_cartesian(xlim=year_range) +
  scale_x_continuous(breaks=unique(country_final_only$Year), labels=unique(country_final_only$Year), 
                     minor_breaks = NULL, expand = c(0.01,0)) +
  #scale_y_continuous(limits = c(0,max(country_final_only$Value))) +
  labs(x=NULL, y = "Births per month",
       title=country, subtitle=subtitle_text,
       caption=caption_text) +
  geom_line(alpha=0.9, size=1, lineend = "round") +
  annotate( geom="text", x=seq(year_range[1],year_range[2],1)+0.5, y=max(country_final_only$Value), label=zodiac_year, color=zodiac_color )
  #annotate( geom="text", x=unique(country_final_only$Year)+0.5, y=max(country_final_only$Value), label=zodiac_year, color=zodiac_color )

country_w_underscores = gsub(",","",gsub(" ","_",country))
outputfilename = paste0("~/git/birth_rate_by_month/images/", country_w_underscores, ".timeline.UNdata_20250419.pdf")
print(outputfilename)
#outputfilename = paste0("~/git/birth_rate_by_month/countries/", country_w_underscores, ".timeline.UNdata_20250419.png")
#ggsave(outputfilename, cgg, device="pdf", width=12, height=6, family="Japan1")
#ggsave(outputfilename, cgg, device="png", width=8, height=6, dpi = 90)
cairo_pdf(filename = outputfilename, width=12, height=6, family="Arial Unicode MS")
cgg
dev.off()
  
#} # end for loop


################################################################################
################################################################################

# add data from
# https://pubmed.ncbi.nlm.nih.gov/2750875/

# attempt at circular plot
country = "Sweden"
print(country)
country_final_only = filter(birthdata, Reliability == "Final figure, complete",
                            Month %in% months,
                            Country.or.Area==country) %>%
  select( Country.or.Area, Month, Year, Value ) %>%
  mutate( mi = match(Month,months) ) %>%
  arrange(Country.or.Area, Year, mi)
ggplot(country_final_only, aes(x=match(Month,months), 
                               y=Value, 
                               group=interaction(Country.or.Area,Year) ) ) + 
  theme(axis.text.y=element_text(size=13),
        axis.title.y=element_text(size=16),
        legend.position=c(1,0.8),
        legend.justification = "right",
        legend.title = element_text(size=16),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size=25)) +
  coord_polar() + 
  xlim(0,12 ) +
  scale_color_gradient(low="#00220b", high="#66e284") +
  labs(x=NULL, y = "Births relative to monthly average of that year",
       title=country, subtitle=subtitle_text,
       caption=caption_text) +
  geom_line(aes(x=mi%%12, colour=Year), alpha=0.3, size=3, lineend = "round")


# P.W. LESLIE AND P.H. FRY (1989) Extreme Seasonality of Births Among Nomadic Turkana Pastoralists
# https://pubmed.ncbi.nlm.nih.gov/2750875/
# TABLE 1. Distributions of Ngisonyoka Turkana births and rainfall, by month
# TABLE 2. Observed and fitted monthlv orouortions o f conceutions and rainfall
leslie1989_text = "month	month_num	boy_births	girl_births	total_births	births_running_average	rainfall_mm	conceptions_3mo_av	conceptions_fitted	rainfall_pct	rainfall_fitted
December	0	10	13	23	28	1	8.3	8.1	0.3	2.9
January	1	9	9	18	15	11	8.1	8.6	3.8	3.6
February	2	2	2	4	26	23	7.4	7.5	7.8	7.8
March	3	25	31	56	39.7	42	6.2	5.3	14.3	12.7
April	4	37	22	59	58	41	3.3	4.1	14	15.3
May	5	31	28	59	58	40	5.8	5.6	13.7	14.2
June	6	31	25	56	45.3	31	8.8	9.1	10.6	10.9
July	7	13	8	21	41	33	12.8	12.2	11.3	8
August	8	18	28	46	33.3	11	12.8	12.8	3.8	7.1
September	9	22	11	33	37.7	23	10	10.8	7.8	7.2
October	10	20	14	34	36.7	18	9.1	8.5	6.1	6.4
November	11	18	25	43	33.3	19	7.4	7.5	6.5	4.3
December	12	10	13	23	28	1	8.3	8.1	0.3	2.9
sum	0	236	216	452	452	293	100	100	100	100"
leslie1989_data = read.table(text=leslie1989_text, header=TRUE, sep="\t")
leslie1989_data.nosum = leslie1989_data[which(leslie1989_data$month!="sum"),]
ggplot(leslie1989_data.nosum, aes(x=month_num, 
                               y=total_births ) ) + 
  theme(axis.text.y=element_text(size=13),
        axis.title.y=element_text(size=16),
        panel.background = element_rect( fill="#00000011" ),
        panel.grid.major = element_line( color = "#00000044" ),
        legend.position=c(1,0.8),
        legend.justification = "right",
        legend.title = element_text(size=16),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size=25)) +
  coord_polar(start = 0, direction = 1) + 
  scale_x_continuous(limits=c(0,12), breaks = seq(0,11,1),  labels = leslie1989_data.nosum$month[-1] ) +
  scale_y_continuous(limits=c(0,60) ) +
  scale_color_gradient(low="#00220b", high="#66e284") +
  labs(x=NULL, y = "",
       title="Turkana births", subtitle="of nomadic Turkana women in northwest Kenya",
       caption="Data from Leslie and Fry (1989) Extreme Seasonality of Births Among Nomadic Turkana Pastoralists" ) +
  geom_line( colour="#00220b", alpha=0.5, size=5, lineend = "round") +
  annotate("polygon", x=leslie1989_data.nosum$month_num, y=leslie1989_data.nosum$rainfall_mm,
           colour="#000b8888", fill="#000b88", alpha=0.5, size=2 ) +
  annotate(geom="text", x=5, y=20, colour="#000b58", label="Rainfall", size=4 )

#