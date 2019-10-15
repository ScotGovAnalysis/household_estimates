# READ ME #####################################################################
# AUTHOR:Stefania.Sechis@nrscotland.gov.uk
# PURPOSE OF SCRIPT: Code for interactive vis for Household Estimates, 2018.
# CONTACTS:  Sandy Taylor
# SOURCES:   household estimates data ("\\scotland.gov.uk\DC1\SUB_LL1_PSB\Household estimates branch\Household estimates\2018\Figures and tables\
#Data for Table - 2018 - Data for Shiny App.CSV")
# NOTES: ---
###############################################################################
 ## 00 Required Datasets ---
#load("dummy.RData")
load("household_estimates_2018.RData")
# creating the list of areas
list_of_areas <- as.list(as.character(unique(hh$Area)))
list_of_sdpa <- list_of_areas[c(34, 35, 36, 37)]
list_of_np <- list_of_areas[c(38, 39)]
list_of_council <- list_of_areas[c(1:25, 27:33)]
## find a better way to do it 
list_of_year_sdpa <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 
  2017, 2018)
### 0 Common features ----------
g2<-"#ADBD8E"
g1<-"#5c7b1e"
theme_set(theme_minimal(base_size = 16))
server <- function(input, output)
  { 
#error messages if Area= SDPAS and NPs and start year <2008
#Plot 1
area_selection <- reactive(
  {
  req(input$Area1)
  choose_area <- subset(hh, Area == input$Area1) %>%
   filter(Year >= input$end_year1[1] & Year <= input$end_year1[2])
  choose_area <- droplevels(choose_area)
  return(choose_area)
})  
#Plot 3
area_selection2 <- reactive(
  {
    req(input$Area3)
    plot_data0 <- select(hh, Area, Year, Households) %>%
      filter(Year>= input$end_year3[1], Year<= input$end_year3[2])
plot_data0 <-plot_data0[order(plot_data0$Area , plot_data0$Year),]
plot_data1 <- filter(plot_data0, Year== input$end_year3[1])
names(plot_data1)[c(2,3)] <- c("fyear","base")
plot_data0 <- plot_data0 %>%
  left_join(plot_data1)
plot_data2<- mutate(plot_data0, perc_change = (Households-base)/base)
choose_area <- subset(plot_data2, Area %in% input$Area3) %>%
select(Area, Year, perc_change)
choose_area <- droplevels(choose_area)
return(choose_area)
}
)# end of reactive
  ### 1. First Chart - Households trends -----------
output$my_plot1 <- renderPlotly(
{ 
# show an error message if Area= SDPAS and NPs and start year <2008
validate(
  need(nrow(area_selection()) ==  (input$end_year1[2]-input$end_year1[1])+1, 
 "No data for this selection, please pick a year from 2008 onwards using the slider to the left."))
## 1.1 Preparing data ----- 
plot_data<-as.data.frame (area_selection())
plot_annot <- filter(plot_data, Year %in%  c(input$end_year1))
# create a dummy variables for customising x-axis (a weird method) if possible
# find a better way to produce a dinamic x-axis
plot_annot$Year1[1]=plot_annot$Year[1]-1.5
plot_annot$Year1[2]=plot_annot$Year[2]+1.1
plot_annot$Year2[1]=plot_annot$Year[1]-0.4
plot_annot$Year2[2]=plot_annot$Year[2]+0.4
plot_annot$d=input$end_year1[2]-input$end_year1[1]
plot_annot$Year3 <-ifelse( plot_annot$d>10, plot_annot$Year1 , plot_annot$Year2)
## 1.2 Plotting the data -----
g1 <-  ggplot(plot_data, mapping=aes(x=Year, y=Households)) +
  geom_area(fill=g2) +
  geom_line(aes(group=1, text = 
                  paste(Year,": ", format(round(Households,-2), big.mark = ","))), 
            colour=g1, size=1.25
            )+
  geom_point(data=plot_annot,aes(x=Year, y=Households), size=3.5, colour=g1) +
#Adding labels to the plot
geom_text(data= plot_annot, 
  aes(label=
        paste0(plot_annot$Year, ":", "\n", format(round(plot_annot$Households,-2), 
                                                  big.mark = ","))), 
  y=plot_annot$Households*1.15, x=plot_annot$Year, hjust=1, vjust=0, colour=g1) +
theme_minimal(base_size = 16) +
#setting the axes
scale_x_continuous(name="", 
                   limits = c(min(plot_annot$Year3), max(plot_annot$Year3)),
                   breaks = c(input$end_year1[1],input$end_year1[2]), 
                   labels = paste0(c(input$end_year1[1],input$end_year1[2])),
                   expand = c(0, 0)) +
scale_y_continuous(name='', breaks = seq(0, max(plot_data$Households)*1.03, 
                                         by=max(plot_data$Households)/2),
                   labels = c("","", ""), expand = c(0, 0.0),
                   limits = c(0, max(plot_data$Households)*1.4)
                   ) 
## 1.3 Plotting the data with ggplotly-----
ggplotly(g1, tooltip=c("text")) %>% 
  config(displayModeBar = FALSE) %>% 
  layout(annotations = list(
    list(x = 0.05 , y = 1.05, text = input$Area1, showarrow = F, 
         xref='paper', yref='paper', font = list(size = 20)
         ),
  list(x = 0.05 , y = 0.97, text = 
         paste0("Number of households, ", input$end_year1[1], "-", 
                input$end_year1[2]), showarrow = F, xref='paper', yref='paper', 
       font = list(size = 16), hovermode="x")
  )
)
}
)# end of plot 1
### 2. Second Page - Number of households----
 ## 2.1 Chart a (Councils)----
  output$my_plot2<-renderPlotly({
    ## 2.1.1 preparing files ----   
plot_data0 <- select(hh, Area, Year, Households) %>%
filter( Area %in% list_of_council, Year==input$end_year2[1] | Year==input$end_year2[2] )
hh_4_arrange <- plot_data0 %>%
  spread(Year, Households, sep=" ") %>%
  setNames( c("Area", "old", "new") )  %>%
  group_by(Area)  %>%
  mutate(Diff = new - old,
 pc_change = round((Diff/old),4))
hh_4_arrange<- tibble::rowid_to_column(hh_4_arrange, "ID")
hh_4_arrange$Area <- factor(hh_4_arrange$Area, levels = hh_4_arrange$Area[order(-hh_4_arrange$new)])
  ## 2.1.2 Plotting the chart directly with plotly ----
plot_ly (hh_4_arrange, x=~Area, y=~old , 
 # If working with R-studio Version 1.1.463 put levels before the hh_4_arrange$Area 
 # otherwise is not working --> a bug in plotly?
 # type="bar", marker=list(color=ifelse(levels(hh_4_arrange$Area) %in% input$Area2, g2 , "lightgray")),
 type="bar", marker=list(color=ifelse((hh_4_arrange$Area) %in% input$Area2, g2 , "lightgray")),
text = paste(input$end_year2[1], ":", format(round(hh_4_arrange$old,0), big.mark = ",")), hoverinfo="text", name=input$end_year2[1]) %>%
 add_trace(y = ~Diff, 
          #  marker=list(color=ifelse(levels(hh_4_arrange$Area) %in% input$Area2, g1, "darkgray")), name=input$end_year2[2],
          marker=list(color=ifelse((hh_4_arrange$Area) %in% input$Area2, g1, "darkgray")), name=input$end_year2[2],
          text = paste(input$end_year2[2], "households:", format(round(hh_4_arrange$new,0), big.mark = ","), "\n", "change since",
input$end_year2[1], hh_4_arrange$pc_change*100, "%") ,hoverinfo="text") %>%
  layout(hovermode="x", autosize=T, yaxis = list(title = ""), xaxis = list(title = ""),
title="Change in the number of households (thousands)", barmode="stack",
annotations = list(
  # list(x = 0.5 , y = 1.10, text = "Change in the number of households (thousands)", showarrow = F, xref='paper', 
  #      yref='paper', font = list(size = 20)),
  list(x = 0.5 , y = 1, text = paste0("Council areas, ", 
                                input$end_year2[1], "-", input$end_year2[2]), showarrow = F, xref='paper', yref='paper',
       font = list(size = 16)))) %>% 
     config(displayModeBar = FALSE)
  })
  ## 2.2 Chart b (SPDAs and NPs)----  
   output$my_plot2b<-renderPlotly({
     ## 2.2.1 preparing files---     
plot_data0 <- select(hh, Area, Year, Households) %>%
  filter( !(Area %in% c(list_of_council,"Scotland")) ,  Year==input$end_year2b[1] | Year==input$end_year2b[2])
 hh_4_arrange <- plot_data0 %>%
  spread(Year, Households, sep=" ") %>%
  setNames( c("Area", "old", "new") )  %>%
  group_by(Area)  %>%
  mutate(Diff = new - old,
 pc_change = round((Diff/old),4))
hh_4_arrange<- tibble::rowid_to_column(hh_4_arrange, "ID")
hh_4_arrange$Area <- factor(hh_4_arrange$Area, 
                            levels = hh_4_arrange$Area[order(-hh_4_arrange$new)])
## 2.2.2 Plotting the chart directly with plotly ----
plot_ly (hh_4_arrange, x=~Area, y=~old, 
 type="bar", 
 #marker=list(color=ifelse(levels(hh_4_arrange$Area) %in% input$Area2, g2, "lightgray")),
 marker=list(color=ifelse((hh_4_arrange$Area) %in% input$Area2, g2, "lightgray")),
 text = paste(input$end_year2b[1], ":", 
              format(round(hh_4_arrange$old,0), big.mark = ",")), 
 hoverinfo="text", name=input$end_year2b[1]) %>%
  add_trace(y = ~Diff, 
            # marker=list(color=ifelse(levels(hh_4_arrange$Area) %in% input$Area2, g1, "darkgray")), 
            marker=list(color=ifelse((hh_4_arrange$Area) %in% input$Area2, g1, "darkgray")), 
    name=input$end_year2b[2], 
    text = paste(input$end_year2b[2], "households:", 
                 format(round(hh_4_arrange$new,0), big.mark = ","), "\n", 
                 "change since", input$end_year2b[1], hh_4_arrange$pc_change*100, "%"), 
    hoverinfo="text") %>% 
  layout(hovermode="x", autosize=T, yaxis = list(title = ""),xaxis = list(title = ""),
#adding titles
annotations = list(
  list(x = 0.5 , y = 1.05, text = paste0("SDPAs and NPs, " , input$end_year2b[1], "-", 
   input$end_year2b[2]), showarrow = F, xref='paper', yref='paper', 
   font = list(size = 16))),
barmode="stack") %>% 
  config(displayModeBar = FALSE)
  }
)# end of plot 2
   ### 3. Third Tab - Percentage change----
output$my_plot3<-renderPlotly(
  # show an error message if Area= SDPAS and NPs and start year <2008
  { 
  validate(
need(nrow(area_selection2()) ==  (input$end_year3[2]-input$end_year3[1]+1)*length(input$Area3), 
     "No data for this selection, please pick a year from 2008 onwards using the slider to the left.")
  )
## 3.1 preparing files----
plot_data0 <- select(hh, Area, Year, Households) %>%
filter(Year >= input$end_year3[1] , Year<=input$end_year3[2])
plot_data0 <-plot_data0[order(plot_data0$Area , plot_data0$Year),]
plot_data1 <- filter(plot_data0, Year==input$end_year3[1])
names(plot_data1)[c(2,3)] <- c("fyear","base")
plot_data0 <- plot_data0 %>%
left_join(plot_data1)
plot_data2<- mutate(plot_data0, perc_change = (Households-base)/base)
label1 <- plot_data2 %>% filter(Year==input$end_year3[2]) %>% 
arrange(desc(perc_change)) %>% 
.$Area
plot_annot<- as.data.frame (area_selection2())
plot_annot1<-filter(plot_annot, Year %in%  input$end_year3)
#Find a better way to create a dinamic x-axis
plot_annot1$Year1=plot_annot1$Year+0.7
plot_annot1$Year2=plot_annot1$Year+0.3
plot_annot1$d=input$end_year3[2]-input$end_year3[1]
plot_annot1$Year3 <-ifelse( plot_annot1$d>10, plot_annot1$Year1 , plot_annot1$Year2)
#test
print("plot annot:")
print(plot_annot$Area)
cat("\n")
print("plot annot1:")
print(plot_annot1$Area)
cat("\n")
## 3.2 Plotting the chart ----
g3<- ggplot(plot_data2, mapping = aes(x=Year, y=perc_change, group=Area))+ #, colour=Area)) +
geom_line(colour="grey70", size=0.3,
          aes(text=paste0(plot_data2$Area, ":", 
                          ifelse(perc_change >0, "+",""),format(perc_change*100, digits=0, nsmall = 0),"%")
          )) +   
theme_minimal(base_size = 16) +
#guides(colour=FALSE) +
geom_line( data=plot_annot, 
   aes(#x=plot_annot$Year, y=plot_annot$perc_change, 
     group=Area,
       text=paste0(plot_annot$Area, ":",
                 ifelse(plot_annot$perc_change >0, "+",""),
                format(plot_annot$perc_change*100, digits=0, nsmall = 0),"%")),
   colour=g1, size=1.1) +
geom_text(data=plot_annot1 %>% filter(Year == last(Year)),
  aes(label=paste0(ifelse(perc_change >0, "+",""),
   format(perc_change*100, digits=0, nsmall = 0),"%"), 
  y=perc_change, x=Year3, text=Area), 
  hjust=1, vjust=0, colour="black") +
scale_x_continuous(
  name=NULL,
  limits = c(input$end_year3[1]-1,input$end_year3[2]+1.3),
  breaks = input$end_year3,
  labels = input$end_year3,
  expand = c(0.01, 0)) +
scale_y_continuous(name=NULL,  
   limits = c(min(plot_data2$perc_change, na.rm=T), 
  max(plot_data2$perc_change, na.rm=T)) ,
  expand = c(0.01, 0.01),
  breaks= c(0, max(plot_data2$perc_change, na.rm=T)),
  labels= c(0,paste0(format(max(plot_data2$perc_change, na.rm=T)*100, digits=0, nsmall = 0),"%")))  +
theme(plot.margin=unit(c(0,0,0,0), "cm"),
  plot.title=element_text(hjust=0), 
  axis.text.y = element_text(hjust=0.6,vjust=0), 
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  legend.position="bottom")
## 3.3 Plotting with ggplotly ----
ggplotly(g3, tooltip=c("text")) %>% 
     config(displayModeBar = FALSE)
  }
)# end of plot 3
## 4. Forth Tab - Average household size ---
output$my_plot4<-renderPlotly({
## 4.1 preparing files ---
  plot_hh <- filter(hh, Year>=input$end_year4[1], Year<=input$end_year4[2])
  plot_data3 <- filter(hh, Area %in% input$Area4, Year>=input$end_year4[1], Year<=input$end_year4[2])
  plot_annot <- filter(plot_data3,Year %in% input$end_year4)
## 4.2 plotting the chart --- 
  g4<- ggplot(plot_hh, mapping = aes(x=Year, y=`Av_HH_size`)) + 
    geom_line(mapping=aes(x=Year, y=Av_HH_size, group=Area, 
                          text=paste0(Area," ", Year, ":","\n", 
                                      round(plot_hh$Av_HH_size, digits = 2))),
              data=plot_hh, colour= "grey70", size=0.4) +
theme_minimal(base_size = 16) +
guides(colour=FALSE) +
scale_x_continuous(limits = c(plot_annot$Year3[1],plot_annot$Year3[2]),
   breaks = input$end_year4,
   labels = input$end_year4,
   expand = c(0.1, 0)) +
scale_y_continuous(name=NULL, expand = c(0.1, 0.1), 
   limits = c(min(plot_hh$Av_HH_size), max(plot_hh$Av_HH_size)),
   sec.axis = sec_axis(~ . * 1)) +
theme(plot.margin=unit(c(0,0,0,0), "cm"),
  plot.title=element_text(hjust=0),
  axis.title.y = element_blank(),
  axis.title.x = element_blank()) +
geom_line(mapping=aes(x=Year, y=Av_HH_size, group=Area, 
  text=paste0(Area," ", Year, ":","\n", round(plot_data3$Av_HH_size, digits = 2))),
  data=plot_data3,
  colour=ifelse(plot_data3$Area=="Scotland", "grey40",g1), size=1.5) +
annotate("text", label=round(plot_annot$Av_HH_size, digits = 2),
 x= plot_annot$Year+c(-0.5,0.5), y=plot_annot$Av_HH_size,
 colour=ifelse(plot_annot$Area=="Scotland", "grey40",g1), size=4)
## 4.3 Plotting with ggplotly ---
ggplotly(g4,tooltip="text") %>%
  config(displayModeBar = FALSE) %>%
  layout(#hovermode="x", 
        annotations = list(
          x = 0.01 , y = 1.00, text = paste0( "Overall change in average household size, ", input$end_year4[1], "-", input$end_year4[2]),
   showarrow = F, xref='paper', yref='paper', font = list(size = 16))) %>%
  layout(showlegend = FALSE)
}
)# end of plot 4
}#end Server