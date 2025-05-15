rm(list=ls())
cat("\014")
packages<-c("shiny","shinydashboard","plotly","dplyr","lubridate","data.table","viridis","shinyjs","shinythemes","DT","leaflet","scales","RColorBrewer","shinyWidgets")
for(pkg in packages){if(!require(pkg,character.only=TRUE))install.packages(pkg)}
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lubridate)
library(data.table)
library(viridis)
library(shinyjs)
library(shinythemes)
library(DT)
library(leaflet)
library(scales)
library(RColorBrewer)
library(shinyWidgets)
data<-fread("/Users/darpanradadiya/Downloads/SuperStore(in).csv")
create_safe_dates<-function(n){
  start_date<-as.Date("2015-01-01")
  end_date<-as.Date("2018-12-31")
  dates<-seq(start_date,end_date,by="day")
  sampled_dates<-sample(dates,n,replace=TRUE)
  return(sort(sampled_dates))
}
data$Order.Date<-create_safe_dates(nrow(data))
data$Ship.Date<-data$Order.Date+sample(1:5,nrow(data),replace=TRUE)
data$Year<-year(data$Order.Date)
data$Month<-month(data$Order.Date)
data$Quarter<-quarter(data$Order.Date)
data$YearMonth<-floor_date(data$Order.Date,"month")
data$YearQuarter<-paste0(data$Year,"Q",data$Quarter)
data$HalfYear<-ifelse(data$Month<=6,1,2)
min_date<-min(data$Order.Date,na.rm=TRUE)
max_date<-max(data$Order.Date,na.rm=TRUE)
unique_categories<-c("All",unique(data$Category))
unique_segments<-c("All",unique(data$Segment))
unique_regions<-c("All",unique(data$Region))
category_colors<-c("Furniture"="#4E79A7","Office Supplies"="#F28E2B","Technology"="#E15759")
region_colors<-c("Central"="#FFD966","East"="#6AA84F","South"="#E67C73","West"="#8E7CC3")
segment_colors<-c("Consumer"="#F1948A","Corporate"="#85C1E9","Home Office"="#82E0AA")
states_coords<-data.table(
  State=unique(data$State),
  lat=runif(length(unique(data$State)),25,49),
  lng=runif(length(unique(data$State)),-125,-70)
)
ui<-dashboardPage(
  skin="blue",
  dashboardHeader(
    title=tags$span(tags$i(class="fa fa-chart-line",style="margin-right:10px;"),"Elite Superstore"),
    titleWidth=300
  ),
  dashboardSidebar(
    width=250,
    useShinyjs(),
    sidebarMenu(
      id="sidebar",
      menuItem("Overview",tabName="overview",icon=icon("dashboard")),
      menuItem("Sales Analysis",tabName="sales",icon=icon("line-chart")),
      menuItem("Predictive Analytics",tabName="predictive",icon=icon("chart-line")),
      menuItem("Geographical Analysis",tabName="geo_analysis",icon=icon("globe")),
      menuItem("Profitability Analysis",tabName="profitability",icon=icon("money-bill")),
      menuItem("Data Explorer",tabName="data_explorer",icon=icon("table"))
    ),
    div(
      style="padding:15px;",
      h4("Filters",style="color:#3498DB;text-align:center;border-bottom:1px solid #3498DB;padding-bottom:10px;margin-bottom:15px;"),
      dateRangeInput("date_range","Date Range:",start=min_date,end=max_date,separator=" to "),
      selectInput("timeframe","Time Grouping:",choices=c("Monthly","Quarterly","Half-Yearly"),selected="Monthly"),
      selectInput("category","Category:",choices=unique_categories,selected="All"),
      selectInput("segment","Segment:",choices=unique_segments,selected="All"),
      selectInput("region","Region:",choices=unique_regions,selected="All"),
      div(style="text-align:center;margin-top:20px;",
          actionButton("reset_filters","Reset Filters",icon=icon("sync"),
                       style="width:100%;background-color:#3498DB;color:white;border-radius:20px;"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper{background-color:#f8f9fa;}
      .box{border-radius:15px;box-shadow:0 4px 8px rgba(0,0,0,0.1);border-top:3px solid #3498DB;margin-bottom:20px;}
      .value-box{border-radius:15px;box-shadow:0 4px 8px rgba(0,0,0,0.1);transition:transform 0.3s;}
      .value-box:hover{transform:scale(1.03);}
      .main-header .logo{font-weight:bold;font-size:20px;}
      .content-header h1{font-weight:bold;color:#3498DB;}
      .box-header{background-color:#f8f9fa;border-bottom:1px solid #e9ecef;}
      .box-title{font-weight:bold;color:#2c3e50;}
      .tab-pane{animation:fadeEffect 0.5s;}
      @keyframes fadeEffect{from{opacity:0;}to{opacity:1;}}
      .form-control{border-radius:10px;border:1px solid #bdc3c7;}
      .form-control:focus{border-color:#3498DB;box-shadow:0 0 0 0.2rem rgba(52,152,219,0.25);}
      .input-daterange{border-radius:10px;}
      .time-controls{text-align:center;margin-bottom:15px;}
      .time-controls .btn{margin:0 5px;background-color:#3498DB;color:white;border:none;border-radius:20px;}
      .time-controls .btn.active{background-color:#2980B9;}
      .leaflet-container{height:600px !important;}
      .dataTables_wrapper .dataTables_filter input{border-radius:20px;border:1px solid #3498DB;padding:5px 10px;}
      .dataTables_wrapper .dataTables_length select{border-radius:20px;border:1px solid #3498DB;padding:5px 10px;}
      .dataTables_filter{margin-bottom:10px;}
      .dataTables_length{margin-bottom:10px;}
      .dataTables_info{margin-top:10px;}
      .dataTables_paginate{margin-top:10px;}
    "))),
    tabItems(
      tabItem(tabName="overview",
              h2("Dashboard Overview",style="margin-bottom:20px;color:#3498DB;"),
              fluidRow(
                valueBoxOutput("total_sales",width=4),
                valueBoxOutput("total_profit",width=4),
                valueBoxOutput("total_customers",width=4)
              ),
              fluidRow(
                box(title="Sales by Category",status="primary",solidHeader=FALSE,width=6,height=380,
                    plotlyOutput("sales_category_plot",height=320)),
                box(title="Profit by Region",status="info",solidHeader=FALSE,width=6,height=380,
                    plotlyOutput("profit_region_plot",height=320))
              ),
              fluidRow(
                box(title="Monthly Performance",status="success",solidHeader=FALSE,width=12,height=380,
                    div(class="time-controls",
                        radioButtons("time_period","View Period:",choices=c("Last 6 Months","Last Year","All Time"),
                                     selected="All Time",inline=TRUE)),
                    plotlyOutput("monthly_performance",height=320))
              )
      ),
      tabItem(tabName="sales",
              h2("Sales Analysis",style="margin-bottom:20px;color:#3498DB;"),
              fluidRow(
                box(title="Sales Over Time",status="primary",solidHeader=FALSE,width=12,height=580,
                    div(class="time-controls",
                        radioButtons("sales_period","View Period:",choices=c("Last 6 Months","Last Year","All Time"),
                                     selected="All Time",inline=TRUE)),
                    plotlyOutput("sales_time_plot",height=320))
              ),
              fluidRow(
                box(title="Top Sub-Categories by Sales",status="info",solidHeader=FALSE,width=12,height=520,
                    plotlyOutput("sales_subcat_plot",height=320))
              )
      ),
      tabItem(tabName="predictive",
              h2("Predictive Analytics",style="margin-bottom:20px;color:#3498DB;"),
              fluidRow(
                box(title="Sales Forecast (Next 12 Months)",status="primary",solidHeader=FALSE,width=12,height=480,
                    div(class="time-controls",
                        radioButtons("forecast_period","Forecast Base Period:",choices=c("Last Year","Last 2 Years","All Available"),
                                     selected="All Available",inline=TRUE)),
                    plotlyOutput("forecast_plot",height=420))
              )
      ),
      tabItem(tabName="geo_analysis",
              h2("Geographical Analysis",style="margin-bottom:20px;color:#3498DB;"),
              fluidRow(
                box(title="Sales by Geography",status="primary",solidHeader=FALSE,width=12,height=600,
                    div(style="position:absolute;top:60px;right:20px;z-index:1000;background:white;padding:10px;border-radius:5px;",
                        radioButtons("geo_metric","View Metric:",choices=c("Sales","Profit","Quantity"),
                                     selected="Sales",inline=TRUE)),
                    leafletOutput("geo_map",height=540))
              )
      ),
      tabItem(tabName="profitability",
              h2("Profitability Analysis",style="margin-bottom:20px;color:#3498DB;"),
              fluidRow(
                box(title="Profit vs. Sales by Category",status="primary",solidHeader=FALSE,width=12,height=480,
                    plotlyOutput("profit_sales_plot",height=420))
              ),
              fluidRow(
                box(title="Profit Margin by Sub-Category",status="info",solidHeader=FALSE,width=12,height=480,
                    plotlyOutput("profit_margin_plot",height=420))
              )
      ),
      tabItem(tabName="data_explorer",
              h2("Data Explorer",style="margin-bottom:20px;color:#3498DB;"),
              fluidRow(
                box(title="Data Filters",status="primary",solidHeader=FALSE,width=12,collapsible=TRUE,
                    fluidRow(
                      column(3,selectInput("dt_category","Filter by Category:",choices=unique_categories,selected="All")),
                      column(3,selectInput("dt_segment","Filter by Segment:",choices=unique_segments,selected="All")),
                      column(3,selectInput("dt_region","Filter by Region:",choices=unique_regions,selected="All")),
                      column(3,div(style="margin-top:25px;",
                                   actionButton("dt_reset","Reset Table Filters",icon=icon("sync"),
                                                style="background-color:#3498DB;color:white;")))
                    ),
                    fluidRow(
                      column(6,sliderInput("dt_sales","Sales Range:",min=0,max=max(data$Sales,na.rm=TRUE),
                                           value=c(0,max(data$Sales,na.rm=TRUE)),step=100)),
                      column(6,sliderInput("dt_profit","Profit Range:",min=min(data$Profit,na.rm=TRUE),
                                           max=max(data$Profit,na.rm=TRUE),
                                           value=c(min(data$Profit,na.rm=TRUE),max(data$Profit,na.rm=TRUE)),step=100))
                    ),
                    div(style="float:right;margin:10px 0;",
                        downloadButton("download_filtered_data","Download Filtered Data",
                                       style="background-color:#3498DB;color:white;"))
                )
              ),
              fluidRow(
                box(title="Filtered Data Table",status="info",solidHeader=FALSE,width=12,height=600,
                    DTOutput("data_table",height=520))
              )
      )
    )
  )
)
server<-function(input,output,session){
  observeEvent(input$reset_filters,{
    updateDateRangeInput(session,"date_range",start=min_date,end=max_date)
    updateSelectInput(session,"category",selected="All")
    updateSelectInput(session,"segment",selected="All")
    updateSelectInput(session,"region",selected="All")
    updateRadioButtons(session,"time_period",selected="All Time")
    updateRadioButtons(session,"sales_period",selected="All Time")
    updateRadioButtons(session,"forecast_period",selected="All Available")
  })
  observeEvent(input$dt_reset,{
    updateSelectInput(session,"dt_category",selected="All")
    updateSelectInput(session,"dt_segment",selected="All")
    updateSelectInput(session,"dt_region",selected="All")
    updateSliderInput(session,"dt_sales",value=c(0,max(data$Sales,na.rm=TRUE)))
    updateSliderInput(session,"dt_profit",value=c(min(data$Profit,na.rm=TRUE),max(data$Profit,na.rm=TRUE)))
  })
  filtered_data<-reactive({
    filtered<-copy(data)
    if(!is.null(input$date_range)){
      filtered<-filtered[Order.Date>=input$date_range[1]&Order.Date<=input$date_range[2]]
    }
    if(input$category!="All"){filtered<-filtered[Category==input$category]}
    if(input$segment!="All"){filtered<-filtered[Segment==input$segment]}
    if(input$region!="All"){filtered<-filtered[Region==input$region]}
    if(nrow(filtered)==0){return(data[1:10])}
    return(filtered)
  })
  data_table_filtered<-reactive({
    filtered<-filtered_data()
    if(input$dt_category!="All"){filtered<-filtered[Category==input$dt_category]}
    if(input$dt_segment!="All"){filtered<-filtered[Segment==input$dt_segment]}
    if(input$dt_region!="All"){filtered<-filtered[Region==input$dt_region]}
    filtered<-filtered[Sales>=input$dt_sales[1]&Sales<=input$dt_sales[2]]
    filtered<-filtered[Profit>=input$dt_profit[1]&Profit<=input$dt_profit[2]]
    if(nrow(filtered)==0){return(data[1:10])}
    return(filtered)
  })
  time_filtered_data<-reactive({
    filtered<-filtered_data()
    if(input$time_period=="Last 6 Months"){
      max_date<-max(filtered$Order.Date,na.rm=TRUE)
      min_date<-max_date-months(6)
      filtered<-filtered[Order.Date>=min_date]
    }else if(input$time_period=="Last Year"){
      max_date<-max(filtered$Order.Date,na.rm=TRUE)
      min_date<-max_date-years(1)
      filtered<-filtered[Order.Date>=min_date]
    }
    return(filtered)
  })
  sales_time_filtered_data<-reactive({
    filtered<-filtered_data()
    if(input$sales_period=="Last 6 Months"){
      max_date<-max(filtered$Order.Date,na.rm=TRUE)
      min_date<-max_date-months(6)
      filtered<-filtered[Order.Date>=min_date]
    }else if(input$sales_period=="Last Year"){
      max_date<-max(filtered$Order.Date,na.rm=TRUE)
      min_date<-max_date-years(1)
      filtered<-filtered[Order.Date>=min_date]
    }
    return(filtered)
  })
  forecast_time_filtered_data<-reactive({
    filtered<-filtered_data()
    if(input$forecast_period=="Last Year"){
      max_date<-max(filtered$Order.Date,na.rm=TRUE)
      min_date<-max_date-years(1)
      filtered<-filtered[Order.Date>=min_date]
    }else if(input$forecast_period=="Last 2 Years"){
      max_date<-max(filtered$Order.Date,na.rm=TRUE)
      min_date<-max_date-years(2)
      filtered<-filtered[Order.Date>=min_date]
    }
    return(filtered)
  })
  time_aggregated_data<-reactive({
    filtered_data<-time_filtered_data()
    if(input$timeframe=="Monthly"){
      monthly_data<-filtered_data[,.(Sales=sum(Sales,na.rm=TRUE),Profit=sum(Profit,na.rm=TRUE)),
                                  by=YearMonth]
      monthly_data<-monthly_data[order(YearMonth)]
      return(list(data=monthly_data,x_col="YearMonth",x_title="Month",format="%b %Y"))
    }else if(input$timeframe=="Quarterly"){
      quarterly_data<-filtered_data[,.(Sales=sum(Sales,na.rm=TRUE),Profit=sum(Profit,na.rm=TRUE)),
                                    by=.(Year,Quarter)]
      quarterly_data[,Period:=as.Date(paste0(Year,"-",Quarter*3,"-01"))]
      quarterly_data<-quarterly_data[order(Period)]
      return(list(data=quarterly_data,x_col="Period",x_title="Quarter",format="Q%q %Y"))
    }else{
      half_yearly_data<-filtered_data[,.(Sales=sum(Sales,na.rm=TRUE),Profit=sum(Profit,na.rm=TRUE)),
                                      by=.(Year,HalfYear)]
      half_yearly_data[,Period:=as.Date(paste0(Year,"-",ifelse(HalfYear==1,"01","07"),"-01"))]
      half_yearly_data<-half_yearly_data[order(Period)]
      return(list(data=half_yearly_data,x_col="Period",x_title="Half Year",format="H%q %Y"))
    }
  })
  sales_time_aggregated_data<-reactive({
    filtered_data<-sales_time_filtered_data()
    if(input$timeframe=="Monthly"){
      monthly_data<-filtered_data[,.(Total_Sales=sum(Sales,na.rm=TRUE)),by=YearMonth]
      monthly_data<-monthly_data[order(YearMonth)]
      return(list(data=monthly_data,x_col="YearMonth",format="%b %Y"))
    }else if(input$timeframe=="Quarterly"){
      quarterly_data<-filtered_data[,.(Total_Sales=sum(Sales,na.rm=TRUE)),by=.(Year,Quarter)]
      quarterly_data[,Period:=as.Date(paste0(Year,"-",Quarter*3,"-01"))]
      quarterly_data<-quarterly_data[order(Period)]
      return(list(data=quarterly_data,x_col="Period",format="Q%q %Y"))
    }else{
      half_yearly_data<-filtered_data[,.(Total_Sales=sum(Sales,na.rm=TRUE)),
                                      by=.(Year,HalfYear)]
      half_yearly_data[,Period:=as.Date(paste0(Year,"-",ifelse(HalfYear==1,"01","07"),"-01"))]
      half_yearly_data<-half_yearly_data[order(Period)]
      return(list(data=half_yearly_data,x_col="Period",format="H%q %Y"))
    }
  })
  output$total_sales<-renderValueBox({
    sales_sum<-sum(filtered_data()$Sales,na.rm=TRUE)
    valueBox(paste0("$",format(round(sales_sum),big.mark=",")),
             "Total Sales",icon=icon("dollar-sign"),color="green")
  })
  output$total_profit<-renderValueBox({
    profit_sum<-sum(filtered_data()$Profit,na.rm=TRUE)
    valueBox(paste0("$",format(round(profit_sum),big.mark=",")),
             "Total Profit",icon=icon("chart-line"),color="blue")
  })
  output$total_customers<-renderValueBox({
    customer_count<-length(unique(filtered_data()$Customer.ID))
    valueBox(customer_count,"Unique Customers",icon=icon("users"),color="purple")
  })
  output$monthly_performance<-renderPlotly({
    agg_data<-time_aggregated_data()
    plot_ly()%>%
      add_trace(data=agg_data$data,x=as.formula(paste0("~",agg_data$x_col)),y=~Sales,
                type="scatter",mode="lines+markers",name="Sales",
                line=list(color="#3498DB",width=3),
                marker=list(color="#3498DB",size=10),
                text=~paste0(format(get(agg_data$x_col),agg_data$format),"<br>",
                             "Sales: $",format(round(Sales),big.mark=",")),
                hoverinfo="text")%>%
      add_trace(data=agg_data$data,x=as.formula(paste0("~",agg_data$x_col)),y=~Profit,
                type="scatter",mode="lines+markers",name="Profit",
                line=list(color="#2ECC71",width=3),
                marker=list(color="#2ECC71",size=10),
                text=~paste0(format(get(agg_data$x_col),agg_data$format),"<br>",
                             "Profit: $",format(round(Profit),big.mark=",")),
                hoverinfo="text")%>%
      layout(title=list(text="Monthly Performance",font=list(family="Arial",size=18,color="#2c3e50")),
             xaxis=list(title=agg_data$x_title,gridcolor="#e9ecef",showgrid=TRUE),
             yaxis=list(title="Amount ($)",gridcolor="#e9ecef",showgrid=TRUE),
             legend=list(orientation="h",y=1.1),
             hovermode="closest",
             plot_bgcolor="#ffffff",
             paper_bgcolor="#ffffff")
  })
  output$sales_category_plot<-renderPlotly({
    category_data<-filtered_data()[,.(Total_Sales=sum(Sales,na.rm=TRUE)),by=Category]
    category_data<-category_data[order(-Total_Sales)]
    colors<-c("#4E79A7","#F28E2B","#E15759")[1:nrow(category_data)]
    plot_ly(data=category_data,x=~Category,y=~Total_Sales,type="bar",
            marker=list(color=colors,line=list(color="#FFFFFF",width=1)),
            text=~paste0("$",format(round(Total_Sales),big.mark=",")),
            textposition="auto",
            hoverinfo="text+x")%>%
      layout(title=list(text="Sales by Category",font=list(family="Arial",size=18,color="#2c3e50")),
             xaxis=list(title="",showgrid=FALSE),
             yaxis=list(title="Sales ($)",gridcolor="#e9ecef",showgrid=TRUE),
             showlegend=FALSE,plot_bgcolor="#ffffff",paper_bgcolor="#ffffff")
  })
  output$profit_region_plot<-renderPlotly({
    region_data<-filtered_data()[,.(Total_Profit=sum(Profit,na.rm=TRUE)),by=Region]
    region_data<-region_data[order(-Total_Profit)]
    colors<-c("#FFD966","#6AA84F","#E67C73","#8E7CC3")[1:nrow(region_data)]
    plot_ly(data=region_data,x=~Region,y=~Total_Profit,type="bar",
            marker=list(color=colors,line=list(color="#FFFFFF",width=1)),
            text=~paste0("$",format(round(Total_Profit),big.mark=",")),
            textposition="auto",
            hoverinfo="text+x")%>%
      layout(title=list(text="Profit by Region",font=list(family="Arial",size=18,color="#2c3e50")),
             xaxis=list(title="",showgrid=FALSE),
             yaxis=list(title="Profit ($)",gridcolor="#e9ecef",showgrid=TRUE),
             showlegend=FALSE,plot_bgcolor="#ffffff",paper_bgcolor="#ffffff")
  })
  output$sales_time_plot<-renderPlotly({
    agg_data<-sales_time_aggregated_data()
    plot_ly(data=agg_data$data,
            x=as.formula(paste0("~",agg_data$x_col)),
            y=~Total_Sales,
            type="scatter",mode="lines+markers",
            line=list(color="#3498DB",width=3),
            marker=list(color="#3498DB",size=10),
            text=~paste0(format(get(agg_data$x_col),agg_data$format),"<br>",
                         "$",format(round(Total_Sales),big.mark=",")),
            hoverinfo="text")%>%
      layout(title=list(text="Sales Over Time",font=list(family="Arial",size=18,color="#2c3e50")),
             xaxis=list(title="",gridcolor="#e9ecef",showgrid=TRUE),
             yaxis=list(title="Sales ($)",gridcolor="#e9ecef",showgrid=TRUE),
             plot_bgcolor="#ffffff",paper_bgcolor="#ffffff")
  })
  output$sales_subcat_plot<-renderPlotly({
    subcat_data<-filtered_data()[,.(Total_Sales=sum(Sales,na.rm=TRUE)),by=.(Sub.Category,Category)]
    subcat_data<-subcat_data[order(-Total_Sales)]
    if(nrow(subcat_data)>15){subcat_data<-subcat_data[1:15]}
    category_map<-c("Furniture"="#4E79A7","Office Supplies"="#F28E2B","Technology"="#E15759")
    colors<-category_map[subcat_data$Category]
    subcat_data$Sub.Category<-factor(subcat_data$Sub.Category,
                                     levels=subcat_data$Sub.Category[order(subcat_data$Total_Sales)])
    plot_ly(data=subcat_data,y=~Sub.Category,x=~Total_Sales,type="bar",orientation="h",
            marker=list(color=colors,line=list(color="#FFFFFF",width=1)),
            text=~paste0(Category," - ",Sub.Category,"<br>",
                         "$",format(round(Total_Sales),big.mark=",")),
            hoverinfo="text")%>%
      layout(title=list(text="Top Sub-Categories by Sales",font=list(family="Arial",size=18,color="#2c3e50")),
             xaxis=list(title="Sales ($)",gridcolor="#e9ecef",showgrid=TRUE),
             yaxis=list(title="",showgrid=FALSE,autorange="reversed"),
             legend=list(title="Category",orientation="h",y=1.1),
             showlegend=FALSE,plot_bgcolor="#ffffff",paper_bgcolor="#ffffff")
  })
  output$customer_segments_plot<-renderPlotly({
    segment_data<-filtered_data()[,.(Total_Sales=sum(Sales,na.rm=TRUE)),by=Segment]
    segment_data<-segment_data[order(-Total_Sales)]
    colors<-c("#F1948A","#85C1E9","#82E0AA")[1:nrow(segment_data)]
    segment_data[,Percentage:=round(Total_Sales/sum(Total_Sales)*100,1)]
    plot_ly(data=segment_data,labels=~Segment,values=~Total_Sales,type="pie",
            marker=list(colors=colors,line=list(color="#FFFFFF",width=1)),
            textinfo="label+percent",
            hoverinfo="text",
            text=~paste0(Segment,": $",format(round(Total_Sales),big.mark=",")," (",Percentage,"%)"),
            insidetextorientation="radial")%>%
      layout(title=list(text="Sales by Customer Segment",font=list(family="Arial",size=18,color="#2c3e50")),
             showlegend=TRUE,
             legend=list(orientation="h",xanchor="center",x=0.5,y=-0.1),
             plot_bgcolor="#ffffff",paper_bgcolor="#ffffff")
  })
  output$top_customers_plot<-renderPlotly({
    customer_data<-filtered_data()[,.(Total_Sales=sum(Sales,na.rm=TRUE)),by=.(Customer.Name,Segment)]
    customer_data<-customer_data[order(-Total_Sales)][1:10]
    segment_map<-c("Consumer"="#F1948A","Corporate"="#85C1E9","Home Office"="#82E0AA")
    colors<-segment_map[customer_data$Segment]
    customer_data$Customer.Name<-factor(customer_data$Customer.Name,
                                        levels=customer_data$Customer.Name[order(customer_data$Total_Sales)])
    plot_ly(data=customer_data,y=~Customer.Name,x=~Total_Sales,type="bar",orientation="h",
            marker=list(color=colors,line=list(color="#FFFFFF",width=1)),
            text=~paste0(Customer.Name," (",Segment,")<br>","$",format(round(Total_Sales),big.mark=",")),
            hoverinfo="text")%>%
      layout(title=list(text="Top 10 Customers by Sales",font=list(family="Arial",size=18,color="#2c3e50")),
             xaxis=list(title="Sales ($)",gridcolor="#e9ecef",showgrid=TRUE),
             yaxis=list(title="",showgrid=FALSE,autorange="reversed"),
             showlegend=FALSE,plot_bgcolor="#ffffff",paper_bgcolor="#ffffff")
  })
  output$customer_table<-renderDT({
    customer_summary<-filtered_data()[,.(
      Total_Sales=sum(Sales,na.rm=TRUE),
      Total_Profit=sum(Profit,na.rm=TRUE),
      Orders=length(unique(Order.ID)),
      Avg_Order=sum(Sales,na.rm=TRUE)/length(unique(Order.ID))
    ),by=.(Customer.Name,Segment)]
    customer_summary<-customer_summary[order(-Total_Sales)]
    datatable(customer_summary,
              options=list(pageLength=10,
                           scrollX=TRUE,
                           dom='<"top"flp>rt<"bottom"ip>',
                           columnDefs=list(list(
                             targets=c(2:5),
                             render=JS("function(data,type){if(type==='display'){return'$'+parseFloat(data).toLocaleString('en-US',{maximumFractionDigits:2});}return data;}")
                           ))
              ))%>%
      formatStyle('Total_Sales',
                  background=styleColorBar(customer_summary$Total_Sales,c('#E9F7EF','#1ABC9C')),
                  backgroundSize='98% 88%',
                  backgroundRepeat='no-repeat',
                  backgroundPosition='center')%>%
      formatStyle('Total_Profit',
                  background=styleColorBar(customer_summary$Total_Profit,c('#E9F7EF','#3498DB')),
                  backgroundSize='98% 88%',
                  backgroundRepeat='no-repeat',
                  backgroundPosition='center')
  })
  output$download_customer_data<-downloadHandler(
    filename=function(){paste("customer-data-",Sys.Date(),".csv",sep="")},
    content=function(file){
      customer_summary<-filtered_data()[,.(
        Total_Sales=sum(Sales,na.rm=TRUE),
        Total_Profit=sum(Profit,na.rm=TRUE),
        Orders=length(unique(Order.ID)),
        Avg_Order=sum(Sales,na.rm=TRUE)/length(unique(Order.ID))
      ),by=.(Customer.Name,Segment)]
      customer_summary<-customer_summary[order(-Total_Sales)]
      fwrite(customer_summary,file)
    }
  )
  output$forecast_plot<-renderPlotly({
    ts_data<-forecast_time_filtered_data()[,.(Total_Sales=sum(Sales,na.rm=TRUE)),by=YearMonth]
    if(nrow(ts_data)<12){
      return(plot_ly()%>%layout(title="Not enough data for forecast. Need at least 12 months."))
    }
    ts_data<-ts_data[order(YearMonth)]
    sales_ts<-ts(ts_data$Total_Sales,frequency=12)
    fit<-tryCatch({auto.arima(sales_ts)},error=function(e){arima(sales_ts,order=c(1,0,0))})
    fc<-tryCatch({forecast(fit,h=12)},error=function(e){NULL})
    if(is.null(fc)){
      return(plot_ly()%>%layout(title="Could not generate forecast due to data issues."))
    }
    forecast_dates<-seq.Date(from=max(ts_data$YearMonth)%m+%months(1),by="month",length.out=12)
    plot_ly()%>%
      add_trace(x=ts_data$YearMonth,y=ts_data$Total_Sales,type="scatter",mode="lines+markers",
                name="Historical Sales",line=list(color="#3498DB"),marker=list(color="#3498DB",size=6))%>%
      add_trace(x=forecast_dates,y=as.numeric(fc$mean),type="scatter",mode="lines",
                name="Forecast",line=list(color="#E74C3C",width=2,dash="dash"))%>%
      add_ribbons(x=forecast_dates,ymin=as.numeric(fc$lower[,2]),ymax=as.numeric(fc$upper[,2]),
                  name="95% Confidence",line=list(color="transparent"),fillcolor="rgba(231,76,60,0.2)")%>%
      layout(title=list(text="Sales Forecast (Next 12 Months)",font=list(family="Arial",size=18,color="#2c3e50")),
             xaxis=list(title="",gridcolor="#e9ecef",showgrid=TRUE,range=c(min(ts_data$YearMonth),max(forecast_dates))),
             yaxis=list(title="Sales ($)",gridcolor="#e9ecef",showgrid=TRUE),
             legend=list(orientation="h",y=1.1),hovermode="x unified",
             plot_bgcolor="#ffffff",paper_bgcolor="#ffffff")
  })
  output$geo_map<-renderLeaflet({
    if(input$geo_metric=="Sales"){
      geo_data<-filtered_data()[,.(Value=sum(Sales,na.rm=TRUE)),by=.(Country,Region,State)]
      geo_data<-merge(geo_data,states_coords,by="State",all.x=TRUE)
      title<-"Sales"
      color_palette<-"viridis"
    }else if(input$geo_metric=="Profit"){
      geo_data<-filtered_data()[,.(Value=sum(Profit,na.rm=TRUE)),by=.(Country,Region,State)]
      geo_data<-merge(geo_data,states_coords,by="State",all.x=TRUE)
      title<-"Profit"
      color_palette<-"RdYlGn"
    }else{
      geo_data<-filtered_data()[,.(Value=sum(Quantity,na.rm=TRUE)),by=.(Country,Region,State)]
      geo_data<-merge(geo_data,states_coords,by="State",all.x=TRUE)
      title<-"Quantity"
      color_palette<-"YlOrRd"
    }
    if(nrow(geo_data)==0){
      return(leaflet()%>%addTiles()%>%setView(lng=-98.5795,lat=39.8283,zoom=4))
    }
    pal<-colorNumeric(palette=color_palette,domain=geo_data$Value)
    leaflet(geo_data)%>%
      addProviderTiles(providers$CartoDB.DarkMatter)%>%
      addCircleMarkers(~lng,~lat,
                       radius=~ifelse(Value<0,5,sqrt(abs(Value))/50+5),
                       color=~pal(Value),
                       fillOpacity=0.8,
                       stroke=TRUE,
                       weight=1,
                       popup=~paste(
                         "<strong>State:</strong>",State,"<br>",
                         "<strong>Region:</strong>",Region,"<br>",
                         "<strong>",title,":</strong>",
                         ifelse(title%in%c("Sales","Profit"),
                                paste0("$",format(round(Value),big.mark=",")),
                                format(round(Value),big.mark=","))
                       ),
                       label=~State)%>%
      addLegend(position="bottomright",pal=pal,values=~Value,
                title=ifelse(title%in%c("Sales","Profit"),paste0(title," ($)"),title),
                opacity=0.8)%>%
      setView(lng=-98.5795,lat=39.8283,zoom=4)
  })
  output$profit_sales_plot<-renderPlotly({
    plot_data<-filtered_data()[,.(Category=Category,Sales=Sales,Profit=Profit,Product=Product.Name,
                                  Quantity=Quantity,Sub.Category=Sub.Category)]
    if(nrow(plot_data)==0){
      return(plot_ly()%>%layout(title="No data available for selected filters"))
    }
    colors<-c("#4E79A7","#F28E2B","#E15759")
    category_levels<-unique(plot_data$Category)
    color_map<-setNames(colors[1:length(category_levels)],category_levels)
    plot_ly(plot_data,x=~Sales,y=~Profit,color=~Category,colors=color_map,size=~Quantity,
            sizes=c(5,30),marker=list(opacity=0.7,sizemode="diameter"),
            text=~paste(
              "Product:",Product,"<br>",
              "Sub-Category:",Sub.Category,"<br>",
              "Sales: $",format(round(Sales),big.mark=","),"<br>",
              "Profit: $",format(round(Profit),big.mark=","),"<br>",
              "Quantity:",Quantity
            ),
            hoverinfo="text",type="scatter",mode="markers")%>%
      layout(title=list(text="Profit vs. Sales by Category",font=list(family="Arial",size=18,color="#2c3e50")),
             xaxis=list(title="Sales ($)",gridcolor="#e9ecef",showgrid=TRUE),
             yaxis=list(title="Profit ($)",gridcolor="#e9ecef",showgrid=TRUE),
             showlegend=TRUE,legend=list(orientation="h",y=1.1),
             shapes=list(list(type="line",x0=0,x1=1,xref="paper",y0=0,y1=0,yref="y",
                              line=list(color="rgba(100,100,100,0.5)",width=2,dash="dash"))),
             plot_bgcolor="#ffffff",paper_bgcolor="#ffffff")
  })
  output$profit_margin_plot<-renderPlotly({
    margin_data<-filtered_data()[,.(
      Sales=sum(Sales,na.rm=TRUE),
      Profit=sum(Profit,na.rm=TRUE),
      Count=.N
    ),by=.(Sub.Category,Category)]
    margin_data[,Margin:=Profit/Sales*100]
    margin_data<-margin_data[order(-Margin)]
    if(nrow(margin_data)>15){margin_data<-margin_data[1:15]}
    category_map<-c("Furniture"="#4E79A7","Office Supplies"="#F28E2B","Technology"="#E15759")
    colors<-category_map[margin_data$Category]
    margin_data$Sub.Category<-factor(margin_data$Sub.Category,
                                     levels=margin_data$Sub.Category[order(margin_data$Margin)])
    plot_ly(data=margin_data,y=~Sub.Category,x=~Margin,type="bar",orientation="h",
            marker=list(color=colors,line=list(color="#FFFFFF",width=1)),
            text=~paste0(
              Category," - ",Sub.Category,"<br>",
              "Margin: ",round(Margin,1),"%","<br>",
              "Sales: $",format(round(Sales),big.mark=","),"<br>",
              "Profit: $",format(round(Profit),big.mark=",")
            ),
            hoverinfo="text")%>%
      layout(title=list(text="Profit Margin by Sub-Category",font=list(family="Arial",size=18,color="#2c3e50")),
             xaxis=list(title="Profit Margin (%)",gridcolor="#e9ecef",showgrid=TRUE),
             yaxis=list(title="",showgrid=FALSE,autorange="reversed"),
             showlegend=FALSE,plot_bgcolor="#ffffff",paper_bgcolor="#ffffff")
  })
  output$data_table<-renderDT({
    datatable(data_table_filtered(),
              options=list(
                pageLength=10,scrollX=TRUE,autoWidth=TRUE,
                dom='<"top"flp>rt<"bottom"ip>',
                columnDefs=list(list(
                  targets=c(14,17,21),
                  render=JS("function(data,type){if(type==='display'){return'$'+parseFloat(data).toLocaleString('en-US',{maximumFractionDigits:2});}return data;}")
                )),
                initComplete=JS("function(settings,json){$('.dataTables_scrollBody').css('max-height','450px');}")
              ),
              escape=FALSE,rownames=FALSE,selection="multiple",
              class="display compact")
  })
  output$download_filtered_data<-downloadHandler(
    filename=function(){paste("filtered-data-",Sys.Date(),".csv",sep="")},
    content=function(file){fwrite(data_table_filtered(),file)}
  )
}
shinyApp(ui,server)