require(shiny)
require(shinyWidgets)
require(plotly)
require(countrycode)
require(rintrojs)
require(shinyTime)
require(htmlwidgets)
require(data.table)
require(redux)
require(stringr)

matrix_types <<- c("raw", "prct")
map_types <<- c("Packets", "Bytes", "Unique Scanners")
time_windows <<- c("10 Minutes", "Hourly", "Daily")
list_matrices <<- array(list(), dim=c(2,3,3), dimnames = list(matrix_types, map_types, time_windows));
list_port_matrices <<- array(list(), dim=c(2,3,3), dimnames = list(matrix_types, map_types, time_windows));
list_asn_matrices <<- array(list(), dim=c(2,3,3), dimnames = list(matrix_types, map_types, time_windows));
list_global_ind <<- array(list(), dim=c(3,3), dimnames = list(map_types, time_windows));
list_global_ind_ports <<- array(list(), dim=c(3,3), dimnames = list(map_types, time_windows));
list_global_ind_asn <<- array(list(), dim=c(3,3), dimnames = list(map_types, time_windows));
changedSlider <<- NULL
lastEventData <<- ""

# Gets corresponding data matrix from Redis DB
get_redis <- function(r, matrix_type, map_type, time_windows, type_val){
  if (type_val == 'country'){
    key <- paste(matrix_type, map_type, time_windows) 
  } else if (type_val == 'port') {
    key <- paste("Ports", matrix_type, map_type, time_windows)
  } else if (type_val == 'asn'){
    key <- paste("ASN", matrix_type, map_type, time_windows)
  }
  return(redux::bin_to_object(r$GET(key)))
}

# Only loads choropleth country data initially
populate_from_redis <- function(host = "127.0.0.1", port = 6379){
  s1 <- Sys.time()
  earliest_times <<- c(0,0,0,0,0)
  latest_times <<- c(0,0,0,0,0)
  earliest_times_asn <<- c(0,0,0,0,0)
  latest_times_asn <<- c(0,0,0,0,0)
  earliest_times_port <<- c(0,0,0,0,0)
  latest_times_port <<- c(0,0,0,0,0)
  names(earliest_times) <<- c("Early Packets", "Early Bytes", "Early uS 10 Minutes", "Early uS Hourly", "Early uS Daily")
  names(latest_times) <<- c("Late Packets", "Late Bytes", "Late uS 10 Minutes", "Late uS Hourly", "Late uS Daily")
  names(earliest_times_asn) <<- c("Early Packets", "Early Bytes", "Early uS 10 Minutes", "Early uS Hourly", "Early uS Daily")
  names(latest_times_asn) <<- c("Late Packets", "Late Bytes", "Late uS 10 Minutes", "Late uS Hourly", "Late uS Daily")
  names(earliest_times_port) <<- c("Early Packets", "Early Bytes", "Early uS 10 Minutes", "Early uS Hourly", "Early uS Daily")
  names(latest_times_port) <<- c("Late Packets", "Late Bytes", "Late uS 10 Minutes", "Late uS Hourly", "Late uS Daily")
  r <<- hiredis(host = host, port = port) 
  CC <<- bin_to_object(r$GET("countrycode"))
  unique_countries <<- bin_to_object(r$GET("unique_countries"))
  unique_ports <<- bin_to_object(r$GET("unique_ports"))
  unique_asn <<- bin_to_object(r$GET("unique_asn"))
  missing_time <<- bin_to_object(r$GET("missing_time"))
  for (i in names(earliest_times)){
    earliest_times[i] <<- r$GET(i)
    earliest_times_asn[i] <<- r$GET(paste("ASN", i))
    earliest_times_port[i] <<- r$GET(paste("port", i))
  }
  for (i in names(latest_times)){
    latest_times[i] <<- r$GET(i)
    latest_times_asn[i] <<- r$GET(paste("ASN", i))
    latest_times_port[i] <<- r$GET(paste("port", i))
  }
  for (time_window in time_windows){
    for (map_type in map_types){
      list_global_ind[map_type, time_window][[1]] <<- bin_to_object(r$GET(paste("Global Index", map_type, time_window)))
      for (matrix_type in matrix_types){
        list_matrices[matrix_type, map_type, time_window][[1]] <<- get_redis(r, matrix_type, map_type, time_window, "country")
      }
    }
  }
  e1 <- Sys.time()
  print(paste("Took", round(e1-s1,2), "seconds to load from Redis DB"))
}
print("Getting cached data from Redis DB")
populate_from_redis()

ui <- fluidPage(
  introjsUI(),
  
  # Setting color for error/warning message
  tags$head(
    tags$style(HTML("
      p {text-align: center;
        padding-left:5%;
      },
      
      .introjs-helperLayer {
          background: transparent;
      }
      
      .introjs-overlay {
          opacity: 0 !important;
      }
      
      .introjs-helperLayer:before {
          opacity: 0;
          content: '';
          position: fixed;
          width: inherit;
          height: inherit;
          border-radius: 0.5em;
          box-shadow: 0 0 0 1000em rgba(0,0,0, .7);
          opacity: 1;
      }
    "))
  ),
  tabsetPanel(id = "tabs",
              # tabPanel('Home', imageOutput("choropleth_pic")),
              tabPanel('Internet-wide Scanning Map', value = 'choropleth',
                       HTML("<p>This map depicts network traffic (packets, bytes, unique scanners, or IP addresses) detected 
                            worldwide from December 2019 to the present. Click"),
                       actionButton("helpChoropleth", "this button"),
                       HTML("to learn more</p>."),
                       plotlyOutput(outputId = "plotMap", width = "100%", height = "100%"),
                       fluidRow(
                         column(10, align="center", style="margin-top:-5%;",
                                div(style="display: inline-block;padding-right:2.5%;margin-left:20%;",img(src="merit_logo.png", height=40, width=80)),
                                div(style="display: inline-block;padding-right:2.5%;",img(src="umich_logo.png", height=50, width=50)),
                                div(style="display: inline-block;",img(src="nsf_logo.png", height=50, width=50)))
                       )
              ),
              tabPanel("Country-wise Summary", value = "summary",
                       HTML("<p>This plot summarizes the overall network traffic worldwide; you can also select k top countries 
                            with the highest overall network traffic. Click"),
                       actionButton("helpSummary", "this button"),
                       HTML("to learn more.</p>"),
                       absolutePanel(
                         id = "summaryPanel", draggable = TRUE, width = 150, height = 100, left = 200, top = 80, fixed = TRUE,
                         numericInput("kTopCountry",label = "K Top Countries", value=3, min = 1, max = 10, step = 1, width = "100%"),
                         style = "opacity: 0.85; z-index: 10;"
                       ),
                       plotlyOutput(outputId = "Summary", width = "100%", height = "120%")
              ),
              tabPanel('Country-wise Timeseries', value = "series",
                       HTML("<p>This plot shows the network traffic timeseries for selected countries from December 2019 to the present. Click"),
                       actionButton("helpTimeseries", "this button"),
                       HTML("to learn more.</p>"),
                       plotlyOutput("TS_plot", width = "100%", height = "100%"),
                       absolutePanel(
                         id = "tsPanel", draggable = TRUE, width = 150, height = 100, left = 200, top = 80, fixed = TRUE,
                         pickerInput("selected_countries", "Country/Countries", multiple = T, 
                                     choices = unique_countries, options = list(`actions-box` = TRUE), 
                                     selected = c("Russia", "Netherlands", "United States", "China")),
                         style = "opacity: 0.85; z-index: 10;"
                       )
              ),
              tabPanel("Port-wise Summary", value = "port_summary",
                       HTML("<p>This plot summarizes the overall network traffic worldwide with respect to ports; you can also select k 
                            top ports with the highest overall network traffic. Click"),
                       actionButton("helpSummaryPorts", "this button"),
                       HTML("to learn more.</p>"),
                       absolutePanel(
                         id = "summaryPortsPanel", draggable = TRUE, width = 150, height = 100, left = 200, top = 80, fixed = TRUE,
                         numericInput("kTopPort",label = "K Top Ports", value=10, min = 1, max = 20, step = 1, width = "100%"),
                         style = "opacity: 0.85; z-index: 10;"
                       ),
                       plotlyOutput(outputId = "SummaryPorts", width = "100%", height = "120%")
              ),
              tabPanel('Port-wise Timeseries', value = "port_series",
                       HTML("<p>This plot shows the network traffic timeseries for selected ports from December 2019 to the present. Click"),
                       actionButton("helpTimeseriesPorts", "this button"),
                       HTML("to learn more.</p>"),
                       absolutePanel(
                         id = "tsPortsPanel", draggable = TRUE, width = 150, height = 100, left = 200, top = 80, fixed = TRUE,
                         pickerInput("selected_ports", "Ports", multiple = T,
                                     choices = unique_ports, options = list(`actions-box` = TRUE), selected = c(80, 3395, 8081, 8291, 23)),
                         style = "opacity: 0.85; z-index: 10;"
                       ),
                       plotlyOutput("TS_plot_Ports", width = "100%", height = "100%")
              ),
              tabPanel("ASN-wise Summary", value = "asn_summary",
                       HTML("<p>This plot summarizes the overall network traffic worldwide with respect to ports; you can also select 
                            k top ports with the highest overall network traffic. Click"),
                       actionButton("helpSummaryASN", "this button"),
                       HTML("to learn more.</p>"),
                       absolutePanel(
                         id = "summaryASN_Panel", draggable = TRUE, width = 150, height = 100, left = 200, top = 80, fixed = TRUE,
                         numericInput("kTopASN",label = "K Top ASN", value=10, min = 1, max = 20, step = 1, width = "100%"),
                         style = "opacity: 0.85; z-index: 10;"
                       ),
                       plotlyOutput(outputId = "Summary_asn", width = "100%", height = "120%")
              ),
              tabPanel('ASN-wise Timeseries', value = "asn_series",
                       HTML("<p>This plot shows the network traffic timeseries for selected ports from December 2019 to the present. Click"),
                       actionButton("helpTimeseriesASN", "this button"),
                       HTML("to learn more.</p>"),
                       absolutePanel(
                         id = "tsASN_Panel", draggable = TRUE, width = 150, height = 100, left = 200, top = 80, fixed = TRUE,
                         pickerInput("selected_asns", "ASNs", multiple = T,
                                     choices = unique_asn, options = list(`actions-box` = TRUE), 
                                     selected = c("6939 Hurricane Electric LLC", "237 Merit Network, Inc.",
                                                  "7922 Comcast Cable Communications, LLC", "7018 AT&T Services, Inc.")),
                         style = "opacity: 0.85; z-index: 10;"
                       ),
                       plotlyOutput("TS_plot_asn", width = "100%", height = "100%")
              ),
              tabPanel('', value = "hidden")
  ),
  absolutePanel(
    id = "Opts", top=50, left = 25, width = 300, fixed = TRUE, height = 240, draggable = TRUE,
    
    selectInput(inputId = "map_type", label = "Modality", width = '50%',
                c("Packets","Bytes", "Unique Scanners")),
    
    selectInput("sliderStep", "Time Window", width = '50%',
                choices=c("Daily","Hourly","10 Minutes")),
    
    radioButtons("colorScheme", "Color Scheme:", 
                 c("Color Ramp" = "ramp", "Grayscale" = "gray")
    ),
    
    uiOutput("plotsTitle"),
    plotlyOutput("topTrafficRaw", height = "50%"),
    plotlyOutput("topTrafficPrct", height = "50%"),
    uiOutput("slider"),
    style = "opacity: 0.85; z-index: 9;"
  )
)

numextract <- function(string){ 
  return(str_extract(string, "\\-*\\d+\\.*\\d*"))
} 

# Gets index of column vector in matrix
get_index_in_matrix <- function(stepSize, sliderTime){
  vec_unix <- seq.int(start_unix, end_unix, 600*stepSize)
  D = vec_unix[2]-vec_unix[1];
  ind = min(max(floor((sliderTime - vec_unix[1])/D), 1), length(vec_unix))
  return(ind)
}

# Converts unixtime into datetime
to_date <- function(unix_time) {
  return(as.POSIXct(as.numeric(as.character(unix_time)), origin="1970-01-01", tz="UTC"))
}

# Converts datetime into unixtime
to_unix <- function(date) {
  return(as.numeric(as.POSIXct(date, origin="1970-01-01", tz = "UTC")))
}

# Returns top x largest countries, ports, ASN
largestTypes <- function(df, number_c){
  df <- df[order(-df$value),]
  largestTypes <- df[1:number_c]
  return(largestTypes)
}

# Returns top k largest countries overall
largest_countries_overall <- function(map_type, sliderStep, k){
  raw <- list_matrices["raw", map_type, sliderStep][[1]]
  sum_raw <- rowSums(raw)
  sum_raw <- sum_raw[order(-sum_raw)]
  return(names(sum_raw)[1:k])
}

# Returns ports generating top k traffic
topKports <- function(map_type, sliderStep, k){
  raw <- list_port_matrices["raw", map_type, sliderStep][[1]]
  sum_raw <- rowSums(raw)
  sum_raw <- sum_raw[order(-sum_raw)]
  return(names(sum_raw)[1:k])
}

# Returns ASNs generating top k traffic
topK_ASN <- function(map_type, sliderStep, k){
  raw <- list_asn_matrices["raw", map_type, sliderStep][[1]]
  sum_raw <- rowSums(raw)
  sum_raw <- sum_raw[order(-sum_raw)]
  return(names(sum_raw)[1:k])
}

# truncate packets and unique scanners
truncate_values <- function(val){
  if (val >= 1000000000){
    val <- paste(round(val/1000000000, 2), "Billion")
  }
  else if (val >= 1000000){
    val <- paste(round(val/1000000, 2), "Million")
  }
  return(val)
}

# truncate bytes into MB, GB
truncate_bytes <- function(bytes){
  if (bytes >= 1000000000){
    bytes <- paste(round(bytes / 1000000000, 2), "GB")
  }
  else if (bytes >= 1000000){
    bytes <- paste(round(bytes / 1000000, 2), "MB")
  }
  else if (bytes >= 1000){
    bytes <- paste(round(bytes / 1000, 2), "KB")
  }
  return(bytes)
}

# number of days it would take for an event with this scanning intensity
return_periods <- function(p){
  if (p >= 0.5){
    val <- round(1/(1-p), 2)
  } else {
    val <- round(1/p, 2)
  }
  if (stepSize == 144){
    return(paste(val, "days"))
  } else if (stepSize == 6){
    if (val >= 24){
      return(paste(round(val/24, 2), "days"))
    }
    return(paste(val, "hours"))
  } else {
    if (val >= 6){
      return(paste(round(val/6, 2), "hours"))
    }
    return(paste(val*10, "minutes"))
  }
}

# Samples top and bottom 10% data point plus every 10th point
sample_extreme_ind <- function(rank){
  # Gets indices for outliers and every 10th point
  low_idx <- which(rank <= 10)
  high_idx <- which(rank >= 90)
  tmp <- seq.int(1, length(rank), 10)
  sample_idx <- union(low_idx, high_idx)
  sample_idx <- union(tmp, sample_idx)
  return(sample_idx)
}

# Loads data matrices for ASNs
load_asn <- function() {
  s1 <- Sys.time()
  for (time_window in time_windows){
    for (map_type in map_types){
      list_global_ind_asn[map_type, time_window][[1]] <<- bin_to_object(r$GET(paste("Global ASN Index", map_type, time_window)))
      for (matrix_type in matrix_types){
        list_asn_matrices[matrix_type, map_type, time_window][[1]] <<- get_redis(r, matrix_type, map_type, time_window, "asn")
      }
    }
  }
  e1 <- Sys.time()
  print(paste("Loading ASN takes", round(e1-s1, 2) , "seconds"))
}

# Loads data matrices for ports
load_port <- function(){
  s1 <- Sys.time()
  for (time_window in time_windows){
    for (map_type in map_types){
      list_global_ind_ports[map_type, time_window][[1]] <<- bin_to_object(r$GET(paste("Global Port Index", map_type, time_window)))
      for (matrix_type in matrix_types){
        list_port_matrices[matrix_type, map_type, time_window][[1]] <<- get_redis(r, matrix_type, map_type, time_window, "port")
      }
    }
  }
  e1 <- Sys.time()
  print(paste("Loading port takes", round(e1-s1, 2) , "seconds"))
}

server <- function(input, output, session) {
  
  # Earliest and latest times adapt based on modality
  earliest_time <<- reactive({
    if (input$tabs == "asn_summary" || input$tabs == "asn_series"){
      if (input$map_type == "Packets"){
        earliest_times_asn[1]
      } else if (input$map_type == "Bytes"){
        earliest_times_asn[2]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "10 Minutes"){
        earliest_times_asn[3]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Hourly"){
        earliest_times_asn[4]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Daily"){
        earliest_times_asn[5]
      }
    } else if (input$tabs == "port_summary" || input$tabs == "port_series"){
      if (input$map_type == "Packets"){
        earliest_times_port[1]
      } else if (input$map_type == "Bytes"){
        earliest_times_port[2]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "10 Minutes"){
        earliest_times_port[3]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Hourly"){
        earliest_times_port[4]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Daily"){
        earliest_times_port[5]
      }
    } else if (input$map_type == "Packets"){
      earliest_times[1]
    } else if (input$map_type == "Bytes"){
      earliest_times[2]
    } else if (input$map_type == "Unique Scanners" && input$sliderStep == "10 Minutes"){
      earliest_times[3]
    } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Hourly"){
      earliest_times[4]
    } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Daily"){
      earliest_times[5]
    }
  })
  
  latest_time <<- reactive({
    if (input$tabs == "asn_summary" || input$tabs == "asn_series"){
      if (input$map_type == "Packets"){
        latest_times_asn[1]
      } else if (input$map_type == "Bytes"){
        latest_times_asn[2]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "10 Minutes"){
        latest_times_asn[3]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Hourly"){
        latest_times_asn[4]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Daily"){
        latest_times_asn[5]
      }
    } else if (input$tabs == "port_summary" || input$tabs == "port_series"){
      if (input$map_type == "Packets"){
        latest_times_port[1]
      } else if (input$map_type == "Bytes"){
        latest_times_port[2]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "10 Minutes"){
        latest_times_port[3]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Hourly"){
        latest_times_port[4]
      } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Daily"){
        latest_times_port[5]
      }
    } else if (input$map_type == "Packets"){
      latest_times[1]
    } else if (input$map_type == "Bytes"){
      latest_times[2]
    } else if (input$map_type == "Unique Scanners" && input$sliderStep == "10 Minutes"){
      latest_times[3]
    } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Hourly"){
      latest_times[4]
    } else if (input$map_type == "Unique Scanners" && input$sliderStep == "Daily"){
      latest_times[5]
    }
  })
  
  hideTab("tabs", target = "hidden", session = session)
  
  # Edit the help buttons
  observeEvent(input$helpSummary, {
    introjs(session, options = list(
      steps = data.frame(element = c("#Summary", "#Opts", "#Opts", "#kTopCountry","#Summary"),
                         intro = c("This tab aims to help users visualize and analyze global network traffic data at a specific time point.",
                                   "There are three types of modalities to choose from: packets, bytes, and unique scanners.
                                   The network packet time series shows the number of collected network packets from December 2019 up to the present. 
                                   You can hover over a point to see the number of packets collected at that time and zoom in to observe a smaller time interval.
                                   The bytes time series shows the size (in bytes) of packets we collected over the time interval.
                                   The unique scanners time series shows the number of unique IP addresses that we received packets from over the time interval.",
                                   "You can select one of three time windows: Daily, Hourly, or 10-Minutes to examine the summary timeseries.",
                                   "You can select the number of top countries from a minimum of 3 to a maximum of 10.",
                                   "Clicking on a point on this timeseries will transfer users to the choropleth tab at that specific time point."
                         ))
    ))
  })
  
  observeEvent(input$helpChoropleth, {
    introjs(session, options = list(
      steps = data.frame(element = c("#plotMap", "#Opts","#Opts #colorScheme", "#Opts #topTrafficRaw", "#Opts #topTrafficPrct", "#Opts #slider", "#plotMap"),
                         intro = c("This tab aims to help users visualize network traffic data across countries at a specific time point collected by Merit Network's Distributed Network Telescope.",
                                   "There are three types of modalities to choose from: packets, bytes, and unique scanners.
                                   The network packet time series shows the number of collected network packets from December 2019 up to the present. 
                                   You can hover over a point to see the number of packets collected at that time and zoom in to observe a smaller time interval.
                                   The bytes time series shows the size (in bytes) of packets we collected over the time interval.
                                   The unique scanners time series shows the number of unique IP addresses that we received packets from over the time interval.",
                                   "There are two choices for the color scheme: Users can choose to either look at a colormap choropleth or one that is monochromatic.",
                                   "This is the bar chart that reveals at this time point the 10 countries (descending) with the highest network traffic and their corresponding raw values. 
                                   The colors on the bar chart corresponds to the country's scanning index: redder colors indicate greater traffic while greener colors indicate lower traffic relative to the country's historical network traffic.
                                   The user may also click on a bar to examine that country's network traffic timeseries.",
                                   "This is the corresponding bar chart that shows the rank/percentile of the top 10 countries relative to their historical traffic activity.",
                                   "Moving the slider (adjusted accordingly with respect to the time window) will allow the user to examine network traffic data for all countries at different time points.",
                                   "Clicking on any country in this choropleth will transfer users to the timeseries tab, where the user will examine this country's network traffic from December 2019 up to the present."
                         ))
    ))
  })
  
  observeEvent(input$helpTimeseries, {
    introjs(session, options = list(
      steps = data.frame(element = c("#TS_plot", "#selected_countries"),
                         intro = c("This tab aims to help users visualize a country's network traffic timeseries from December 2019 up to the present.
                                   The user can switch modalities and time windows to examine trends in a country's network traffic. 
                                   Clicking on a point in this timeseries plot will transfer the user back to the choropleth tab at that specific time point that the user chose.",
                                   "You can select a group of countries to compare their timeseries."
                         ))
    ))
  })
  
  observeEvent(input$helpSummaryPorts, {
    introjs(session, options = list(
      steps = data.frame(element = c("#SummaryPorts", "#Opts", "#Opts", "#SummaryPorts"),
                         intro = c("This tab aims to help users visualize and analyze global network traffic data based on ports.",
                                   "There are three types of modalities to choose from: packets, bytes, and unique scanners.
                                   The network packet time series shows the number of collected network packets from December 2019 up to the present. 
                                   You can hover over a point to see the number of packets collected at that time and zoom in to observe a smaller time interval.
                                   The bytes time series shows the size (in bytes) of packets we collected over the time interval.
                                   The unique scanners time series shows the number of unique IP addresses that we received packets from over the time interval.",
                                   "You can select one of three time windows: Daily, Hourly, or 10-Minutes to examine the summary timeseries.",
                                   "Clicking on a point on this timeseries will transfer users to the choropleth tab at that specific time point."
                         ))
    ))
  })
  
  observeEvent(input$helpTimeseriesPorts, {
    introjs(session, options = list(
      steps = data.frame(element = c("#TS_plot_Ports", "#selected_ports"),
                         intro = c("This tab aims to help users visualize a country's network traffic timeseries from December 2019 up to the present.
                                   The user can switch modalities and time windows to examine trends in a country's network traffic. 
                                   Clicking on a point in this timeseries plot will transfer the user back to the choropleth tab at that specific time point that the user chose.",
                                   "You can select a group of ports to compare their timeseries."
                         ))
    ))
  })
  
  observeEvent(input$helpSummaryASN, {
    introjs(session, options = list(
      steps = data.frame(element = c("#Summary_asn", "#Opts", "#Opts", "#Summary_asn"),
                         intro = c("This tab aims to help users visualize and analyze global network traffic data based on ports.",
                                   "There are three types of modalities to choose from: packets, bytes, and unique scanners.
                                   The network packet time series shows the number of collected network packets from December 2019 up to the present. 
                                   You can hover over a point to see the number of packets collected at that time and zoom in to observe a smaller time interval.
                                   The bytes time series shows the size (in bytes) of packets we collected over the time interval.
                                   The unique scanners time series shows the number of unique IP addresses that we received packets from over the time interval.",
                                   "You can select one of three time windows: Daily, Hourly, or 10-Minutes to examine the summary timeseries.",
                                   "Clicking on a point on this timeseries will transfer users to the choropleth tab at that specific time point."
                         ))
    ))
  })
  
  observeEvent(input$helpTimeseriesASN, {
    introjs(session, options = list(
      steps = data.frame(element = c("#TS_plot_asn", "#selected_asns"),
                         intro = c("This tab aims to help users visualize a country's network traffic timeseries from December 2019 up to the present.
                                   The user can switch modalities and time windows to examine trends in a country's network traffic. 
                                   Clicking on a point in this timeseries plot will transfer the user back to the choropleth tab at that specific time point that the user chose.",
                                   "You can select a group of ports to compare their timeseries."
                         ))
    ))
  })
  
  # Renders tab for country summary
  output$Summary <- renderPlotly({
    validate(
      need(input$kTopCountry >= 3, "Please have at least k = 3 for top countries")
    )
    lastSliderStep <<- input$sliderStep
    lastMapType <<- input$map_type
    if (input$tabs == "summary"){
      time <- to_date(colnames(list_matrices["raw", input$map_type, input$sliderStep][[1]]))
      if (input$sliderStep == "Daily"){
        time <- as.POSIXct(format(round(time, "day"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      } else if (input$sliderStep == "Hourly"){
        time <- as.POSIXct(format(round(time, "hours"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      }
      
      largest_countries_summary <- largest_countries_overall(input$map_type, input$sliderStep, input$kTopCountry)
      countries_val <- list_matrices["raw", input$map_type, input$sliderStep][[1]][largest_countries_summary, ]
      countries_prct <- list_matrices["prct", input$map_type, input$sliderStep][[1]][largest_countries_summary,]
      
      summary_val <- list_global_ind[input$map_type, input$sliderStep][[1]][[1]]
      rank <- list_global_ind[input$map_type, input$sliderStep][[1]][[2]]
      if (input$sliderStep == "10 Minutes"){
        sample_idx <- sample_extreme_ind(rank)
        sample_idx <- sort(sample_idx)
        summary_val <- summary_val[sample_idx]
        rank <- rank[sample_idx]
        time <- time[sample_idx] 
        countries_val <- countries_val[, sample_idx]
        countries_prct <- countries_prct[,sample_idx]
      }
      if (input$map_type == "Bytes"){
        val <- sapply(summary_val, truncate_bytes)
      } else {
        val <- sapply(summary_val, truncate_values)
      }
      hover_text <- paste(time, "<br>Value:", val, input$map_type, "<br>Global Percentile:", rank,"%")
      title <- paste("<b>Captured", input$map_type, "Globally</b>, Grouped By Countries")
      fig <- plot_ly(x = time, y = summary_val, type = "scatter", 
                     hoverinfo = 'text', name = 'Overall',
                     mode = "lines", source = "Summary", text = hover_text) 
      
      fig <- fig %>% layout(title = title, 
                            xaxis = list(title = "Timestamp"), 
                            yaxis = list(title = paste("Prober", input$map_type)),
                            margin = list(l = 400, r = 100),
                            autosize = T)
      
      for (country in largest_countries_summary){
        if (input$map_type == "Bytes"){
          val <- sapply(countries_val[country,], truncate_bytes)
        } else {
          val <- sapply(countries_val[country,], truncate_values)
        }
        hover_text <- paste(country, "<br>", time, "<br>Value:", val, "<br>Global Percentile:", round(countries_prct[country,] * 100, 2), "%")
        fig <- fig %>% add_trace(y = countries_val[country,], name = country,
                                 mode = "lines", text = hover_text, 
                                 stackgroup = 'one', line = list(width = 0.5))
      }
      toWebGL(fig)
      fig 
    } 
  })
  
  # Allows changing to choropleth tab after clicking on points
  observe({
    d = event_data(event = "plotly_click", source = "Summary")
    if (!is.null(d)){
      if (lastSliderStep != input$sliderStep || lastMapType != input$map_type){
        print("Not supposed to change tabs")
      } else {
        changedSlider <<- d$x
        updateSlider(session, d$x, input$sliderStep)
        updateTabsetPanel(session, "tabs", selected = "choropleth") 
      }
    }
  })
  
  # Renders tab for ports summary
  output$SummaryPorts <- renderPlotly({
    validate(
      need(input$kTopPort >= 3, "Please have at least k = 3 for top ports")
    )
    lastSliderStep <<- input$sliderStep
    lastMapType <<- input$map_type
    if (input$tabs == "port_summary"){
      time <- to_date(colnames(list_port_matrices["raw", input$map_type, input$sliderStep][[1]]))
      if (input$sliderStep == "Daily"){
        time <- as.POSIXct(format(round(time, "day"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      } else if (input$sliderStep == "Hourly"){
        time <- as.POSIXct(format(round(time, "hours"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      }
      largest_ports_summary <- topKports(input$map_type, input$sliderStep, input$kTopPort)
      ports_val <- list_port_matrices["raw", input$map_type, input$sliderStep][[1]][largest_ports_summary, ]
      ports_prct <- list_port_matrices["prct", input$map_type, input$sliderStep][[1]][largest_ports_summary,]
      
      summary_val <- as.numeric(colSums(list_port_matrices["raw", input$map_type, input$sliderStep][[1]]))
      rank <- round(rank(summary_val) / length(summary_val), 4) * 100
      if (input$sliderStep == "10 Minutes"){
        sample_idx <- sample_extreme_ind(rank)
        sample_idx <- sort(sample_idx)
        summary_val <- summary_val[sample_idx]
        rank <- rank[sample_idx]
        time <- time[sample_idx] 
        ports_val <- ports_val[, sample_idx]
        ports_prct <- ports_prct[,sample_idx]
      }
      
      if (input$map_type == "Bytes"){
        val <- sapply(summary_val, truncate_bytes)
      } else {
        val <- sapply(summary_val, truncate_values)
      }
      hover_text <- paste(time, "<br>Value:", val, input$map_type, "<br>Global Percentile:", rank,"%")
      title <- paste("<b>Captured", input$map_type, "Globally, Grouped By Ports</b>")
      fig <- plot_ly(x = time, y = summary_val, type = "scatter", 
                     hoverinfo = 'text', name = 'Overall',
                     mode = "lines", source = "Summary", text = hover_text) 
      
      fig <- fig %>% layout(title = title, 
                            xaxis = list(title = "Timestamp"), 
                            yaxis = list(title = paste("Prober", input$map_type)),
                            margin = list(l = 400, t = 50, r = 150),
                            autosize = T)
      
      for (port in largest_ports_summary){
        if (input$map_type == "Bytes"){
          val <- sapply(ports_val[port,], truncate_bytes)
        } else {
          val <- sapply(ports_val[port,], truncate_values)
        }
        hover_text <- paste(port, "<br>", time, "<br>Value:", val, "<br>Global Percentile:", round(ports_prct[port,] * 100, 2), "%")
        fig <- fig %>% add_trace(y = ports_val[port,], name = port,
                                 mode = "lines", text = hover_text, 
                                 stackgroup = 'one', line = list(width = 0.5))
      }
      toWebGL(fig)
      fig  
    }
  })
  
  # Allows changing to choropleth tab after clicking on points
  observe({
    d = event_data(event = "plotly_click", source = "SummaryPorts")
    if (!is.null(d)){
      if (lastSliderStep != input$sliderStep || lastMapType != input$map_type){
        print("Not supposed to change tabs")
      } else {
        changedSlider <<- d$x
        updateSlider(session, d$x, input$sliderStep)
        updateTabsetPanel(session, "tabs", selected = "choropleth") 
      }
    }
  })
  
  # Renders tab for ASN summary
  output$Summary_asn <- renderPlotly({
    validate(
      need(input$kTopASN >= 3, "Please have at least k = 3 for top ASNs")
    )
    lastSliderStep <<- input$sliderStep
    lastMapType <<- input$map_type
    if (input$tabs == "asn_summary"){
      time <- to_date(colnames(list_asn_matrices["raw", input$map_type, input$sliderStep][[1]]))
      if (input$sliderStep == "Daily"){
        time <- as.POSIXct(format(round(time, "day"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      } else if (input$sliderStep == "Hourly"){
        time <- as.POSIXct(format(round(time, "hours"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      }
      largest_asn_summary <- topK_ASN(input$map_type, input$sliderStep, input$kTopASN)
      asn_val <- list_asn_matrices["raw", input$map_type, input$sliderStep][[1]][largest_asn_summary, ]
      asn_prct <- list_asn_matrices["prct", input$map_type, input$sliderStep][[1]][largest_asn_summary,]
      
      summary_val <- as.numeric(colSums(list_asn_matrices["raw", input$map_type, input$sliderStep][[1]]))
      rank <- round(rank(summary_val) / length(summary_val), 4) * 100
      if (input$sliderStep == "10 Minutes"){
        sample_idx <- sample_extreme_ind(rank)
        sample_idx <- sort(sample_idx)
        summary_val <- summary_val[sample_idx]
        rank <- rank[sample_idx]
        time <- time[sample_idx] 
        asn_val <- asn_val[, sample_idx]
        asn_prct <- asn_prct[,sample_idx]
      }
      
      if (input$map_type == "Bytes"){
        val <- sapply(summary_val, truncate_bytes)
      } else {
        val <- sapply(summary_val, truncate_values)
      }
      hover_text <- paste(time, "<br>Value:", val, input$map_type, "<br>Global Percentile:", rank,"%")
      title <- paste("<b>Captured", input$map_type, "Globally, Grouped By ASNs</b>")
      fig <- plot_ly(x = time, y = summary_val, type = "scatter", 
                     hoverinfo = 'text', name = 'Overall',
                     mode = "lines", source = "Summary", text = hover_text) 
      
      fig <- fig %>% layout(title = title, 
                            xaxis = list(title = "Timestamp"), 
                            yaxis = list(title = paste("Prober", input$map_type)),
                            margin = list(l = 400, t = 50, r = 150, p = 20),
                            autosize = T, 
                            height = 560,
                            legend = list(orientation = 'h', y = -0.2))
      
      for (asn in largest_asn_summary){
        if (input$map_type == "Bytes"){
          val <- sapply(asn_val[asn,], truncate_bytes)
        } else {
          val <- sapply(asn_val[asn,], truncate_values)
        }
        hover_text <- paste(asn, "<br>", time, "<br>Value:", val, "<br>Global Percentile:", round(asn_prct[asn,] * 100, 2), "%")
        fig <- fig %>% add_trace(y = asn_val[asn,], name = asn,
                                 mode = "lines", text = hover_text, 
                                 stackgroup = 'one', line = list(width = 0.5))
      }
      toWebGL(fig)
      fig  
    }
  })
  
  # Allows changing to choropleth tab after clicking on points
  observe({
    d = event_data(event = "plotly_click", source = "Summary_asn")
    if (!is.null(d)){
      if (lastSliderStep != input$sliderStep || lastMapType != input$map_type){
        print("Not supposed to change tabs")
      } else {
        changedSlider <<- d$x
        updateSlider(session, d$x, input$sliderStep)
        updateTabsetPanel(session, "tabs", selected = "choropleth") 
      }
    }
  })
  
  # Renders tab for choropleth
  output$plotMap <- renderPlotly({
    if (input$tabs == "choropleth"){
      req(input$sliderDate, input$map_type, input$colorScheme)
      isolate({
        # light grey boundaries
        l <- list(color = toRGB("grey"), width = 0.5)
        
        # specify map projection/options
        g <- list(
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Mercator')
        )
        
        if (input$colorScheme == "gray"){
          choice_colors <- colorRamp(c("white", "black"))
        } else {
          choice_colors <- colorRamp(c("green","red"))
        }
        
        sliderValue <<- input$sliderDate
        if (!is.null(changedSlider)){
          sliderValue <<- changedSlider
          changedSlider <<- NULL
        }
        
        #Acquire indexed column of matrix
        ind <- get_index_in_matrix(stepSize, to_unix(sliderValue))
        raw_value <- (list_matrices["raw", input$map_type, input$sliderStep][[1]])[, ind]
        prct_value <- (list_matrices["prct", input$map_type, input$sliderStep][[1]])[, ind]
        if (input$map_type == "Packets"){
          type <- "Total Prober Packets:"
        } else if (input$map_type == "Bytes"){
          type <- "Total Prober Bytes:"
        } else {
          type <- "Total Unique Scanners:"
        }
        val <- list_global_ind[input$map_type, input$sliderStep][[1]][[1]][ind]
        rank <- list_global_ind[input$map_type, input$sliderStep][[1]][[2]][ind]
        
        # Checking if this is part of missing time
        current_time <- to_unix(sliderValue)
        for (time in missing_time){
          if (current_time >= time && current_time <= time + 600*stepSize){
            showNotification("There is missing data at this specific time.", type = "warning")
            break()
          }
        }
        
        # Calculating return period (# of days) for the scanning intensity of countries
        return_pd <- sapply(prct_value, return_periods)
        prct_value <- round(prct_value*100, digits = 2)
        df_map <- data.table("value" = raw_value, "rank" = prct_value)
        
        
        # Filling in countrycodes and hovertext
        df_map$country <- unique_countries
        if(input$map_type == "Packets") {
          df_map$trunc_value = sapply(df_map$value, truncate_values)
          df_map$hover <- with(df_map, paste(df_map$country, '<br>Total Packets:', df_map$trunc_value,
                                             "<br>Percentile:", df_map$rank,"%", "<br>Return Period:", return_pd))
        }
        else if(input$map_type == "Bytes"){
          bytes_truncated <- sapply(df_map$value, truncate_bytes)
          df_map$hover <- with(df_map, paste(df_map$country, '<br>Total Bytes:', bytes_truncated, 
                                             '<br>Percentile:', df_map$rank,"%", "<br>Return Period:", return_pd))
        } else {
          df_map$trunc_value = sapply(df_map$value, truncate_values)
          df_map$hover <- with(df_map, paste(df_map$country, '<br>Total Unique Scanners:', 
                                             df_map$trunc_value, "<br>Percentile:", df_map$rank,"%", "<br>Return Period:", return_pd))
        }
        df_map$CC = CC
        df_map <<- df_map
        
        #Color scheme: Grayscale, ramp
        p <- plot_geo(df_map, source = "plotMap") %>%
          add_trace(
            z = ~rank, color = ~rank, colors = choice_colors,
            text = ~hover, locations = ~CC, marker = list(line = l)
          ) %>%
          colorbar(title = 'Traffic Index') %>%
          layout(
            title = paste("<b>Scanning Intensity</b> For", sliderValue, "UTC", "<b>",type,"</b>", truncate_values(val), "(",rank,"%)"),
            margin = list(l = 50, r = 0, b = 0, t = 40, pad = 0),
            geo = g
          ) 
      })
    }
  })
  
  outputOptions(output, "plotMap", priority = 10)
  
  # Allows changing to country timeseries tab after clicking on points
  # TODO: Implement functionality to also shift to ports, ASN timeseries
  observe({
    d = event_data(event = "plotly_click", source = "plotMap")
    if (!is.null(d)){
      selected_country <<- paste(df_map[d[,2]+1]$country)
      updatePickerInput(session, "selected_countries", selected = selected_country)
      updateTabsetPanel(session, "tabs", selected = "series")
    } 
  })
  
  # Renders the slider initially
  output$slider <- renderUI({
    if (input$sliderStep == "Daily"){
      sliderValue <<- as.Date(round(to_date(latest_time()), "day")) - 1
      minDate = as.Date(round(to_date(earliest_time()), "day"))
      maxDate = as.Date(round(to_date(latest_time()), "day")) - 1
      timeStep = 1
      stepSize <<- 144
    } else if (input$sliderStep == "Hourly"){
      sliderValue <<- round(to_date(latest_time()), "hour") - 3600
      minDate = round(to_date(earliest_time()), "hour")
      maxDate = round(to_date(latest_time()), "hour")  - 3600
      timeStep = 3600
      stepSize <<- 6
    } else if (input$sliderStep == "10 Minutes"){
      sliderValue <<- to_date(latest_time())
      minDate = to_date(earliest_time())
      maxDate = to_date(latest_time())
      timeStep = 600
      stepSize <<- 1
    }
    
    start_unix <<- to_unix(minDate)
    end_unix <<- to_unix(maxDate)
    vec_unix <<- seq.int(start_unix, end_unix, 600*stepSize)
    
    sliderInput(inputId = "sliderDate",
                label = "Dates:",
                value = sliderValue,
                min = minDate, 
                max = maxDate,
                step = timeStep,
                timezone = "+0000",
                animate = animateOptions(duration = 3000))  
  })
  
  output$plotsTitle <- renderUI({
    div(
      style = "text-align:center, font-size:15px", paste("Top 10 Countries (Raw / Percentile)")
    )
  })
  
  # Changes the dataframe for the bar charts on sidebar panel
  largest_c_raw_df <- reactive({
    req(input$sliderDate, input$map_type, input$colorScheme)
    if (input$colorScheme == "gray"){
      choice_colors <- colorRamp(c("white", "black"))
    } else {
      choice_colors <- colorRamp(c("green","red"))
    }
    # Find the largest ASNs, ports, or countries for slider time
    sliderValue <<- input$sliderDate
    ind <- get_index_in_matrix(stepSize, to_unix(sliderValue))
    if (input$tabs == "port_summary" || input$tabs == "port_series"){
      if (length(list_port_matrices["raw", "Packets", "Daily"][[1]]) == 0){
        load_port()
      }
      df_ports <- data.table("value" = (list_port_matrices["raw", input$map_type, input$sliderStep][[1]])[, ind], 
                             "rank" = round((list_port_matrices["prct", input$map_type, input$sliderStep][[1]])[, ind], 4)) * 100
      df_ports$port <- rownames(list_port_matrices["raw", input$map_type, input$sliderStep][[1]])
      largest_ports <<- largestTypes(df_ports, 10)
      
      if (input$map_type == "Bytes"){
        largest_ports$trunc_value = sapply(largest_ports$value, truncate_bytes)
      } else {
        largest_ports$trunc_value = sapply(largest_ports$value, truncate_values)
      }
      
      largest_ports$CC = largest_ports$port
      largest_ports$hover = with(largest_ports, paste(largest_ports$port, '<br>Total', input$map_type, ':', 
                                                        largest_ports$trunc_value, "<br>Percentile:", largest_ports$rank,"%"))
      
      largest_df = largest_ports
    } else if (input$tabs == "asn_summary" || input$tabs == "asn_series"){
      if (length(list_asn_matrices["raw", "Packets", "Daily"][[1]]) == 0){
        load_asn()
      }
      df_asns <- data.table("value" = (list_asn_matrices["raw", input$map_type, input$sliderStep][[1]])[, ind], 
                            "rank" = round((list_asn_matrices["prct", input$map_type, input$sliderStep][[1]])[, ind], 4)) * 100
      df_asns$asn <- rownames(list_asn_matrices["raw", input$map_type, input$sliderStep][[1]])
      largest_asns <<- largestTypes(df_asns, 10)
      
      if (input$map_type == "Bytes"){
        largest_asns$trunc_value = sapply(largest_asns$value, truncate_bytes)
      } else {
        largest_asns$trunc_value = sapply(largest_asns$value, truncate_values) 
      }
      
      largest_asns$CC = numextract(largest_asns$asn)
      largest_asns$hover = with(largest_asns, paste(largest_asns$asn, '<br>Total', input$map_type, ':', 
                                                      largest_asns$trunc_value, "<br>Percentile:", largest_asns$rank,"%"))
      largest_df = largest_asns
    } else {
      raw_value <- (list_matrices["raw", input$map_type, input$sliderStep][[1]])[, ind]
      prct_value <- (list_matrices["prct", input$map_type, input$sliderStep][[1]])[, ind]
      return_pd <- sapply(prct_value, return_periods)
      prct_value <- round(prct_value*100, digits = 2)
      df_map <- data.table("value" = raw_value, "rank" = prct_value)
      df_map$country <- unique_countries
      if(input$map_type != "Packets") {
        df_map$trunc_value = sapply(df_map$value, truncate_values)
        df_map$hover <- with(df_map, paste(df_map$country, '<br>Total Packets:', df_map$trunc_value,
                                           "<br>Percentile:", df_map$rank,"%", "<br>Return Period:", return_pd))
      }
      else if(input$map_type == "Bytes"){
        bytes_truncated <- sapply(df_map$value, truncate_bytes)
        df_map$hover <- with(df_map, paste(df_map$country, '<br>Total Bytes:', bytes_truncated, 
                                           '<br>Percentile:', df_map$rank,"%", "<br>Return Period:", return_pd))
      } else {
        df_map$trunc_value = sapply(df_map$value, truncate_values)
        df_map$hover <- with(df_map, paste(df_map$country, '<br>Total Unique Scanners:', 
                                           df_map$trunc_value, "<br>Percentile:", df_map$rank,"%", "<br>Return Period:", return_pd))
      }
      df_map$CC = CC
      
      # Find the largest countries (raw or percentiles) and returns as a dataframe
      largest_c_raw <<- largestTypes(df_map, 10)
      largest_df = largest_c_raw
    }
    tmp <- choice_colors(with(largest_df, ((rank - min(rank)) / diff(range(rank)))))
    largest_df$color <- rgb(tmp, maxColorValue = 256)
    s <- input$sliderDate
    t <- input$sliderStep
    m <- input$map_type
    v <- input$colorScheme
    
    if (TRUE){
      largest_df
    }
  })
  
  # Changes the bar charts to top 10 largest countries, ports, or ASNs
  output$topTrafficRaw <- renderPlotly({
    validate(
      need(input$tabs == "choropleth" || exists("df_map"), "Please wait for choropleth to render")
    )
    req(input$sliderDate)
    if (input$map_type == "Packets"){
      val <- "Total Packets"
    } else if (input$map_type == "Bytes"){
      val <- "Total Bytes"
    } else {
      val <- "Total Unique Scanners"
    }
    x_title <- list(
      title = "Country Codes"
    )
    fig <- plot_ly(largest_c_raw_df(), x = ~reorder(CC, -value), y = ~value, type = "bar", 
                   text=~hover, source = "topTrafficRaw", showlegend = F,
                   marker = list(color = ~color))
    fig <- fig %>% layout(xaxis = x_title) 
  })
  
  # Allows changing to appropriate timeseries tab after clicking on points
  observe({
    d = event_data(event = "plotly_click", source = "topTrafficRaw")
    if (!is.null(d) && d$x != lastEventData){
      lastEventData <<- d$x;
      df <- largest_c_raw_df();
      if (input$tabs == "port_summary" || input$tabs == "port_series") {
        selected_port <<- paste(df[df$port == d$x]$port)
        updatePickerInput(session, "selected_ports", selected = selected_port)
        updateTabsetPanel(session, "tabs", "hidden")
        updateTabsetPanel(session, "tabs", selected = "port_series")
      } else if (input$tabs == "asn_summary" || input$tabs == "asn_series"){
        selected_asn <<- paste(df[df$CC == d$x]$asn)
        updatePickerInput(session, "selected_asns", selected = selected_asn)
        updateTabsetPanel(session, "tabs", "hidden")
        updateTabsetPanel(session, "tabs", selected = "asn_series")
      } else {
        selected_country <<- paste(df_map[df_map$CC == d$x]$country)
        updatePickerInput(session, "selected_countries", selected = selected_country)
        updateTabsetPanel(session, "tabs", "hidden")
        updateTabsetPanel(session, "tabs", selected = "series")
      }
    }
  })
  
  output$topTrafficPrct <- renderPlotly({
    validate(
      need(input$tabs == "choropleth" || exists("df_map"), "Please wait for choropleth to render")
    )
    req(input$sliderDate)
    if (input$map_type == "Packets"){
      val <- "Packets Percentile"
    } else {
      val <- "Bytes Percentile"
    }
    x_title <- list(
      title = "Country Codes"
    )
    fig <- plot_ly(largest_c_raw_df(), x = ~reorder(CC, -value), y = ~rank, type = "bar", 
                   text=~hover, source = "topTrafficPrct", showlegend=F,
                   marker = list(color = ~color))
    fig <- fig %>% layout(xaxis = x_title) 
  })
  
  observe({
    d = event_data(event = "plotly_click", source = "topTrafficPrct")
    if (!is.null(d) && d$x != lastEventData){
      lastEventData <<- d$x;
      df <- largest_c_raw_df();
      if (input$tabs == "port_summary" || input$tabs == "port_series") {
        selected_port <<- paste(df[df$port == d$x]$port)
        updatePickerInput(session, "selected_ports", selected = selected_port)
        updateTabsetPanel(session, "tabs", "hidden")
        updateTabsetPanel(session, "tabs", selected = "port_series")
      } else if (input$tabs == "asn_summary" || input$tabs == "asn_series"){
        selected_asn <<- paste(df[df$CC == d$x]$asn)
        updatePickerInput(session, "selected_asns", selected = selected_asn)
        updateTabsetPanel(session, "tabs", "hidden")
        updateTabsetPanel(session, "tabs", selected = "asn_series")
      } else {
        selected_country <<- paste(df_map[df_map$CC == d$x]$country)
        updatePickerInput(session, "selected_countries", selected = selected_country)
        updateTabsetPanel(session, "tabs", "hidden")
        updateTabsetPanel(session, "tabs", selected = "series")
      }
    }
  })

  # Renders timeseries for countries
  output$TS_plot <- renderPlotly({
    validate(
      need(length(input$selected_countries) > 0, "Please select a country/countries to see their timeseries")
    )
    lastSliderStep <<- input$sliderStep
    lastMapType <<- input$map_type
    if (input$tabs == "series"){
      value <- as.matrix(list_matrices["raw", input$map_type, input$sliderStep][[1]][input$selected_countries,])
      if (length(input$selected_countries) == 1){
        time <- to_date(rownames(value)) 
        title <- paste("<b>Captured", input$map_type, "from", input$selected_countries, "</b>") 
      } else {
        time <- to_date(colnames(value))
        title <- paste("<b>Captured", input$map_type, "from Multiple Countries</b>")
      }
      if (input$sliderStep == "Daily"){
        time <- as.POSIXct(format(round(time, "day"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      } else if (input$sliderStep == "Hourly"){
        time <- as.POSIXct(format(round(time, "hours"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      }
      rank <- round(as.matrix(list_matrices["prct", input$map_type, input$sliderStep][[1]][input$selected_countries,]), 4) * 100
      
      if (input$sliderStep == "10 Minutes" & length(input$selected_countries) > 1){
        overall_traffic <- as.numeric(colSums(value))
        overall_rank <- round(rank(overall_traffic) / length(overall_traffic), 4) * 100
        sample_idx <- sample_extreme_ind(overall_rank)
        sample_idx <- sort(sample_idx)
        rank <- rank[, sample_idx]
        time <- time[sample_idx] 
        value <- value[, sample_idx]
      }
      
      fig <- plot_ly(type = "scatter", mode = "lines", source = "TS_plot", stackgroup = 'one', line = list(width = 0.5))
      fig <- fig %>% layout(title = title,
                            xaxis = list(title = "Timestamp"), 
                            yaxis = list(title = paste("Prober", input$map_type)),
                            margin = list(l = 425, t = 50),
                            autosize = T)
      
      for (country in input$selected_countries){
        if (length(input$selected_countries) == 1){
          if (input$map_type == "Bytes"){
            hover_val <- sapply(value, truncate_bytes)
          } else {
            hover_val <- sapply(value, truncate_values)
          }
          hover_text <- paste(time, "<br>Value:", hover_val, input$map_type,"<br>Percentile:", rank,"%")
          fig <- fig %>% add_trace(x = time, y = value, hoverinfo = 'text', name = country, text = hover_text)
        } else {
          if (input$map_type == "Bytes"){
            hover_val <- sapply(value[country,], truncate_bytes)
          } else {
            hover_val <- sapply(value[country,], truncate_values)
          }
          hover_text <- paste(time, "<br>", input$map_type, hover_val,"<br>Percentile:", rank[country,],"%")
          fig <- fig %>% add_trace(x = time, y = value[country,], hoverinfo = 'text', name = country, text = hover_text)
          toWebGL(fig)
        }
      }
      fig
    }
  })
  
  # Allows changing to choropleth tab after clicking on points
  observe({
    d = event_data(event = "plotly_click", source = "TS_plot")
    if (!is.null(d)){
      if (lastSliderStep != input$sliderStep || lastMapType != input$map_type){
        print("Not supposed to change tabs")
      } else {
        changedSlider <<- d$x
        updateSlider(session, d$x, input$sliderStep)
        updateTabsetPanel(session, "tabs", selected = "choropleth") 
      }
    }
  })
  
  # Renders timeseries for ports
  output$TS_plot_Ports <- renderPlotly({
    validate(
      need(length(input$selected_ports) > 0, "Please select a port/ports to see their timeseries")
    )
    lastSliderStep <<- input$sliderStep
    lastMapType <<- input$map_type
    if (input$tabs == "port_series"){
      value <- as.matrix(list_port_matrices["raw", input$map_type, input$sliderStep][[1]][input$selected_ports,])
      if (length(input$selected_ports) == 1){
        time <- to_date(rownames(value)) 
        port <- paste("Port", input$selected_ports)
        title <- paste("<b>Captured", input$map_type, "from", port, "</b>") 
      } else {
        time <- to_date(colnames(value))
        title <- paste("<b>Captured", input$map_type, "from Multiple Ports</b>")
      }
      if (input$sliderStep == "Daily"){
        time <- as.POSIXct(format(round(time, "day"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      } else if (input$sliderStep == "Hourly"){
        time <- as.POSIXct(format(round(time, "hours"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      }
      rank <- round(as.matrix(list_port_matrices["prct", input$map_type, input$sliderStep][[1]][input$selected_ports,]), 4) * 100
      
      if (input$sliderStep == "10 Minutes" & length(input$selected_ports) > 1){
        overall_traffic <- as.numeric(colSums(value))
        overall_rank <- round(rank(overall_traffic) / length(overall_traffic), 4) * 100
        sample_idx <- sample_extreme_ind(overall_rank)
        sample_idx <- sort(sample_idx)
        rank <- rank[, sample_idx]
        time <- time[sample_idx] 
        value <- value[, sample_idx]
      }
      
      fig <- plot_ly(type = "scatter", mode = "lines", source = "TS_plot_Ports", stackgroup = 'one', line = list(width = 0.5))
      fig <- fig %>% layout(title = title,
                            xaxis = list(title = "Timestamp"), 
                            yaxis = list(title = paste("Prober", input$map_type)),
                            margin = list(l = 425, t = 50),
                            autosize = T)
      
      for (port in input$selected_ports){
        if (length(input$selected_ports) == 1){
          if (input$map_type == "Bytes"){
            hover_val <- sapply(value, truncate_bytes)
          } else {
            hover_val <- sapply(value, truncate_values)
          }
          hover_text <- paste(time, "<br>Value:", hover_val, input$map_type,"<br>Percentile:", rank,"%")
          fig <- fig %>% add_trace(x = time, y = value, hoverinfo = 'text', name = port, text = hover_text)
        } else {
          if (input$map_type == "Bytes"){
            hover_val <- sapply(value[port,], truncate_bytes)
          } else {
            hover_val <- sapply(value[port,], truncate_values)
          }
          hover_text <- paste(time, "<br>", input$map_type, hover_val,"<br>Percentile:", rank[port,],"%")
          fig <- fig %>% add_trace(x = time, y = value[port,], hoverinfo = 'text', name = port, text = hover_text)
          toWebGL(fig)
        }
      }
      fig
    }
  })
  
  # Allows changing to choropleth tab after clicking on points
  observe({
    d = event_data(event = "plotly_click", source = "TS_plot_Ports")
    if (!is.null(d)){
      if (lastSliderStep != input$sliderStep || lastMapType != input$map_type){
        print("Not supposed to change tabs")
      } else {
        changedSlider <<- d$x
        updateSlider(session, d$x, input$sliderStep)
        updateTabsetPanel(session, "tabs", selected = "choropleth") 
      }
    }
  })
  
  # Renders timeseries for ASNs
  output$TS_plot_asn <- renderPlotly({
    validate(
      need(length(input$selected_asns) > 0, "Please select an ASN/ASNs to see their timeseries")
    )
    lastSliderStep <<- input$sliderStep
    lastMapType <<- input$map_type
    if (input$tabs == "asn_series"){
      value <- as.matrix(list_asn_matrices["raw", input$map_type, input$sliderStep][[1]][input$selected_asns,])
      if (length(input$selected_asns) == 1){
        time <- to_date(rownames(value)) 
        title <- paste("<b>Captured", input$map_type, "from", input$selected_asns, "</b>") 
      } else {
        time <- to_date(colnames(value))
        title <- paste("<b>Captured", input$map_type, "from Multiple ASNs</b>")
      }
      if (input$sliderStep == "Daily"){
        time <- as.POSIXct(format(round(time, "day"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      } else if (input$sliderStep == "Hourly"){
        time <- as.POSIXct(format(round(time, "hours"), '%Y-%m-%d %H:%M:%S UTC'), tz = "UTC")
      }
      rank <- round(as.matrix(list_asn_matrices["prct", input$map_type, input$sliderStep][[1]][input$selected_asns,]), 4) * 100
      
      if (input$sliderStep == "10 Minutes" & length(input$selected_asns) > 1){
        overall_traffic <- as.numeric(colSums(value))
        overall_rank <- round(rank(overall_traffic) / length(overall_traffic), 4) * 100
        sample_idx <- sample_extreme_ind(overall_rank)
        sample_idx <- sort(sample_idx)
        rank <- rank[, sample_idx]
        time <- time[sample_idx] 
        value <- value[, sample_idx]
      }
      
      fig <- plot_ly(type = "scatter", mode = "lines", source = "TS_plot_asn", stackgroup = 'one', line = list(width = 0.5))
      fig <- fig %>% layout(title = title,
                            xaxis = list(title = "Timestamp"), 
                            yaxis = list(title = paste("Prober", input$map_type)),
                            margin = list(l = 425, t = 50),
                            autosize = T,
                            height = 560,
                            legend = list(orientation = 'h', y = -0.2))
      
      for (asn in input$selected_asns){
        if (length(input$selected_asns) == 1){
          if (input$map_type == "Bytes"){
            hover_val <- sapply(value, truncate_bytes)
          } else {
            hover_val <- sapply(value, truncate_values)
          }
          hover_text <- paste(time, "<br>Value:", hover_val, input$map_type,"<br>Percentile:", rank,"%")
          fig <- fig %>% add_trace(x = time, y = value, hoverinfo = 'text', name = asn, text = hover_text)
        } else {
          if (input$map_type == "Bytes"){
            hover_val <- sapply(value[asn,], truncate_bytes)
          } else {
            hover_val <- sapply(value[asn,], truncate_values)
          }
          hover_text <- paste(time, "<br>", input$map_type, hover_val,"<br>Percentile:", rank[asn,],"%")
          fig <- fig %>% add_trace(x = time, y = value[asn,], hoverinfo = 'text', name = asn, text = hover_text)
          fig <- fig %>% layout(legend = list(orientation = 'h'))
          toWebGL(fig)
        }
      }
      fig
    }
  })
  
  # Allows changing to choropleth tab after clicking on points
  observe({
    d = event_data(event = "plotly_click", source = "TS_plot_asn")
    if (!is.null(d)){
      if (lastSliderStep != input$sliderStep || lastMapType != input$map_type){
        print("Not supposed to change tabs")
      } else {
        changedSlider <<- d$x
        updateSlider(session, d$x, input$sliderStep)
        updateTabsetPanel(session, "tabs", selected = "choropleth") 
      }
    }
  })
  
  # Updates slider based on modality, time window, traffic level
  updateSlider <- function(session, sliderTime, sliderStep){
    if (sliderStep == "Daily"){
      sliderTime <- as.Date(sliderTime)
      minDate = as.Date(round(to_date(earliest_time()), "day"))
      maxDate = as.Date(round(to_date(latest_time()), "day")) - 1
      timeStep = 1
    } else if (sliderStep == "Hourly"){
      sliderTime <- as.POSIXct(sliderTime, format = "%Y-%m-%d %H:%M", tz = "UTC")
      minDate = round(to_date(earliest_time()), "hour")
      maxDate = round(to_date(latest_time()), "hour")
      timeStep = 3600
    } else if (sliderStep == "10 Minutes"){
      sliderTime <- as.POSIXct(sliderTime, format = "%Y-%m-%d %H:%M", tz = "UTC")
      minDate = round(to_date(earliest_time()))
      maxDate = round(to_date(latest_time()))
      timeStep = 600
    }
    
    updateSliderInput(session, "sliderDate", label = "Dates:", value = sliderTime)
  }
}

shinyApp(ui, server)
