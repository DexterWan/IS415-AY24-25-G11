#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
pacman::p_load(sf, sparr, tidyverse, tmap, ggpubr, bslib, tmap, shinydashboard, shinythemes, spdep, dplyr)

st_kde_quarter = read_rds("data/rds/st_kde_quarter_simplified.rds")
province_drug_stats_all <- read_rds("data/rds/province_drug_stats_all.rds")
drug_cases_sf = read_rds("data/rds/drug_cases_sf.rds")

Bulacan_owin = read_rds("data/rds/Bulacan_owin.rds")
Cebu_owin = read_rds("data/rds/Cebu_owin.rds")
Laguna_owin = read_rds("data/rds/Laguna_owin.rds")
Metropolitan_Manila_owin = read_rds("data/rds/Manila_owin.rds")
Pangasinan_owin = read_rds("data/rds/Pangasinan_owin.rds")

slr_all = read_rds("data/rds/drug_cases_no_manila_slr.rds")
slr_2018 = read_rds("data/rds/drug_cases_2018_slr.rds")
slr_2019 = read_rds("data/rds/drug_cases_2019_slr.rds")
slr_2020 = read_rds("data/rds/drug_cases_2020_slr.rds")
slr_2021 = read_rds("data/rds/drug_cases_2021_slr.rds")
slr_2022 = read_rds("data/rds/drug_cases_2022_slr.rds")

# Geovisualisation
GeoVisualisation = tabPanel("GeoVisualisation",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("year_selection", "Select Year for Analysis:", choices = 2016:2024),
                                selectInput("quarter_selection", "Select Quarter for Analysis:", choices = 1:4),
                                actionButton("show_points", "Show Points Map")
                              ),
                              mainPanel(
                                tmapOutput("drug_case_map", width = "100%", height = 580)
                              )
                            )
)

FirstOrder = tabPanel("First-order Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("year_selection_fo", "Select Year for Analysis:", choices = 2016:2024),
                          selectInput("area_selection_fo", "Select Province:",
                                      choices = list("Metropolitan Manila" = "^Metropolitan Manila",
                                                     "Pangasinan" = "^Pangasinan",
                                                     "Bulacan" = "^Bulacan",
                                                     "Laguna" = "^Laguna",
                                                     "Cebu" = "^Cebu")),
                          selectInput("method_selection1", "Select Method:",
                                      choices = list("gaussian" = "gaussian", "epanechnikov" = "epanechnikov",
                                                     "quartic" = "quartic", "disc" = "disc")),
                          actionButton("first_order_analysis", "Run First-order Analysis")
                        ),
                        mainPanel(
                          plotOutput("first_order_plot")
                        )
                      )
)

# Point Pattern Analysis tab
SecondOrder = tabPanel("Second-order Analysis",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("year_selection_so", "Select Year for Analysis:", choices = 2016:2024),
                           selectInput("area_selection_so", "Select Province:",
                                       choices = list("Metropolitan Manila" = "^Metropolitan Manila",
                                                      "Pangasinan" = "^Pangasinan",
                                                      "Bulacan" = "^Bulacan",
                                                      "Laguna" = "^Laguna",
                                                      "Cebu" = "^Cebu")),
                           # selectInput("method_selection", "Select Method:",
                           #             choices = list("F-function" = "Fest",
                           #                            "L-Function" = "Lest")),
                           actionButton("Second_order_analysis", "Run Second-order Analysis")
                         ),
                         mainPanel(
                           plotOutput("Second_order_plot")
                         )
                       )
)


nearest_neighbour <- tabPanel("Nearest Neighbour", {
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "maxDistance",
                  label = "Distance (km)",
                  min = 100, max = 1000,
                  value = 386, step = 1),
      sliderInput(inputId = "k",
                  label = "k",
                  min = 1, max = 20,
                  value = 7, step = 1)
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("neighbourDistance")),
        column(6, plotOutput("neighbourK"))
      )
    )
  )
})

moran_i <- tabPanel("Moran's I", {
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "k",
                  label = "k",
                  min = 1, max = 20,
                  value = 7, step = 1),
      selectInput(inputId = "style",
                  label = "Style",
                  selected = "B",
                  choices = list("W" = "W", "B" = "B", "C" = "C", "U" = "U", "minmax" = "minmax", "S" = "S")),
      sliderInput(inputId = "simulations",
                  label = "Number of Simulations",
                  min = 99, max = 499,
                  value = 99, step = 100),
      sliderInput(inputId = "breaks",
                  label = "Breaks",
                  min = 1, max = 100,
                  value = 20, step = 5)
    ),
    mainPanel(
      plotOutput("globMoranI")
    )
  )
})

geary_c <- tabPanel("Geary's C", {
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "k",
                  label = "k",
                  min = 1, max = 20,
                  value = 7, step = 1),
      selectInput(inputId = "style",
                  label = "Style",
                  choices = list("W" = "W", "B" = "B", "C" = "C", "U" = "U", "minmax" = "minmax", "S" = "S")),
      sliderInput(inputId = "simulations",
                  label = "Number of Simulations",
                  min = 99, max = 499,
                  value = 99, step = 100),
      sliderInput(inputId = "breaks",
                  label = "Breaks",
                  min = 1, max = 100,
                  value = 20, step = 5)
    ),
    mainPanel(
      plotOutput("globGearyC")
    )
  )
})

local_moran <- tabPanel("Local Moran", {
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "k",
                  label = "k",
                  min = 1, max = 20,
                  value = 7, step = 1),
      selectInput(inputId = "style",
                  label = "Style",
                  choices = list("W" = "W", "B" = "B", "C" = "C", "U" = "U", "minmax" = "minmax", "S" = "S")),
      selectInput("localMoranStats", "Local Moran's Stat",
                  choices = c("local moran (ii)" = "Ii",
                              "expectation (eii)" = "E.Ii",
                              "variance (var_ii)" = "Var.Ii",
                              "std deviation (z_ii)" = "Z.Ii"),
                  selected = "local moran(ii)")
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("localMiMap")),
        column(6, plotOutput("localMiPValueMap"))
      )
    )
  )
})

moran_scatterplot <- tabPanel("Moran Scatterplot", {
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "k",
                  label = "k",
                  min = 1, max = 20,
                  value = 7, step = 1),
      selectInput(inputId = "style",
                  label = "Style",
                  choices = list("W" = "W", "B" = "B", "C" = "C", "U" = "U", "minmax" = "minmax", "S" = "S"))
    ),
    mainPanel(
      plotOutput("moranScatterplot")
    )
  )
})

lisa_map <- tabPanel("LISA Map", {
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "k",
                  label = "k",
                  min = 1, max = 20,
                  value = 7, step = 1),
      selectInput(inputId = "style",
                  label = "Style",
                  choices = list("W" = "W", "B" = "B", "C" = "C", "U" = "U", "minmax" = "minmax", "S" = "S")),
      selectInput(inputId = "lisaClass",
                  label = "Lisa Classification",
                  choices = c("mean" = "mean",
                              "median" = "median",
                              "pysal" = "pysal"),
                  selected = "mean"),
      radioButtons(inputId = "moranConf",
                   label = "Confidence Level",
                   choices = c("0.95" = 0.05,
                               "0.99" = 0.01),
                   selected = 0.05,
                   inline = TRUE),
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("cases")),
        column(6, plotOutput("lisaMap"))
      )
    )
  )
})

local_gi <- tabPanel("Local Gi", {
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "k",
                  label = "k",
                  min = 1, max = 20,
                  value = 7, step = 1),
      selectInput(inputId = "style",
                  label = "Style",
                  choices = list("W" = "W", "B" = "B", "C" = "C", "U" = "U", "minmax" = "minmax", "S" = "S")),
      
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("cases2")),
        column(6, plotOutput("giMap"))
      )
    )
  )
})

ui <- navbarPage(

    # Application title
  title = "The War on Drugs",
	fluid = TRUE,
  theme = bs_theme(preset = "lumen", bg = "#FFF", fg = "#000"),
	id = "navbarID",
	tabPanel("EDA",
	         sidebarLayout(
	           sidebarPanel(
	             # Date-range
	             dateRangeInput(inputId = "eda_date",
	                            label = "Select dates",
	                            start = "2016-01-01",
	                            end = "2024-06-30",
	                            min = "2016-01-01",
	                            max = "2024-06-30"),
	             # Fatality of incidents
	             radioButtons( inputId = "eda_fatality",
	                           label = "Choose Incident Fatality Type",
	                           choices = list(
	                             "All" = 1,
	                             "Fatal incidents only" = 2,
	                             "Non-Fatal incidents only" = 3
	                           )
	                          ),
	             # Type of Event ("Violence against Civilians", "Battles", "Strategic developments")
	             checkboxGroupInput(inputId = "eda_event_type",
	                                label = "Event Types",
	                                choices = c("Violence against civilians", "Battles", "Strategic developments"),
	                                selected = c("Violence against civilians", "Battles", "Strategic developments")
	             )
	           ),
	           mainPanel(plotOutput("eda_plot"))
	         )
	         ),
   tabPanel("Spatio-Temporal",
            navset_card_pill(
              full_screen = TRUE,
              nav_panel( "Animation",
                sidebarLayout(
                  sidebarPanel(
                    # Monthly/Quarterly
                    radioButtons(inputId = "st_date_range_type_anim",
                                 label = "Type of date range:",
                                 choices = c("Monthly", "Quarterly"),
                                 selected = "Monthly"
                    )
                  ),
                  mainPanel(imageOutput("st_animation"))
                )
              ),
              nav_panel("Static",
                         sidebarLayout(
                           sidebarPanel(
                             # Monthly/Quarterly
                             # radioButtons(inputId = "st_date_range_type_static",
                             #              label = "Type of date range:",
                             #              choices = c("Monthly", "Quarterly"),
                             #              selected = "Monthly"
                             # ),
                             # Date-range
                             uiOutput("selector_ui"),
                             # Fixed-range T/F
                             input_switch(id = "st_fixed_range",
                                          label = "Fixed Range",
                                          value = FALSE),
                             actionButton("st_static_plot", "Plot")
                            ),
                           mainPanel(plotOutput("st_static", height = "2500px", width = "1000px"))
                         )

                )
              )
    ),
  navbarMenu("Spatial Point Pattern", GeoVisualisation, FirstOrder, SecondOrder),
  navbarMenu("Spatial Auto-Correlation", nearest_neighbour, moran_i, geary_c, local_moran, moran_scatterplot, lisa_map, local_gi),
  tabPanel("Linear Regression",
           sidebarLayout(
             sidebarPanel(
               # All years in 1 graph vs individual years (can select range)
               input_switch(id = "lm_all_years",
                            label = "All years (2018-2022) consolidated",
                            value = FALSE),
               uiOutput("lm_year_slider")
             ),
             mainPanel(
               navset_card_tab(
                 full_screen = TRUE,
                 nav_panel("Plot",
                           plotOutput("lm_plot")),
                 nav_panel("Summary",
                           verbatimTextOutput("lm_summary"))
               )
             )
           )
         )
)



#==================================================#
#                    SHINY SERVER
#==================================================#

# Define server logic required to draw a histogram
server <- function(input, output) { 
  #==================================================#
  #                       EDA
  #==================================================#
  output$eda_plot = renderPlot({
    output_cases = drug_cases_sf %>% filter(
      event_date >= input$eda_date[1],
      event_date <= input$eda_date[2],
      event_type %in% input$eda_event_type #https://www.statology.org/r-filter-in/
    )

    if(input$eda_fatality == 2){
      output_cases = output_cases %>% filter(fatalities > 0)
    }else if(input$eda_fatality == 3){
      output_cases = output_cases %>% filter(fatalities == 0)
    }
    ggplot(data = output_cases,
           aes(x= `event_date`)) +
      geom_histogram(bins = 20,
                     color = "black",
                     fill = "light blue")
  })

  #==================================================#
  #                      STKDE
  #==================================================#
  
  output$st_animation = renderImage({
    if(input$st_date_range_type_anim == "Quarterly"){
      list(src = "drug_stkde_quarters.gif", height = "100%")
    }else{
      list(src = "drug_stkde_month.gif", height = "100%")
    }
  }, deleteFile = FALSE)
  
  observeEvent(input$st_static_plot,{
    
    output$st_static = renderPlot({
      # if(input$st_date_range_type_static == "Quarterly"){
      par(mfrow = c(5,2))
      for(q in input$st_date_select_q){
        q = as.integer(q)
        plot(st_kde_quarter, q,
             override.par = FALSE,
             fix.range = input$st_fixed_range,
             main = paste("Drug Cases on ", floor(q/10), "Q", q %% 10))
      }
    })
  })

  output$selector_ui = renderUI({
      selectizeInput(inputId = "st_date_select_q",
                     label = "Date Range",
                     multiple = TRUE,
                     choices = quarter_range
      )
  })
  
    #==================================================#
    #             Spatial Point Pattern
    #==================================================#

  # Server logic for Second-order Spatial Point Patterns Analysis
  observeEvent(input$Second_order_analysis, {
    selected_year <- as.numeric(input$year_selection_so)

    output$Second_order_plot <- renderPlot({
      # 根据年份和季度过滤数据
      drug_case_selected <- drug_cases_sf %>%
        filter(year(event_date) == selected_year, 
               str_detect(admin2, input$area_selection_so)) # 确保使用正确的过滤条件
      drug_case_ppp <- as.ppp(drug_case_selected)
      
      file = str_c(sub(" ", "_", sub(".", "", input$area_selection_so)),"_owin")
      drug_case_ppp = drug_case_ppp[get(file)]
      tm.csr <- envelope(drug_case_ppp, "Fest", nsim = 99)
      plot(tm.csr)


    })
  })

  # 分别生成每月、每季度的 case count
  cases_by_month <- drug_cases_sf %>%
    group_by(event_month) %>%
    summarise(case_count = n()) %>%
    arrange(event_month)

  cases_by_quarter <- drug_cases_sf %>%
    group_by(event_quarter) %>%
    summarise(case_count = n()) %>%
    arrange(event_quarter)


  # tmap 显示点地图，根据选择的年份
  observeEvent(input$show_points, {
    selected_year <- as.numeric(input$year_selection)  # 获取选择的年份
    selected_quarter <- as.numeric(input$quarter_selection)  # 获取选择的季度

    # 根据选择的年份和季度过滤数据
    filtered_data <- drug_cases_sf %>%
      filter(year(event_date) == selected_year,
             quarter(event_date) == selected_quarter)  # 使用 year 和 quarter 进行过滤

    # 渲染 tmap
    output$drug_case_map <- renderTmap({
      tmap_mode("view")
      tm_shape(filtered_data) + tm_dots()
    })
  })


  # Server logic for First-order Spatial Point Patterns Analysis
  observeEvent(input$first_order_analysis, {
    selected_year <- as.numeric(input$year_selection_fo)

    output$first_order_plot <- renderPlot({
      # 根据年份和季度过滤数据
      drug_case_selected <- drug_cases_sf %>%
        filter(year(event_date) == selected_year,
               str_detect(admin2, input$area_selection_fo)) # 确保使用正确的过滤条件
      drug_case_ppp <- as.ppp(drug_case_selected)
     
      file = str_c(sub(" ", "_", sub(".", "", input$area_selection_so)),"_owin")
      drug_case_ppp = drug_case_ppp[get(file)]
      drug_case_ppp.km <- rescale.ppp(drug_case_ppp, 1000, "km")

      # 执行 First-order Spatial Point Pattern Analysis (Kernel Density Estimation)
      kde_drug_bw <- density(drug_case_ppp.km,
                                   sigma=bw.ppl,
                                   edge=TRUE,
                                   kernel=input$method_selection1)

      # 绘图
      plot(kde_drug_bw)
    })
  })

    #==================================================#
    #              Spatial Autocorrelation
    #==================================================#

  set.seed(27)
  longitude <- map_dbl(province_drug_stats_all$geometry, ~st_centroid(.x)[[1]])
  latitude <- map_dbl(province_drug_stats_all$geometry, ~st_centroid(.x)[[2]])
  coords <- cbind(longitude, latitude)
  output$neighbourDistance <- renderPlot({
    wm_d <- dnearneigh(coords, 0, input$maxDistance, longlat = TRUE)
    plot(province_drug_stats_all$geometry, border="lightgrey")
    plot(wm_d, coords, pch = 19, cex = 0.6, add = TRUE, col= "red")
  })
  knn <- eventReactive(input$k, {
    return(knn2nb(knearneigh(coords, k=input$k)))
  })
  output$neighbourK <- renderPlot({
    plot(province_drug_stats_all$geometry, border="lightgrey")
    plot(knn(), coords, pch = 19, cex = 0.6, add = TRUE, col= "red")
  })
  knn_lw <- eventReactive(list(input$k, input$style), {
    return(nb2listw(knn(), style = input$style))
  })
  output$globMoranI <- renderPlot({
    bperm <- moran.mc(province_drug_stats_all$num_events, listw = knn_lw(), zero.policy = TRUE, na.action = na.omit, nsim = input$simulations)
    hist(bperm$res, 
         freq=TRUE,
         breaks=input$breaks, 
         xlab="Simulated Moran's I", main = "Moran's I")
    abline(v=0, col="red")
  })
  output$globGearyC <- renderPlot({
    cperm <- geary.mc(province_drug_stats_all$num_events, listw = knn_lw(), nsim = input$simulations)
    hist(cperm$res, freq=TRUE, breaks=input$breaks, xlab="Simulated Geary's C", main = "Geary's C")
    abline(v=1, col="red")
  })
  cases <- renderPlot({
    qtm(province_drug_stats_all, "num_events", title = "Drug cases") +
      tm_text("ADM2_EN", col = "black", size = 0.5, auto.placement = TRUE)
  })
  output$cases <- cases
  output$cases2 <- cases
  localMI <- eventReactive(list(input$k, input$style), {
    return(localmoran(province_drug_stats_all$num_events, knn_lw()))
  })
  provinces.localMI <- eventReactive(list(input$k, input$style), {
    return(cbind(province_drug_stats_all, localMI()) %>% rename(Pr.Ii = Pr.z....E.Ii..))
  })
  output$localMiMap <- renderPlot({
    print(names(provinces.localMI()))
    tm_shape(provinces.localMI()) +
      tm_fill(col = input$localMoranStats,
              style = "pretty",
              palette = "RdBu",
              title = "Local Moran statistics") +
      tm_borders(alpha = 0.5) +
      tm_text("ADM2_EN", col = "black", size = 0.5, auto.placement = TRUE)
  })
  output$localMiPValueMap <- renderPlot({
    tm_shape(provinces.localMI()) +
      tm_fill(col = "Pr.Ii",
              breaks=c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
              palette="-Blues",
              title = "Local Moran's I p-values") +
      tm_borders(alpha = 0.5) +
      tm_text("ADM2_EN", col = "red", size = 0.5, auto.placement = TRUE)
  })
  output$moranScatterplot <- renderPlot({
    moran.plot(province_drug_stats_all$Z.cases, knn_lw(),
               labels=as.character(province_drug_stats_all$ADM2_EN),
               xlab="z-cases 2012",
               ylab="Spatially Lagged z-cases")
  })
  output$lisaMap <- renderPlot({
    provinces_localMI <- provinces.localMI()
    quadrant <- vector(mode="numeric",length=nrow(localMI()))
    province_drug_stats_all$lag_cases <- lag.listw(knn_lw(), province_drug_stats_all$num_events)
    DV <- switch(input$lisaClass,
                 "mean" = province_drug_stats_all$lag_cases - mean(province_drug_stats_all$lag_cases),
                 "median" = province_drug_stats_all$lag_cases - median(province_drug_stats_all$lag_cases),
                 "pysal" = scale(province_drug_stats_all$lag_cases)
    )
    LM_I <- switch(input$lisaClass,
                   "mean" = localMI()[,1] - mean(localMI()[,1]),
                   "median" = localMI()[,1] - median(localMI()[,1]),
                   "pysal" = scale(localMI()[,1])
    )
    signif <- input$moranConf
    quadrant[DV <0 & LM_I>0] <- 1
    quadrant[DV >0 & LM_I<0] <- 2
    quadrant[DV <0 & LM_I<0] <- 3
    quadrant[DV >0 & LM_I>0] <- 4
    quadrant[localMI()[,5]>signif] <- 0
    provinces_localMI$quadrant <- quadrant
    colors <- c("#ffffff", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
    clusters <- c("insignificant", "low-low", "low-high", "high-low", "high-high")
    tm_shape(provinces_localMI) +
      tm_fill(col = "quadrant",
              style = "cat", 
              palette = colors[c(sort(unique(quadrant)))+1], 
              labels = clusters[c(sort(unique(quadrant)))+1],
              title = "Quadrant",
              popup.vars = c("")) +
      tm_view(set.zoom.limits = c(11,17)) +
      tm_borders(alpha=0.5) +
      tm_text("ADM2_EN", col = "black", size = 0.5, auto.placement = TRUE)
  })
  output$giMap <- renderPlot({
    gi.adaptive <- localG(province_drug_stats_all$num_events, knn_lw())
    cases.gi <- cbind(province_drug_stats_all, as.matrix(gi.adaptive)) %>%
      rename(gstat_adaptive = as.matrix.gi.adaptive.)
    tm_shape(cases.gi) + 
      tm_fill(col = "gstat_adaptive", 
              style = "pretty", 
              palette="-RdBu", 
              title = "Local Gi") + 
      tm_borders(alpha = 0.5) +
      tm_text("ADM2_EN", col = "black", size = 0.5, auto.placement = TRUE)
  })
  
    #==================================================#
    #                 Linear Regression
    #==================================================#

    output$lm_year_slider = renderUI({
      if(input$lm_all_years == FALSE){
        sliderInput(inputId = "lm_year",
                    label = "Year data",
                    ticks = FALSE,
                    min = 2018, max = 2022,
                    value = 2018)
      }
    })

    output$lm_summary = renderPrint({
      if(input$lm_all_years == TRUE){
        summary(slr_all)
      }else{
        summary(get(str_c("slr_", as.character(input$lm_year))))
      }
      })

    output$lm_plot = renderPlot({
      if(input$lm_all_years == TRUE){
        ggplot(data = slr_all,
               aes(x = `GDPPC`, y = `drug_case_rate`)) +
          geom_point() +
          geom_smooth(method = lm)
      }else{
        file = str_c("slr_", as.character(input$lm_year))
        ggplot(data = get(file),
               aes(x = `GDPPC`, y = `drug_case_rate`)) +
          geom_point() +
          geom_smooth(method = lm)
      }
    })

}

quarter_range = list("2016" = list("2016 Q1" = "20161",
                                   "2016 Q2" = "20162",
                                   "2016 Q3" = "20163",
                                   "2016 Q4" = "20164"),
                     
                     "2017" = list("2017 Q1" = "20171",
                                   "2017 Q2" = "20172",
                                   "2017 Q3" = "20173",
                                   "2017 Q4" = "20174"),
                     
                     "2018" = list("2018 Q1" = "20181",
                                   "2018 Q2" = "20182",
                                   "2018 Q3" = "20183",
                                   "2018 Q4" = "20184"),
                     
                     "2019" = list("2019 Q1" = "20191",
                                   "2019 Q2" = "20192",
                                   "2019 Q3" = "20193",
                                   "2019 Q4" = "20194"),
                     
                     "2020" = list("2020 Q1" = "20201",
                                   "2020 Q2" = "20202",
                                   "2020 Q3" = "20203",
                                   "2020 Q4" = "20204"),
                     
                     "2021" = list("2021 Q1" = "20211",
                                   "2021 Q2" = "20212",
                                   "2021 Q3" = "20213",
                                   "2021 Q4" = "20214"),
                     
                     "2022" = list("2022 Q1" = "20221",
                                   "2022 Q2" = "20222",
                                   "2022 Q3" = "20223",
                                   "2022 Q4" = "20224"),
                     
                     "2023" = list("2023 Q1" = "20231",
                                   "2023 Q2" = "20232",
                                   "2023 Q3" = "20233",
                                   "2023 Q4" = "20234"),
                     
                     "2024" = list("2024 Q1" = "20241",
                                   "2024 Q2" = "20242")
                     )

# Run the application 
shinyApp(ui = ui, server = server)
