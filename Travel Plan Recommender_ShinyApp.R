library(shiny)
library(leaflet)
library(RColorBrewer)
library(cpm)
library(TSP)
t_types <- unique(tf$go)
my_min <- 1
my_max <- 6

### 5 star rating JS
rating1 <- "<link rel='stylesheet' href='css/starability-minified/starability-all.min_1.css'/> \n<script src='rate/js/jquery.min.js'></script>\n<fieldset class='starability-basic'>\n<input type='radio' id='rate5' name='rating' value='5' />\n<label for='rate5'>5 star</label>\n<input type='radio' id='rate4' name='rating' value='4' />\n<label for='rate4'>4 star</label>\n<input type='radio' id='rate3' name='rating' value='3' />\n<label for='rate3'>3 star</label>\n<input type='radio' id='rate2' name='rating' value='2' />\n<label for='rate2'>2 star</label>\n<input type='radio' id='rate1' name='rating' value='1' />\n<label for='rate1'>1 star</label>\n\n</fieldset>\n<script>\n$(':radio').change(function(){$('.choice').text( this.value );})\n</script>"


### ShinyApp
ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}", HTML(' #sidebar { background-color: #FFFFFF; text-align: center; vertical-align: middle; border:6px solid silver;} ')), 
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 0, left = 0, width = 320, draggable = T, id = "sidebar", #align = "center",
                h3("@Trip planner"),
                p(),
                h4("Start planning your trip..."),
                sliderInput(inputId = "num", 
                            label = "Total Travel Budget", 
                            value = 500, min = 200, step = 100, max = 2000),
                div(style="display:inline-block",selectInput(inputId="fromcity", label="From",c("-", dfLongLat$city), width=150)),
                div(style="display:inline-block",selectInput(inputId="tocity", label="To",c("-", dfLongLat$city), width=150)),
                div(style="display:inline-block",selectInput(inputId="dur", label="Duration - Days", 3:30, width = 150)),
                div(style="display:inline-block",selectInput(inputId="hur", label="Max. Traffic Hours", 3:10, selected = 6 ,width = 150)),
                div(style="display:inline-block",dateRangeInput('dateRange',
                                                                label = 'Select Departure And Return Date',
                                                                start = Sys.Date() +1, end = Sys.Date() + 8
                )),
                checkboxGroupInput(inputId  = 'ttypes', 
                                   label    = "Prefered Travel Types:", 
                                   choices  = t_types,
                                   selected = t_types,
                                   inline   = T),
                actionButton("recalc", " Plan a Trip "), actionButton("reset", "Reset Map"),
                p()
  ),
  
  absolutePanel(top = 0, right = 0, width = 500, draggable = T, id = "sidebar", 
                selectInput(inputId="plans", label = "Select your travel plan", choices = c("-"), width=500)#,
  ),
  absolutePanel(bottom =  0, left = 0, width = "100%", draggable = T, id = "sidebar",
                fluidRow(
                  column(width = 10,
                         checkboxGroupInput("atts", "Destination Recommender : Select Prefered Characteristics",unique(rate$att)[c(1:4,8,5:7,9:10,21,11:20)] , selected = unique(rate$att)[c(1:4,8,5)], inline   = T)
                         
                  ),
                  column(width = 1, 
                         
                         div(style="display:inline-block",selectInput(inputId="fav", label="Similar Cities",c("-", dfLongLat$city), width=100))
                         
                  ),
                  column(width = 1, 
                         h6(strong("Save Options")),
                         actionButton("saveatt", "Save")
                         
                  )
                )
  )
)

server <- function(input, output, session) {
  
  ### popup buttons click events
  frombtn <- "<button onclick='Shiny.onInputChange(\"button_click_f\",  Math.random())' id='frompop' type='button' class='btn btn-default action-button'>From</button>"
  tobtn <- "<button onclick='Shiny.onInputChange(\"button_click_t\",  Math.random())' id='topop' type='button' class='btn btn-default action-button'>To</button>"
  
  cityIconex <- makeIcon(iconUrl = "D:/img/gry.png", iconWidth = 30, iconHeight = 30) 
  cityIconf <- makeIcon(iconUrl = "D:/img/blu.png", iconWidth = 32, iconHeight = 32) 
  cityIcont <- makeIcon(iconUrl = "D:/img/pnk.png", iconWidth = 32, iconHeight = 32) 
  
  ###path global variable
  pathtbl <- reactiveValues()
  pathtbl$df <- data.frame()
  
  output$map <- renderLeaflet({
    
    city_popup <- paste0(dfLongLat$city, "&nbsp;&nbsp;&nbsp;",HTML(rating1), pop,
                         frombtn,"&nbsp;&nbsp;&nbsp;", tobtn,"&nbsp;&nbsp;&nbsp;")
    cityIcon <- makeIcon(iconUrl = "D:/img/gry.png", iconWidth = 24, iconHeight = 24)
    
    leaflet(dfLongLat) %>%
      #addTiles() %>%
      addTiles('//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png') %>%
      addMarkers(~lon, ~lat+.1, icon = cityIcon, popup = city_popup, layerId=dfLongLat$city)
  })
  proxy <- leafletProxy("map", data = dfLongLat)
  
  ######### Observations
  observe({
    click<-input$map_marker_click
    if(is.null(click))
      return()
    text2<-paste("You've selected point", click$id)
    output$notice<-renderText({ text2 })
  })
  
  observe({
    re_city <- dfLongLat[dfLongLat$city %in% recom(input$fav),]
    pop_re <-  pop1[as.numeric(rownames(re_city))]
    city_popup <- paste0(dfLongLat$city, "&nbsp;&nbsp;&nbsp;",HTML(rating1), pop,
                         frombtn,"&nbsp;&nbsp;&nbsp;", tobtn,"&nbsp;&nbsp;&nbsp;")
    cityIcon <- makeIcon(iconUrl = "D:/img/gry.png", iconWidth = 24, iconHeight = 24)
    re_city_popup <- paste0(h4("Recommand Destination :"), pop_re, tobtn)
    if(!input$fav %in% "-"){
      proxy <- leafletProxy("map", data = re_city) 
      proxy %>% clearMarkers() %>% clearShapes() %>% addMarkers(~lon, ~lat+.1, icon = cityIcont,popup = re_city_popup, layerId=re_city$city)
    } else{
      leafletProxy("map", data = dfLongLat) %>% clearMarkers() %>% addMarkers(~lon, ~lat+.1, icon = cityIcon, popup = city_popup, layerId=dfLongLat$city)
    }
  })
  ######## observe popup buttons
  
  observeEvent(input$button_click_f, {
    updateSelectInput(session, "fromcity",choices = dfLongLat$city, selected = input$map_marker_click$id)
  })
  observeEvent(input$button_click_t, {
    updateSelectInput(session, "tocity",choices = dfLongLat$city, selected = input$map_marker_click$id)
  })
  
  
  ######## reactive input values
  
  from_to_ex <- reactive({
    dfLongLat[-which(dfLongLat$city %in% c(input$fromcity,input$tocity)) ,]
  })
  from_city <- reactive({
    dfLongLat[dfLongLat$city %in% c(input$fromcity),]
  })
  to_city <- reactive({
    dfLongLat[dfLongLat$city %in% c(input$tocity),]
  })
  
  ######## observe input Recauculate
  observeEvent(input$recalc, {
    txt <- paste0("From ", input$fromcity, " to ", input$tocity )
    output$text <- renderText({ txt })
    ####### inbound and outbound cities
    inbound <- find_in1(input$fromcity, input$tocity, as.numeric(input$dur))
    outbound <- find_out_1(input$fromcity, input$tocity, as.numeric(input$dur))
    
    time <- as.numeric(input$hur)
    go_att <- input$atts
    go_typ <- input$ttypes#c("Train", "Bus", "Fly")
    num_cities <- ceiling((as.numeric(input$dur)-2.5)/2.5)
    
    tra_cities <- unique(c(input$fromcity, input$tocity, as.character(outbound$from), as.character(inbound$to)))
    tfc_cities <- tf[tf$from %in% tra_cities & tf$to %in% tra_cities & tf$go %in% go_typ & tf$time < time,]###time
    tfc_cities$from <- as.character(tfc_cities$from)
    tfc_cities$to <- as.character(tfc_cities$to)
    tfc_cities$by <- as.character(tfc_cities$by)
    min_trf <- trf_min_f(tfc_cities)
    
    #print(min_trf) 
    matrix_cbn <- path_cbn(min_trf, input$fromcity) #matx
    trip_ctys <- tra_cities[!tra_cities %in% c(input$fromcity, input$tocity)]
    ifelse(length(trip_ctys)>= num_cities, cty_consist <- t(combn(trip_ctys, num_cities)),cty_consist <- t(combn(trip_ctys, length(trip_ctys)-1)) )
    cty_consist <- cbind(rep(input$fromcity, nrow(cty_consist)), cty_consist, rep(input$tocity, nrow(cty_consist)))
    #print(cty_consist)
    
    ################# for loop of path finding
    all_list_1 <- list()
    all_list_2 <- list()
    all_list <- matrix(1,nrow =2)
    for(i in 1:nrow(cty_consist)){
      rep_list <- replicate(10, {
        find_path1(input$fromcity, min_trf, matrix_cbn, cty_consist,i)
      })
      rep_list <- rep_list[,!duplicated(lapply(rep_list[1,], round))]
      if(length(rep_list)<3){
        rep_list_2 <- rep_list
        x <- as.matrix(rep_list_2[[2]])
        all_list_2[[length(all_list_2)+1]] <- list(x)
        all_list_1[[length(all_list_1)+1]] <- rep_list_2[[1]]
        all_list <- cbind(all_list, rep_list_2)
      }else{
        rep_list_2 <- rep_list[,which(unlist(rep_list[1,]) %in% c(min(unlist(rep_list[1,])),
                                                                  min( unlist(rep_list[1,])[unlist(rep_list[1,])!=min(unlist(rep_list[1,]))])) )] 
        x <- lapply(rep_list_2[2,], as.matrix)
        
        all_list_2[[length(all_list_2)+1]] <- list(x)
        all_list_1[[length(all_list_1)+1]] <- rep_list_2[1,]
        all_list <- cbind(all_list, rep_list_2)
      }
    }
    all_list <- all_list[,order(unlist(all_list[1,]))][,-1]
    #print(all_list)
    
    ifelse(ncol(all_list) < 6, all_list <- all_list, all_list <- all_list[,1:5])
    fin_df <- data.frame()
    if(length(all_list) > 2){
      for (i in 1:ncol(all_list)) {
        x <- matrix(unlist(all_list[2,i]), ncol  = 7, byrow = TRUE)
        x <- cbind(x, rep(all_list[1,i], nrow(x)))
        x <- data.frame(x , stringsAsFactors = F, row.names = 1:nrow(x))
        fin_df <- rbind(fin_df, x)
      }
    }else{
      x <- matrix(unlist(all_list[2]), ncol  = 7, byrow = TRUE)
      x <- cbind(x, rep(all_list[1], nrow(x)))
      x <- data.frame(x , stringsAsFactors = F, row.names = 1:nrow(x))
      fin_df <- rbind(fin_df, x)
    }
    
    if(length(all_list) > 2){
      fin_df$id <- unlist(lapply(1:ncol(all_list) , function(x) rep(x, nrow(fin_df[fin_df$X8 %in% unique(fin_df$X8)[x],]))))
    }else{
      fin_df$id <- 1
    }
    #print(fin_df)
    if(max(fin_df$id) < 5){
      x <- tf[tf$from == input$fromcity & tf$to == input$tocity,]
      x <- x[order(x$cp),][1:(5-max(unique(fin_df$id))),]
      y <- tf[tf$from == input$tocity & tf$to == input$fromcity,]
      y <- y[order(y$cp),][1:(5-max(unique(fin_df$id))),]
      mtx_xy <- matrix(0, nrow = length(rownames(x)), ncol = length(rownames(y)), dimnames = list(rownames(x),rownames(y)))
      for(i in rownames(x)){
        for(j in rownames(y)){
          mtx_xy[i,j] <- sum(x[rownames(x) %in% i,7], y[rownames(y) %in% j,7])
        }
      }
      
      
      if (nrow(mtx_xy)>1){
        tmp <- as.vector(mtx_xy)[order(as.vector(mtx_xy))][1:(5-max(unique(fin_df$id)))]
        #print(tmp) #########
        mtx_tr <- matrix(1, nrow = 1, ncol = 2)
        for(i in 1:length(tmp)){
          mtx_tr <- rbind(mtx_tr,which(mtx_xy == tmp[i], arr.ind = T))
        }
        
        mtx_tr <- mtx_tr[-1,]
        df_tr <- data.frame()
        
        for(i in 1:nrow(mtx_tr)){
          tmp1 <- rbind(tf[as.numeric(rownames(mtx_xy)[mtx_tr[i,1]]),], tf[as.numeric(colnames(mtx_xy)[mtx_tr[i,2]]),])
          colnames(tmp1) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7")
          tmp1$X8 <- mtx_xy[mtx_tr[i,1], mtx_tr[i,2]]
          tmp1$id <- i
          df_tr <- rbind(df_tr, tmp1)
          #print(tf[as.numeric(rownames(mtx_xy)[mtx_tr[i,1]]),])
          #print(tf[as.numeric(colnames(mtx_xy)[mtx_tr[i,2]]),])
        }
      } else {
        print(mtx_xy)
        df_tr <- rbind(tf[as.numeric(rownames(mtx_xy)),], tf[as.numeric(colnames(mtx_xy)),])
        colnames(df_tr) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7")
        df_tr$X8 <- mtx_xy[1,1]
        df_tr$id <- 1
      }
      
      
      fin_df$id <- fin_df$id+max(df_tr$id)
      fin_df <- rbind(df_tr, fin_df)
      for(i in 1:max(unique(fin_df$id))){
        x <- fin_df[fin_df$id == i,]
        assign(paste0("plan", i), paste(paste(i, ":"), paste0(c(x$X1, x$X2[nrow(x)]), collapse = ">") , paste0("GBP:", x$X8)))
      }
      
      #print(all_plans)
    }else {
      
      for(i in 1:5){
        x <- fin_df[fin_df$id ==i,]
        assign(paste0("plan", i), paste(paste(i, ":"), paste0(c(x$X1, x$X2[nrow(x)]), collapse = ">")  , paste0("GBP:", x$X8[1])))
      }
    }
    updateSelectInput(session, "plans",choices = c("-", plan1, plan2, plan3, plan4, plan5))
    
    pop2[[as.numeric(rownames(dfLongLat[dfLongLat$city %in% input$fromcity,]))]] <- NULL
    pop2[[as.numeric(rownames(dfLongLat[dfLongLat$city %in% input$tocity,]))]] <- NULL
    city_popup_ex <- paste0(from_to_ex()$city, "&nbsp;&nbsp;&nbsp;",HTML(rating1), pop2,
                            frombtn,"&nbsp;&nbsp;&nbsp;", tobtn,"&nbsp;&nbsp;&nbsp;")
    pop2 <-  pop[[as.numeric(rownames(dfLongLat[dfLongLat$city %in% input$fromcity,]))]]
    F_city_popup <- paste0(from_city()$city, "&nbsp;&nbsp;&nbsp;",HTML(rating1), pop2,
                           actionButton("From" , "From"))
    pop3 <-  pop[[as.numeric(rownames(dfLongLat[dfLongLat$city %in% input$tocity,]))]]
    T_city_popup <- paste0(to_city()$city, "&nbsp;&nbsp;&nbsp;",rating1, pop3,
                           actionButton("To", "To"))
    
    leafletProxy("map", data = from_to_ex()) %>% clearMarkers() %>% clearShapes()
    proxy <- leafletProxy("map", data = from_to_ex()) 
    proxy %>% addMarkers(~lon, ~lat+.1, icon = cityIconex, popup = city_popup_ex, layerId=from_to_ex()$city)
    proxy <- leafletProxy("map", data = from_city()) 
    proxy %>% addMarkers(~lon, ~lat+.1, icon = cityIconf, popup = F_city_popup)
    proxy <- leafletProxy("map", data = to_city()) 
    proxy %>% addMarkers(~lon, ~lat+.1, icon = cityIcont, popup = T_city_popup)
    
    ############## plot Max travel distance ###################
    proxy %>% clearShapes()
    dis <- as.numeric(input$dur)
    proxy %>% addCircles(~lon, ~lat+.1, color = "#ddaf22", radius = 100000*dis*0.75, weight = 2,fillOpacity = 0.1)
    
    isolate(pathtbl$df <- fin_df)
    
  })
  ######## end obs input Recauculate  
  
  ######## observe input data
  observeEvent(input$frompop, {
    city_popup <- paste0(dfLongLat$city, "&nbsp;&nbsp;&nbsp;",HTML(rating1), pop,
                         actionButton("From" , "From"),"&nbsp;&nbsp;&nbsp;", actionButton("To", "To"),"&nbsp;&nbsp;&nbsp;",  actionButton("Relay", "Add Relay"))
    cityIcon <- makeIcon(iconUrl = "D:/img/gry.png", iconWidth = 24, iconHeight = 24)
    leafletProxy("map", data = dfLongLat) %>% clearMarkers() %>% clearShapes() %>% addMarkers(~lon, ~lat+.1, icon = cityIcon, popup = city_popup)
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "fromcity", choices = c("-",dfLongLat$city))
    updateSelectInput(session, "tocity", choices = c("-",dfLongLat$city))
    city_popup <- paste0(dfLongLat$city, "&nbsp;&nbsp;&nbsp;",HTML(rating1), pop,
                         frombtn,"&nbsp;&nbsp;&nbsp;", tobtn,"&nbsp;&nbsp;&nbsp;")
    cityIcon <- makeIcon(iconUrl = "D:/img/gry.png", iconWidth = 24, iconHeight = 24)
    leafletProxy("map", data = dfLongLat) %>% clearMarkers() %>% clearShapes() %>% addMarkers(~lon, ~lat+.1, icon = cityIcon, popup = city_popup, layerId=dfLongLat$city)
  })
  
  ### observe plan selection
  observeEvent(input$plans, {
    if(!input$plans %in% "-"){
      ############## Draw path 
      proxy <- leafletProxy("map", data = dfLongLat) %>% clearShapes()
      plan_n <- as.numeric(gsub("\\s\\:.*", "", input$plans))
      plan_tbl <- pathtbl$df[pathtbl$df$id %in% plan_n,]
      print(plan_tbl)
      ######### add hyperlink
      x <- dfLongLat$sim[match(plan_tbl$X1[1], dfLongLat$city)]
      y <- dfLongLat$sim[match(plan_tbl$X2[1], dfLongLat$city)]
      if(plan_tbl$X6[1] %in% "Fly"){
        lnk <- paste0("<a target='_blank' href='https://www.kayak.com/flights/", x, "-", y, "/", Sys.Date()+7, "'>Book a Flight</a>")
      } else if(plan_tbl$X6[1] %in% "Train"){
        lnk <- paste0("<a target='_blank' href='http://www.thetrainline-europe.com/plan/", plan_tbl$X1[1], "-to-", plan_tbl$X2[1], "?departDate=", Sys.Date()+7, "&departTime=morning'>Book Train Tickets</a>")
      } else if(plan_tbl$X6[1] %in% "Drive"){
        lnk <- paste0("<a target='_blank' href='http://www.rentalcars.com/en'>Car Hire</a>")
      } else if(plan_tbl$X6[1] %in% "Rideshare"){
        lnk <- paste0("<a target='_blank' href='https://www.blablacar.co.uk/search?fn=", plan_tbl$X1[1], "&tn=", plan_tbl$X2[1], "'>Find Car Share</a>")
      } else if(plan_tbl$X6[1] %in% "Bus"){
        lnk <- paste0("<a target='_blank' href='https://www.busradar.com/search/?From=", plan_tbl$X1[1], "&To=", plan_tbl$X2[1], "&When=", Sys.Date()+7, "&WhenReturn=&Company=All+companies&Passengers=1&SearchMode=0&Radius=15000'>Book Bus Tickets</a>")
      } else {
        lnk <- paste0("<a target='_blank' href='https://www.rome2rio.com/s/", plan_tbl$X1[1], "/", plan_tbl$X2[1], "'>Find Tickets</a>")
      }
      
      path_popup <- paste("From", plan_tbl$X1[1], "To", plan_tbl$X2[1], br(), "By", plan_tbl$X3[1], br(), "Time", plan_tbl$X4[1], "hrs", br(), "Avg. Price", "GBP", plan_tbl$X5[1], br(), lnk)
      leafletProxy("map", data = dfLongLat[dfLongLat$city %in% as.character(plan_tbl[1,1:2]),]) %>% 
        addPolylines(~lon, ~lat, color = "steelblue", popup = path_popup)
    }
    
  })
  
  ######### Observe mouse clicks
  observe({
    click<-input$map_marker_click
    if(is.null(click))
      return()
    text2<-paste("You've selected point", click$id)
    output$notice<-renderText({ text2 })
  })
  
  observeEvent(input$button_click_f, {
    updateSelectInput(session, "fromcity",choices = dfLongLat$city, selected = input$map_marker_click$id)
  })
  observeEvent(input$button_click_t, {
    updateSelectInput(session, "tocity",choices = dfLongLat$city, selected = input$map_marker_click$id)
  })
  
  ### debug rendertext
  output$Selected <- renderText({
    paste(input$SelecetedVars,collapse=",")
  })
  
  
  ### selection monitor
  observe({
    if(length(input$atts) > my_max)
    {
      updateCheckboxGroupInput(session, "atts", selected= tail(input$atts,my_max))
    } else {
      updateCheckboxGroupInput(session, "atts", selected= head(input$atts,my_min))
    }
    
  })
  
}

shinyApp(ui, server)
