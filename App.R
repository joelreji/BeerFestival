library(shiny)
library(ggplot2)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(stringdist)

# Pre-processing
# Read in the file, re-code the lowercase states, & get the unique states
beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')
beer_awards <- beer_awards %>% mutate(state = 
                                        recode(state, "wa" = "WA",
                                               "Ak" = "AK"))
states <- distinct(beer_awards, state)
medal_colors = c("#965A38","#DFBC00","#BFC1C2")
brewery<-distinct(beer_awards, brewery)
beer <- droplevels(subset(beer_awards, brewery != "Pearl Brewing Co."))

# Function that will be called by event reactive on button click 
getDetails <- function(s, startYear, endYear) {
  sYear <- as.numeric(substr(startYear, 1, 4))
  eYear <- as.numeric(substr(endYear, 1, 4))
  
  medals <- filter(beer_awards, state == s & year >= sYear & year <= eYear)
  # print(eYear)
  # glimpse(medals)
  return (medals)
}

#Find fuzzy duplicates with this function
remove_duplicates <- function(vec) {
  wordcount <- vec %>%
    tibble(txt=vec) %>% 
    dplyr::count(txt,sort=T)
  
  words <- wordcount$txt
  
  out <- sapply(seq_along(words)[-1],function(i) {
    dist2 <- stringdist(words[i],words[1:i-1],method='jw',p=0.1)
    best_fit <- which.min(dist2)
    similarity2 <- min(dist2)
    return(c(similarity2,best_fit))
  }) %>% 
    t() %>%
    as.data.frame() %>% 
    add_row(V1=1,V2=1,.before = 1) %>% 
    cbind(wordcount) %>% 
    dplyr::rename(distance=V1,best_fit=V2) %>% 
    mutate(replacement=txt[best_fit])
  
  return(out)
}

out <- beer %>% 
  rowwise() %>%
  mutate(city_brewery=paste(city,brewery,sep=";")) %>%
  .$city_brewery %>% 
  remove_duplicates() %>% 
  separate(txt,into = c("city1","brewery_orig"),sep = ";") %>%
  separate(replacement,into=c("city2","brewery_repl"),sep=";") %>% 
  filter(city1==city2) %>%
  mutate(dist=stringdist(brewery_orig,brewery_repl,method="jw"))

dict <- out %>% 
  mutate(brewery_repl=ifelse(dist<=0.15,brewery_repl,brewery_orig)) %>%
  filter(brewery_orig!=brewery_repl) 

beer$brewery2 <- plyr::mapvalues(beer$brewery,from=dict$brewery_orig,to=dict$brewery_repl)
top10 <- beer %>% count(brewery,city,state,sort=T)
top10b <- beer %>% count(brewery2,city,state,sort=T)

# Basic UI for Shiny app with components
ui <- fluidPage(
  titlePanel("Great American Beer Festival"),
  sidebarLayout(
    sidebarPanel(
      h3("Medal Settings"),
      selectInput("states_inp", "Choose a state:", choices = states),
      airDatepickerInput("startYear_inp",
                         label = "Start Year:",
                         value = "1987-1-01",
                         maxDate = "2020-12-30",
                         minDate = "1987-01-01",
                         view = "years",
                         minView = "years",
                         dateFormat = "yyyy" ),
      airDatepickerInput("endYear_inp",
                         label = "End Year:",
                         value = "2020-1-01",
                         maxDate = "2020-12-30",
                         minDate = "1987-01-01",
                         view = "years",
                         minView = "years",
                         dateFormat = "yyyy"),
      actionButton("run", "Run App"),
      hr(),
      h3("Brewery Settings"),
      selectInput("brewery_inp", "Choose a brewery:", choices = top10[1:30,]$brewery, selected="Deschutes Brewery"),
      sliderInput("mincount", "Select min count Medal",   min = 10, max = 25, value = 10 )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Medals", plotlyOutput("medal", height = "600px")),
                  tabPanel("Breweries", plotOutput("brewery", height = "900px"))
      )
    )
  )
)

# Server side logic for UI components with reactive elements
server <- function(input, output) {
  
   evt <- eventReactive(input$run, {
    withProgress({
      setProgress(message = "Processing....")
      getDetails(input$states_inp, input$startYear_inp, input$endYear_inp)
    })
  })
   
   output$medal <- renderPlotly({
     m <- evt()
     md <- m %>%
       group_by(year, medal) %>%
       mutate(counts = n())
     
     p <- ggplot(md ,aes(
       x = year,
       fill = medal)) +scale_fill_manual(values = medal_colors)+
       geom_bar(position="stack", 
                stat='count')
     ggplotly(p)
   })
   
   observeEvent(input$run, {
     output$brewery <- renderPlot({

       before <- top10 %>% slice(1:25) %>%
         mutate(deschutes=ifelse(brewery==input$brewery_inp,"Y","N")) %>%
         filter(n>input$mincount)%>%
         ggplot(aes(x=n,y=reorder(brewery,n),fill=deschutes))+
         geom_col()+
         geom_text(aes(label=n,x=n-5),col="white",size=5)+
         geom_text(aes(label=state,x=5),col="white",size=4)+
         scale_fill_manual(values=c("grey40","goldenrod"))+
         theme(legend.position = "none",
               axis.text.x = element_blank())+
         labs(x="Number medals",y="",title="Before",subtitle="Original registered names")
       
       after <- top10b %>% slice(1:25) %>%
         mutate(deschutes=ifelse(brewery2==input$brewery_inp,"Y","N")) %>%
         filter(n>input$mincount)%>%
         ggplot(aes(x=n,y=reorder(brewery2,n),fill=deschutes))+
         geom_col()+
         geom_text(aes(label=n,x=n-5),col="white",size=5)+
         geom_text(aes(label=state,x=5),col="white",size=4)+
         scale_fill_manual(values=c("grey40","goldenrod"))+
         theme(legend.position = "none",
               axis.text.x = element_blank())+
         labs(x="Number medals",y="",title="After",subtitle="No fuzzy duplicates")
       
       deschutes <- beer %>%
         filter(brewery2==input$brewery_inp & year>as.numeric(substr(input$startYear_inp, 1, 4)) & year<as.numeric(substr(input$endYear_inp, 1, 4))) %>% 
         mutate(medal_num=case_when(medal=="Gold" ~ 3,
                                    medal=="Silver" ~ 2,
                                    medal == "Bronze" ~ 1)) %>%
         ggplot(aes(x=year,y=medal_num,label=beer_name))+
         geom_segment(aes(x=as.numeric(substr(input$startYear_inp, 1, 4)),xend=as.numeric(substr(input$endYear_inp, 1, 4)),y=0,yend=0))+
         geom_segment(aes(xend=year,yend=0,col=brewery),size=1.5)+
         geom_point(size=2)+
         scale_y_continuous(breaks=1:3,labels = c("Bronze","Silver","Gold"))+
         labs(x="",y="",title="Why?",subtitle="Two different names",col=NULL)+
         theme(legend.position = c(0.8,0.7))
       
       #Combining the plots
       
       ((before | after) / deschutes )+
         plot_layout(heights=c(0.7,0.3))&
         theme(plot.background = element_rect(fill = "white"),
               panel.background = element_rect(fill="white"),
               axis.title = element_text(family = "sans" ,size=14),
               axis.text = element_text(family = "sans" ,size=14),
               plot.title = element_text(family = "sans", face = "bold", size = 20),
               plot.subtitle = element_text(family = "sans" ,size=16))
     })
   })
}

shinyApp(ui = ui, server = server)
