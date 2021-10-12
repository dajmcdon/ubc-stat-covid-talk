#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)

primary = "#654ea3"
secondary = "#F9AC2F"


fl_actuals <- readRDS("fl_actuals.rds") %>% 
    rename(forecast_date = target_end_date)
fl_preds <- readRDS("fl_predictions.rds") 

g <- ggplot() +
    geom_line(data = fl_actuals, aes(forecast_date, value), color = "black") +
    #geom_point(data = fl_actuals, aes(target_end_date, value), color = "black") +
    coord_cartesian(xlim = ymd(c("2020-06-01", NA))) +
    ylab("Weekly incident cases") +
    xlab("Date") +
    scale_y_continuous(labels = scales::label_number_si()) +
    scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
    theme_bw(base_size = 24) +
    theme(legend.position = "bottom", legend.title = element_blank())

filt <- function(fd) {
    fd <- ymd(fd)
    df <- filter(fl_preds, forecast_date == fd)
    layer <- list(
        geom_vline(xintercept = fd, linetype = "dashed"),
        geom_line(data = df, aes(target_end_date, point, color = forecaster)),
        geom_point(data = df, aes(target_end_date, point, color = forecaster)),
        geom_ribbon(data = df, 
                    aes(x = target_end_date, ymin = lo, ymax = hi, fill = forecaster),
                    alpha = .4),
        scale_color_manual(values = c(primary, secondary)),
        scale_fill_manual(values = c(primary, secondary))
    )
    return(layer)
}


forecast_dates <- sort(unique(fl_preds$forecast_date)) 
forecast_dates <- forecast_dates[forecast_dates < "2021-10-01"]
fcast_df <- fl_preds %>% filter(forecast_date == min(forecast_date))


ui <- fluidPage(
    fluidRow(
        sliderInput("forecast_date", min = min(forecast_dates), 
                label = "", timeFormat = "%d %b %Y",
                max = max(forecast_dates), step = 7, 
                animate = TRUE,
                value = min(forecast_dates), width = "100%")
        ),
    fluidRow(plotOutput("plot1", height = "500px"))
)

server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        g + filt(input$forecast_date)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
