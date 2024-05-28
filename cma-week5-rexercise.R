#Task 1 ----

bmi <- \(height, weight) weight/(height^2)

deg_far <-\(celsius) celsius*9/5+32

euclidean_distance <-\(x1,x2,y1,y2)  sqrt((x2-x1)^2+(y2-y1)^2)

#Task 2 ----
library("readr")
library("dplyr")
library("sf")
library("lubridate")
library("ggplot2")
library("plotly")

wildschwein <- read_delim("wildschwein_BE_2056.csv", ",")

wildschwein <- wildschwein |>
  filter(TierName =="Sabi"|TierName=="Rosa", DatetimeUTC > "2015-04-01 02:00:00", DatetimeUTC < "2015-04-16 02:00:00")#weird 02:00:00 timestamp because of the timezone-format

#Task 3 ----
wildschwein$DatetimeUTC <-round_date(wildschwein$DatetimeUTC,"15 mins")

#Task 4 ----
sabi<-wildschwein |>filter(TierName =="Sabi")
rosa<-wildschwein |>filter(TierName =="Rosa")

beide <- inner_join(sabi,rosa,by=join_by(DatetimeUTC), suffix=c(".Sabi",".Rosa"))
beide$euclidean <-euclidean_distance(beide$E.Sabi, beide$E.Rosa, beide$N.Sabi, beide$N.Rosa)

beide <- beide |>
  mutate(meet = euclidean <= 100)

#Task 5 ----
beide_filter <- beide |>  filter(meet==TRUE)

ggplot()+
  geom_point(sabi, mapping=aes(x=E,y=N), color="#00BFC4", alpha=0.3)+
  geom_point(rosa, mapping=aes(x=E,y=N),color="#F8766D", alpha=0.3)+
  geom_point(beide_filter,mapping=aes(x=E.Sabi,y=N.Sabi),pch=21, alpha=0.7,fill="#00BFC4", color="black")+
  geom_point(beide_filter,mapping=aes(x=E.Rosa,y=N.Rosa),pch=21, alpha=0.7,fill="#F8766D", color="black")

#Task 6 ----
#this part is adapted from Saskia's code
plot_ly(beide, x = ~E.Rosa, y = ~N.Rosa, z = ~DatetimeUTC, 
        type = 'scatter3d', mode = 'lines', opacity = 0.4, 
        line = list(width = 6, color = "#F8766D", reverscale = FALSE)) |> 
  add_trace(beide, x = ~E.Sabi, y = ~N.Sabi, z = ~DatetimeUTC, 
            type = 'scatter3d', mode = 'lines', opacity = 0.4, 
            line = list(width = 6, color = "#00BFC4", reverscale = FALSE)) |>
  add_markers(beide_filter, x = ~beide_filter$E.Rosa, y = ~beide_filter$N.Rosa, 
              z = ~beide_filter$DatetimeUTC, 
              type = 'scatter3d', mode = 'markers', opacity = 0.8, 
              marker = list(size = 6, color = "#F8766D"))  |> 
  add_markers(beide_filter, x = ~beide_filter$E.Sabi, y = ~beide_filter$N.Sabi, 
              z = ~beide_filter$DatetimeUTC, 
              type = 'scatter3d', mode = 'markers', opacity = 0.8, 
              marker = list(size = 6, color = "#00BFC4", reverscale = FALSE)) 
