library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(viridis)

#import weather data
weather_2020 <- read_csv("D:\\Projects\\crime_weather\\data\\cvg_weather_daily_2020.csv")

#import crime report
crime_2020 <- read_csv("D:\\Projects\\crime_weather\\data\\PDI__Police_Data_Initiative__Crime_Incidents.csv")

#clean and organize weather
weather_2020_cl <- weather_2020 %>%
  mutate(Date = as.Date(DATE),
         DailyAvgTemp = as.numeric(TAVG)) %>%
  select(Date, DailyAvgTemp) %>%
  arrange(Date) %>%
  drop_na()

#some simple stats
  #counts by crime types
  crime_cnts_offenses <- crime_2020 %>%
    count(OFFENSE, sort = TRUE)

  #counts by gender
  crime_cnts_sex <- crime_2020 %>%
    count(VICTIM_GENDER, sort = TRUE)

  #counts by age
  crime_cnts_age <- crime_2020 %>%
    count(VICTIM_AGE, sort = TRUE)

  #counts by facility
  crime_cnts_facility_type <- crime_2020 %>%
    count(LOCATION, sort = TRUE)

  #counts by neighborhood
  crime_cnts_neighborhood <- crime_2020 %>%
    count(CPD_NEIGHBORHOOD, sort = TRUE)

  #counts by hate crime
  crime_cnts_hate_crimes <- crime_2020 %>%
    count(HATE_BIAS, sort = TRUE)

#clean and organize crime
crime_2020_cl <- crime_2020 %>%
  mutate(Effective_Date = str_sub(DATE_FROM, start = 1, end = 10),
         Effective_Date = as.Date(Effective_Date, format = "%m/%d/%Y")) %>%
  filter(Effective_Date >= as.Date("2020-01-01") & Effective_Date <= Sys.Date()) %>%
  group_by(Effective_Date, OFFENSE) %>%
  summarize(Counts = n())

#identify highest freq crimes (top 5) for 2020
high_freq_2020 <- crime_2020_cl %>%
  group_by(OFFENSE) %>%
  summarize(cnt = n()) %>%
  arrange(desc(cnt)) %>%
  slice_max(n = 5, order_by = cnt)

#stat_smooth_func
stat_smooth_func <- function(mapping = NULL, data = NULL,
                             geom = "smooth", position = "identity",
                             ...,
                             method = "auto",
                             formula = y ~ x,
                             se = TRUE,
                             n = 80,
                             span = 0.75,
                             fullrange = FALSE,
                             level = 0.95,
                             method.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             xpos = NULL,
                             ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}


StatSmoothFunc <- ggproto("StatSmooth", Stat,
                          
                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))
                              
                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }
                            
                            params
                          },
                          
                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }
                            
                            if (is.null(data$weight)) data$weight <- 1
                            
                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }
                            
                            if (is.character(method)) method <- match.fun(method)
                            
                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))
                            
                            m = model
                            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                                             list(a = format(coef(m)[1], digits = 3), 
                                                  b = format(coef(m)[2], digits = 3), 
                                                  r2 = format(summary(m)$r.squared, digits = 3)))
                            func_string = as.character(as.expression(eq))
                            
                            if(is.null(xpos)) xpos = min(data$x)*0.9
                            if(is.null(ypos)) ypos = max(data$y)*0.9
                            data.frame(x=xpos, y=ypos, label=func_string)
                            
                          },
                          
                          required_aes = c("x", "y")
)


#join to weather, filter by high-frequency crimes
crime_weather_2020_plot <- crime_2020_cl %>%
  left_join(weather_2020_cl, by = c("Effective_Date" = "Date")) %>%
  drop_na() %>%
  inner_join(high_freq_2020, by = c("OFFENSE")) %>%
  mutate(OFFENSE = case_when(OFFENSE == "CRIMINAL DAMAGING/ENDANGERING" ~ "DMG/ENDANG",
                             OFFENSE == "DOMESTIC VIOLENCE" ~ "DOM VIOLENCE",
                             TRUE ~ OFFENSE)) %>%
  #ggplot
  ggplot(aes(x = DailyAvgTemp, y = Counts)) +
  #geoms
  geom_point(aes(color = OFFENSE), show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x) +
  #labels
  stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) +
  labs(title = "Cincinnati Crime and Avg Daily Temp",
       subtitle = "by Top 5 Criminal Offense Type (2020)",
       x = "Daily Avg Temp (deg. F)") +
  #scales
  scale_color_viridis(discrete = TRUE) +
  #facet
  facet_grid(rows = vars(OFFENSE)) +
  #theme
  theme(text = element_text(family = "mono", size = 12),
        title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10))

ggsave(filename = "cincy_top_5_crimes_vs_avgdailytemp_2020.pdf", device = "pdf", path = "D:\\Projects\\crime_weather\\plots\\", width = 17, height = 11, units = c("in"))
