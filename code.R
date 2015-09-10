require(readxl)
require(dplyr)
require(DT)
require(htmltools)
require(htmlwidgets)
require(metricsgraphics)
require(RColorBrewer)

df <-read_excel("all_classes.xlsx")

df_no_sup <- df %>% 
	filter(Enrolled > 3, A_Year != 'AY15_16') %>% 
	select(A_Year, Enrolled, Course, LD_UD_Grad)


datatable(df_no_sup, 
	rownames = FALSE,
	extensions = 'ColVis',
	options = list(
		order = list(list(0, 'desc'),list(3, 'asc')),
		initComplete = JS(
				"function(settings, json) {",
			    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
				"}"
		),
		dom = 'C<"clear">lfrtip')
) %>% 
formatStyle(
    'Enrolled',
    background = styleColorBar(df_no_sup$Enrolled, 'lightblue'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    fontWeight = 'bold'
) %>%
formatStyle(
	'LD_UD_Grad',
	target = 'row',
	backgroundColor = styleEqual(c('LD', 'UD', 'Grad'), c('white', 'lightyellow', 'lightorange')),
	textAlign = 'center'
) 

# For each academcy year find min, mean , max classe sizes and the total number of classes

df_stats <- df_no_sup %>% 
	group_by(A_Year) %>% 
	summarize_each(funs(min(., na.rm = TRUE), 
		                mean(., na.rm = TRUE), 
		                max(., na.rm = TRUE),
		                sum(., na.rm = TRUE),
		                n()), matches("Enrolled"))

ay <-bind_cols(data.frame(year=c(2002:2014), df_stats))

ay %>%
  mjs_plot(x=year, y=mean, width= 800, height = 600) %>%
  mjs_point(point_size = 5, least_squares = TRUE) %>%
  mjs_labs(x = "Year", y = "Average Class Size") 

ay %>%
  mjs_plot(x=year, y=sum, width= 800, height = 600) %>%
  mjs_point(least_squares = TRUE) %>%
  mjs_labs(x = "Year", y = "Total Number of Students Enrolled") %>%
  mjs_point(least_squares= TRUE,
            size_accessor=n,
            size_range=c(5, 15)
            ) %>%
	mjs_axis_x(xax_count = 10, 
		 extended_ticks = FALSE, 
		 min_x = 2002, 
		 max_x = 2014,
		 show_secondary_x_label = TRUE
		 ) %>% 
	mjs_axis_y(yax_count = 10, 
		 extended_ticks = FALSE 
		 ) %>% 
  mjs_add_marker(2007, "Chair of Faculty and Sabbatical") 



