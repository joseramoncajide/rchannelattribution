library(rchannelattribution)

import_path <- file.path("data")


ga_data_files <- list.files(import_path, pattern="*.csv$",full.names = T)

check_report_dates('data')

get_report_dates(ga_data_files[1])

devtools::install_github('hadley/lineprof')
library(lineprof)


prof <- lineprof(load_from_csv(import_path))
shine(prof)
imported_data <- load_from_csv(import_path)

mcf<- import_path %>% load_from_csv() %>% preprocess_mcf_data()

imported_data_res <- import_path %>% load_from_csv() %>% preprocess_mcf_data() %>% model_mcf_data

imported_data_res$data %>% summarise(tot_modelo = sum(total_conversion_value), tot_first = sum(first_touch_value))

imported_data_res$data %>% View()

data_driven_model <- function(import_path) {
  data_driven_model_res <- import_path %>%
    load_from_csv() %>%
    preprocess_mcf_data() %>%
    model_mcf_data

  data_driven_model_res
}

attribution_res <- data_driven_model(import_path)

dataDrivenModelReport <- function(import_path, export_patch) {


}


all_files <- unzip(zipfile = 'data/Archivo comprimido.zip', exdir = 'tmp')
all_files <- list.files('tmp', full.names = T)
csv_files <- list.files('tmp', full.names = T, pattern = "*.csv")
unlink(setdiff(all_files, csv_files),recursive = T)

setwd("~/Documents/GitHub_EAM/rchannelattribution")
appDir <- system.file("app", "attribution", package = "rchannelattribution")
shiny::runApp(appDir, display.mode = "normal")
runApp()

# devtools::install_github('rstudio/rmarkdown')


conversion_value.df <- imported_data_res$data
highchart() %>%
  hc_chart(type = "column") %>%
  hc_xAxis(categories = conversion_value.df$path) %>%
  hc_add_series(name = "Modelo basado en datos", data = conversion_value.df$total_conversion_value, color=viridis::inferno(4)[1]) %>%
  hc_add_series(name = "Última interacción", data = conversion_value.df$last_touch_value, color=viridis::inferno(4)[2]) %>%
  hc_add_series(name = "Primera interacción", data = conversion_value.df$first_touch_value, color=viridis::inferno(4)[3]) %>%
  hc_add_series(name = "Lineal", data = conversion_value.df$linear_touch_value, color=viridis::inferno(4)[4]) %>%
  hc_yAxis(gridLineWidth = 0,labels = list(format = "{value} €", useHTML = TRUE, title = list(text = "Ingresos"))) %>%
  hc_xAxis(title = list(text = "Canal")) %>%
  hc_yAxis(title = list(text = "Ingresos")) %>%
  hc_tooltip(headerFormat = "<b>{series.name}</b><br>",
             pointFormat = "{point.y} euros")  %>%
  hc_title(text = "Comparativa entre modelos",
           useHTML = TRUE, margin = 50,
           align = "left",
           style = list(color = viridis::magma(3)[2])) %>%
  hc_subtitle(text="Importe generado por las conversiones según cada uno de los modelos",
              align = "left",
              useHTML = TRUE,
              margin = 50)


imported_data_res$transition_matrix

tm <- imported_data_res$transition_matrix %>%
  spread(key= channel_to, value = transition_probability) %>%
  replace(., is.na(.), 0)

tm_mat <- as.matrix(tm[-1]) * 100
row.names(tm_mat) <- tm$channel_from
hchart(tm_mat, type = "heatmap") %>%
  hc_colorAxis(stops = color_stops(colors = rev(viridis::inferno(8)))) %>%
  # hc_colorAxis(minColor = "#FFFFFF", maxColor = "#434348") %>%
  hc_xAxis(gridLineWidth = 0,
           labels = list(enabled = T, format = "{value}", useHTML = TRUE),
           title = list(text = "Ingresos")) %>%
  hc_xAxis(title = list(text = "Canal #2")) %>%
  hc_yAxis(title = list(text = "Canal #1")) %>%
  hc_title(text = "Interacciones entre canales",
           useHTML = TRUE, margin = 50,
           align = "left",
           style = list(color = viridis::magma(3)[2])) %>%
  hc_subtitle(text="Probabilidad de impacto por el canal #2 tras ser impacatado por el canal #1",
              align = "left",useHTML = TRUE, margin = 50)




foo <- data_driven_model(import_path = file.path("data"))
re_df <- foo$removal_effect

highchart() %>%
  hc_chart(type = "line", polar=T) %>%
  hc_xAxis(categories = re_df$channel_name) %>%
  hc_add_series(name = "Removal effect",
                data = re_df$removal_effects_conversion * 100,
                dataLabels = list(enabled = TRUE, format = '{point.y:.1f}'),
                color=viridis::inferno(4)[1]) %>%
  # hc_add_series(name = "Última interacción", data = re_df$removal_effects_conversion_value) %>%
  hc_yAxis(type = "logarithmic",gridLineWidth = 0,labels = list(enabled = F, format = "{value} %", useHTML = TRUE, title = list(text = "Ingresos"))) %>%
  hc_xAxis(title = list(text = "Canal")) %>%
  hc_yAxis(title = list(text = "Ingresos")) %>%
  hc_tooltip(headerFormat = "<b>{series.name}</b><br>",
             pointFormat = "{point.y}%")  %>%
  hc_title(text = "Importancia del canal",
           useHTML = TRUE, margin = 50,
           align = "left",
           style = list(color = viridis::magma(3)[2])) %>%
  hc_subtitle(text="Porcentaje de transacciones que se perderían de no existir el canal", align = "left",useHTML = TRUE, margin = 50)


# Install
library(devtools)
devtools::install_github("joseramoncajide/rchannelattribution")
library(rchannelattribution)
flex_app_dir <- system.file("rmd/app.Rmd", package = "rchannelattribution")
rmarkdown::run(flex_app_dir)
