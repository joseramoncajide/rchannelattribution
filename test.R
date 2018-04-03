library(rchannelattribution)

import_path <- file.path("data")


ga_data_files <- list.files(import_path, pattern="*.csv$",full.names = T)

check_report_dates('data')

get_report_dates(ga_data_files[1])


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


imported_data_res$transition_matrix

tm <- imported_data_res$transition_matrix %>%
  spread(key= channel_to, value = transition_probability) %>%
  replace(., is.na(.), 0)

tm_mat <- as.matrix(tm[-1])
row.names(tm_mat) <- tm$channel_from
hchart(tm_mat, type = "heatmap") %>%
  hc_colorAxis(stops = color_stops(colors = rev(viridis::inferno(8)))) %>%
  hc_xAxis(gridLineWidth = 0,labels = list(format = "{value}", useHTML = TRUE, title = list(text = "Ingresos"))) %>%
  hc_xAxis(title = list(text = "Días tras el primer impacto")) %>%
  hc_yAxis(title = list(text = "Canal"))




foo <- data_driven_model(import_path = file.path("data"))
re_df <- foo$removal_effect

highchart() %>%
  hc_chart(type = "line", polar=T) %>%
  hc_xAxis(categories = re_df$channel_name) %>%
  hc_add_series(name = "Modelo basado en datos",
                data = re_df$removal_effects_conversion,
                dataLabels = list(enabled = TRUE, format = '{point.y:.1f}')) %>%
  # hc_add_series(name = "Última interacción", data = re_df$removal_effects_conversion_value) %>%
  hc_yAxis(type = "logarithmic",gridLineWidth = 0,labels = list(enabled = F, format = "{value} €", useHTML = TRUE, title = list(text = "Ingresos"))) %>%
  hc_xAxis(title = list(text = "Canal")) %>%
  hc_yAxis(title = list(text = "Ingresos")) %>%
  hc_tooltip(headerFormat = "<b>{series.name}</b><br>",
             pointFormat = "{point.y}€")  %>%
  hc_title(text = "Ingresos por canal y por modelo de atribución",
           useHTML = TRUE, margin = 50,
           align = "left",
           style = list(color = "#999999"))



gettmpdir <- function() {
  tm <- Sys.getenv(c('TMPDIR', 'TMP', 'TEMP'))
  d <- which(file.info(tm)$isdir & file.access(tm, 2) == 0)
  if (length(d) > 0)
    tm[[d[1]]]
  else if (.Platform$OS.type == 'windows')
    Sys.getenv('R_USER')
  else
    '/tmp'
}

gettmpdir()
tempdir()
