#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

.onLoad <- function(libname, pkgname){
  packageStartupMessage( "rchannelattribution loaded" )
  packageStartupMessage( "Modelo de atribución basado en datos." )
  packageStartupMessage( "Uso: dataDrivenModelReport(importPath, exportPath, archive = F, openreport = F)")
  packageStartupMessage( " importPath: directorio con los archivos csv que contienen las rutas de conversión de los usuarios")
  packageStartupMessage( " exportPath: directorio de generaciÃ³n del informe en Excel")
  packageStartupMessage( " archive: indicar T para archivar los csv procesados")
  packageStartupMessage( " openreport: inficar T para arbrir el informe en Excel tras su generaciÃ³n")
  packageStartupMessage( paste0(lubridate::year(Sys.Date())), " El arte de medir" )
}



get_report_dates <- function(report_file) {
  dates <- strsplit(scan(report_file, skip = 3, nlines = 1, what = character(), quiet =T)[2], "-")
  min_report_date <- min(as.Date(dates[[1]], format = "%Y%m%d"))
  max_report_date <- max(as.Date(dates[[1]], format = "%Y%m%d"))
  return(list(report_file=report_file,min_report_date=min_report_date, max_report_date=max_report_date))

}


check_report_dates <- function(import_path) {

  ga_data_files <- list.files(import_path, pattern="*.csv$",full.names = T)

  report_dates <-  lapply(ga_data_files, get_report_dates)

  identicalValue <- function(x,y) if (identical(x,y)) x else FALSE
  all_equal <- Reduce(identicalValue,report_dates)

  if(is.logical(all_equal)) {
    print(lapply(ga_data_files, get_report_dates) %>% bind_rows)
    stop("Las fechas de los archivos facilitados no son idénticas.")
  }
}

# check_report_dates('data')






load_from_csv <- function(import_path) {
  cat('Importando datos de rutas de conversión de "', import_path, '"\n')
  print(list.files(import_path))

  if (missing(import_path))
    stop("Need to specify import_path for calculations.")

  conversionpathsreports <- rev(list.files(import_path, pattern="*.csv$", full.names = T))

  if(!length(conversionpathsreports))
    stop("No csv files to process")

  # foo<- read_csv(conversionpathsreports[1],
  #                skip = 7,
  #                col_names = c("path", "total_conversions", "total_conversion_value"),
  #                locale = locale(decimal_mark = ",", grouping_mark = "."))

  mcf.df <- conversionpathsreports %>%
    map(read_csv,
        skip=7,
        col_names = c("path", "total_conversions", "total_conversion_value"),
        locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
    dplyr::bind_rows() %>%
    mutate(total_conversion_value = str_replace(total_conversion_value, fixed("."), ""),
           total_conversion_value = str_replace(total_conversion_value, fixed(","), "."),
           total_conversion_value = as.numeric(str_trim(str_replace(total_conversion_value, fixed("€"), "")))) %>%
    arrange(-total_conversions) %>%
    na.omit()

  names(mcf.df) <- c('path', 'total_conversions','total_conversion_value')

  return(mcf.df)
}




codificar <-  function(x){
  df <- data_frame(path = x)
  df <- df %>% left_join(unique_paths.df,by = "path") %>% pull(path_code)
  return(df)
}




preprocess_mcf_data <- function(mcf.df) {
  paths <- as.vector(unlist(strsplit(mcf.df$path,split=' > ', fixed=F)))
  unique_paths.df <- data_frame(path_code = as.character(seq(1:length(unique(paths)))), path = unique(paths))
  paths <- mcf.df  %>% pull(path) %>% strsplit(split=' > ', fixed=F)

  cl <- parallel::makeCluster(parallel::detectCores() )
  parallel::clusterExport(cl=cl, varlist=c("paths", "unique_paths.df", "data_frame", "%>%", "left_join", "pull"), envir=environment())

  paths <- parallel::parLapply(cl, paths, codificar)
  mcf.df$path_code <- parallel::parSapply(cl, paths,function(x) paste(x,collapse=" > "))

  mcf.df <- as_data_frame(mcf.df)
  unique_paths.df <- as_data_frame(unique_paths.df)

  parallel::stopCluster(cl)

  return(list(data=mcf.df,paths=unique_paths.df))
}



model_mcf_data <- function(mcf){

  mcf_unique.df <- mcf$data %>% filter(!grepl('>', path))

  mcf_multiple.df <- mcf$data %>% filter(grepl('>', path))

  cat('Calculando modelos heurísticos ...', "\n")
  H <- ChannelAttribution::heuristic_models(mcf$data,"path_code","total_conversions",var_value="total_conversion_value")

  cat('Calculando modelo probabilístico ...', "\n")
  M <- ChannelAttribution::markov_model(mcf_multiple.df, "path_code", "total_conversions", var_value="total_conversion_value", order = 1, out_more = T)

  unique_paths.df <- mcf$paths

  M$transition_matrix %>% as_tibble() %>%
    mutate(channel_from = as.character(channel_from),
           channel_to = as.character(channel_to)) %>%
    rename(path_code = channel_from) %>%
    inner_join(unique_paths.df, by = c('path_code' = 'path_code')) %>%
    rename(channel_from= path) %>%
    select(-path_code) %>%
    rename(path_code= channel_to) %>%
    left_join(unique_paths.df, by = c('path_code' = 'path_code')) %>%
    rename(channel_to= path) %>%
    mutate(channel_to = if_else(is.na(channel_to), "(conversion)", channel_to)) %>%
    mutate_at(vars(contains("transition_probability")), funs(round),4) %>%
    select(channel_from, channel_to,transition_probability) -> transition_matrix

  R <- H %>% inner_join(M$result, by = c('channel_name' = 'channel_name')) %>%
    as_data_frame() %>%
    mutate(channel_name=as.character(channel_name)) %>%
    rename(path_code= channel_name) %>%
    inner_join(mcf$paths, by = c('path_code' = 'path_code')) %>%
    dplyr::select(-path_code) %>%
    dplyr::select(path, everything()) %>%
    mutate_at(vars(contains("value"),contains("conversions")), funs(round), 2) %>%
    arrange(desc(total_conversions))

  R <- full_join(R, mcf_unique.df %>% select(-path_code)) %>%
    group_by(path) %>%
    summarise_all("sum", na.rm=T) %>%
    mutate_at(.vars = vars(ends_with("touch_conversions")),
              .funs = funs(if_else(is.na(.), total_conversions,.))) %>%
    mutate_at(.vars = vars(ends_with("touch_value")),
              .funs = funs(if_else(is.na(.), total_conversion_value,.))) %>%
    arrange(desc(total_conversions))

  removal_effect <- M$removal_effects %>%
    as_tibble() %>%
    mutate(channel_name = as.character(channel_name)) %>%
    rename(path_code = channel_name) %>%
    left_join(unique_paths.df, by = c('path_code' = 'path_code')) %>%
    rename(channel_name = path) %>%
    select(channel_name, removal_effects_conversion, removal_effects_conversion_value)

  return(list(data=R,transition_matrix=transition_matrix, removal_effect= removal_effect))

}


data_driven_model <- function(import_path) {
  data_driven_model_res <- import_path %>%
    load_from_csv() %>%
    preprocess_mcf_data() %>%
    model_mcf_data

  data_driven_model_res
}

