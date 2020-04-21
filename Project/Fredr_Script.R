#USING FREDR TO GET THE DATA
fredr_set_key("91de61ed607a6478d01c35d9a903017c")


test <- if (requireNamespace("purrr", quietly = TRUE)) {
  
  library(purrr)
  purrr::map_dfr(c('MIAM112NAN', 'SMU12331000500000002SA',
                   'SMU12331000500000003',	'SMU12331000500000011',	'SMU12331000600000001'), fredr)
  
  # Using purrr::pmap_dfr() allows you to use varying optional parameters
  params <- list(
    series_id = c('MIAM112NAN', 'SMU12331000500000002SA',
                  'SMU12331000500000003',	'SMU12331000500000011',	'SMU12331000600000001')
  )
  
  purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr(series_id = .x)
  )
  
} 
data <- pivot_wider(test, 
                    names_from = series_id,
                    values_from = value)
