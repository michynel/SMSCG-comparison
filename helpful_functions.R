# helpful import functions

# pull from EDI

get_edi_file <- function(pkg_id, fnames, verbose = FALSE){
  # get revision
  revision_url <- glue::glue('https://pasta.lternet.edu/package/eml/edi/{pkg_id}')
  all_revisions <- readLines(revision_url, warn = FALSE) 
  latest_revision <- tail(all_revisions, 1)
  if (verbose) {
    message('Latest revision: ', latest_revision)
  }
  # get entities 
  pkg_url <- glue::glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}')
  all_entities <- readLines(pkg_url, warn = FALSE)
  name_urls <- glue::glue('https://pasta.lternet.edu/package/name/eml/edi/{pkg_id}/{latest_revision}/{all_entities}')
  names(all_entities) <- purrr::map_chr(name_urls, readLines, warn = FALSE)
  if (verbose) {
    message('Package contains files:\n', 
            stringr::str_c('    ', names(all_entities), sep = '', collapse = '\n'))
  }
  # select entities that match fnames
  fname_regex <- stringr::str_c(glue::glue('({fnames})'), collapse = '|')
  included_entities <- all_entities[stringr::str_detect(names(all_entities), fname_regex)]
  if(length(included_entities) != length(fnames)){
    stop('Not all specified filenames are included in package')
  }
  # download data
  if (verbose) {
    message('Downloading files:\n',
            stringr::str_c('    ', names(included_entities), sep = '', collapse = '\n'))
  }
  dfs <- purrr::map(glue::glue('https://portal.edirepository.org/nis/dataviewer?packageid=edi.{pkg_id}.{latest_revision}&entityid={included_entities}'),
                    readr::read_csv, guess_max = 1000000)
  names(dfs) <- names(included_entities)
  dfs
}

# pull from SharePoint

abs_path_emp <- function(fp_rel = NULL) {
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/'
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_emp))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_emp, fp_rel))
  }
  
  return(fp_abs)
}