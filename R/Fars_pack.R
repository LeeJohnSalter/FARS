#' fars_read
#'
#' This is a simple function that reads aswirl CSV file creating returning a dataframe.  It
#' has specifically been designed to read the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (FARS).
#' The function is used throughout the set of fars_ functions
#'
#' @param filename -  A character string defining the CSV file to be imported
#'
#' @return This function returns a dataframe of the CSV file being read
#'
#' @note The function first checks for the existence of a file so will stop if the file is not found
#'
#' @examples
#'    fars_read(filename)
#'    fars_read(data/filename.csv)
#'    my_data<- fars_read(data/filename.csv)
#'
#' @seealso
#'  \link{fars_read_years}
#'  \link{fars_summarize_years}
#'  \link{fars_map_state}
#'
#' @references
#' The US National Highway Traffic Safety Administration's Fatality Analysis Reporting System,
#' which is a nationwide census, provides the American public yearly data regarding
#' fatal injuries suffered in motor vehicle traffic crashes.
#' For more information including data that can be downloaded:
#'   \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' make_filename
#'
#' This function creates a filename in a format as created by
#' the Fatality Analysis Reporting System (FARS) for a specified year.
#'
#' @param year -   This is the year - as a 4 digit number. he parameter can be provided as a string, integer or numer since the process
#' uses /link{as.integer} to coerce the parameter to an integer.
#'
#' @return       A character vector containing the filename based on the year provided:
#'               accident_[Year].csv.bz2
#' @note
#' The use of any non-numeric data will therefore cause the function to fail with a
#' warning : NAs introduced by coercion
#'
#'
#' @examples
#'    make_filename("2019")
#'    make_filename(2019)
#'    make_filename(2019.0)
#'
#' @seealso
#'  \link{fars_read_years}
#'  \link{fars_map_state}
#'
#' @references
#' The US National Highway Traffic Safety Administration's Fatality Analysis Reporting System,
#' which is a nationwide census, provides the American public yearly data regarding
#' fatal injuries suffered in motor vehicle traffic crashes.
#' You can download the FARS data at \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' This reads one or more years Fatality Analysis Reporting System (FARS) data
#' created from the read_fars function containing only the Month and year from the data
#'
#' @param year -   Years as a 4 digit number.
#'                 The year  can be provided as single year, as a vector or list of years
#'
#' @return Dataframe  - containing only the year and month from the underlying FARS data.
#'
#' @note
#' Function requires the magittr library to be available.
#' If the underlying CSV file is not stored in the current working directory the process
#' will call an warning :
#'       In value[[3L]](cond) : invalid year: 2013
#'
#' Code needs to be updated to unclude a directory option as an input parameter
#'
#'
#' @examples
#'    fars_read_years(2013)
#'    fars_read_years(c(2013,2014,2015))
#'    fars_read_years(c("2013","2014","2015"))
#'
#' @importFrom magrittr %>%
#'
#' @seealso
#'  \link{fars_read_years}
#'  \link{fars_map_state}
#'  \link{fars_summarize_years}
#'
#' @references
#' The US National Highway Traffic Safety Administration's Fatality Analysis Reporting System,
#' which is a nationwide census, provides the American public yearly data regarding
#' fatal injuries suffered in motor vehicle traffic crashes.
#' You can download the FARS data at \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years
#'
#' fars_summarize_years takes the table(s) of years and months, created by the fars_read_years function
#' and  binds these dataframes together, summarizing the data to show the
#' frequency of accidents for each month, with seperate columns for each year specified.

#' @param year -   Years as a 4 digit number.
#'                 The year  can be provided as single year, as a vector or list of years
#'
#' @return       Dataframe containing:  Month, Year1,[Year2]..[YearN]
#'
#' @note
#' If the underlying CSV file is not stored in the current working directory the process
#' will call an warning :
#'       In value[[3L]](cond) : invalid year: 2013
#' Requires the use of the pipe function from the magrittr library
#' Code needs to be updated to unclude a directory option as an input parameter
#'
#'
#' @examples
#'    fars_summarize_years(2013)
#'    fars_summarize_years(c(2013,2014,2015))
#'    fars_summarize_years(c("2013","2014","2015"))
#'
#'
#' @importFrom magrittr %>%
#'
#' @seealso
#'  \link{fars_read_years}
#'  \link{fars_map_state}
#'  \link{fars_read_years}
#'
#' @references
#' The US National Highway Traffic Safety Administration's Fatality Analysis Reporting System,
#' which is a nationwide census, provides the American public yearly data regarding
#' fatal injuries suffered in motor vehicle traffic crashes.
#' You can download the FARS data at \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @export
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state
#'
#' fars_map_state plots the state given as an integer argument and shows accidents
#' for a given year using the LONGITUD and LATITUDE points stored within the data.
#'
#' @param state.num - State number to provide a plot [Integer]/[String] 2 digits
#' @param year -   Years as a 4 digit number - The year  can be provided as single year, as a vector or list of years
#'
#' @return  dataframe -  of filtered data, if there are no observations for a given state then returns (NULL)
#'
#' @note
#' This function depends on map and points functions from the maps library.
#' Errors occor when an invalid State.num is provided
#' Other errors can be linked to the fars_read function.
#'
#' @examples
#'    fars_map_state(10,2013)
#'
#'
#' @seealso
#'  \link{make_filename}
#'  \link{fars_read}
#'
#' @references
#' The US National Highway Traffic Safety Administration's Fatality Analysis Reporting System,
#' which is a nationwide census, provides the American public yearly data regarding
#' fatal injuries suffered in motor vehicle traffic crashes.
#' You can download the FARS data at \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
