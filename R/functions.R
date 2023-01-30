#' Data
#'
#' The function returns data series or tables. Due to CRAN package size requirements, the data is stored on github.
#'
#' `get_series()` load entire series or tables. For example: get_series(choice="series")
#' `series_data` contains all series available in flow of funds and `tables_data` contains all tables and series
#'
#' @section series_data:
#' \itemize{
#'  \item date - Time-period
#'  \item value - Observed-Value
#'  \item SERIES_NAME - name of a series. There are more than twenty thousands series exist
#'  \item CURRENCY - USD or NA. NA means series is percent or index
#'  \item SERIES_PREFIX - Each prefix letters indicate the series type. see fedz1:::meaning_of_prefix()
#'  \item UNIT - Currency, percent or index
#'  \item UNIT_MULT - 1,1,000 or 1,000,000
#'  \item short description _ description of the series
#'  \item FREQ - A:annually data; Q:quarterly data
#' }
#'@seealso \url{https://www.federalreserve.gov/datadownload/Choose.aspx?rel=z1}
#'
#'@section tables_data:
#'\itemize{
#' \item tname -  name of a table
#'  \item SERIES_NAME - name of a series. There are more than twenty thousands series exist
#'  \item Description - Long Description of series
#'    \item date - Time-period
#'     \item value - Observed-Value
#'      \item UNIT - Currency, percent or index
#'}
#'
#' @seealso \url{https://www.federalreserve.gov/apps/fof/FOFTables.aspx}
#' @param choice can be "series" or "tables
#' @return  series or tables
#' @export
#'
get_series<- function (choice=c("series","tables")){

  if (choice=="series") {

    series_data<-"https://github.com/shaf1430/rfeddata/blob/main/data/series_data.Rda/?raw=true"

    load(url(series_data))

    series_data
  }

  else if (choice=="tables") {

    tables_data<-"https://github.com/shaf1430/rfeddata/blob/main/data/tables_data.Rda/?raw=true"

    load(url(tables_data))

    tables_data

  }
}


#' Title of tables
#'
#' `all_tables_title()` returns title of all tables. It is helpful to get familiar with available tables. Each table contains
#' a number of data set.
#'
#' @seealso \url{https://www.federalreserve.gov/apps/fof/FOFTables.aspx}
#' @return  A tibble of title of all tables
#' @examples
#' all_tables_title()
#' @export
#'
all_tables_title<-function() {

  df<-unique(table_detail$name)
  df
}

#' Description of a table
#'
#' `table_descr()` return the definition of a table. Each table contains a number of data set.
#' @param table_title is name of a single table or vector of multiple tables. You can obtain title of tables by `all_tables_title()`
#'
#' @seealso \url{https://www.federalreserve.gov/apps/fof/Guide/z1_tables_description.pdf}
#' @return  character for definition of a table(s)
#' @examples
#' table_descr('Credit Unions')
#' table_descr(c('Net Capital Transfers',"Closed-End Funds"))
#' @export
#'

table_descr<-function (table_title){

  df<-subset(table_detail[,c("summary","name")],name %in% table_title)
  df<-unique(df)
  print(df$summary)
}


#' Definition of series prefix
#'
#' `meaning_of_prefix()` returns definition of series. Each series start with two letters. Each prefix letters indicate the series type.
#' @param prefix is SERIES_PREFIX. Value could be FA, FC, FG, FI, FL, FR, FS, FU, FV, LA, LM, PC.
#'
#' @seealso \url{https://www.federalreserve.gov/apps/fof/SeriesStructure.aspx}
#' @return  character for definition of series
#' @examples
#' meaning_of_prefix("FL")
#' @export
#'

meaning_of_prefix<-function (prefix) {

  df<-subset(mprefix,SERIES_PREFIX == prefix)

  print(df$meaning)

  message("value could be FA, FC, FG, FI, FL, FR, FS, FU, FV, LA, LM, PC")

}

#' search series
#'
#' `search_series()` return all series with the search. For example: search_series("Real estate investment trusts")
#' @param series is SERIES name
#' @return  A tibble of series
#' @export
#'
search_series<-function(series) {

  if (exists("series_data")==FALSE) {
    series_data<-"https://github.com/shaf1430/rfeddata/blob/main/data/series_data.Rda/?raw=true"
    load(url(series_data))

    df<-subset(series_data,grepl(series,`short description`))
    df[,c("date","value","SERIES_NAME","UNIT","UNIT_MULT","short description")]
  }

  else{

    df<-subset(series_data,grepl(series,`short description`))
    df[,c("date","value","SERIES_NAME","UNIT","UNIT_MULT","short description")]
  }
}

#' search tables
#'
#' `search_tables()` return series associated with a table
#' @param tables is table name
#' @return  A tibble of series in a table
#' @examples
#' search_tables("Nonfinancial Corporate Business")
#' @export
#'
search_tables<-function(tables) {

  if (exists("tables_data")==FALSE) {
    tables_data<-"https://github.com/shaf1430/rfeddata/blob/main/data/tables_data.Rda/?raw=true"
    load(url(tables_data))

    df<-subset(tables_data,grepl(tables,tname))
    df[,c("date","value","SERIES_NAME","UNIT","tname")]
  }

  else{

    df<-subset(tables_data,grepl(tables,tname))
    df[,c("date","value","SERIES_NAME","UNIT","tname")]
  }
}


globalVariables(c("tables_data", "mprefix", "SERIES_PREFIX","Description","tname","series_data","SERIES_NAME",
                  "table_detail","name","series","tables","short description"))


#' Title of series with a short description
#'
#' Data is obtained from a web scraping from flow of funds website
#'
#' @format ## `prds`
#' A data frame with 11,291 rows and 2 columns:
#' \describe{
#'   \item{title1}{series name}
#'   \item{short description}{description of the series}
#' }
#' @source <https://www.federalreserve.gov/datadownload/Choose.aspx?rel=z1>
"prds"

#' Title of tables
#'
#' Data is obtained from a web scraping from Financial Accounts Guide
#'
#' @format ## `table_detail`
#' A data frame with 198 rows and 3 columns:
#' \describe{
#'   \item{summary}{title as shows on the source website}
#'   \item{name}{title of the table}
#'   \item{code}{code of each table as shows on the source website}
#' }
#' @source <https://www.federalreserve.gov/apps/fof/FOFTables.aspx>
"table_detail"


#' Meaning of prefix
#'
#' Each prefix letters indicate the series type
#'
#' @format ## `mprefix`
#' A data frame with 12 rows and 2 columns:
#' \describe{
#'   \item{SERIES_PREFIX}{prefix of series}
#'   \item{meaning }{meaning of a prefix}
#' }
#' @source <https://www.federalreserve.gov/apps/fof/SeriesStructure.aspx>
"mprefix"














