library(htmlwidgets)

#' @export
#'
idToName <- function(x) {
  s <- strsplit(x, "_")[[1]]
  paste(s, sep="", collapse=" ")
}

#' @export
#'
nameToId <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(s, sep="", collapse="_")
}

#' Returns a template for the Query Builder filter on the basis of a Data Frame
#'
#' @param data Data Frame from which the list of filters will be created.
#'
#' @import htmlwidgets
#'
#' @examples
#' #### Iris data example.
#' # Load data
#' data(iris)
#' # Rename columns and get filters
#' colnames(iris)<-c("sepal_length","sepal_width","petal_length","petal_width","species")
#' filters<-getFiltersFromTable(iris)
#' # Set initial rules to NULL
#' rules<-NULL
#' # Create Query Builder widget
#' queryBuildR(rules,filters)
#' @export
#'
getFiltersFromTable<-function() {
  filters<-list()
  data<-c('coupon','maturity','price','yeild')
  namesCol<-colnames(data)
  niceNames<-as.vector(sapply(namesCol,idToName))
  filterCol = list(
              list( id= 'Callable',
                  label= 'Callable',
                  type= 'string',
                  input = 'select',
                  values = c('Yes', 'No', 'NA'),
                  default_value='Yes',
                  operators=list('equal','not_equal','contains', 'in', 'not_in','is_null', 'is_not_null')),
               list( id= 'coupon',
                  label= 'coupon',
                  type= 'double',
                  default_value=3,
                  operators=list('equal','not_equal','less', 'less_or_equal', 'greater','greater_or_equal','between','in', 'not_in','is_null', 'is_not_null')),
               list( id= 'maturity',
                  label= 'maturity',
                  type= 'datetime',
                  default_value='01/01/2020',
                  operators=list('equal','not_equal','less', 'less_or_equal', 'greater','greater_or_equal','between','in', 'not_in','is_null', 'is_not_null')),
               list( id= 'price',
                  label= 'price',
                  type= 'double',
                  default_value=100,
                  operators=list('equal','not_equal','less', 'less_or_equal', 'greater','greater_or_equal','between','in', 'not_in','is_null', 'is_not_null')),
               list(id= 'yeild',
                  label= 'yeild',
                  type= 'double',
                  default_value=3,
                  operators=list('equal','not_equal','less', 'less_or_equal', 'greater','greater_or_equal','between','in', 'not_in','is_null', 'is_not_null'))
              )
    filters<-c(filters,filterCol)
  filters
}

#' Create a Query Builder widget
#'
#' @param rules List initializing the set of rules for the Query Builder widget.
#' @param filters List specifying the variables, conditions and value ranges for building the queries.
#'
#' @import htmlwidgets
#'
#' @examples
#' #### Iris data example.
#' # Load data
#' data(iris)
#' # Rename columns and get filters
#' colnames(iris)<-c("sepal_length","sepal_width","petal_length","petal_width","species")
#' filters<-getFiltersFromTable(iris)
#' # Set initial rules to NULL
#' rules<-NULL
#' # Create Query Builder widget
#' queryBuildR(rules,filters)
#'
#' @export
#'
queryBuildR <- function(rules,filters, width = NULL, height = NULL) {

  x = list(
    rules = rules,
    filters=filters
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'queryBuildR',
    x,
    width = width,
    height = height,
    package = 'queryBuildR'
  )
}

#' @export
queryBuildROutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'queryBuildR', width, height, package = 'queryBuildR')
}

#' @export
renderQueryBuildR <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, queryBuildROutput, env, quoted = TRUE)
}
