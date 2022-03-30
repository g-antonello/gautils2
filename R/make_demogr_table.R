#' Make a complex demographic table
#'
#' Summarize multiple **categorical** variable columns in one handy function
#' NB: Doesn't yet work perfectly, also, it doesn't make summary statistics of continuous variables
#'
#' @param df A `data.frame` or `tibble` containing several variables of interest to make a table from
#' @param row_variables_as_are a `vector` of column names to subdivide rows by
#' @param row_var_names_nicer (Facultative) a character vector of variables names in `row_variables_as_are`, but more "presentation ready"
#' @param col_variable A `character` vector with the name of the df colum which will become columns of the
#' @param col_var_names_nicer if you want nicer-looking names to the column variable items/levels,
#' @param add_perc_column Add per-table-column's percentage column
#' @param caption . Default is `NULL`
#'
#' @return
#' @export
#'
#' @examples
#' make_demogr_table(df= mtcars,
#' row_variables_as_are = c("mpg","gear", "carb"),
#' row_var_names_nicer = c("Miles per gallon", "N° gears", "Fuel type"),
#' col_variable = "cyl",
#' add_perc_column = TRUE)
#'

make_demogr_table <- function(df, row_variables_as_are, row_var_names_nicer, col_variable, col_var_names_nicer, add_perc_column, caption=NULL){

  # remove NAs from the variables of the table, because NAs cannot yet be plotted in
  df_complete <- df[complete.cases(df[, c(row_variables_as_are, col_variable)]),]
  print(paste("Incomplete rows removed:", nrow(df) - nrow(df_complete)))

  # make list of tables, transformed as matrices
  l <- lapply(row_variables_as_are, function(x) as.matrix(table(df_complete[[x]], df_complete[[col_variable]]))) # for each row variable, build a table, in a matrix form

  # in case you want a percent column next to the original column
  if (add_perc_column){
    l <- lapply(l, add_percent_columns)
    headr <- c("1",rep("2", length(levels(factor(df[[col_variable]]))))) # initiate a header character vector, with length = 2*columns of the input table
    final_table_matr <- do.call(rbind, l) # bind all tables in the "l" list
  }else{
    # even if you don't want the relative population, still you need a header, but with the same length of the initial matrix
    headr <- c("1",rep("1", length(levels(factor(df[[col_variable]])))))
    final_table_matr <- do.call(rbind, l) # bind all tables in the "l" list
    colnames(final_table_matr) <- NULL
  }


  # initiate the header variable, the first column will be empty
  if(is.null(col_var_names_nicer)){

    col_var_names_nicer <- levels(factor(df[[col_variable]]))
  }

 names(headr) <- c(" ", col_var_names_nicer)

  library(kableExtra)
  kb <- kableExtra::kbl(final_table_matr[rowSums(final_table_matr) != 0,],format = "html", caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    add_header_above(headr)

  row_point <- 1
  row_names_both <- cbind(row_variables_as_are, row_var_names_nicer)
  for(r in 1:nrow(row_names_both)){
    kb <- pack_rows(kable_input = kb,
                    group_label = row_names_both[r,2],
                    start_row = row_point,
                    end_row = length(na.omit(unique(as.character(df_complete[[row_names_both[r,1]]])))),
                    label_row_css = "background-color: #666; color: #fff;",indent = F
    )

    row_point <-  row_point + length(na.omit(unique(as.character(df_complete[[row_names_both[r,1]]]))))
    #print(length(na.omit(unique(as.character(df_complete[[r]])))))

  }
  return(kb)

}
