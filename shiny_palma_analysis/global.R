######################################
# functions for shiny app
######################################

####### create reactives #########

############### functions ############

# function to create line charts
plotly_plots <- function(df, plot_type) {
  
  # save most recent year as object to be used in plots
  recent_year <- max(df$year)
  
  # create list of margins
  m <- list(
    l = 50,
    r = 50,
    b = 50,
    t = 50,
    pad = 4
  )
  
  # create function for tool tip, since it will be used in all plots
  # easiest to create an empty function instead of object since objects are used in the funtion
  tool_tip <- function() {
    ~paste0("Geography: ", geo_description,
            "<br>Year:  ", year,
            "<br>Estimate:  ", round( estimate, 2 ),
            "<br>MOE:  ", round( moe, 2 ),
            "<br>SE:  ", round( se, 2),
            "<br>CV:  ", round( cv, 2))
  }
  
  # create line graph
  if (plot_type == 'line') {
    
    df %>%
      # we only want forsyth county for PUMAs
      # filter(type == input_type,
      #        subtype == if (input_type == 'puma') 'Forsyth' else .$subtype) %>%
      plot_ly(x = ~year, y = ~estimate, 
              color = ~geo_description, 
              mode = 'lines', type = 'scatter',
              # tooltip info
              hoverinfo = 'text',
              text = tool_tip()) %>%
      add_ribbons(ymin = ~estimate - moe,
                  ymax = ~estimate + moe,
                  alpha = 0.15,
                  line = list(width = 0, dash = 'dot'),
                  showlegend = FALSE) %>%
      layout(title = 'Yearly changes in Palma ratios',
             margin = m)
    
  } else if (plot_type == 'bar') {
    
      df %>%
        # only keep most current year
        filter(year == recent_year) %>%
        plot_ly(x = ~geo_description, y = ~estimate,
                type = 'bar',
                error_y = ~list(type = 'data',
                                array = moe,
                                color = '#000000'),
                # tooltip info
                hoverinfo = 'text',
                text = tool_tip()) %>%
        layout(title = paste0('Differences By Comparison Communities in ', recent_year),
               xaxis = list(title = 'Geographic Unit'),
               margin = m)
      }
}


z_score_table <- function(list_data, df, demo) {
  # function that creates z scores
  
  
  # create dataset to be used for z scores
  zscore_df <- list_data[[df]] %>%
    # only keep selected demographic
    filter(type == demo)
  
  ff_acs_zscore(zscore_df, 'estimate', 'se', 
                c('geo_description', 'year', 'subtype'))
  
}


ff_acs_zscore <- function(data_frame, estimate, se, var_names = NULL) {
  
  # This function returns a square symetrical matrix of z scores for all combinations of values
  # The matrix length and with equal the number of rows in the data frame
  #
  # The formula comes from:
  #    U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-18
  #
  # Parameters:
  #   data_frame: the dataframe where the estimates and se are housed
  #   estimate: a string that is the column name of the column containing the estimate
  #   se: a string that is the column name of the column containing the se
  #   var_names: a character vector of variables that can be combined to created
  #              distinct names for each row and column
  
  
  # initialize an empty data frame with one column and the same number
  # of rows as the final dataframe
  z_score_mat <- data.frame(n = seq(1, nrow(data_frame)))
  
  # iterate through each row in the dataframe
  for (i in 1:nrow(data_frame)) {
    
    # calculate the point estimate differences and the sum of
    # of standard errors for the given row and all other rows
    # this will return a vector
    estimate_diff <- data_frame[[i, estimate]] - data_frame[[estimate]]
    se_diff <- sqrt( data_frame[[i, se]]^2 + data_frame[[se]]^2 )
    
    # calculate the z score for all row values, rounds to two decimals
    z_score <- abs( estimate_diff / se_diff) %>% round(2)
    
    # add the row of z scores to the z score matrix
    z_score_mat[, i] <- z_score
    
  }
  
  if (!is.null(var_names)) {
    
    # if there is only one variable name, then use this as the label
    # otherwise paste together variable names
    if (length(var_names) == 1) {
      
      # sometime isolating a column returns a data frame, and sometimes it returns a vector
      # if a dataframe is returned, isolate first, and only, column as a vector
      if (is.data.frame(unique(data_frame[ , var_names])) == TRUE) {
        
        names_vec <- unique(data_frame[ , var_names])[[1]]
        
      } else {
        
        names_vec <- unique(data_frame[ , var_names])
        
      }
      
    } else {
      
      # create vector of label names by pasting columns together
      names_vec <- apply( data_frame[ , var_names], 1, paste, collapse = ": " )
      
    }
    
    # shorted names so they appear cleaner and shorter in the matrix as column and row headers
    
    # replace any United States and North Carolina values with NC and US
    names_vec <- str_replace_all(names_vec, 'United States', 'US') %>%
      str_replace_all('North Carolina', 'NC') %>%
      str_replace_all(' County, NC', '') %>%
      # replace and ethnicities with abbreviation
      str_replace_all('African American', 'AA') %>%
      str_replace_all('Hispanic/Latino', 'HL') %>%
      str_replace_all('White, non-Hispanic', 'Wh') %>%
      # shorten age descriptions (take off the word 'year')
      str_replace_all(' years', '') %>%
      str_replace_all(' and over', '+') %>%
      # shorten age by converting 'to' to '-'
      str_replace_all(' to ', '-') %>%
      # remove word 'ratio;
      str_replace_all(' ratio', '')
    
    # add labels as column and row names
    colnames(z_score_mat) <- names_vec
    row.names(z_score_mat) <- names_vec
    
  }
  
  return(z_score_mat)
  
}


ff_acs_zscore_kable <- function(data_frame, estimate, se, var_names = NULL, table_name = 'Z-Scores') {
  
  # This function takes as input a matrix of z score generated by ff_acs_zscore
  # it returns a kable table of z scores with scores over 1.96 in bold
  
  # input:
  #   zscore_matrix: matrix of z-scores generated from ff_acs_zscore
  #   table_name: table caption name for kable table
  
  data_frame %>%
    ff_acs_zscore(estimate, se, var_names) %>%
    # bold any z score over 1.96
    mutate_all(funs(cell_spec(., bold = ifelse(. > 1.96, T,F)))) %>%
    # add column names as the first row because row names do not print
    mutate(Compare = colnames(.),
           # bold column of column / row names
           Compare = cell_spec(Compare, bold = T)) %>%
    # only keep rows of Forsyth County or Winston
    filter(str_detect(Compare, 'Forsyth') | str_detect(Compare, 'Winston')) %>%
    # make the comparison column (column and row names) the first column
    select(Compare, everything()) %>%
    # create kable table
    kable(caption = table_name, escape = F)  %>%
    # add formating (every other row in gray)
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left", font_size = 10) %>%
    # bold row names
    column_spec(1, bold = T)
}

ff_data_dt <- function(df, col_names, for_tableau=FALSE) {
  
  # Input:
  #   df: a dataframe of raw data that you want convert to a DT datatable
  #   col_names: column names for the datatable
  #   for_tableau: whether this table is for tableau output
  #
  # To color cv values, the cv column must be names 'cv'
  #
  # Note: Do not sue this to create a table of z-scores; use ff_acs_zscore_dt
  
  if (for_tableau == FALSE) {
    
    datatable(data = df,
              extensions = 'Buttons', rownames = FALSE, colnames = col_names,
              options = list(dom = "Blfrtip",
                             buttons = list("copy", 
                                            list(extend = "collection",
                                                  buttons = c("csv", "excel"),
                                                  text = "Download"
                                  ) ), # end of buttons customization
                                
                                # customize the length menu
                                lengthMenu = list( c(10, 20, -1), # declare values
                                                    c(10, 20, "All") # declare titles
                                ), # end of lengthMenu customization
                                pageLength = 10
                                
                              ) # end of options
                              
    ) %>%
      # color cv numbers based on value, only if column named 'cv' exists
      formatStyle('cv', color = styleInterval(c(12, 30), c('black', 'blue', 'red')))
    
  } else {
    
    # if the table is for tableau, we need to add additional rows that represent Forsyth County totals,
    # but change type from Comparison Community to Total
    
    df %>%
      # filter for rows with Forsyth County as the county, and where type starts with Comparison
      filter(str_detect(geo_description, '^Forsyth'),
             str_detect(type, '^Comparison')) %>%
      # change type columns from Comparison to Total
      mutate(type = 'Total') %>%
      # bind these rows to the original dataframe
      bind_rows(df)  %>%
      # create datatable
      datatable(filter='top', extensions='Buttons', rownames = FALSE,
                colnames = col_names,
                options = list(scrollX = TRUE, scrollY = TRUE, dom = 'Bfrtip'))
    
  }
}
    