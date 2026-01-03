qc_vpd <- function(data, parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Check arguments
    # data frame
    if (!is.data.frame(data)) {
      stop("data object is not a data frame")
    }

    # check variables
    if (any(is.null(data[['rh']]), is.null(data[['ta']]))) {
      stop("data not contains rh and/or ta variables")
    }

    # check if vpd already exists
    if (!is.null(data[['vpd']])) {
      warning("data already has a vpd variable. Please revise if there",
              " is really necessary to recalculate vpd")
      return(data)
    }

    # STEP 1
    # Get the values of ta and rh
    ta <- data[['ta']]
    rh <- data[['rh']]

    # STEP 2
    # Calculate the VPD
    vpd <- 0.61078 * (1 - rh / 100) * exp(17.08085 * ta / (234.175 + ta))

    # 2.1 check for sure that vpd is not negative, and if it is transform to 0
    vpd_checked <- dplyr::case_when(
      vpd < 0 ~ 0,
      TRUE ~ vpd
    )

    # STEP 3
    # Build the data res object
    data[['vpd']] <- vpd_checked

    # STEP 4
    # Return the data
    return(data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_vpd',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_vpd',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_vpd',
                                                        sep = '.'))})
}