#' Identifies the filters required to read a dataset
#' If filters aren't available it will try to identify them
#' and print the names to the user.
h5checkFilters <- function(h5id) {
    
    truetype = H5Iget_type(h5id)
    
    if(truetype == "H5I_DATASET") {
        
        pid <- H5Dget_create_plist(h5id)
        on.exit(H5Pclose(pid))
        
    } else if(truetype == "H5I_GENPROP_LST") {
        ## we require this to be a DATASET creation plist
        pid <- h5checktypeAndPLC(h5id, "H5P_DATASET_CREATE")
    }
    
    nfilters = H5Pget_nfilters(pid)
    if( (nfilters > 0) && (H5Pall_filters_avail(pid) == 0) ) {
        
        err <-"Unable to read dataset.\nNot all required filters available.\n"
        missing <- NULL
        for(i in seq_len( nfilters )) {
            filter <- H5Pget_filter(pid, i-1)
            avail <- H5Zfilter_avail(filter_id = filter[[1]])
            if(!avail) {
                missing <- c(missing, filter[[2]])
            }
        }
        err <- paste0(err, "Missing filters: ", paste(missing, collapse = " "))
        return(err)   
    } else {
        return("")
    }
}