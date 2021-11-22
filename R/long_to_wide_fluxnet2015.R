long_to_wide_fluxnet2015 <- function( sitename, long ){

  sub <- long %>% dplyr::filter( SITE_ID==sitename )
    
  ## remove variable groups that have lots of duplicates w.r.t. variable
  sub <- sub %>% 
    dplyr::filter( VARIABLE_GROUP!="GRP_TEAM_MEMBER", VARIABLE!="NETWORK", VARIABLE_GROUP!="GRP_DM_FERT_M", VARIABLE_GROUP!="GRP_DM_AGRICULTURE", VARIABLE_GROUP!="GRP_DM_PESTICIDE", VARIABLE_GROUP!="GRP_DM_PLANTING", VARIABLE_GROUP!="GRP_DM_TILL"  )
  
  ## determine duplicates
  df_nduplicates <- sub %>% 
    group_by( SITE_ID, VARIABLE ) %>% 
    summarize( n = n()) %>% 
    filter( n>1 )
  
  ##  treat duplicates
  ## Use only first entry for 'reference paper'
  if (nrow(df_nduplicates)>0){
    if ("REFERENCE_PAPER" %in% df_nduplicates$VARIABLE %>% as.character()){
      varname <- "REFERENCE_PAPER"
      groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
      sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
    }

    ## Use only first entry for 'reference usage'
    df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
    if (nrow(df_nduplicates)>0){
      if ("REFERENCE_USAGE" %in% df_nduplicates$VARIABLE %>% as.character()){
        varname <- "REFERENCE_USAGE"
        groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
        sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
      }
    
      ## Use only first entry for 'reference usage'
      df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
      if (nrow(df_nduplicates)>0){
        if ("REFERENCE_DOI" %in% df_nduplicates$VARIABLE %>% as.character()){
          varname <- "REFERENCE_DOI"
          groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
          sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
        }

        ## Use only first entry for 'reference usage'
        df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
        if (nrow(df_nduplicates)>0){
          if ("REFERENCE_COMMENT" %in% df_nduplicates$VARIABLE %>% as.character()){
            varname <- "REFERENCE_COMMENT"
            groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
            sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
          }          

          ## Use only first entry for 'DOM_DIST_MGMT'
          df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
          if (nrow(df_nduplicates)>0){
            if ("DOM_DIST_MGMT" %in% df_nduplicates$VARIABLE %>% as.character()){
              varname <- "DOM_DIST_MGMT"
              groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
              sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
            }

            ## Use only first entry for 'DM_COMMENT'
            df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
            if (nrow(df_nduplicates)>0){
              if ("DM_COMMENT" %in% df_nduplicates$VARIABLE %>% as.character()){
                varname <- "DM_COMMENT"
                groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
              }

              ## Use only first entry for 'DM_DATE'
              df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
              if (nrow(df_nduplicates)>0){
                if ("DM_DATE" %in% df_nduplicates$VARIABLE %>% as.character()){
                  varname <- "DM_DATE"
                  groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                  sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                }

                ## Use only first entry for 'DM_DATE_UNC'
                df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                if (nrow(df_nduplicates)>0){
                  if ("DM_DATE_UNC" %in% df_nduplicates$VARIABLE %>% as.character()){
                    varname <- "DM_DATE_UNC"
                    groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                    sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                  }

                  ## Use only first entry for 'DM_SURF'
                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                  if (nrow(df_nduplicates)>0){
                    if ("DM_SURF" %in% df_nduplicates$VARIABLE %>% as.character()){
                      varname <- "DM_SURF"
                      groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                      sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                    }

                    ## Use only first entry for 'DM_SURF_MEAS_UNC'
                    df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                    if (nrow(df_nduplicates)>0){
                      if ("DM_SURF_MEAS_UNC" %in% df_nduplicates$VARIABLE %>% as.character()){
                        varname <- "DM_SURF_MEAS_UNC"
                        groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                        sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                      }

                      ## Use only first entry for 'DM_DATE_START'
                      df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                      if (nrow(df_nduplicates)>0){
                        if ("DM_DATE_START" %in% df_nduplicates$VARIABLE %>% as.character()){
                          varname <- "DM_DATE_START"
                          groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                          sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                        } 
                        

                        ## Use only first entry for 'DM_DATE_END'
                        df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                        if (nrow(df_nduplicates)>0){
                          if ("DM_DATE_END" %in% df_nduplicates$VARIABLE %>% as.character()){
                            varname <- "DM_DATE_END"
                            groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                            sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                          }

                          ## Use only first entry for 'DM_DATE_END'
                          df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                          if (nrow(df_nduplicates)>0){
                            if ("DM_GRAZE" %in% df_nduplicates$VARIABLE %>% as.character()){
                              varname <- "DM_GRAZE"
                              groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                              sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                            }
                            
                            ## some towers had a new location (very slight) after re-installation
                            df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                            
                            if (nrow(df_nduplicates)>0){

                              if ("LOCATION_COMMENT" %in% df_nduplicates$VARIABLE){
                                
                                groupid <- sub %>% filter( VARIABLE=="LOCATION_COMMENT") %>% filter( DATAVALUE!="Reinstallation after management activities" ) %>% dplyr::select( GROUP_ID )
                                
                                if (length(unlist(groupid))>1){
                                  print(sub %>% filter( VARIABLE=="LOCATION_COMMENT"))
                                  ans <- readline(prompt = "Enter row number of which group should be used ")
                                  groupid <- groupid[as.numeric(ans),]
                                }
                                
                                if (length(unlist(groupid))==1){
                                  
                                  ## determine variables available for this group id
                                  duplicatedvars <- sub %>% filter( GROUP_ID==groupid$GROUP_ID ) %>% dplyr::select( VARIABLE ) %>% unlist() %>% as.character()
                                  
                                  ## remove rows if VARIABLE is element of duplicatedvars and if its GROUP_ID is not equal to groupid
                                  sub <- sub %>% filter( !(VARIABLE %in% duplicatedvars & GROUP_ID!=groupid$GROUP_ID )  )
                                  
                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                } else {
                                  
                                  # ## a bit more bold: consider only distinct entries after removing GROUP_ID
                                  # sub <- sub %>% 
                                  #   select(-GROUP_ID) %>% 
                                  #   filter(VARIABLE != "ACKNOWLEDGEMENT") %>% 
                                  #   distinct()    

                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                  if (nrow(df_nduplicates)>0) rlang::abort("fuck")
                                  
                                }
                                
                              } 

                              df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                              
                              if (nrow(df_nduplicates)>0){
                                
                                if ("FLUX_MEASUREMENTS_VARIABLE" %in% df_nduplicates$VARIABLE %>% as.character()){
                                  
                                  ## Often, info is given for measurements of multiple GHGs. Take only info relevant for CO2
                                  groupid <- sub %>% filter( VARIABLE=="FLUX_MEASUREMENTS_VARIABLE") %>% filter( DATAVALUE=="CO2" ) %>% dplyr::select( GROUP_ID )
                                  
                                  if ( length(unlist(groupid))>1 ){
                                    
                                    ## sometimes there are chamber measurements and eddy covariance, use only the latter
                                    tmp <- sub %>% filter( GROUP_ID %in% groupid$GROUP_ID & VARIABLE == "FLUX_MEASUREMENTS_METHOD" )
                                    tmp$DATAVALUE <- as.character(unlist(tmp$DATAVALUE))
                                    
                                    ## get group id for chambers method
                                    groupid_eddy <- tmp %>% filter( DATAVALUE=="Eddy Covariance") %>% dplyr::select( GROUP_ID )
                                    groupid <- groupid %>% filter( GROUP_ID==groupid_eddy$GROUP_ID )
                                    
                                    if ( length(unlist(groupid))>1 ){
                                      ## apparently, both are Eddy Covariance. Use only first.
                                      groupid <- groupid[1,]
                                    }
                                                                      
                                  }
                                  
                                }
                                
                                if (length(unlist(groupid))==1){
                                  
                                  ## determine variables available for this group id
                                  duplicatedvars <- sub %>% filter( GROUP_ID==groupid$GROUP_ID ) %>% dplyr::select( VARIABLE ) %>% unlist() %>% as.character()
                                  
                                  ## remove rows if VARIABLE is element of duplicatedvars and if its GROUP_ID is not equal to groupid
                                  sub <- sub %>% filter( !(VARIABLE %in% duplicatedvars & GROUP_ID!=groupid$GROUP_ID )  )
                                  
                                  # ## a bit more bold: consider only distinct entries after removing GROUP_ID
                                  # sub <- sub %>% 
                                  #   select(-GROUP_ID) %>% 
                                  #   filter(VARIABLE != "ACKNOWLEDGEMENT") %>% 
                                  #   distinct()
                                  
                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                  if (nrow(df_nduplicates)>0){
                                  
                                    ## Take only GROUP_ID corresponding to CO2 measurements
                                    use_group_id <- sub %>% 
                                      filter(VARIABLE == "FLUX_MEASUREMENTS_VARIABLE" & DATAVALUE == "CO2") %>% 
                                      pull(GROUP_ID)
                                  
                                    ## treat remaining duplicated variables
                                    ## all rows that have a duplicate w.r.t. VARIABLE
                                    tmp <- sub %>% dplyr::select( -VARIABLE_GROUP )
                                    isduplicated <- tmp %>% dplyr::select( VARIABLE ) %>% unlist() %>% as.character() %>% duplicated()
                                    
                                    sub <- sub %>%
                                      mutate(isduplicated = isduplicated) %>% 
                                      filter(!(isduplicated & !GROUP_ID == use_group_id))
                                    
                                    tmp <- sub %>% dplyr::select( -VARIABLE_GROUP )
                                    isduplicated <- tmp %>% dplyr::select( VARIABLE ) %>% unlist() %>% as.character() %>% duplicated()
                                    rownr_duplicated <- isduplicated %>% which()
                                    varname_duplicated <- tmp[ rownr_duplicated,]$VARIABLE %>% as.character()
                                    rownr_duplicated_all <- which( tmp$VARIABLE%in%varname_duplicated )
                                    
                                    ## determine remaining duplicates
                                    df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                    
                                    if (nrow(df_nduplicates)>0){
                                      
                                      print("duplicated rows:")
                                      print( tmp[rownr_duplicated_all,] )
                                      
                                      ans <- readline( prompt = "continue taking maximum w.r.t. DATAVALUE? (y/n) ")
                                      
                                      if (ans=="y"){
                                        
                                        tmp$DATAVALUE[rownr_duplicated_all] <- as.numeric( as.character( unlist( tmp[ rownr_duplicated_all, "DATAVALUE" ] ) ) )
                                        droprownr_duplicated <- rownr_duplicated_all[ which.min( tmp$DATAVALUE[rownr_duplicated_all] ) ]
                                        sub <- sub[-droprownr_duplicated,]
                                        
                                      } else {
                                        
                                        ans <- readline( prompt = "continue using only first occurrence of duplicates (y/n) ")
                                        if (ans=="y"){
                                          sub <- sub[-rownr_duplicated,]
                                        }
                                        
                                      }
                                      
                                    }
                                    
                                  }
                                  
                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                  # ## a bit more bold: consider only distinct entries after removing GROUP_ID
                                  # sub <- sub %>% 
                                  #   select(-GROUP_ID) %>% 
                                  #   filter(VARIABLE != "ACKNOWLEDGEMENT") %>% 
                                  #   distinct()    
                                  
                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                  if (nrow(df_nduplicates)>0) rlang::abort("shitfuck.")
                                  
                                }
                              }
                            }
                          }                          
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
    
  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
  if (nrow(df_nduplicates)==0 && nrow(sub)>0){
    wide <- sub %>% dplyr::select( -VARIABLE_GROUP, -GROUP_ID ) %>% tidyr::spread( VARIABLE, DATAVALUE )
  } else {
    print(df_nduplicates)
    rlang::warn(paste("remaining duplicates for site", sitename))
    wide <- NULL
  }

  return(wide)

}