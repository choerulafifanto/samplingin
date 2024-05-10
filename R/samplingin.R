#' round_preserve_sum
#'
#' @param x a number
#' @param digits 0 (default)
round_preserve_sum = function(x, digits = 0) {
  up = 10 ^ digits
  x = x * up
  y = floor(x)
  indices = tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  return(y / up)
}

popCheck = function(sampl,pop){
  tot = 0
  for(i in 1:NROW(sampl)){
    if(sampl[i] >pop[i]){
      tot = tot +(sampl[i]-pop[i])
      sampl[i] <-pop[i]
    }
  }
  indx = order(pop,decreasing = T)
  j=1
  while(tot >0){
    if(j>NROW(pop)) j=1

    if(pop[indx[j]]-sampl[indx[j]]>0){
      sampl[indx[j]]=sampl[indx[j]]+1
      # show(tot)

      tot = tot-1
    }

    j=j+1
  }
  return(sampl)
}

#' Allocate Predetermined Allocations to Smaller Levels
#'
#' Allocate predetermined allocations to smaller levels using proportional allocation method
#'
#' @param data population tabulation dataframe
#' @param n_alloc total allocation dataframe
#' @param group group of allocation level to be obtained
#' @param pop_var population variable in data
#' @param secondary how many times the secondary sample compares to primary sample
#'
#' @return allocation at more detailed level
#' @export
#' @examples
#'
#' library(samplingin)
#' library(magrittr)
#'
#' contoh_alokasi = alokasi_dt %>%
#'    dplyr::select(-n_primary) %>%
#'    dplyr::mutate(nasional = 1)
#'
#' alokasi_dt = get_allocation(
#'    data = contoh_alokasi
#'    , n_alloc = 100
#'    , group = c("nasional")
#'    , pop_var = "jml_kabkota"
#' )
#'
get_allocation = function(data, n_alloc, group, pop_var = "jml", secondary = 0){
  # if( is.na(falok) ){
  #   stop("Please select one variable as allocation sample")
  #   return()
  # }

  data_1 = data.frame()
  if(is.numeric(n_alloc)){
    data_1 = data %>%
      mutate(
        fsqrt=sqrt(eval(parse(text = pop_var)))
      ) %>%
      group_by(across(all_of(group))) %>%
      mutate(sfsqrt = sum(fsqrt)) %>%
      ungroup() %>%
      mutate(
        alokasi_n = n_alloc
      )
  }else{
    data_1 = data %>%
      mutate(
        fsqrt=sqrt(eval(parse(text = pop_var)))
      ) %>%
      group_by(across(all_of(group))) %>%
      mutate(sfsqrt = sum(fsqrt)) %>%
      ungroup() %>%
      mutate(
        alokasi_n = eval(parse(text = n_alloc))
      )
  }

  if( is.na(pop_var) ){
    stop("Please select one variable as population variable")
    return()
  }

  ret = data_1 %>%
    mutate(
      alok0 = fsqrt*alokasi_n/sfsqrt
    ) %>%
    group_by(across(all_of(group))) %>%
    mutate(
      alok = round_preserve_sum(alok0)
      , alok_p = ceiling(secondary * alok)
    ) %>%
    mutate(
      alok_p = ifelse(alok+alok_p > eval(parse(text = pop_var)), eval(parse(text = pop_var)) - alok, alok_p)
    ) %>%
    ungroup()

  if(secondary == 0){
    ret = ret %>%
      select(names(data),alok) %>%
      rename(n_primary = alok)
  }else{
    ret = ret %>%
      select(names(data),alok,alok_p) %>%
      rename(
        n_primary = alok
        , n_secondary = alok_p
      )
  }


  # dd = ret %>%
  #   split(ret %>% select(group))
  #
  # ee = lapply(dd, function(x){
  #   x$alok = popCheck(x$alok, x$j_elig)
  #
  #   x
  # })
  #
  # ee = ee %>%
  #   bind_rows(.id = "id") %>%
  #   select(-id)
  #
  # ee

  return(ret)
}

# get_sample = function(data, n, ar, implicit = NA, method, auxVar=NA){
#   txt_info = ""
#   if( all(!is.na(implicit)) & all(implicit %in% names(data))){
#     data = data %>%
#       arrange_at(implicit)
#   }else{
#     txt_info = "implicit Variables not specified"
#     # warning("Tidak ada variabel implicit")
#   }
#
#   switch (
#     method,
#     "systematic" = {
#       obtain_sys = function(N,n){
#         # if(n>N){
#         #   stop("Number of sample is larger than number of population")
#         #   return()
#         # }
#
#         k = N/n
#         r = ar*k
#
#         ix_start = ifelse(r<1, 1, r)
#
#         ret = round(seq(ix_start, ix_start + k*(n-1), k), 0)
#       }
#
#       if(n > nrow(data)){
#         res = NULL
#         txt_info = "number of sample is larger than number of population"
#       }else{
#         res = res = data[obtain_sys(nrow(data), n), ]
#         txt_info = "success"
#       }
#
#       return(list(N = data %>% nrow, n = n, ar = ar, implicit=implicit, method = method, sampel = res, info = txt_info))
#     },
#     "pps" = {
#       if( is.na(auxVar) ){
#         stop("auxVar cannot be empty")
#         return()
#       }
#
#       return(list(N = data %>% nrow, n = nrow(res), ar = ar, implicit=implicit, method = method, sampel = NULL))
#     }
#   )
# }

createInterval = function(pop, method, strata, auxVar=NA, groupByVar=c("kdprov","kdkab")){
  if(all(c("VMIN","VMAX") %in% names(pop))){
    mRek = pop %>% select(-VMIN,-VMAX)
  }else{
    mRek = pop
  }
  switch (method,
          "srs" = {
            mRek = mRek %>%
              mutate(INDEX = row_number())

            return(mRek)
          },
          "systematic" = {
            mRek = mRek %>% group_by(across(all_of(groupByVar))) %>%
              mutate(
                VMAX = ifelse(!is.na(!!rlang::parse_quo(strata, env = caller_env())), row_number(), 0),
                VMIN = ifelse(!is.na(!!rlang::parse_quo(strata, env = caller_env())), as.numeric(VMAX), 0)) %>%
              ungroup %>%
              mutate(INDEX = row_number())

            return(mRek)
          },
          "pps" = {
            if( is.na(auxVar) ){
              stop("auxVar cannot be empty")
              return()
            }

            # cat("Auxiliary variable is: ",auxVar,"\n")

            mRek = mRek %>% group_by(across(all_of(groupByVar))) %>%
              mutate(VMAX = ifelse(!is.na(!!rlang::parse_quo(strata, env = caller_env())) ,
                                   cumsum(eval(parse(text=auxVar))), 0),
                     VMIN = ifelse(!is.na(!!rlang::parse_quo(strata, env = caller_env())),
                                   as.numeric(VMAX - eval(parse(text=auxVar)) + 1), 0) ) %>%
              ungroup %>%
              mutate(INDEX = row_number())

            return(mRek)
          }
  )
}

#' Select Samples Given its Parameters
#'
#' Samples selection using systematic or PPS (Probability Proportional to Size) sampling method.
#'
#' @param pop pop dataframe
#' @param alloc allocation dataframe
#' @param nsample variable on alloc df as allocation sample
#' @param type type value for sample classification ('U' = Primary Samples, 'P' = Secondary Samples)
#' @param strata strata variable, must available on both pop and alloc dataframe
#' @param ident group by on allocation dataframe
#' @param implicitby variable used as implicit stratification
#' @param method method of sampling : `"systematic"` (the default), `"srs"` or `"pps"`
#' @param auxVar auxiliary variable for pps sampling (`method = "pps"`)
#' @param seed seed
#' @param predetermined_rn predetermined random number variable on allocation dataframe, the default value is NULL, random number will be generated randomly
#' @param is_secondary if the value is `TRUE`, it will maintains existing primary samples and selects units that have not been selected as samples (`FALSE` as default)
#' @param verbose verbose (`TRUE` as default)
#'
#' @return list of population data (`"pop"`), selected samples (`"sampledf"`), and details of sampling process (`"details"`)
#' @export
#' @examples
#'
#' \donttest{
#' library(samplingin)
#' library(magrittr)
#' library(dplyr)
#'
#' # Simple Random Sampling (SRS)
#' dtSampling_srs = doSampling(
#'    pop         = pop_dt
#'    , alloc     = alokasi_dt
#'    , nsample   = "n_primary"
#'    , type      = "U"
#'    , ident     = c("kdprov")
#'    , method    = "srs"
#'    , auxVar    = "Total"
#'    , seed      = 7892
#' )
#'
#' # Population data with flag sample
#' pop_dt = dtSampling_srs$pop
#'
#' # Selected Samples
#' dsampel = dtSampling_srs$sampledf
#'
#' # Details of sampling process
#' rincian = dtSampling_srs$details
#'
#' # PPS Sampling
#' dtSampling_pps = doSampling(
#'    pop         = pop_dt
#'    , alloc     = alokasi_dt
#'    , nsample   = "n_primary"
#'    , type      = "U"
#'    , ident     = c("kdprov")
#'    , method    = "pps"
#'    , auxVar    = "Total"
#'    , seed      = 1234
#' )
#'
#' # Population data with flag sample
#' pop_dt = dtSampling_pps$pop
#'
#' # Selected Samples
#' dsampel = dtSampling_pps$sampledf
#'
#' # Details of sampling process
#' rincian = dtSampling_pps$details
#'
#' # Systematic Sampling
#' dtSampling_sys = doSampling(
#'    pop         = pop_dt
#'    , alloc     = alokasi_dt
#'    , nsample   = "n_primary"
#'    , type      = "U"
#'    , ident     = c("kdprov")
#'    , method    = "systematic"
#'    , seed      = 4321
#' )
#'
#' # Population data with flag sample
#' pop_dt = dtSampling_sys$pop
#'
#' # Selected Samples
#' dsampel = dtSampling_sys$sampledf
#'
#' # Details of sampling process
#' rincian = dtSampling_sys$details
#'
#' # Systematic Sampling (Secondary Samples)
#'
#' alokasi_dt_p = alokasi_dt %>%
#'    mutate(n_secondary = 2 * n_primary)
#'
#' dtSampling_sys_p = doSampling(
#'    pop           = dtSampling_sys$pop
#'    , alloc       = alokasi_dt_p
#'    , nsample     = "n_secondary"
#'    , type        = "P"
#'    , ident       = c("kdprov")
#'    , method      = "systematic"
#'    , seed        = 6789
#'    , is_secondary = TRUE
#' )
#'
#' # Population data with flag sample
#' pop_dt = dtSampling_sys_p$pop
#'
#' # Selected Samples
#' dsampel = dtSampling_sys_p$sampledf
#'
#' # Details of sampling process
#' rincian = dtSampling_sys_p$details
#'
#' # Systematic Sampling with predetermined random number (predetermined_rn parameter)
#'
#' alokasi_dt_rn = alokasi_dt %>% rowwise() %>% mutate(ar = runif(n(),0,1)) %>% ungroup
#'
#' dtSampling_sys = doSampling(
#'    pop         = pop_dt
#'    , alloc     = alokasi_dt_rn
#'    , nsample   = "n_primary"
#'    , type      = "U"
#'    , ident     = c("kdprov")
#'    , method    = "systematic"
#'    , predetermined_rn = "ar"
#'    , seed      = 4321
#' )
#'
#' # Population data with flag sample
#' pop_dt = dtSampling_sys$pop
#'
#' # Selected Samples
#' dsampel = dtSampling_sys$sampledf
#'
#' # Details of sampling process
#' rincian = dtSampling_sys$details
#' }
doSampling = function(pop, alloc, nsample, type, strata=NULL, ident=c("kdprov","kdkab"), implicitby = NULL, method="systematic",
                      auxVar=NA, seed=1, predetermined_rn = NULL, is_secondary = FALSE, verbose = TRUE){
  # Warning apabila syarat tidak terpenuhi
  if( length(method)==0 | length(method)>1 | !(method %in% c("srs","systematic", "pps"))){
    stop("Please select one method. srs, systematic or pps")
    return()
  }

  if( is.na(nsample) ){
    stop("Please select one variable as allocation sample")
    return()
  }

  # if(nsample %in% c("nsam")){
  #   alloc = alloc %>%
  #     rename(n_aloc = nsam)
  #
  #   nsample = "n_aloc"
  # }

  null_strata = 0

  if(is.null(strata)){
    null_strata = 1
    pop = pop %>%
      mutate(tmp_strata = as.integer(1))

    alloc = alloc %>%
      mutate(tmp_strata = as.integer(1))

    strata = "tmp_strata"
  }

  if( !is.null(implicitby) ){
    sortVar = c(ident, strata, implicitby)
    if(verbose){
      if(!null_strata){
        message("sort by: ",paste(ident, collapse = ", "),", ",paste(strata, collapse = ", ")," and ",paste(implicitby, collapse = ", "),"\n")
      }else{
        message("sort by: ",paste(ident, collapse = ", ")," and ",paste(implicitby, collapse = ", "),"\n")
      }
    }
  }else{
    sortVar = c(ident, strata)
    if(verbose){
      if(!null_strata){
        message("no implicit stratification variable chosen, sort by: ",paste(ident, collapse = ", ")," and ",paste(strata, collapse = ", "),"\n")
      }else{
        message("no implicit stratification variable chosen, sort by: ",paste(ident, collapse = ", "),"\n")
      }
    }
  }

  pop = pop %>%
    arrange_at(sortVar)

  # Inisialisasi daftar sampel
  dsampel    = NULL

  # Inisialisasi variabel FLAGS (Flag Sampel) dan TANGGAL pada data populasi
  if(!("flags" %in% names(pop))){
    pop = pop %>% mutate(flags=NA)
  }

  if(!("tanggal" %in% names(pop))){
    pop = pop %>% mutate(tanggal=NA)
  }

  if(!is_secondary){

    pop = pop %>% mutate(flags=NA, tanggal=NA)

    # Variabel groupByVar merupakan gabungan parameter ident dan strata
    # sesuai dengan level group pada alokasi
    groupByVar = c(ident,strata)
  }else{
    pop = pop %>%
      mutate(is_secondary_tmp = ifelse(!is.na(flags), NA, as.integer(1)))

    alloc = alloc %>%
      mutate(is_secondary_tmp = as.integer(1))

    groupByVar = c(ident,strata,"is_secondary_tmp")
  }

  if(null_strata){
    groupByVar_conds = c(ident)
  }else{
    groupByVar_conds = c(ident,strata)
  }

  # filter out null allocation

  alokasi_minus = alloc %>%
    filter(eval(parse(text = nsample)) < 0)

  alokasi_nol = alloc %>%
    filter(eval(parse(text = nsample)) == 0)

  if(verbose){
    message("Negative allocation: ",nrow(alokasi_minus),"\n")

    if(nrow(alokasi_minus)>0){
      stop("Allocation cannot be negative")
    }

    message("Zero allocation: ",nrow(alokasi_nol),"\n")

    if(nrow(alokasi_nol)>0){
      message("Removing Zero allocation\n")
    }

    if(!is.null(predetermined_rn)){
      message("Using Predetermined Random Number on Allocation (",predetermined_rn,")\n")
    }

    if(is_secondary){
      message("Sampling Secondary Units\n")
    }
  }

  alloc = alloc %>%
    filter(eval(parse(text = nsample)) > 0)

  switch (method,
          "srs" = {
            pop = createInterval(pop, method, strata, groupByVar = groupByVar)
            # Rekap populasi berdasarkan group
            mRek = pop %>%
              group_by(across(all_of(groupByVar))) %>%
              summarise(npop = n()) %>%
              ungroup()

            set.seed(seed)

            rincian = left_join(alloc, mRek, by=groupByVar) %>%
              mutate_at(c("npop"), ~replace(., is.na(.), 0)) %>%
              rowwise() %>%
              mutate(
                # ar = ifelse(is.null(predetermined_rn), runif(n(),0,1), eval(parse(text = predetermined_rn))),
                npop=ifelse(!is.na(npop),npop,0),
                # k = npop/eval(parse(text = nsample)),
                sisa=9999) %>%
              ungroup() %>%
              as.data.frame()

            # Proses penarikan sampel per rincian alokasi
            for(i in 1:nrow(rincian)){
              # cat(i,"\n")
              if(rincian[i,nsample] == 0) next

              # Apabila populasinya ternyata 0, maka variabel sisa = alokasi
              if(rincian[i,"npop"] == 0){
                rincian[i,"sisa"] = rincian[i,nsample]
                next
              }

              # Inisialisasi list variabel
              lis         = list()

              # Membuat list variabel sesuai dengan parameter ident yang diberikan
              for(ii in 1:length(ident)){
                lis[[ident[ii]]] = rincian[i, ident[ii]]
              }

              # Membuat list variabel pelengkap lainnya
              lis[[strata]]  = rincian[i, strata]
              lis[[nsample]]   = rincian[i, nsample]
              lis$npop    = rincian[i,"npop"]
              lis$ar      = rincian[i,"ar"]
              lis$k       = rincian[i,"k"]

              # if(verbose) cat("START: ")

              # Membuat kondisi penarikan sampel
              conds = ""
              for(ii in 1:length(groupByVar_conds)){
                # Menuliskan pada console, progres penarikan sampelnya
                txt = eval(parse(text=paste0("lis[['",groupByVar_conds[ii],"']]"))) %>% as.character()
                if( ii == length(groupByVar_conds) ){
                  if(verbose) cat(" ",groupByVar_conds[[ii]],txt,"\n")
                }else{
                  if(verbose) cat(" ",groupByVar_conds[[ii]],txt)
                }

                # Penyusunan kondisi penarikan sampel sesuai
                # dengan rincian dan variabel group
                if( ii==1 ){
                  conds = ""
                }else{
                  conds = paste0(conds, " & ")
                }
                conds = paste0(conds, "eval(parse(text='",groupByVar_conds[ii],"')) == lis[['",groupByVar_conds[ii],"']] %>% as.character")
              }

              # Menyeleksi populasi sesuai dengan kondisi untuk
              # setiap rincian penarikan sampel
              if(!is_secondary){
                y = pop %>%
                  filter(eval(parse(text=conds))) %>%
                  select(INDEX)
              }else{
                y = pop %>%
                  filter(eval(parse(text=conds)) & is_secondary_tmp %in% c(1)) %>%
                  select(INDEX)
              }

              # Inisialisasi variabel
              flag    = NULL
              dsts    = NULL
              st      = NULL

              # Apabila jumlah alokasi lebih atau sama dengan populasi,
              # maka sampel diambil secara take all
              if(lis[[nsample]] >= lis$npop){
                st = y %>%
                  select(INDEX) %>%
                  unlist %>% as.numeric
              }else{ # untuk alokasi < populasi
                set.seed(seed)
                st = sample(pull(y), lis[[nsample]])
              }

              # Menandai FLAGS dan TANGGAL sampel terpilih dan menyeleksinya dari populasi
              if(!is.null(st)){
                pop = pop %>%
                  mutate(
                    flags   = replace(flags, INDEX %in% st, type),
                    tanggal = replace(tanggal, INDEX %in% st, paste(format(Sys.Date(), format="%d/%m/%y"))))

                dsts = pop %>%
                  filter(INDEX %in% st)
              }

              # Menggabungkan sampel terpilih ke daftar sampel
              # Update variabel sisa pada rincian
              if (!is.null(dsts)) {
                # ix = match(c("VMIN","VMAX","INDEX"), colnames(dsts))
                # dsts[,ix] = NULL
                dsampel = rbindlist(list(dsampel, dsts), use.names = T, fill = T)
                rincian[i,"sisa"] = rincian[i,nsample] - nrow(dsts)
              }
            }

            # Membuat rekap hasil penarikan sampel
            rek = dsampel %>%
              group_by(across(all_of(groupByVar))) %>%
              summarise(n_selected=n()) %>%
              ungroup()

            # Join rincian dengan hasil penarikan sampel
            rincian = left_join(rincian, rek) %>%
              mutate_at(c("n_selected"), ~replace(., is.na(.), 0)) %>%
              as.data.frame()

            pop = pop %>%
              select(-INDEX) %>%
              as.data.frame()

            dsampel = dsampel %>%
              select(-INDEX) %>%
              as.data.frame()

            if(strata == "tmp_strata"){
              pop = pop %>%
                select(-tmp_strata)

              dsampel = dsampel %>%
                select(-tmp_strata)
            }

            sisa = rincian %>%
              summarise(sum(sisa, na.rm=T)) %>%
              pull()

            total = alloc %>%
              summarise(sum(eval(parse(text = nsample)), na.rm=T))

            persentase = round((dsampel %>% nrow) * 100.0/total, 2)

            if(sisa>0){
              message("WARNING: There are still ",sisa," allocations for which samples have not been selected. Selected ", dsampel %>% nrow," out of ", total," (",persentase,"%)\n")
            }else{
              if(verbose) message("All allocations have been selected. Selected ", dsampel %>% nrow," out of ", total," (",persentase,"%)\n")
            }

            rincian = rincian %>%
              rename(n_deficit = sisa)

            if("is_secondary_tmp" %in% names(pop)){
              pop = pop %>% select(-is_secondary_tmp)
            }

            if("is_secondary_tmp" %in% names(rincian)){
              rincian = rincian %>% select(-is_secondary_tmp)
            }

            if("is_secondary_tmp" %in% names(dsampel)){
              dsampel = dsampel %>% select(-is_secondary_tmp)
            }

            # Return value
            return(list(pop=pop, sampledf=dsampel, details=rincian))
          },
          "systematic" = {
            pop = createInterval(pop, method, strata, groupByVar = groupByVar)

            # Rekap populasi berdasarkan group
            mRek = pop %>%
              group_by(across(all_of(groupByVar))) %>%
              summarise(npop = n()) %>%
              ungroup()

            # Membuat rincian penarikan sampel seperti
            # alokasi sampel per group,
            # populasi per group,
            # angka random yang dipakai, interval K, dan sisa penarikan sampel
            # seed digunakan untuk menetapkan angka random
            set.seed(seed)

            rincian = left_join(alloc, mRek, by=groupByVar) %>%
              mutate_at(c("npop"), ~replace(., is.na(.), 0)) %>%
              rowwise() %>%
              mutate(
                ar = ifelse(is.null(predetermined_rn), runif(n(),0,1), eval(parse(text = predetermined_rn))),
                npop=ifelse(!is.na(npop),npop,0),
                k = npop/eval(parse(text = nsample)),
                sisa=9999) %>%
              ungroup() %>%
              as.data.frame()

            # Proses penarikan sampel per rincian alokasi
            for(i in 1:nrow(rincian)){
              # cat(i,"\n")
              if(rincian[i,nsample] == 0) next

              # Apabila populasinya ternyata 0, maka variabel sisa = alokasi
              if(rincian[i,"npop"] == 0){
                rincian[i,"sisa"] = rincian[i,nsample]
                next
              }

              # Inisialisasi list variabel
              lis         = list()

              # Membuat list variabel sesuai dengan parameter ident yang diberikan
              for(ii in 1:length(ident)){
                lis[[ident[ii]]] = rincian[i, ident[ii]]
              }

              # Membuat list variabel pelengkap lainnya
              lis[[strata]]  = rincian[i, strata]
              lis[[nsample]]   = rincian[i, nsample]
              lis$npop    = rincian[i,"npop"]
              lis$ar      = rincian[i,"ar"]
              lis$k       = rincian[i,"k"]

              # if(verbose) cat("START: ")

              # Membuat kondisi penarikan sampel
              conds = ""
              for(ii in 1:length(groupByVar_conds)){
                # Menuliskan pada console, progres penarikan sampelnya
                txt = eval(parse(text=paste0("lis[['",groupByVar_conds[ii],"']]"))) %>% as.character()
                if( ii == length(groupByVar_conds) ){
                  if(verbose) cat(" ",groupByVar_conds[[ii]],txt,"\n")
                }else{
                  if(verbose) cat(" ",groupByVar_conds[[ii]],txt)
                }

                # Penyusunan kondisi penarikan sampel sesuai
                # dengan rincian dan variabel group
                if( ii==1 ){
                  conds = ""
                }else{
                  conds = paste0(conds, " & ")
                }
                conds = paste0(conds, "eval(parse(text='",groupByVar_conds[ii],"')) == lis[['",groupByVar_conds[ii],"']] %>% as.character")
              }

              # Menyeleksi populasi sesuai dengan kondisi untuk
              # setiap rincian penarikan sampel
              if(!is_secondary){
                y = pop %>%
                  filter(eval(parse(text=conds))) %>%
                  select(VMIN,VMAX,INDEX)
              }else{
                y = pop %>%
                  filter(eval(parse(text=conds)) & is_secondary_tmp %in% c(1)) %>%
                  select(VMIN,VMAX,INDEX)
              }

              # Inisialisasi variabel
              flag    = NULL
              dsts    = NULL
              st      = NULL

              # Apabila jumlah alokasi lebih atau sama dengan populasi,
              # maka sampel diambil secara take all
              if(lis[[nsample]] >= lis$npop){
                st = y %>%
                  select(INDEX) %>%
                  unlist %>% as.numeric
              }else{ # untuk alokasi < populasi
                ix = lis[["k"]] * lis[["ar"]]
                if(ix < 1){
                  sys = 1 + (0:(lis[[nsample]]-1)) * lis[["k"]]
                }else{
                  sys = lis[["k"]] * (lis[["ar"]] + 0:(lis[[nsample]]-1))
                }

                sys = sapply(sys, function(x){ifelse(x<1, ceiling(x), round(x))})
                st = y %>%
                  slice(findInterval(sys, y$VMAX)) %>%
                  select(INDEX) %>%
                  unlist %>% as.numeric
              }

              # Menandai FLAGS dan TANGGAL sampel terpilih dan menyeleksinya dari populasi
              if(!is.null(st)){
                pop = pop %>%
                  mutate(
                    flags   = replace(flags, INDEX %in% st, type),
                    tanggal = replace(tanggal, INDEX %in% st, paste(format(Sys.Date(), format="%d/%m/%y"))))

                dsts = pop %>%
                  filter(INDEX %in% st)
              }

              # Menggabungkan sampel terpilih ke daftar sampel
              # Update variabel sisa pada rincian
              if (!is.null(dsts)) {
                # ix = match(c("VMIN","VMAX","INDEX"), colnames(dsts))
                # dsts[,ix] = NULL
                dsampel = rbindlist(list(dsampel, dsts), use.names = T, fill = T)
                rincian[i,"sisa"] = rincian[i,nsample] - nrow(dsts)
              }
            }

            # Membuat rekap hasil penarikan sampel
            rek = dsampel %>%
              group_by(across(all_of(groupByVar))) %>%
              summarise(n_selected=n()) %>%
              ungroup()

            # Join rincian dengan hasil penarikan sampel
            rincian = left_join(rincian, rek) %>%
              mutate_at(c("n_selected"), ~replace(., is.na(.), 0)) %>%
              as.data.frame()

            pop = pop %>%
              select(-VMIN,-VMAX,-INDEX) %>%
              as.data.frame()

            dsampel = dsampel %>%
              select(-VMIN,-VMAX,-INDEX) %>%
              as.data.frame()

            if(strata == "tmp_strata"){
              pop = pop %>%
                select(-tmp_strata)

              dsampel = dsampel %>%
                select(-tmp_strata)
            }

            sisa = rincian %>%
              summarise(sum(sisa, na.rm=T)) %>%
              pull()

            total = alloc %>%
              summarise(sum(eval(parse(text = nsample)), na.rm=T))

            persentase = round((dsampel %>% nrow) * 100.0/total, 2)

            if(sisa>0){
              message("WARNING: There are still ",sisa," allocations for which samples have not been selected. Selected ", dsampel %>% nrow," out of ", total," (",persentase,"%)\n")
            }else{
              if(verbose) message("All allocations have been selected. Selected ", dsampel %>% nrow," out of ", total," (",persentase,"%)\n")
            }

            rincian = rincian %>%
              rename(n_deficit = sisa)

            if("is_secondary_tmp" %in% names(pop)){
              pop = pop %>% select(-is_secondary_tmp)
            }

            if("is_secondary_tmp" %in% names(rincian)){
              rincian = rincian %>% select(-is_secondary_tmp)
            }

            if("is_secondary_tmp" %in% names(dsampel)){
              dsampel = dsampel %>% select(-is_secondary_tmp)
            }

            # Return value
            return(list(pop=pop, sampledf=dsampel, details=rincian))
          },
          "pps" = {
            # Berhenti apabila parameter auxVar kosong
            if( is.na(auxVar) ){
              stop("auxVar cannot be empty")
              return()
            }

            # if(!("certainty" %in% names(pop))){
            #   pop = pop %>% mutate(certainty=NA)
            # }

            # using inclusionprobabilities

            # Membuat interval berdasarkan auxVar
            pop = createInterval(pop, method, strata, auxVar = auxVar, groupByVar = groupByVar)

            mRek = pop %>%
              group_by(across(all_of(groupByVar))) %>%
              summarise(npop = n()) %>%
              ungroup()

            set.seed(seed)
            rincian = left_join(alloc, mRek, by=groupByVar) %>%
              # mutate_at(c("npop","numobs","ncertainty"), ~replace(., is.na(.), 0)) %>%
              mutate_at(c("npop"), ~replace(., is.na(.), 0)) %>%
              rowwise() %>%
              mutate(
                ar = ifelse(is.null(predetermined_rn), runif(n(),0,1), eval(parse(text = predetermined_rn))),
                npop=ifelse(!is.na(npop),npop,0),
                k = npop/eval(parse(text = nsample)),
                sisa=9999) %>%
              ungroup() %>%
              as.data.frame()

            pop = pop %>%
              left_join(
                rincian %>%
                  select(all_of(c(groupByVar,nsample)))
              ) %>%
              mutate_at(nsample, ~replace(., is.na(.), 0)) %>%
              group_by(across(all_of(groupByVar))) %>%
              mutate(
                pi = inclusionprobabilities(eval(parse(text=auxVar)), unique(eval(parse(text = nsample))))
                , cumsumpi = cumsum(pi)
                , cumsumpi = cumsumpi - pi
              ) %>%
              ungroup()

            # Proses penarikan sampel per rincian alokasi
            for(i in 1:nrow(rincian)){
              if(rincian[i,nsample] == 0) next

              # Apabila populasinya ternyata 0, maka variabel sisa = alokasi
              if(rincian[i,"npop"] == 0){
                rincian[i,"sisa"] = rincian[i, nsample]
                next
              }

              # Inisialisasi list variabel
              lis         = list()

              # Membuat list variabel sesuai dengan parameter ident yang diberikan
              for(ii in 1:length(ident)){
                lis[[ident[ii]]] = rincian[i, ident[ii]]
              }

              # Membuat list variabel pelengkap lainnya
              lis[[strata]]  = rincian[i,strata]
              lis[[nsample]] = rincian[i,nsample]
              lis$nsam = rincian[i,"nsam"]
              lis$npop    = rincian[i,"npop"]
              lis$numobs  = rincian[i,"numobs"]
              lis$ar      = rincian[i,"ar"]
              lis$k       = rincian[i,"k"]

              # if(verbose) cat("START: ")

              # Membuat kondisi penarikan sampel
              conds = ""
              for(ii in 1:length(groupByVar_conds)){
                # Menuliskan pada console, progres penarikan sampelnya
                txt = eval(parse(text=paste0("lis[['",groupByVar_conds[ii],"']]"))) %>% as.character()
                if( ii == length(groupByVar_conds) ){
                  if(verbose) cat(" ",groupByVar_conds[[ii]],txt,"\n")
                }else{
                  if(verbose) cat(" ",groupByVar_conds[[ii]],txt)
                }

                # Penyusunan kondisi penarikan sampel sesuai
                # dengan rincian dan variabel group
                if( ii==1 ){
                  conds = ""
                }else{
                  conds = paste0(conds, " & ")
                }
                conds = paste0(conds, "eval(parse(text='",groupByVar_conds[ii],"')) == lis[['",groupByVar_conds[ii],"']] %>% as.character")
              }

              # Menyeleksi populasi sesuai dengan kondisi untuk
              # setiap rincian penarikan sampel
              if(!is_secondary){
                y = pop %>%
                  filter(eval(parse(text=conds))) %>%
                  select(VMIN,VMAX,INDEX,cumsumpi)
              }else{
                y = pop %>%
                  filter(eval(parse(text=conds)) & is_secondary_tmp %in% c(1)) %>%
                  select(VMIN,VMAX,INDEX,cumsumpi)
              }

              # Inisialisasi variabel
              flag    = NULL
              dsts    = NULL
              st      = NULL

              sys = 0:(lis[[nsample]] - 1) +  lis$ar

              # Apabila jumlah alokasi lebih atau sama dengan populasi,
              # maka sampel diambil secara take all
              if(lis[[nsample]] >= lis$npop){
                st = y %>%
                  select(INDEX) %>%
                  unlist %>% as.numeric
              }else{ # untuk alokasi < populasi
                st = y %>%
                  slice(findInterval(sys, y$cumsumpi)) %>%
                  select(INDEX) %>%
                  unlist %>% as.numeric
              }

              # Menandai FLAGS dan TANGGAL sampel terpilih dan menyeleksinya dari populasi
              if(!is.null(st)){
                pop = pop %>%
                  mutate(flags   = replace(flags, INDEX %in% st, type),
                         tanggal = replace(tanggal, INDEX %in% st, paste(format(Sys.Date(), format="%d/%m/%y"))))

                dsts = pop %>%
                  filter(INDEX %in% st)
              }

              # Menggabungkan sampel terpilih ke daftar sampel
              # Update variabel sisa pada rincian
              if (!is.null(dsts)) {
                # ix = match(c("VMIN","VMAX","INDEX"), colnames(dsts))
                # dsts[,ix] = NULL
                dsampel = rbindlist(list(dsampel, dsts), use.names = T, fill = T)
                rincian[i,"sisa"] = rincian[i,nsample] - nrow(dsts)
              }
            }

            # Membuat rekap hasil penarikan sampel
            rek = dsampel %>%
              group_by(across(all_of(groupByVar))) %>%
              summarise(n_selected=n()) %>%
              ungroup()

            # Join rincian dengan hasil penarikan sampel dan update variabel sisa
            rincian = left_join(rincian, rek) %>%
              mutate_at(c("n_selected"), ~replace(., is.na(.), 0)) %>%
              mutate(sisa = eval(parse(text = nsample)) - n_selected) %>%
              as.data.frame()

            pop = pop %>%
              select(-VMIN,-VMAX,-INDEX, -one_of(nsample), -pi, -cumsumpi) %>%
              as.data.frame()

            dsampel = dsampel %>%
              select(-VMIN,-VMAX,-INDEX, -one_of(nsample), -pi, -cumsumpi) %>%
              as.data.frame()

            if(strata == "tmp_strata"){
              pop = pop %>%
                select(-tmp_strata)

              dsampel = dsampel %>%
                select(-tmp_strata)
            }

            sisa = rincian %>%
              summarise(sum(sisa, na.rm=T)) %>%
              pull()

            total = alloc %>%
              summarise(sum(eval(parse(text = nsample)), na.rm=T))

            persentase = round((dsampel %>% nrow) * 100.0/total, 2)

            if(sisa>0){
              message("WARNING: There are still ",sisa," allocations for which samples have not been selected. Selected ", dsampel %>% nrow," out of ", total," (",persentase,"%)\n")
            }else{
              if(verbose) message("All allocations have been selected. Selected ", dsampel %>% nrow," out of ", total," (",persentase,"%)\n")
            }

            rincian = rincian %>%
              rename(n_deficit = sisa)

            if("is_secondary_tmp" %in% names(pop)){
              pop = pop %>% select(-is_secondary_tmp)
            }

            if("is_secondary_tmp" %in% names(rincian)){
              rincian = rincian %>% select(-is_secondary_tmp)
            }

            if("is_secondary_tmp" %in% names(dsampel)){
              dsampel = dsampel %>% select(-is_secondary_tmp)
            }

            # Return value
            return(list(pop=pop, sampledf=dsampel, details=rincian))
          }
  )

}
