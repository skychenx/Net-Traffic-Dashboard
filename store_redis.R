require(redux)
require(data.table)
require(countrycode)

matrix_types <<- c("raw", "prct")
map_types <<- c("Packets", "Bytes", "Unique Scanners")
time_windows <<- c("10 Minutes", "Hourly", "Daily")
list_matrices <<- array(list(), dim=c(2,3,3), dimnames = list(matrix_types, map_types, time_windows));
list_port_matrices <<- array(list(), dim = c(2,3,3), dimnames = list(matrix_types, map_types, time_windows));
list_asn_matrices <<- array(list(), dim = c(2,3,3), dimnames = list(matrix_types, map_types, time_windows));
df_asn_org <<- fread(file = "./data/darknet/metadata/merit_asns.txt", header = TRUE)

# Read in csv files once every day
df_pkts_view <<- fread(file = "./data/darknet/metadata/packets.csv", header = TRUE)
df_bytes_view <<- fread(file = "./data/darknet/metadata/bytes.csv", header = TRUE)
df_uS_1d <<- fread(file = "./data/darknet/metadata/unique1d.csv", header = TRUE)
df_uS_1h <<- fread(file = "./data/darknet/metadata/unique1h.csv", header = TRUE)
df_uS_10m <<- fread(file = "./data/darknet/metadata/unique10m.csv", header = TRUE)

# For top 200 ports currently
df_bytes_port <<- fread(file = "./data/darknet/metadata/bytes-ports.csv", header = TRUE)
df_pkts_port <<- fread(file = "./data/darknet/metadata/packets-ports.csv", header = TRUE)
df_uS_1d_port <<- fread(file = "./data/darknet/metadata/unique1d-ports.csv", header = TRUE)
df_uS_1h_port <<- fread(file = "./data/darknet/metadata/unique1h-ports.csv", header = TRUE)
df_uS_10m_port <<- fread(file = "./data/darknet/metadata/unique10m-ports.csv", header = TRUE)

# For top 49 ASNs
df_pkts_asn <<- fread(file = "./data/darknet/metadata/packets-asns.csv", header = TRUE)
df_bytes_asn <<- fread(file = "./data/darknet/metadata/bytes-asns.csv", header = TRUE)
df_uS_10m_asn <<- fread(file = "./data/darknet/metadata/unique10m-asns.csv", header = TRUE)
df_uS_1h_asn <<- fread(file = "./data/darknet/metadata/unique1h-asns.csv", header = TRUE)
df_uS_1d_asn <<- fread(file = "./data/darknet/metadata/unique1d-asns.csv", header = TRUE)

# Filters to exclude countries and rename column
countries_excluded <<- c("Antarctica", "")
df_pkts_view <<- df_pkts_view[!df_pkts_view$country %in% countries_excluded,]
df_bytes_view <<- df_bytes_view[!df_bytes_view$country %in% countries_excluded,]
df_uS_1d <<- df_uS_1d[!df_uS_1d$country %in% countries_excluded,]
df_uS_1h <<- df_uS_1h[!df_uS_1h$country %in% countries_excluded,]
df_uS_10m <<- df_uS_10m[!df_uS_10m$country %in% countries_excluded,]

df_pkts_asn <<- df_pkts_asn[df_pkts_asn$time <= max(df_pkts_view$time)]
df_bytes_asn <<- df_bytes_asn[df_bytes_asn$time <= max(df_pkts_view$time)]
df_uS_10m_asn <<- df_uS_10m_asn[df_uS_10m_asn$time <= max(df_pkts_view$time)]
df_uS_1h_asn <<- df_uS_1h_asn[df_uS_1h_asn$time <= max(df_pkts_view$time)]
df_uS_1d_asn <<- df_uS_1d_asn[df_uS_1d_asn$time <= max(df_pkts_view$time)]

unique_countries <<- unique(df_pkts_view$country)
earliest_time_pkts <<- min(df_pkts_view$time)
latest_time_pkts <<- max(df_pkts_view$time)
earliest_time_bytes <<- min(df_bytes_view$time)
latest_time_bytes <<- max(df_bytes_view$time)
earliest_time_uS_10m <<- min(df_uS_10m$time)
latest_time_uS_10m <<- max(df_uS_10m$time)
earliest_time_uS_1h <<- min(df_uS_1h$time)
latest_time_uS_1h <<- max(df_uS_1h$time)
earliest_time_uS_1d <<- min(df_uS_1d$time)
latest_time_uS_1d <<- max(df_uS_1d$time) + 86400

earliest_time_pkts_asn <<- min(df_pkts_asn$time)
latest_time_pkts_asn <<- max(df_pkts_asn$time)
earliest_time_bytes_asn <<- min(df_bytes_asn$time)
latest_time_bytes_asn <<- max(df_bytes_asn$time)
earliest_time_uS_10m_asn <<- min(df_uS_10m_asn$time)
latest_time_uS_10m_asn <<- max(df_uS_10m_asn$time)
earliest_time_uS_1h_asn <<- min(df_uS_1h_asn$time)
latest_time_uS_1h_asn <<- max(df_uS_1h_asn$time)
earliest_time_uS_1d_asn <<- min(df_uS_1d_asn$time)
latest_time_uS_1d_asn <<- max(df_uS_1d_asn$time) 

earliest_time_pkts_port <<- min(df_pkts_port$time)
latest_time_pkts_port <<- max(df_pkts_port$time)
earliest_time_bytes_port <<- min(df_bytes_port$time)
latest_time_bytes_port <<- max(df_bytes_port$time)
earliest_time_uS_10m_port <<- min(df_uS_10m_port$time)
latest_time_uS_10m_port <<- max(df_uS_10m_port$time)
earliest_time_uS_1h_port <<- min(df_uS_1h_port$time)
latest_time_uS_1h_port <<- max(df_uS_1h_port$time)
earliest_time_uS_1d_port <<- min(df_uS_1d_port$time)
latest_time_uS_1d_port <<- max(df_uS_1d_port$time) 

earliest_time <<- c(earliest_time_pkts, earliest_time_bytes, earliest_time_uS_10m, earliest_time_uS_1h, earliest_time_uS_1d)
names(earliest_time) = c("Early Packets", "Early Bytes", "Early uS 10 Minutes", "Early uS Hourly", "Early uS Daily")
latest_time <<- c(latest_time_pkts, latest_time_bytes, latest_time_uS_10m, latest_time_uS_1h, latest_time_uS_1d)
names(latest_time) = c("Late Packets", "Late Bytes", "Late uS 10 Minutes", "Late uS Hourly", "Late uS Daily")

earliest_time_asn <<- c(earliest_time_pkts_asn, earliest_time_bytes_asn, earliest_time_uS_10m_asn, earliest_time_uS_1h_asn, earliest_time_uS_1d_asn)
names(earliest_time_asn) = c("ASN Early Packets", "ASN Early Bytes", "ASN Early uS 10 Minutes", "ASN Early uS Hourly", "ASN Early uS Daily")
latest_time_asn <<- c(latest_time_pkts_asn, latest_time_bytes_asn, latest_time_uS_10m_asn, latest_time_uS_1h_asn, latest_time_uS_1d_asn)
names(latest_time_asn) = c("ASN Late Packets", "ASN Late Bytes", "ASN Late uS 10 Minutes", "ASN Late uS Hourly", "ASN Late uS Daily")

earliest_time_port <<- c(earliest_time_pkts_port, earliest_time_bytes_port, earliest_time_uS_10m_port, earliest_time_uS_1h_port, earliest_time_uS_1d_port)
names(earliest_time_port) = c("port Early Packets", "port Early Bytes", "port Early uS 10 Minutes", "port Early uS Hourly", "port Early uS Daily")
latest_time_port <<- c(latest_time_pkts_port, latest_time_bytes_port, latest_time_uS_10m_port, latest_time_uS_1h_port, latest_time_uS_1d_port)
names(latest_time_port) = c("port Late Packets", "port Late Bytes", "port Late uS 10 Minutes", "port Late uS Hourly", "port Late uS Daily")

replace_ASN_names <- function(df) {
  df$asn <- as.character(df$asn)
  for (ASN in unique(df_asn_org$asn)){
    ASNname <- df_asn_org[asn == ASN]$aut_name
    df[asn == as.character(ASN)]$asn <- paste(as.character(ASN), ASNname)
  }
  return(df)
}

df_pkts_asn <<- replace_ASN_names(df_pkts_asn)
df_bytes_asn <<- replace_ASN_names(df_bytes_asn)
df_uS_10m_asn <<- replace_ASN_names(df_uS_10m_asn)
df_uS_1d_asn <<- replace_ASN_names(df_uS_1d_asn)
df_uS_1h_asn <<- replace_ASN_names(df_uS_1h_asn)


# Gets summary value and percentile for each time point
store_indices <- function(r, map_type, sliderStep){
  global_ind <- as.numeric(colSums(list_matrices["raw", map_type, sliderStep][[1]]))
  rank <- round(rank(global_ind) / length(global_ind), 4) * 100
  r$SET(paste("Global Index", map_type, sliderStep), object_to_bin(list(global_ind, rank)))
}

store_indices_ports <- function(r, map_type, sliderStep){
  global_ind <- as.numeric(colSums(list_port_matrices["raw", map_type, sliderStep][[1]]))
  rank <- round(rank(global_ind) / length(global_ind), 4) * 100
  r$SET(paste("Global Port Index", map_type, sliderStep), object_to_bin(list(global_ind, rank)))
}

store_indices_asn <- function(r, map_type, sliderStep){
  global_ind <- as.numeric(colSums(list_asn_matrices["raw", map_type, sliderStep][[1]]))
  rank <- round(rank(global_ind) / length(global_ind), 4) * 100
  r$SET(paste("Global ASN Index", map_type, sliderStep), object_to_bin(list(global_ind, rank)))
}

df_to_matrix <- function(df, map_type, time, stepSize, unique_values, uS='uS 10 Minutes'){
  
  if (map_type == "Unique Scanners"){
    names(df)[names(df) == "total_scanners"] <- "value"
    vec_unix = seq.int(earliest_time[paste("Early", uS)], latest_time[paste("Late", uS)], 600*stepSize)
  } else if (map_type == "Packets"){
    names(df)[names(df) == "total_packets"] <- "value"
    vec_unix = seq.int(earliest_time[paste("Early", map_type)], latest_time[paste("Late",map_type)], 600)
  } else {
    names(df)[names(df) == "total_bytes"] <- "value"
    vec_unix = seq.int(earliest_time[paste("Early", map_type)], latest_time[paste("Late",map_type)], 600)
  }
  
  ## Unique values could be countries, ports, ASNs
  nr = length(unique_values)
  nc = length(vec_unix)
  x_raw = matrix(0, nrow = nr, ncol = nc);
  rownames(x_raw)<-unique_values;
  colnames(x_raw)<-vec_unix;
  
  x = matrix(as.numeric(df$value),
             nrow=length(unique_values),
             ncol=nrow(df)/length(unique_values),
             byrow=FALSE);
  
  rownames(x) <- unique_values
  
  matrix_idx <- which(vec_unix %in% unique(df$time))
  
  # Fills in raw matrix with dataframe-converted matrix
  for (val in unique_values){
    x_raw[val, matrix_idx] <- x[val,]
  }
  
  # Fills in missing columns with previous column
  if (!exists("missing_time")){
    missing_time <<- as.numeric(setdiff(vec_unix, unique(df$time)))
  }
  
  for (time in missing_time){
    idx = which(colnames(x_raw) == time)
    x_raw[,idx] <- x_raw[,idx-1]
  }
  return(x_raw)
}

# Returns matrix where column names are unixtime
get_aggregated_raw_matrix <- function(x_raw, time_windows=1){
  print("Aggregating")
  x_raw <- t(apply(x_raw,1,cumsum));
  cx0 = cbind(0,x_raw);
  m = length(cx0[1,]);
  idx = time_windows*c(0:max(2,floor(m/time_windows)));
  idx = idx + 1;
  cx0 = t(apply(cx0[,idx],1,diff));
  rownames(cx0) = rownames(x_raw);
  return(cx0);
}

# Returns percentiles for raw matrix
get_percentiles_per_row <-function(x_raw, ties.method = "average"){
  p=t(apply(x_raw,1,rank,ties.method = ties.method))/length(x_raw[1,]);
  rownames(p)<- rownames(x_raw);
  return(p);
}

# Creates all matrices for countries
create_matrices <- function(matrix_type, map_type, time_windows, sliderStep){
  s1 <- Sys.time()
  if (map_type == "Bytes"){
    if (length(unlist(list_matrices["raw", "Bytes", "10 Minutes"])) == 0){
      list_matrices["raw", "Bytes", "10 Minutes"][[1]] <<- df_to_matrix(df_bytes_view, map_type, sliderStep, time_windows, unique_countries)
    }
  } else if (map_type == "Packets"){
    if (length(unlist(list_matrices["raw", "Packets", "10 Minutes"])) == 0){
      list_matrices["raw", "Packets", "10 Minutes"][[1]] <<- df_to_matrix(df_pkts_view, map_type, sliderStep, time_windows, unique_countries)
    }
  }
  if (time_windows != 1 && map_type != "Unique Scanners"){
    list_matrices["raw", map_type, sliderStep][[1]] <<- get_aggregated_raw_matrix(list_matrices["raw", map_type, "10 Minutes"][[1]], time_windows)
  } else if (map_type == "Unique Scanners"){
    if (length(unlist(list_matrices["raw", "Unique Scanners", "10 Minutes"])) == 0 && sliderStep == "10 Minutes"){
      list_matrices["raw", "Unique Scanners", sliderStep][[1]] <<- df_to_matrix(df_uS_10m, map_type, sliderStep, time_windows, unique_countries, "uS 10 Minutes")
    } else if (length(unlist(list_matrices["raw", "Unique Scanners", "Hourly"])) == 0 && sliderStep == "Hourly"){
      list_matrices["raw", "Unique Scanners", sliderStep][[1]] <<- df_to_matrix(df_uS_1h, map_type, sliderStep, time_windows, unique_countries, "uS Hourly")
    } else if (length(unlist(list_matrices["raw", "Unique Scanners", "Daily"])) == 0 && sliderStep == "Daily"){
      list_matrices["raw", "Unique Scanners", sliderStep][[1]] <<- df_to_matrix(df_uS_1d, map_type, sliderStep, time_windows, unique_countries, "uS Daily")
    }
  }
  if (matrix_type == "prct"){
    list_matrices["prct", map_type, sliderStep][[1]] <<- get_percentiles_per_row(list_matrices["raw", map_type, sliderStep][[1]])
  }
  e1 <- Sys.time()
  print(paste("Creating matrix for", matrix_type, sliderStep, map_type, "took", round(e1-s1,2),"seconds"))
}

# Edit the create_matrices so it works for both ports and countries
create_port_matrices <- function(matrix_type, map_type, time_windows, sliderStep){
  s1 <- Sys.time()
  if (map_type == "Bytes"){
    if (length(unlist(list_port_matrices["raw", "Bytes", "10 Minutes"])) == 0){
      unique_ports <<- as.character(unique(df_bytes_port$port))
      list_port_matrices["raw", "Bytes", "10 Minutes"][[1]] <<- df_to_matrix(df_bytes_port, map_type, sliderStep, time_windows, unique_ports)
    }
  } else if (map_type == "Packets"){
    if (length(unlist(list_port_matrices["raw", "Packets", "10 Minutes"])) == 0){
      unique_ports <<- as.character(unique(df_pkts_port$port))
      list_port_matrices["raw", "Packets", "10 Minutes"][[1]] <<- df_to_matrix(df_pkts_port, map_type, sliderStep, time_windows, unique_ports)
    }
  }
  if (time_windows != 1 && map_type != "Unique Scanners"){
    list_port_matrices["raw", map_type, sliderStep][[1]] <<- get_aggregated_raw_matrix(list_port_matrices["raw", map_type, "10 Minutes"][[1]], time_windows)
  } else if (map_type == "Unique Scanners"){
    unique_ports <<- as.character(unique(df_uS_10m_port$port))
    if (length(unlist(list_port_matrices["raw", "Unique Scanners", "10 Minutes"])) == 0 && sliderStep == "10 Minutes"){
      list_port_matrices["raw", "Unique Scanners", sliderStep][[1]] <<- df_to_matrix(df_uS_10m_port, map_type, sliderStep, time_windows, unique_ports, "uS 10 Minutes")
    } else if (length(unlist(list_port_matrices["raw", "Unique Scanners", "Hourly"])) == 0 && sliderStep == "Hourly"){
      list_port_matrices["raw", "Unique Scanners", sliderStep][[1]] <<- df_to_matrix(df_uS_1h_port, map_type, sliderStep, time_windows, unique_ports, "uS Hourly")
    } else if (length(unlist(list_port_matrices["raw", "Unique Scanners", "Daily"])) == 0 && sliderStep == "Daily"){
      list_port_matrices["raw", "Unique Scanners", sliderStep][[1]] <<- df_to_matrix(df_uS_1d_port, map_type, sliderStep, time_windows, unique_ports, "uS Daily")
    }
  }
  if (matrix_type == "prct"){
    list_port_matrices["prct", map_type, sliderStep][[1]] <<- get_percentiles_per_row(list_port_matrices["raw", map_type, sliderStep][[1]])
  }
  e1 <- Sys.time()
  print(paste("Creating port matrix for", matrix_type, sliderStep, map_type, "took", round(e1-s1,2),"seconds"))
}

# Edit the create_matrices so it works for both ports and countries
create_asn_matrices <- function(matrix_type, map_type, time_windows, sliderStep){
  s1 <- Sys.time()
  if (map_type == "Bytes"){
    if (length(unlist(list_asn_matrices["raw", "Bytes", "10 Minutes"])) == 0){
      unique_asn <<- as.character(unique(df_bytes_asn$asn))
      list_asn_matrices["raw", "Bytes", "10 Minutes"][[1]] <<- df_to_matrix(df_bytes_asn, map_type, sliderStep, time_windows, unique_asn)
    }
  } else if (map_type == "Packets"){
    if (length(unlist(list_asn_matrices["raw", "Packets", "10 Minutes"])) == 0){
      unique_asn <<- as.character(unique(df_pkts_asn$asn))
      list_asn_matrices["raw", "Packets", "10 Minutes"][[1]] <<- df_to_matrix(df_pkts_asn, map_type, sliderStep, time_windows, unique_asn)
    }
  }
  if (time_windows != 1 && map_type != "Unique Scanners"){
    list_asn_matrices["raw", map_type, sliderStep][[1]] <<- get_aggregated_raw_matrix(list_asn_matrices["raw", map_type, "10 Minutes"][[1]], time_windows)
  } else if (map_type == "Unique Scanners"){
    if (length(unlist(list_asn_matrices["raw", "Unique Scanners", "10 Minutes"])) == 0 && sliderStep == "10 Minutes"){
      list_asn_matrices["raw", "Unique Scanners", sliderStep][[1]] <<- df_to_matrix(df_uS_10m_asn, map_type, sliderStep, time_windows, unique_asn, "uS 10 Minutes")
    } else if (length(unlist(list_asn_matrices["raw", "Unique Scanners", "Hourly"])) == 0 && sliderStep == "Hourly"){
      list_asn_matrices["raw", "Unique Scanners", sliderStep][[1]] <<- df_to_matrix(df_uS_1h_asn, map_type, sliderStep, time_windows, unique_asn, "uS Hourly")
    } else if (length(unlist(list_asn_matrices["raw", "Unique Scanners", "Daily"])) == 0 && sliderStep == "Daily"){
      list_asn_matrices["raw", "Unique Scanners", sliderStep][[1]] <<- df_to_matrix(df_uS_1d_asn, map_type, sliderStep, time_windows, unique_asn, "uS Daily")
    }
  }
  if (matrix_type == "prct"){
    list_asn_matrices["prct", map_type, sliderStep][[1]] <<- get_percentiles_per_row(list_asn_matrices["raw", map_type, sliderStep][[1]])
  }
  e1 <- Sys.time()
  print(paste("Creating asn matrix for", matrix_type, sliderStep, map_type, "took", round(e1-s1,2),"seconds"))
}

# Computes all matrices
compute_matrices <- function(){
  list_matrices <<- array(list(), dim=c(2,3,3), dimnames = list(c("raw","prct"),c("Packets","Bytes", "Unique Scanners"),c("10 Minutes","Hourly","Daily") ));
  for (matrix_type in matrix_types){
    for (map_type in map_types){
      for (time_window in time_windows){
        if (time_window == "Daily"){
          time_step = 144
        } else if (time_window == "Hourly"){
          time_step = 6
        } else {
          time_step = 1
        }
        print(paste("Creating matrix for", matrix_type, map_type, time_window))
        create_matrices(matrix_type, map_type, time_step, time_window)
        create_port_matrices(matrix_type, map_type, time_step, time_window)
        create_asn_matrices(matrix_type, map_type, time_step, time_window)
        print(paste("Matrix for", matrix_type, map_type, time_window, "successfully created"))
      }
    }
  }
}

to_date <- function(unix_time) {
  return(as.POSIXct(as.numeric(as.character(unix_time)), origin="1970-01-01", tz="UTC"))
}

# Updates existing matrices once a day
set_redis <- function(r, matrix_type, map_type, time_windows){
  key <- paste(matrix_type, map_type, time_windows)
  val <- list_matrices[matrix_type, map_type, time_windows][[1]]
  r$SET(key, redux::object_to_bin(val))
}

set_redis_port <- function(r, matrix_type, map_type, time_windows){
  key <- paste("Ports", matrix_type, map_type, time_windows)
  val <- list_port_matrices[matrix_type, map_type, time_windows][[1]]
  r$SET(key, redux::object_to_bin(val))
}

set_redis_asn <- function(r, matrix_type, map_type, time_windows){
  key <- paste("ASN", matrix_type, map_type, time_windows)
  val <- list_asn_matrices[matrix_type, map_type, time_windows][[1]]
  r$SET(key, redux::object_to_bin(val))
}

# Creates redis object at given host (default: "127.0.0.1") and port (default: 6379)
redis_populate <- function(host = "127.0.0.1", port = 6379){
  r <- hiredis(host = host, port = port)
  print("Storing countrycode")
  CC <- countrycode(unique_countries, 'country.name', 'iso3c')
  r$SET("countrycode", object_to_bin(CC))
  unique_asn <<- union(as.character(unique(df_pkts_asn$asn)), as.character(unique(df_uS_10m_asn$asn)))
  unique_ports <<- union(as.character(unique(df_pkts_port$port)), as.character(unique(df_uS_10m_port$port)))
  r$SET("unique_ports", object_to_bin(unique_ports))
  r$SET("unique_asn", object_to_bin(unique_asn))

  # Add earliest, latest, and missing times
  vec_unix <- seq.int(earliest_time[1], latest_time[1], 600)
  missing_time_pkts <- as.numeric(setdiff(vec_unix, unique(df_pkts_view$time)))
  print("Storing missing times and unique countries")
  r$SET("unique_countries", object_to_bin(unique_countries))
  r$SET("missing_time", object_to_bin(missing_time_pkts))

  for (i in names(earliest_time)){
    r$SET(i, earliest_time[i])
  }
  for (i in names(latest_time)){
    r$SET(i, latest_time[i])
  }
  for (i in names(earliest_time_port)){
    r$SET(i, earliest_time_port[i])
  }
  for (i in names(latest_time_port)){
    r$SET(i, latest_time_port[i])
  }
  for (i in names(earliest_time_asn)){
    r$SET(i, earliest_time_asn[i])
  }
  for (i in names(latest_time_asn)){
    r$SET(i, latest_time_asn[i])
  }
  print("Storing matrices")
  for (time_window in time_windows){
    for (map_type in map_types){
      store_indices(r, map_type, time_window)
      store_indices_ports(r, map_type, time_window)
      store_indices_asn(r, map_type, time_window)
      for (matrix_type in matrix_types){
        set_redis(r, matrix_type, map_type, time_window)
        set_redis_port(r, matrix_type, map_type, time_window)
        set_redis_asn(r, matrix_type, map_type, time_window)
        print(paste("Stored matrix for", matrix_type, map_type, time_window))
      }
    }
  }
}

print("Getting ready to compute matrices!")
compute_matrices()
print("Transferring data from local workspace onto Redis server!")
redis_populate()
print("Done with storing data!")
