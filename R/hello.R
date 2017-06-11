#' lineage_pyrate
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
lineage_pyrate <- function(x) {
  x.1 <- x
  colnames(x.1) <- c("clade", "species", "min_age", "max_age")

  species_pyrate_data <- data.frame(
    clade = as.numeric(factor(x.1$clade)),
    species = as.numeric(factor(x.1$species)),
    ts = (max(x.1$max_age) - x.1$min_age),
    te = (max(x.1$max_age) - x.1$max_age)
  )

  fixed_clade_key <-
    data.frame(clade_name = x.1$clade, clade_no = as.numeric(factor(x.1$clade))) %>%
    group_by(clade_name) %>%
    summarize(No = first(clade_no),
              count = n()) %>%
    arrange(No)

  colnames(fixed_clade_key) <- c("Names", "ID Number", "Count")

  fixed_species_key <-
    data.frame(species_name = x.1$species,
               species_no = as.numeric(factor(x.1$species))) %>%
    arrange(species_no)

  colnames(fixed_species_key) <- c("Names", "ID Number")

  return(list(species_pyrate_data = species_pyrate_data,
              fixed_clade_key = fixed_clade_key,
              fixed_species_key = fixed_species_key))
}

#' occurrence_pyrate
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
occurrence_pyrate <- function(x) {
  x$status <- "extinct"
  colnames(x) <- c("species", "min_age", "max_age", "trait", "status")
  x$status[which(x$min_age == 0)] <- "extant"

  occurrence_pyrate_data <- data.frame(
    Species = x$species,
    Status = x$status,
    min_age = x$min_age,
    max_age = x$max_age,
    trait = x$trait
  )

  return(occurrence_pyrate_data)

}



#
#' LTT Plots
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
plot_LTT <- function(x, y) {
  a <- rev(sort(x$ts))
  a <- c(a, 0)
  n <- length(a)
  z <- max(a) - (a)
  vn <- 1:n
  f <- log(vn + 1) ~ z
  plot(
    f,
    xlab = "Time \n (0 is Most Recent)",
    ylab = "Log of speciess",
    type = "l",
    xaxt = "n",
    col = "dodgerblue",
    lwd = 2,
    main = "Log species \n Through Time"
  )
  year.labels <-
    seq(round(min(z), digits = 0), round(max(z), digits = 0), by = y)
  axis(1,
       at = seq(0, round(max(z), digits = 0), by = y),
       labels = rev(year.labels))
  segments(min(z), min(log(vn + 1)), max(z), max(log(vn + 1)), lty = 2)
}

#' plot_LTT_occurrence
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
plot_LTT_occurrence <- function(x, y) {
  a <- rev(sort(x$max_age))
  a <- c(a, 0)
  n <- length(a)
  z <- max(a) - (a)
  vn <- 1:n
  f <- log(vn + 1) ~ z
  plot(
    f,
    xlab = "Time \n (0 is Most Recent)",
    ylab = "Log of speciess",
    type = "l",
    xaxt = "n",
    col = "dodgerblue",
    lwd = 2,
    main = "Log species \n Through Time"
  )
  year.labels <-
    seq(round(min(z), digits = 0), round(max(z), digits = 0), by = y)
  axis(1,
       at = seq(0, round(max(z), digits = 0), by = y),
       labels = rev(year.labels))
  segments(min(z), min(log(vn + 1)), max(z), max(log(vn + 1)), lty = 2)
}

###Histogram Functions

#' lifespan_hist
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
lifespan_hist <- function(x, y) {
  lifespans <-
    x %>% group_by(species) %>%
    summarize(lifespan = mean(ts) - mean(te))

  hist(
    lifespans$lifespan,
    breaks = seq(floor(min(
      lifespans$lifespan
    )), ceiling(max(
      lifespans$lifespan
    ))),
    right = TRUE,
    xlab = "Lifespans",
    main = "Histogram of \n Lifespans",
    xaxt = "n"
  )
  lifespan.labels <-
    seq(round(min(lifespans$lifespan)), round(max(lifespans$lifespan)), by =
          y)
  axis(1, at = seq(round(min(
    lifespans$lifespan
  )), round(max(
    lifespans$lifespan
  )), by = y), labels = lifespan.labels)
}

#' lifespan_hist_occurrence
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
lifespan_hist_occurrence <- function(x, y) {
  lifespans <-
    x %>% group_by(Species) %>%
    summarize(lifespan = mean(max_age) - mean(min_age))

  hist(
    lifespans$lifespan,
    breaks = seq(floor(min(
      lifespans$lifespan
    )), ceiling(max(
      lifespans$lifespan
    ))),
    right = TRUE,
    xlab = "Lifespans",
    main = "Histogram of Species \n Lifespans",
    xaxt = "n"
  )
  lifespan.labels <-
    seq(round(min(lifespans$lifespan)), round(max(lifespans$lifespan)), by =
          y)
  axis(1, at = seq(round(min(
    lifespans$lifespan
  )), round(max(
    lifespans$lifespan
  )), by = y), labels = lifespan.labels)
}

### Average Lifespan Through Time


#' average_lifespan
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
average_lifespan <- function(x, y) {
  rounded <-
    x %>% mutate(round_ts = round(ts, digits = 0),
                 round_te = round(te, digits = 0))
  average_lifespan <-
    rounded %>% group_by(round_ts) %>% summarize(diff = mean(ts) - mean(te))
  z <- average_lifespan$round_ts
  plot(
    average_lifespan,
    xlab = "Time \n (0 is Most Recent)",
    ylab = "Mean Lifespan",
    col = "dodgerblue",
    lwd = 2,
    type = "l",
    xaxt = "n",
    main = "Average Lifespan \n through Time",
    xlim = c(max(z), min(z))
  )
  year.labels <- seq(floor(min(z)), ceiling(max(z)), by = y)
  axis(1, at = seq(floor(min(z)), ceiling(max(z)), by = y), labels = year.labels)
}

#' average_lifespan_occurrence
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
average_lifespan_occurrence <- function(x, y) {
  rounded <-
    x %>% mutate(
      round_max_age = round(max_age, digits = 0),
      round_min_age = round(min_age, digits = 0)
    )
  average_lifespan <-
    rounded %>% group_by(round_max_age) %>% summarize(diff = mean(max_age) -
                                                        mean(min_age))
  z <- average_lifespan$round_max_age
  plot(
    average_lifespan,
    xlab = "Time \n (0 is Most Recent)",
    ylab = "Mean Lifespan",
    col = "dodgerblue",
    lwd = 2,
    type = "l",
    xaxt = "n",
    main = "Average Lifespan \n through Time",
    xlim = c(max(z), min(z))
  )
  year.labels <- seq(floor(min(z)), ceiling(max(z)), by = y)
  axis(1, at = seq(floor(min(z)), ceiling(max(z)), by = y), labels = year.labels)
}


###Diversity Through Time

#' diversity_time
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
diversity_time <- function(x, y) {
  x$ts <- round(x$ts, digits = 0)
  x$te <- round(x$te, digits = 0)
  ts.counts <-
    x %>%
    group_by(ts) %>%
    summarize(total_ts = n_distinct(species)) %>%
    mutate(cumsum = cumsum(total_ts)) %>%
    mutate(ID = ts)

  plot(
    ts.counts$ts,
    ts.counts$cumsum,
    type = "l",
    xlab = "Time \n (0 is Most Recent)",
    col = "dodgerblue",
    lwd = 2,
    ylab = "Cumulative Diversity",
    xaxt = "n",
    main = "Cumulative Diversity \n Through Time"
  )
  year.labels <-
    seq(floor(min(ts.counts$ts)), ceiling(max(ts.counts$ts)), by = y)
  axis(1,
       at = seq(floor(min(ts.counts$ts)), ceiling(max(ts.counts$ts)), by = y),
       labels = rev(year.labels))
}

diversity_time_occurrence <- function(x, y) {
  x$max_age <- round(x$max_age, digits = 0)
  x$min_age <- round(x$min_age, digits = 0)
  max_age.counts <-
    x %>% group_by(max_age) %>% summarize(total_max_age = n_distinct(Species)) %>%
    mutate(cumsum = cumsum(total_max_age)) %>% mutate(ID = max_age)
  plot(
    max_age.counts$max_age,
    max_age.counts$cumsum,
    type = "l",
    xlab = "Time \n (0 is Most Recent)",
    col = "dodgerblue",
    lwd = 2,
    ylab = "Cumulative Diversity",
    xaxt = "n",
    main = "Cumulative Diversity \n Through Time"
  )
  year.labels <-
    seq(floor(min(max_age.counts$max_age)), ceiling(max(max_age.counts$max_age)), by =
          y)
  axis(1,
       at = seq(floor(min(
         max_age.counts$max_age
       )), ceiling(max(
         max_age.counts$max_age
       )), by = y),
       labels = rev(year.labels))
}

#' summary_plots
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
summary_plots <- function(x, y) {
  par(mfrow = c(2, 2))
  plot_LTT(x, y)
  diversity_time(x, y)
  average_lifespan(x, y)
  lifespan_hist(x, y)
  par(mfrow = c(1, 1))
}

#' summary_plots_occurrence
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
summary_plots_occurrence <- function(x, y) {
  par(mfrow = c(2, 2))
  plot_LTT_occurrence(x, y)
  diversity_time_occurrence(x, y)
  average_lifespan_occurrence(x, y)
  lifespan_hist_occurrence(x, y)
  par(mfrow = c(1, 1))
}

#' plot_RTT
#'
#' @param age
#' @param hpd_M
#' @param hpd_m
#' @param mean_m
#' @param color
#' @param RM
#'
#' @return
#' @export
#'
#' @examples
plot_RTT <- function (age, hpd_M, hpd_m, mean_m, color, RM) {
  N = 100
  rem = 1
  for (i in 1:(N - 1)) {
    trans = 1 / N #-0.0045
    polygon(c(age, rev(age)),
            c(hpd_M - ((hpd_M - mean_m) * rem), rev(hpd_m + ((mean_m - hpd_m) * rem
            ))),
            col = alpha(color, trans),
            border = NA)
    rem = rem - (1 / N)
    #print(c(rem,trans))
  }
  lines(rev(age), rev(mean_m), col = color, lwd = 3)
}

#' shift.points.func
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
shift.points.func <- function(x, y) {
  shift.data <- hist(x, breaks = (max(round(x)) - min(round(x))), plot = FALSE)
  shift.data <-
    data.frame(breaks = shift.data$mids, counts = shift.data$counts)
  summary <- data.frame(
    max_age = (y - shift.data$breaks),
    min_age = (y - shift.data$breaks),
    counts = shift.data$counts,
    proportion = (shift.data$counts / sum(shift.data$counts))
  )
  summary <- arrange(summary, desc(counts))
  summary_top_3 <- head(summary, 3)
  print(summary_top_3)
}


#' extract.ages
#'
#' @param file
#' @param replicates
#' @param cutoff
#' @param random
#'
#' @return
#' @export
#'
#' @examples
extract.ages <-
  function(data = data,
           replicates = 1,
           cutoff = NULL,
           random = TRUE) {
    if (is.null(data))
      stop("you must enter the name of your occurrance data frame\n")

    rnd <- random
    q <- cutoff
    dat1 <- data
    outfile <- paste(deparse(substitute(data)), "_PyRate.py", sep = "")

    dat1[, 1] <- gsub("[[:blank:]]{1,}", "_", dat1[, 1])

    if (replicates > 1) {
      rnd <- TRUE
    }

    if (any(is.na(dat1[, 1:4]))) {
      stop("the input file contains missing data in species names, status or ages)\n")
    }

    if (!is.null(q)) {
      dat <- dat1[!(dat1[, 4] - dat1[, 3] >= q), ]
    } else {
      dat <- dat1
    }

    if (length(dat) == 5) {
      colnames(dat) <-
        c("Species", "Status", "min_age", "max_age", "trait")
    } else {
      colnames(dat) <- c("Species", "Status", "min_age", "max_age")
    }

    dat$new_age <- "NA"
    splist <- unique(dat[, c(1, 2)])[order(unique(dat[, c(1, 2)][, 1])), ]


    if (any(is.element(splist$Species[splist$Status == "extant"], splist$Species[splist$Status == "extinct"]))) {
      print(intersect(splist$Species[splist$Status == "extant"], splist$Species[splist$Status == "extinct"]))
      stop("at least one species is listed as both extinct and extant\n")
    }

    cat("#!/usr/bin/env python",
        "from numpy import * ",
        "",
        file = outfile,
        sep = "\n")

    for (j in 1:replicates) {
      times <- list()
      cat ("\nreplicate", j)

      dat[dat$min_age == 0, 3] <- 0.001

      if (any(dat[, 4] < dat[, 3])) {
        cat("\nWarning: the min age is older than the max age for at least one record\n")
        cat ("\nlines:", 1 + as.numeric(which(dat[, 4] < dat[, 3])), sep = " ")
      }

      if (isTRUE(rnd)) {
        dat$new_age <-
          round(runif(
            length(dat[, 1]),
            min = apply(dat[, 3:4], FUN = min, 1),
            max = apply(dat[, 3:4], FUN = max, 1)
          ), digits = 6)
      } else {
        for (i in 1:length(dat[, 1])) {
          dat$new_age[i] <- mean(c(dat[i, 3], dat[i, 4]))
        }
      }

      dat2 <- subset(dat, select = c("Species", "new_age"))
      taxa <- sort(unique(dat2$Species))

      for (n in 1:length(taxa)) {
        times[[n]] <- dat2$new_age[dat2$Species == taxa[n]]
        if (toupper(splist$Status[splist$Species == taxa[n]]) == toupper("extant")) {
          times[[n]] <- append(times[[n]], "0", after = length(times[[n]]))
        }
      }

      dat3 <-
        matrix(data = NA,
               nrow = length(times),
               ncol = max(sapply(times, length)))
      rownames(dat3) <- taxa

      for (p in 1:length(times)) {
        dat3[p, 1:length(times[[p]])] <- times[[p]]
      }

      cat(noquote(sprintf("\ndata_%s=[", j)), file = outfile, append = TRUE)

      for (n in 1:(length(taxa) - 1)) {
        rec <- paste(dat3[n, !is.na(dat3[n, ])], collapse = ",")
        cat(
          noquote(sprintf("array([%s]),", rec)),
          file = outfile,
          append = TRUE,
          sep = "\n"
        )
      }

      n <- n + 1
      rec <- paste(dat3[n, !is.na(dat3[n, ])], collapse = ",")
      cat(
        noquote(sprintf("array([%s])", rec)),
        file = outfile,
        append = TRUE,
        sep = "\n"
      )

      cat("]",
          "",
          file = outfile,
          append = TRUE,
          sep = "\n")
    }


    data_sets <- ""
    names <- ""

    if (replicates > 1) {
      for (j in 1:(replicates - 1)) {
        data_sets <- paste(data_sets, noquote(sprintf("data_%s,", j)))
        names <- paste(names, noquote(sprintf(" '%s_%s',", deparse(substitute(data)), j)))
      }

      data_sets <- paste(data_sets, noquote(sprintf("data_%s", j + 1)))
      names <- paste(names, noquote(sprintf(" '%s_%s',", deparse(substitute(data)), j + 1)))
    } else {
      data_sets <- "data_1"
      names <- noquote(sprintf(" '%s_1'", deparse(substitute(data))))
    }

    cat(
      noquote(sprintf("d=[%s]", data_sets)),
      noquote(sprintf("names=[%s]", names)),
      "def get_data(i): return d[i]",
      "def get_out_name(i): return  names[i]",
      file = outfile,
      append = TRUE,
      sep = "\n"
    )


    tax_names <- paste(taxa, collapse = "','")
    cat(
      noquote(sprintf("taxa_names=['%s']", tax_names)),
      "def get_taxa_names(): return taxa_names",
      file = outfile,
      append = TRUE,
      sep = "\n"
    )


    if ("trait" %in% colnames(dat)) {
      datBM <- dat[, 1]
      splist$Trait <- NA
      for (n in 1:length(splist[, 1])) {
        splist$Trait[n] <- mean(dat$trait[datBM == splist[n, 1]], na.rm = T)
      }
      s1 <- "\ntrait1=array(["
      BM <- gsub("NaN|NA", "nan", toString(splist$Trait))
      s2 <-
        "])\ntraits=[trait1]\ndef get_continuous(i): return traits[i]"
      STR <- paste(s1, BM, s2)
      cat(STR,
          file = outfile,
          append = TRUE,
          sep = "\n")
    }

    splistout <-
      paste(deparse(substitute(data)), "_SpeciesList.txt", sep = "")
    lookup <- as.data.frame(taxa)
    lookup$status  <- "extinct"

    write.table(
      splist,
      file = splistout,
      sep = "\t",
      row.names = F,
      quote = F
    )
    cat("\n\nPyRate input file was saved in: ",
        sprintf("%s", outfile),
        "\n\n")

  }


fit.prior <- function(file = NULL, lineage = "root_age"){

  require(fitdistrplus)

  if (is.null(file)){
    stop("You must enter a valid filename.\n")
  }

  dat <- read.table(file, header=T, stringsAsFactors=F, row.names=NULL, sep="\t")
  fname <- no.extension(basename(file))
  outfile <- paste(dirname(file), "/", lineage, "_Prior.txt", sep="")

  lineage2 <- paste(lineage,"_TS", sep="")
  if (!is.element(lineage2, colnames(dat))){
    stop("Lineage not found, please check your input.\n")
  }

  time <- dat[,which(names(dat) == lineage2)]
  time2 <- time-(min(time)-0.01)
  gamm <- fitdist(time2, distr="gamma", method = "mle")$estimate

  cat("Lineage: ", lineage, "; Shape: ", gamm[1], "; Scale: ", 1/gamm[2], "; Offset: ", min(time), sep="", file=outfile, append=FALSE)
}
