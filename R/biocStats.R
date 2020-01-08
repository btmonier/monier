#' @title Get Bioconductor statistics
#'
#' @description Pulls package data from Bioconductor and plots number of
#'    downloads from distinct IP addresses and total numbers, regardless of
#'    IP address distinction. Requires an active internet connection.
#'
#' @param pkg A Bioconductor package title.
#'
#' @import ggplot2
#' @importFrom RCurl url.exists
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#' @importFrom utils read.delim
#'
#' @export
getBiocStats <- function(pkg) {
    # Contact Bioconductor server
    message("Contacting server...")

    # Retrieve data
    url <- "http://bioconductor.org/packages/stats/bioc/%s/%s_stats.tab"
    url <- gsub("%s", pkg, url, fixed = TRUE)

    if (RCurl::url.exists(url)) {
        df <- utils::read.delim(url, header = TRUE)
    } else {
        stop("This Bioconductor package does not exist.")
    }

    # Data munging
    df$Year <- as.factor(df$Year)
    df <- df[!(df$Month == "all"), ]
    df$Month <- factor(df$Month)
    df$Month <- factor(
        df$Month, levels = c(
            "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
        )
    )
    df.m <- reshape2::melt(
        data = df,
        id.vars = c(
            "Year",
            "Month"
        ),
        measure.vars = c(
            "Nb_of_distinct_IPs",
            "Nb_of_downloads"
        )
    )
    df.m <- df.m[order(as.Date(df.m$Month, format="%d/%m/%Y")), ]
    df.m$variable <- gsub("_", " ", df.m$variable)
    df.m$variable <- gsub("Nb", "No.", df.m$variable)

    # Visualization
    biocPlot <- ggplot2::ggplot(
        data = df.m,
        ggplot2::aes(x = .data$Month, y = .data$value, fill = .data$variable)
    ) +
        ggplot2::geom_bar(
            stat = "identity",
            position = ggplot2::position_dodge()
        ) +
        ggplot2::scale_fill_brewer(palette = "Paired") +
        ggplot2::labs(
            x = "",
            y = "",
            title = paste(pkg, "downloads:", Sys.time())
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = "")) +
        ggplot2::facet_grid(Year ~ .) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")

    return(biocPlot)

}