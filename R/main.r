################################################################################
#                 Inset graph within map in R
#                 Milos Popovic
#                 2023/06/06
################################################################################

# libraries we need
libs <- c(
    "rgho", "tidyverse", "sf",
    "giscoR", "grid", "classInt"
)

# install missing libraries
installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries

invisible(lapply(libs, library, character.only = T))

# 1. LOAD GHO DATA
#-----------------
rgho::get_gho_values(dimension = "GHO")
rgho::search_values("infant mortality", dimension = "GHO")
res <- rgho::get_gho_data(
    code = "MDG_0000000001",
    filter = list(
        YEAR = 2021
    )
)

head(res)

# 2. FILTER EUROPE
#-----------------

europe_sf <- giscoR::gisco_get_countries(
    resolution = "3",
    region = "Europe"
)

europe_list <- c(unique(
    europe_sf$ISO3_CODE
))

europe_df <- res |>
    dplyr::filter(
        COUNTRY %in% europe_list
    ) |>
    dplyr::filter(
        SEX == "BTSX"
    ) |>
    dplyr::select(
        COUNTRY, NumericValue,
        Low, High
    )

# 3. JOIN DATA
#-------------

spatial_join <- function() {
    europe_joined <- europe_sf |>
        dplyr::left_join(
            europe_df,
            by = c("ISO3_CODE" = "COUNTRY")
        ) |>
        drop_na() |>
        sf::st_as_sf() |>
        dplyr::select(
            NAME_ENGL,
            NumericValue,
            Low, High
        )
    return(europe_joined)
}

europe_joined <- spatial_join()

# 4. BREAKS
#----------

ni <- classInt::classIntervals(
    europe_joined$NumericValue,
    n = 6,
    style = "jenks"
)$brks

labels <- c()

for (i in 1:length(ni)) {
    labels <- c(
        labels, paste0(
            round(ni[i], 0),
            "-",
            round(ni[i + 1], 0)
        )
    )
}

labels <- labels[1:length(labels) - 1]

europe_joined$cat <- cut(
    europe_joined$NumericValue,
    breaks = ni,
    labels = labels,
    include.lowest = T
)

levels(europe_joined$cat)

# 5. MAP
#----------

crs_longlat <- "+proj=longlat +datum=WGS84 +no_defs"
crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

get_bbox <- function() {
    bb <- sf::st_sfc(
        sf::st_polygon(list(
            cbind(
                c(-10.6, 36.0, 36.0, -10.6, -10.6),
                c(32.5, 32.5, 71.05, 71.05, 32.5)
            )
        )),
        crs = crs_longlat
    ) |>
        sf::st_transform(
            crs = crs_lambert
        ) |>
        sf::st_bbox()

    return(bb)
}

bb <- get_bbox()

cols <- c(
    "#f89f5b", "#e6746b", "#cb4978",
    "#9a3678", "#6b2568", "#3f1651"
)

map <- ggplot() +
    geom_sf(
        data = europe_joined,
        aes(
            fill = cat
        ),
        color = "white",
        size = .125
    ) +
    coord_sf(
        crs = crs_lambert,
        xlim = c(bb["xmin"], bb["xmax"]),
        ylim = c(bb["ymin"], bb["ymax"]),
    ) +
    scale_fill_manual(
        name = "deaths per 1k live births",
        values = cols
    ) +
    guides(
        fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.5, "mm"),
            keywidth = unit(15, "mm"),
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = T
        )
    ) +
    labs(
        title = "Infant mortality in 2021",
        caption = "Data source: WHO, Global Health Observatory",
        x = "",
        y = ""
    ) +
    theme_void() +
    theme(
        legend.position = c(.45, .01),
        plot.title = element_text(
            size = 18, color = cols[6],
            hjust = .5, vjust = 1
        ),
        plot.caption = element_text(
            size = 8, color = "grey60",
            hjust = .5, vjust = -3
        ),
        plot.margin = unit(
            c(t = -4, b = -4, r = 0, l = 10),
            "lines"
        )
    )

print(map)

# 6. BARPLOT DATA
#----------------

get_barplot_df <- function() {
    infant_mortality_df <- europe_joined |>
        sf::st_drop_geometry() |>
        dplyr::select(
            NAME_ENGL, NumericValue, cat
        ) |>
        dplyr::mutate(
            NAME_ENGL = replace(
                NAME_ENGL, stringr::str_detect(
                    NAME_ENGL, "Bosnia and Herzegovina"
                ), "BIH"
            )
        ) |>
        dplyr::mutate(
            NAME_ENGL = replace(
                NAME_ENGL, stringr::str_detect(
                    NAME_ENGL, "Russian Federation"
                ), "Russia"
            )
        ) |>
        dplyr::mutate(
            NAME_ENGL = replace(
                NAME_ENGL, stringr::str_detect(
                    NAME_ENGL, "North Macedonia"
                ), "N. Macedonia"
            )
        ) |>
        dplyr::mutate(
            NAME_ENGL = replace(
                NAME_ENGL, stringr::str_detect(
                    NAME_ENGL, "United Kingdom"
                ), "UK"
            )
        )

    return(infant_mortality_df)
}
infant_mortality_df <- get_barplot_df()

infant_mortality_df <- infant_mortality_df[
    order(-infant_mortality_df$NumericValue),
]

# 7. BARPLOT
#-----------

b <- ggplot(
    data = infant_mortality_df,
    aes(x = reorder(NAME_ENGL, NumericValue),
        y = NumericValue,
        fill = cat)) +
        geom_bar(
            stat = "identity"
        ) +
        geom_text(
            data = subset(
                infant_mortality_df,
                NumericValue >= 4
            ),
            aes(label = round(NumericValue, 1)),
            position = position_stack(vjust = .5),
            hjust = .5,
            size = 2.75,
            color = "white",
            fontface = "bold"
        ) +
        geom_text(
            data = subset(
                infant_mortality_df,
                NumericValue < 4
            ),
            aes(label = round(NumericValue, 1)),
            position = position_stack(vjust = .5),
            hjust = .5,
            size = 2.75,
            color = "grey10",
            fontface = "bold"
        ) +
        scale_fill_manual(
            name = "",
            values = cols
        ) +
        labs(
            title ="",
            caption = "",
            x = "",
            y = ""
        ) +
        theme_void() +
        theme(
            axis.text.y = element_text(
                margin = unit(c(3, 0, 0, 0), "mm"),
                color = "grey10",
                size = 8,
                hjust = 1
            ),
            legend.position = "none"
        ) +
        coord_flip()

print(b)

# 8. COMBINE MAP AND BARPLOT
#---------------------------

create_inset_graph_map <- function(){
    vp <- grid::viewport(
        width = .5,
        height = .85,
        x = .25,
        y = .5
    )

    png("infant-mortality-rate-2021.png",
    height = 4000,
    width = 4000,
    res = 600
    )

    grid::grid.newpage()
    print(map)
    print(b, vp = vp)
    dev.off()

    return()
}

create_inset_graph_map()
