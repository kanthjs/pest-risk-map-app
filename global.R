# ------- loading library ----
library(shiny)
library(leaflet)
library(readxl)
library(dplyr)
library(tidyr)
library(sf)
library(curl)
library(ggplot2)
library(plotly)
library(purrr)
library(DT)
library(viridis)

# ---- CONFIG ----
excel_path <- "data/Pest_Risk_Template_77_Provinces_ByPest_update2029.xlsx"
geojson_path <- "data/provinces.geojson"

# ---- Enhanced Helper Functions ----
ensure_geojson <- function(path = geojson_path) {
    tryCatch(
        {
            if (!file.exists(path)) {
                dir.create(
                    dirname(path),
                    showWarnings = FALSE,
                    recursive = TRUE
                )
                url <- "https://raw.githubusercontent.com/chingchai/OpenGISData-Thailand/master/provinces.geojson"
                curl_download(url, path, mode = "wb")
                message("✓ Downloaded GeoJSON successfully.")
            }
            path
        },
        error = function(e) {
            stop("ไม่สามารถดาวน์โหลด GeoJSON: ", e$message)
        }
    )
}

th_month_levels <- c(
    "ม.ค.",
    "ก.พ.",
    "มี.ค.",
    "เม.ย.",
    "พ.ค.",
    "มิ.ย.",
    "ก.ค.",
    "ส.ค.",
    "ก.ย.",
    "ต.ค.",
    "พ.ย.",
    "ธ.ค."
)

norm_month <- function(x) {
    x <- trimws(as.character(x))
    x <- dplyr::recode(
        x,
        "มกราคม" = "ม.ค.",
        "กุมภาพันธ์" = "ก.พ.",
        "มีนาคม" = "มี.ค.",
        "เมษายน" = "เม.ย.",
        "พฤษภาคม" = "พ.ค.",
        "มิถุนายน" = "มิ.ย.",
        "กรกฎาคม" = "ก.ค.",
        "สิงหาคม" = "ส.ค.",
        "กันยายน" = "ก.ย.",
        "ตุลาคม" = "ต.ค.",
        "พฤศจิกายน" = "พ.ย.",
        "ธันวาคม" = "ธ.ค.",
        "Jan" = "ม.ค.",
        "January" = "ม.ค.",
        "Feb" = "ก.พ.",
        "February" = "ก.พ.",
        "Mar" = "มี.ค.",
        "March" = "มี.ค.",
        "Apr" = "เม.ย.",
        "April" = "เม.ย.",
        "May" = "พ.ค.",
        "Jun" = "มิ.ย.",
        "June" = "มิ.ย.",
        "Jul" = "ก.ค.",
        "July" = "ก.ค.",
        "Aug" = "ส.ค.",
        "August" = "ส.ค.",
        "Sep" = "ก.ย.",
        "Sept" = "ก.ย.",
        "September" = "ก.ย.",
        "Oct" = "ต.ค.",
        "October" = "ต.ค.",
        "Nov" = "พ.ย.",
        "November" = "พ.ย.",
        "Dec" = "ธ.ค.",
        "December" = "ธ.ค.",
        .default = x
    )
    factor(
        ifelse(x %in% th_month_levels, x, NA_character_),
        levels = th_month_levels,
        ordered = TRUE
    )
}

norm_prov <- function(x) {
    gsub("\\s+", "", trimws(as.character(x)))
}

# Function to validate risk values
validate_risk <- function(x) {
    x <- as.numeric(x)
    if (is.na(x)) {
        return(NA)
    }
    if (x < 0 | x > 4) {
        warning(paste("Risk value out of range [0-4]:", x))
        return(NA)
    }
    return(x)
}

# ---- Load Data with Error Handling ----
load_data <- function() {
    tryCatch(
        {
            # 1) Load GeoJSON
            ensure_geojson(geojson_path)
            thai_sf <- st_read(geojson_path, quiet = TRUE)

            if (!"pro_th" %in% names(thai_sf)) {
                stop("GeoJSON ไม่มีฟิลด์ 'pro_th' สำหรับชื่อจังหวัดภาษาไทย")
            }
            thai_sf <- thai_sf |> mutate(pro_th = norm_prov(pro_th))

            # 2) Load Excel
            if (!file.exists(excel_path)) {
                stop(paste(
                    "ไม่พบไฟล์ Excel:",
                    excel_path,
                    "- กรุณาตรวจสอบว่าไฟล์อยู่ในโฟลเดอร์ data"
                ))
            }

            sheets <- excel_sheets(excel_path)
            if (length(sheets) == 0) {
                stop("ไฟล์ Excel ไม่มี sheet ข้อมูล")
            }

            pest_long <- purrr::map_dfr(sheets, \(sh) {
                tryCatch(
                    {
                        df <- read_excel(excel_path, sheet = sh)

                        # Standardize column names to lower case for robustness
                        original_names <- names(df)
                        names(df) <- tolower(names(df))

                        # Check for province column, preferring 'province' but falling back to 'จังหวัด'
                        if ("province" %in% names(df)) {
                            # Column is already named 'province'
                        } else if ("จังหวัด" %in% names(df)) {
                            # Column is named 'จังหวัด', rename it to 'province'
                            names(df)[names(df) == "จังหวัด"] <- "province"
                        } else {
                            warning(paste(
                                "Sheet",
                                sh,
                                "could not find a province column (tried 'province' and 'จังหวัด'). Skipping sheet."
                            ))
                            return(NULL)
                        }

                        # Identify columns to ignore in the pivot, including province and region columns
                        ignore_cols <- c("province")
                        if ("region" %in% names(df)) {
                            ignore_cols <- c(ignore_cols, "region")
                        } else if ("ภูมิภาค" %in% names(df)) {
                            ignore_cols <- c(ignore_cols, "ภูมิภาค")
                        }

                        df |>
                            pivot_longer(
                                !all_of(ignore_cols),
                                names_to = "month",
                                values_to = "risk_index"
                            ) |>
                            mutate(
                                pest = sh,
                                month = norm_month(month),
                                province = norm_prov(province),
                                risk_index = sapply(risk_index, validate_risk)
                            ) |>
                            filter(!is.na(month)) # Exclude rows where month could not be parsed
                    },
                    error = function(e) {
                        warning(paste(
                            "Error reading sheet",
                            sh,
                            ":",
                            e$message
                        ))
                        return(NULL)
                    }
                )
            })

            if (nrow(pest_long) == 0) {
                stop("ไม่สามารถอ่านข้อมูลจาก Excel ได้ หรือข้อมูลในไฟล์ไม่ถูกต้อง")
            }

            # Return all necessary data
            list(
                thai_sf = thai_sf,
                pest_long = pest_long,
                pest_choices = sort(unique(pest_long$pest)),
                month_choices = th_month_levels,
                province_list = sort(unique(pest_long$province))
            )
        },
        error = function(e) {
            stop("เกิดข้อผิดพลาดในการโหลดข้อมูล: ", e$message)
        }
    )
}
