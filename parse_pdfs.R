library(tidyverse)
library(pdftools)
library(readxl)

extract_nums <- function(line, skip_chars = 40) {
  if (nchar(line) <= skip_chars) return(numeric(0))
  data_part <- substr(line, skip_chars + 1, nchar(line))
  clean <- gsub(",", "", data_part)
  as.numeric(unlist(regmatches(clean, gregexpr("-?\\d+\\.?\\d*", clean, perl = TRUE))))
}

nums_to_vals <- function(nums) {
  if (length(nums) >= 11) {
    list(value = nums[1],
         actual_m1 = nums[2],  needed_m1 = nums[3],
         actual_m3 = nums[4],  needed_m3 = nums[5],
         actual_m6 = nums[6],  needed_m6 = nums[7],
         actual_m9 = nums[8],  needed_m9 = nums[9],
         actual_m12 = nums[10], needed_m12 = nums[11])
  } else if (length(nums) >= 6) {
    list(value = nums[1],
         actual_m1 = nums[2],  needed_m1 = NA_real_,
         actual_m3 = nums[3],  needed_m3 = NA_real_,
         actual_m6 = nums[4],  needed_m6 = NA_real_,
         actual_m9 = nums[5],  needed_m9 = NA_real_,
         actual_m12 = nums[6], needed_m12 = NA_real_)
  } else {
    NULL
  }
}

find_row_pdf <- function(pages_txt, row_pat, sec_pat = NULL, skip = 40) {
  txt   <- paste(pages_txt, collapse = "\n")
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]

  if (!is.null(sec_pat)) {
    i <- which(grepl(sec_pat, lines, ignore.case = TRUE))[1]
    if (is.na(i)) return(NULL)
    lines <- tail(lines, length(lines) - i)
  }

  i <- which(grepl(row_pat, lines, perl = TRUE))[1]
  if (is.na(i)) return(NULL)
  nums_to_vals(extract_nums(lines[i], skip))
}

find_tbl_pages <- function(pages, tbl_num) {
  pat <- paste0("(Statistical table |Table )A-", tbl_num, "\\.")
  which(sapply(pages, function(p) any(grepl(pat, strsplit(p, "\n", fixed = TRUE)[[1]]))))
}

race_pages_idx <- function(pages, idx, race_pat) {
  idx[sapply(idx, function(i) {
    any(grepl(race_pat, strsplit(pages[i], "\n", fixed = TRUE)[[1]], ignore.case = TRUE))
  })]
}


parse_pdf <- function(pdf_path) {
  pages <- pdf_text(pdf_path)

  a1  <- pages[find_tbl_pages(pages, 1)]
  a2i <- find_tbl_pages(pages, 2)
  a3  <- pages[find_tbl_pages(pages, 3)]
  a4  <- pages[find_tbl_pages(pages, 4)]
  a8  <- pages[find_tbl_pages(pages, 8)]
  # A-10 rate page(s) specifically (title contains "rate indicators")
  a10 <- pages[which(sapply(pages, function(p) {
    any(grepl("Selected unemployment rate indicators", strsplit(p, "\n", fixed = TRUE)[[1]], ignore.case = TRUE))
  }))]
  a11 <- pages[find_tbl_pages(pages, 11)]
  a12 <- pages[find_tbl_pages(pages, 12)]
  a16 <- pages[find_tbl_pages(pages, 16)]

  a2w <- pages[race_pages_idx(pages, a2i, "^\\s*White\\s*$")]
  a2b <- pages[race_pages_idx(pages, a2i, "BLACK OR AFRICAN AMERICAN")]
  a2a <- pages[race_pages_idx(pages, a2i, "^\\s*ASIAN\\s*$")]

  g <- function(pg, rp, sp = NULL) find_row_pdf(pg, rp, sp)

  list(
    pop          = g(a1,  "Civilian noninstitutional population", "TOTAL"),
    lf           = g(a1,  "Civilian labor force",                "TOTAL"),
    lfpr         = g(a1,  "Participation rate",                  "TOTAL"),
    empTotal     = g(a1,  "Employed",                            "TOTAL"),
    emp          = g(a1,  "Employment-population ratio",         "TOTAL"),
    unempTotal   = g(a1,  "Unemployed",                          "TOTAL"),
    ur           = g(a1,  "Unemployment rate",                   "TOTAL"),
    nilf         = g(a1,  "Not in labor force",                  "TOTAL"),
    ur_men20     = g(a1,  "Unemployment rate",    "Men, 20 years"),
    ur_women20   = g(a1,  "Unemployment rate",    "Women, 20 years"),
    ur_total16   = g(a10, "Total, 16 years and over"),
    ur_teens     = g(a10, "16 to 19 years"),
    ur_total25   = g(a10, "25 years and over"),
    ur_white     = g(a2w, "Unemployment rate", "TOTAL"),
    ur_black     = g(a2b, "Unemployment rate", "TOTAL"),
    ur_asian     = g(a2a, "Unemployment rate", "TOTAL"),
    ur_hispanic  = g(a3,  "Unemployment rate", "TOTAL"),
    ur_lessThanHS = g(a4, "Unemployment rate", "Less than a high school diploma"),
    ur_HS        = g(a4,  "Unemployment rate", "High school graduates"),
    ur_someCol   = g(a4,  "Unemployment rate", "Some college or associate"),
    ur_col       = g(a4,  "Unemployment rate", "Bachelor"),
    pt_er        = g(a8,  "Part time for economic reasons",    "AT WORK PART TIME"),
    pt_slackWork = g(a8,  "Slack work or business conditions", "AT WORK PART TIME"),
    pt_onlyFind  = g(a8,  "Could only find part-time work",    "AT WORK PART TIME"),
    pt_ner       = g(a8,  "Part time for noneconomic reasons", "AT WORK PART TIME"),
    rfu_jobs        = g(a11, "Job losers and p",  "NUMBER OF UNEMPLOYED"),
    rfu_jobLeavers  = g(a11, "Job leavers",       "NUMBER OF UNEMPLOYED"),
    rfu_reentrants  = g(a11, "Reentrants",        "NUMBER OF UNEMPLOYED"),
    rfu_newEntrants = g(a11, "New entrants",      "NUMBER OF UNEMPLOYED"),
    dou_5weeks    = g(a12, "Less than 5 weeks",  "NUMBER OF UNEMPLOYED"),
    dou_514weeks  = g(a12, "5 to 14 weeks",      "NUMBER OF UNEMPLOYED"),
    dou_1526weeks = g(a12, "15 to 26 weeks",     "NUMBER OF UNEMPLOYED"),
    dou_27weeks   = g(a12, "27 weeks and over",  "NUMBER OF UNEMPLOYED"),
    nilf_ma = g(a16, "Marginally attached to the labor force", "NOT IN THE LABOR FORCE"),
    nilf_dw = g(a16, "Discouraged workers",                    "NOT IN THE LABOR FORCE")
  )
}


clean_val_xl <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || x %in% c("-", "–", "—", "", "NA")) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

get_col_xl <- function(r, idx) {
  if (idx > ncol(r)) return(NA_real_)
  clean_val_xl(r[[idx]])
}

find_row_xl <- function(d, row_pat, sec_pat = NULL) {
  labels <- as.character(d[[1]])
  start  <- 1L

  if (!is.null(sec_pat)) {
    i <- which(grepl(sec_pat, labels, ignore.case = TRUE))[1]
    if (is.na(i)) return(NULL)
    start <- i + 1L
  }

  sub_labels <- labels[start:nrow(d)]
  j <- which(grepl(row_pat, sub_labels, perl = TRUE))[1]
  if (is.na(j)) return(NULL)

  r <- d[start + j - 1L, ]
  list(
    value      = get_col_xl(r, 2),
    actual_m1  = get_col_xl(r, 3),  needed_m1  = get_col_xl(r, 4),
    actual_m3  = get_col_xl(r, 6),  needed_m3  = get_col_xl(r, 7),
    actual_m6  = get_col_xl(r, 9),  needed_m6  = get_col_xl(r, 10),
    actual_m9  = get_col_xl(r, 12), needed_m9  = get_col_xl(r, 13),
    actual_m12 = get_col_xl(r, 15), needed_m12 = get_col_xl(r, 16)
  )
}

parse_excel <- function(xlsx_path, year, month) {
  rd <- function(sh) suppressMessages(
    read_excel(xlsx_path, sheet = sh, col_names = FALSE, .name_repair = "minimal")
  )

  a1  <- rd("ST A1");  a2  <- rd("ST A2");  a3  <- rd("ST A3")
  a4  <- rd("ST A4");  a8  <- rd("ST A8");  a10 <- rd("ST A10")
  a11 <- rd("ST A11"); a12 <- rd("ST A12"); a16 <- rd("ST A16")

  g <- function(d, rp, sp = NULL) find_row_xl(d, rp, sp)

  res <- list(
    pop         = g(a1,  "Civilian noninstitutional population", "TOTAL"),
    lf          = g(a1,  "Civilian labor force",                "TOTAL"),
    lfpr        = g(a1,  "Participation rate",                  "TOTAL"),
    empTotal    = g(a1,  "^Employed$",                          "TOTAL"),
    emp         = g(a1,  "Employment-population ratio",         "TOTAL"),
    unempTotal  = g(a1,  "^Unemployed$",                        "TOTAL"),
    ur          = g(a1,  "Unemployment rate",                   "TOTAL"),
    nilf        = g(a1,  "Not in labor force",                  "TOTAL"),
    ur_men20    = g(a1,  "Unemployment rate",  "Men, 20 years"),
    ur_women20  = g(a1,  "Unemployment rate",  "Women, 20 years"),
    ur_total16  = g(a10, "Total, 16 years and over", "Unemployment rates"),
    ur_teens    = g(a10, "16 to 19 years",           "Unemployment rates"),
    ur_total25  = g(a10, "25 years and over",        "Unemployment rates"),
    ur_white    = g(a2,  "Unemployment rate", "WHITE"),
    ur_black    = g(a2,  "Unemployment rate", "BLACK OR AFRICAN AMERICAN"),
    ur_asian    = g(a2,  "Unemployment rate", "ASIAN"),
    ur_hispanic = g(a3,  "Unemployment rate", "HISPANIC OR LATINO"),
    ur_lessThanHS = g(a4, "Unemployment rate", "Less than a high school diploma"),
    ur_HS         = g(a4, "Unemployment rate", "High school graduates"),
    ur_someCol    = g(a4, "Unemployment rate", "Some college or associate"),
    ur_col        = g(a4, "Unemployment rate", "Bachelor"),
    pt_er        = g(a8,  "Part time for economic reasons",    "AT WORK PART TIME"),
    pt_slackWork = g(a8,  "Slack work or business conditions", "AT WORK PART TIME"),
    pt_onlyFind  = g(a8,  "Could only find part-time work",    "AT WORK PART TIME"),
    pt_ner       = g(a8,  "Part time for noneconomic reasons", "AT WORK PART TIME"),
    rfu_jobs        = g(a11, "Job losers and p",  "NUMBER OF UNEMPLOYED"),
    rfu_jobLeavers  = g(a11, "Job leavers",       "NUMBER OF UNEMPLOYED"),
    rfu_reentrants  = g(a11, "Reentrants",        "NUMBER OF UNEMPLOYED"),
    rfu_newEntrants = g(a11, "New entrants",      "NUMBER OF UNEMPLOYED"),
    dou_5weeks    = g(a12, "Less than 5 weeks",  "NUMBER OF UNEMPLOYED"),
    dou_514weeks  = g(a12, "5 to 14 weeks",      "NUMBER OF UNEMPLOYED"),
    dou_1526weeks = g(a12, "15 to 26 weeks",     "NUMBER OF UNEMPLOYED"),
    dou_27weeks   = g(a12, "27 weeks and over",  "NUMBER OF UNEMPLOYED"),
    nilf_ma = g(a16, "Marginally attached to the labor force", "NOT IN THE LABOR FORCE"),
    nilf_dw = g(a16, "Discouraged workers",                    "NOT IN THE LABOR FORCE")
  )

  results_to_tibble(res, year, month)
}

results_to_tibble <- function(res, year, month) {
  imap_dfr(res, function(vals, var_name) {
    if (is.null(vals)) {
      message("  MISSING: ", var_name)
      tibble(year = year, month = month, var = var_name,
             value = NA_real_,
             actual_m1 = NA_real_, needed_m1 = NA_real_,
             actual_m3 = NA_real_, needed_m3 = NA_real_,
             actual_m6 = NA_real_, needed_m6 = NA_real_,
             actual_m9 = NA_real_, needed_m9 = NA_real_,
             actual_m12 = NA_real_, needed_m12 = NA_real_)
    } else {
      tibble(year = year, month = month, var = var_name,
             value      = vals$value,
             actual_m1  = vals$actual_m1,  needed_m1  = vals$needed_m1,
             actual_m3  = vals$actual_m3,  needed_m3  = vals$needed_m3,
             actual_m6  = vals$actual_m6,  needed_m6  = vals$needed_m6,
             actual_m9  = vals$actual_m9,  needed_m9  = vals$needed_m9,
             actual_m12 = vals$actual_m12, needed_m12 = vals$needed_m12)
    }
  })
}


pdf_files <- sort(list.files("inputs", pattern = "Combined_A1_A16.*\\.pdf$", full.names = TRUE))
xlsx_file <- "inputs/Combined_A1_A16_r1_D290_2026M04.xlsx"

all_data <- list()

for (f in pdf_files) {
  m    <- regmatches(f, regexpr("\\d{4}M\\d{2}", f))
  year <- as.integer(substr(m, 1, 4))
  mon  <- as.integer(substr(m, 6, 7))

  message("Processing: ", basename(f), " [", year, "/", sprintf("%02d", mon), "]")

  tbl <- tryCatch({
    res <- parse_pdf(f)
    results_to_tibble(res, year, mon)
  }, error = function(e) {
    message("  ERROR: ", e$message)
    NULL
  })

  if (!is.null(tbl)) all_data[[length(all_data) + 1]] <- tbl
}

message("Processing Excel: ", basename(xlsx_file))
tbl_xl <- tryCatch(
  parse_excel(xlsx_file, 2026, 4),
  error = function(e) { message("  ERROR: ", e$message); NULL }
)
if (!is.null(tbl_xl)) all_data[[length(all_data) + 1]] <- tbl_xl

new_data <- bind_rows(all_data)

existing <- read_csv(
  "inputs/processed_pdfs_Jan21_Jan26.csv",
  col_types = cols(.default = "d", var = "c"),
  show_col_types = FALSE
)

combined <- bind_rows(new_data, existing) %>%
  arrange(year, month, var)

write_csv(combined, "inputs/processed_pdfs.csv")
message("Done! Wrote ", nrow(combined), " rows to inputs/processed_pdfs.csv")
