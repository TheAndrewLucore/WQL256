#' Extract MDEQ data
#'
#' @param mdeq_file A .xlsx file
#' @param data_file The title of the output file
#' @param first_time Logical. Is this the first time this function has run? Defaults to FALSE.
#'
#' @return A cleaned .csv sheet
#'
#' @importFrom magrittr %>%
#'
#' @importFrom dplyr mutate mutate_at filter select rename recode
#'
#' @export

mdeq_extract <- function(mdeq_file, data_file, first_time = FALSE) {


  dat <- readxl::read_xlsx(
    path = mdeq_file,
    col_types = "guess"
  ) %>%
    # Selects columns, can break up into mutiple select()
    select(1:2, 4:5, 12:18) %>%
    # Rename columns
    rename(
      "project" = 1,
      "site" = 2,
      "type" = 3,
      "date_time" = 4,
      "tkn" = 5,
      "tss" = 6,
      "op" = 7,
      "amm" = 8,
      "tn" = 9,
      "nn" = 10,
      "tp" = 11
    ) %>%
    # Change factors
    mutate_at(
      .funs = as.factor,
      .vars = vars(1:3)
    ) %>%
    # Sets it to the correct time zone
    lubridate::with_tz(
      "America/Chicago"
    ) %>%
    # Split date_time but keep it too
    mutate(
      year = as.factor(lubridate::year(date_time)),
      month = as.factor(lubridate::month(date_time))
    ) %>%
    # replace MQLs
    mutate(
      tkn = replace(tkn,
                    tkn == "< MQL ( 0.1 )",
                    0.05),
      tss = replace(tss,
                    tss == "< MQL ( 4 )",
                    2),
      op = replace(op,
                   op == "< MQL ( 0.05 )",
                   0.025),
      amm = replace(amm,
                    amm == "< MQL ( 0.04 )",
                    0.02),
      tn = replace(tn,
                   tn == "< MQL ( 0.1 )",
                   0.05),
      nn = replace(nn,
                   nn == "< MQL ( 0.02 )",
                   0.01),
      tp = replace(tp,
                   tp == "< MQL ( 0.02 )",
                   0.01)
    ) %>%
    # Change numbers
    mutate_at(
      .funs = as.numeric,
      .vars = vars(5:11)
    ) %>%
    # Pull out blanks and dupes
    filter(
      !grepl(
        pattern = "(QA/QC)",
        x = type
      )
    )  %>%
    # Take off REACH00
    mutate(
      site = stringr::str_remove(string = site,
                        pattern = "REACH00")
    ) %>%
    # Put right site numbers
    mutate(
      site = recode(site,
                    "17" = "1",
                    "19" = "2",
                    "18" = "3",
                    "20" = "4",
                    "21" = "5",
                    "22" = "6"),
      site = as.factor(site)
    ) %>%
    # Upload date
    mutate(
      upload_date = Sys.Date()
    ) %>%
    # Reorder colums how I want
    select(
      14, 1:4, 12, 13, 5:11
    ) %>%
    # Save as CSV
    readr::write_csv(
      path = data_file,
      append = TRUE,
      col_names = first_time, # This needs to be TRUE for the first upload but FALSE for all the subsequent ones
    )
}

