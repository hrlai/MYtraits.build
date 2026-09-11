library(ellmer)


# Upload journal article (a PDF) to Google
# This PDF is gitignored to avoid copyright issue
# Please download it from the DOI stated in the metadata
file <- google_upload(
  "data/Sterck_2001/raw/A_1017507723365.pdf"
)

# the prompt
prompt <- "Extract the species-specific stature (DBH and height) from Table 1 in the attached PDF file."

# define the output data.frame
type_output <- type_array(
  type_object(
    species = type_string(
      "Species name from the Species column. The species names are followed by authority name. Retrieve only the latin names, such as 'Baccaurea stipulata', not 'Baccaurea stipulata J. J. Sm.'."
    ),
    n = type_number("Sample size in the 'n' column."),
    dbh = type_string("Maximum DBH in cm."),
    height = type_string("Maximum height in m.")
  )
)

# initiate chat
chat <- chat_google_gemini()

# call chat
dat <-
  chat$chat_structured(
    file,
    prompt,
    type = type_output
  )

# save extracted data
write_csv(dat, "data/Sterck_2001/data.csv")
