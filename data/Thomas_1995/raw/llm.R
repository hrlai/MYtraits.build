library(ellmer)


# Upload journal article (a PDF) to Google
# This PDF is gitignored to avoid copyright issue
# Please download it from the DOI stated in the metadata
file <- google_upload(
  "data/Thomas_1995/raw/Thomas-OntogeneticChangesLeaf-1995.pdf"
)

# the prompt
prompt <- "Extract the leaf area of sapling and adult from Table 1 in the attached PDF file. The values are provided as mean ± 1 SD, you will extract them as a full string without breaking them into two numeric columns, e.g., fully as 80.4 ± 23.9. Ignore the last two columns Direction and Voucher."

# define the output data.frame
type_output <- type_array(
  type_object(
    species = type_string(
      "Species name from the Species column. The plant species names are listed under family names in ALL CAPS, do not extract family names. The species names starts with genus names, then followed by species epithets which are tab-indent. Retrieve full latin names, such as Polyalthia glauca and Polyalthia laterifolia. Some species are denoted with an 's' superscript, do not extract this superscript."
    ),
    area_sapling = type_string("Sapling mean leaf area in cm2"),
    area_adult = type_string("Adult mean leaf area in cm2")
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
write_csv(dat, "data/Thomas_1995/raw/data_llm_extract.csv")
