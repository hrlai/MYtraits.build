library(ellmer)


# Upload journal article (a PDF) to Google
# This PDF is not gitignored to avoid copyright issue
# Please download it from the DOI stated in the metadata
file <- google_upload(
  "data/Thomas_1999/raw/table_A1.png"
)

# the prompt
prompt <- "Extract the sapling light response curve parameters from Table A1 in the attached PNG image. Most (but not all) of the values are provided as mean ± 1 SE, you will extract them as a full string without breaking them into two numeric columns, e.g., fully as 407 ± 59. Return the plus-minus sign as '±'."

# define the output data.frame
type_output <- type_array(
  type_object(
    species = type_string(
      "Species name from the Species column."
    ),
    PPNUE = type_string(
      "Photosynthetic capacity expressed on a leaf nitrogen basis, in nmol/g/s"
    ),
    quantum_yield = type_string("Quantum yield in mol/mol"),
    curvature = type_string("Curvature (unitless)"),
    Rd = type_string("Dark respiration in nmol/g/s"),
    comp_Pt = type_string("Light compensation point from the Comp. Pt. column, in umol/m2/s"),
    sat_Pt = type_string("Saturating photosynthesis rate from the Sat. Pt. column in umol/m2/s")
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
write_csv(dat, "data/Thomas_1999/raw/data_llm_extract.csv")
