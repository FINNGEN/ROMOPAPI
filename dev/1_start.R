########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################


usethis::create_package(".", open = FALSE)

## Fill the DESCRIPTION ----
usethis::use_description(
  fields = list(
    Title = "ROMOPAPI",
    Description = "API to access OMOP CDM database",
    `Authors@R` = 'person("Javier", "Gracia-Tabuenca", email = "javier.graciatabuenca@tuni.fi",
                          role = c("aut", "cre"),
                          comment = c(ORCID = "YOUR-ORCID-ID"))',
    License = "MIT",
    Language =  "en"
  )
)


usethis::use_readme_md()

usethis::use_git()

usethis::use_github(organisation = "FINNGEN", private = FALSE)

renv::init()
