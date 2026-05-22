# Inspect the "fertility by education" file
fe <- read.csv("data/raw/fertility by education /fertility by education .csv")
str(fe)
head(fe, 10)

# Inspect the "educational attainment" file  
ea <- read.csv("data/raw/educational attainment /educational attainment .csv")
str(ea)
head(ea, 10)

# And the wcde_data file
wd <- read.csv("data/raw/wcde_data/wcde_data.csv")
str(wd)
head(wd, 10)

# --- demo_faeduc (fertility by education) ---

cat("AGE categories:\n"); print(unique(fe$age))
cat("\nISCED categories:\n"); print(unique(fe$isced11))
cat("\nNumber of countries:", length(unique(fe$geo)), "\n")
cat("Year range:", range(fe$TIME_PERIOD), "\n")
cat("Coverage countries × years:\n")
print(table(fe$geo)[1:15])  # first 15 countries by row count

# --- edat_lfse_03 (population by education) ---

cat("AGE categories:\n"); print(unique(ea$age))
cat("\nSEX categories:\n"); print(unique(ea$sex))
cat("\nISCED categories:\n"); print(unique(ea$isced11))
cat("\nNumber of countries:", length(unique(ea$geo)), "\n")
cat("Year range:", range(ea$TIME_PERIOD), "\n")


# Countries appearing in BOTH files
fe_countries <- unique(fe$geo)
ea_countries <- unique(ea$geo)
common <- intersect(fe_countries, ea_countries)

cat("Countries in fe but not ea:\n")
print(setdiff(fe_countries, ea_countries))

cat("\nCountries in ea but not fe:\n")
print(setdiff(ea_countries, fe_countries))

cat("\nCommon countries (", length(common), "):\n")
print(common)
