# clear the workspace except for key variables
rm(list = setdiff(ls(), do_not_rm))

# check if progeny type was specified. stop with useful error message if not
if (!exists("y_variable")) {
  stop ("you must specify the object 'y_variable', and it must be one of: ", paste(c("total_juv", "adult", "parr", "smolt", "total_juv_grand", "adult_grand"), collapse = ", "))
}

# read in raw data and rename columns
dat = read.csv("00-data/LKG_Adults_and_Juvenile_assignments_with_GrandOffspring_Final.csv", stringsAsFactors = FALSE)
colnames(dat)[colnames(dat) == "total_juvenile_offspring"] = "total_juv"
colnames(dat)[colnames(dat) == "total_adult_offs"] = "adult"
colnames(dat)[colnames(dat) == "Total_Juv_Grand_Offs"] = "total_juv_grand"
colnames(dat)[colnames(dat) == "Total_Adult_Grand_Offs"] = "adult_grand"

# keep only adult spawners (i.e., discard jack spawners)
dat = dat[dat$life_stage == "adult",]

# convert NA's to zero for parr and smolt where ever total juvenile progeny is 0
# these must be zero if the total is zero
dat$parr[dat$total_juv == 0] = 0
dat$smolt[dat$total_juv == 0] = 0

# create the y_var column: used by models as response variable
# allows changing this in only one place and using the same code throughout
dat$y_var = dat[,y_variable]

# abbreviate origin
dat$origin = substr(dat$origin, 1, 1)

# keep only sexes F and M - discards NAs and Unks
dat = dat[dat$sex %in% c("F", "M"),]

# keep only origins N and H - discards NAs and Unks
dat = dat[dat$origin %in% c("N", "H"),]
dat$origin = ifelse(dat$origin == "N", "W", "H")

# keep only relevant columns
dat = dat[,c("sex", "year", "origin", "length", "date", "day", "parr", "smolt", "total_juv", "adult", "total_juv_grand", "adult_grand", "y_var")]

# convert day to be days past the earliest day
dat$day_raw = dat$day
dat$day = dat$day - min(dat$day)

# scale and center length
dat$length_raw = dat$length
dat$length = (dat$length - mean(dat$length))/sd(dat$length)

# create a zero variable
dat$zero = ifelse(dat$y_var == 0, 1, 0)

# create unique year and n_year objects
# ensure year is treated as a factor
dat$year = as.factor(dat$year)
years = as.numeric(levels(dat$year))
n_years = length(years)
