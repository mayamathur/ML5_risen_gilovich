library(data.table)
library(xlsx)


# Subjects who potentially participated twice ---------------------


exclude_table <- read.xlsx("data/Risen_Gilovich excluded data.xlsx", 
                           sheetIndex = 1, 
                           rowIndex   = c(4, 6:12)
)

# Store their IDs in a vector
exclude_twice <- as.character(exclude_table$ResponseId)



# Import ELTE raw data ----------------------------------------------------

elte_raw <- fread("data/raw_eotvos_lorand.csv")
elte_raw <- elte_raw[-c(1, 2)]

# Store column names in a vector
elte_colnames <- colnames(elte_raw)

# Read raw data again, skipping colnames and commment lines
elte_raw <- fread("data/raw_eotvos_lorand.csv", header = FALSE, skip = 3)

# Add colnames
colnames(elte_raw) <- elte_colnames


# Excluding participants --------------------------------------------------


# Exclude any participants who report an ending number >= 561 
# (since this would indicate either having failed to do the 
# counting task or having counted forward instead of backward) 

exclude_counting <- elte_raw[load == 1 & Q28   >= 561,  ResponseId]

# and/or indicate having put all their effort into reading the story.

exclude_effort   <- elte_raw[load == 1 & Q29_1 == 0, ResponseId] 

# Exclude subjects who potentially participated twice or 
# had ending number >= 561 
# or put all their effort into reading (Effort splitting variable: Q29_1 == 0)

elte_clean <- elte_raw[!(ResponseId %in% c(exclude_counting, exclude_twice, exclude_effort))]


# Compute variables -------------------------------------------------------

# Compute lkl score

elte_clean$lkl <- rowSums(elte_clean[ , 
                                      .(L1.R1.scenario_1, L1.R0.scenario_1, L0.R1.text_1, L0.R0.text_1)
                                      ], 
                          na.rm = TRUE
)

# Compute tempt

elte_clean[ , tempt := abs(had.read-1)]


# Descriptives ------------------------------------------------------------

elte_clean[ ,
            lapply(.SD, function(x) { list( round(mean(x), 2), round(sd(x), 2) ) }),
            .SDcols = c("load", "tempt", "lkl")
            ]

