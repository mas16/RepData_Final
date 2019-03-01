library(R.utils)
library(data.table)
library(stringr)
library(dplyr)

# Assign data file name
zip_file <- "repdata_data_StormData.csv.bz2"
# Load the data
storm <- fread(zip_file)
# Columns to keep
col_keep <- c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
# Subset data
storm_df <- select(storm, col_keep)
# Normalize names to all uppercase
storm_df$EVTYPE <- toupper(storm_df$EVTYPE)

clean_evtypes <- function(datacol){
        datacol <- gsub("^ASTRO.*", "ALT", datacol)
        datacol <- gsub("^AVA.*", "AVALANCHE", datacol)
        datacol <- gsub("^BLI.*", "BLIZZARD", datacol)
        datacol <- gsub("^COA.*|CSTL(.*)FLOOD", "COASTAL FL00D", datacol)
        datacol <- gsub("^COLD.*", "C0LD/WINDCHILL", datacol)
        datacol <- gsub("DEBRIS.*", "DEBRIS FLOW", datacol)
        datacol <- gsub("FREEZING FOG.*", "FREEZING F0G", datacol)
        datacol <- str_replace(datacol, "(.*)FOG", "DENSE FOG" )
        datacol <- gsub("^DENSE FOG.*|WALL CLOUD", "DENSE FOG", datacol)
        datacol <- str_replace(datacol, "(.*)SMOKE", "DENSE SMOKE" )
        datacol <- gsub("^DENSE SMOKE.*", "DENSE SMOKE", datacol)
        datacol <- str_replace(datacol, "(.*)DROUGHT|BELOW NORMAL PRECIPITATION", "DROUGHT" )
        datacol <- gsub("^DROUGHT.*|DRIEST MONTH|DRY", "DROUGHT", datacol)
        datacol <- gsub("DUST DEV.*", "DUST DEVIL", datacol)
        datacol <- gsub("^DUST STORM.*|^DUSTSTORM.*|BLOWING DUST", "DUST STORM", datacol)
        datacol <- str_replace(datacol, "(.*)EXCESSIVE HEAT.*|WARM|(.*)EXCESSIVE HE4T.*
                               |(.*)HIGH TEMPERATURE", "EXCESSIVE HE4T")
        datacol <- gsub("EXCESSIVE HE4T.*", "EXCESSIVE HE4T", datacol)
        datacol <- str_replace(datacol, "(.*)COLD|(.*)COOL|(.*)LOW TEMP", "EXTREME COLD" )
        datacol <- gsub("^EXTREME COLD.*|LOW TEMPERATURE RECORD|RECORD LOW|
                        UNSEASONAL LOW TEMP", "EXTREME COLD", datacol)
        datacol <- str_replace(datacol, "(.*)FLASH FLOOD", "FLASH FL00D" )
        datacol <- gsub("^FLASH FL00D.*", "FLASH FL00D", datacol)
        datacol <- str_replace(datacol, "LAKE(.*) FLOOD", "LAKESHORE FL00D" )
        datacol <- gsub("^LAKESHORE FL00D.*", "LAKESHORE FL00D", datacol)
        datacol <- str_replace(datacol, "(.*)FLOOD", "FLOOD" )
        datacol <- gsub("^FLOOD.*", "FLOOD", datacol)
        datacol <- str_replace(datacol, "(.*)FROST|FREEZE", "FROST/FREEZE" )
        datacol <- gsub("^FROST/FREEZE.*", "FROST/FREEZE", datacol)
        datacol <- str_replace(datacol, "(.*)FUNNEL", "FUNNEL CLOUD" )
        datacol <- gsub("^FUNNEL CLOUD.*", "FUNNEL CLOUD", datacol)
        datacol <- gsub("^MARINE HAIL.*", "MARINE H4IL", datacol)
        datacol <- str_replace(datacol, "(.*)HAIL", "HAIL" )
        datacol <- gsub("^HAIL.*", "HAIL", datacol)
        datacol <- str_replace(datacol, "(.*)HEAT|HOT SPELL", "HEAT" )
        datacol <- gsub("^HEAT.*", "HEAT", datacol)
        datacol <- str_replace(datacol, "(.*)RAIN", "HEAVY RAIN" )
        datacol <- gsub("^HEAVY RAIN.*", "HEAVY RAIN", datacol)
        datacol <- gsub("^LAKE(.*)SNOW", "LAKE-EFFECT SN0W", datacol)
        datacol <- str_replace(datacol, "(.*)SNOW", "HEAVY SNOW" )
        datacol <- gsub("^HEAVY SNOW.*", "SNOW", datacol)
        datacol <- str_replace(datacol, "(.*)SURF", "HIGH SURF" )
        datacol <- gsub("^HIGH SURF.*", "HIGH SURF", datacol)
        datacol <- gsub("^MARINE HIGH WIND.*", "MARINE HIGH W1ND", datacol)
        datacol <- gsub("^MARINE STRONG WIND.*", "MARINE STRONG W1ND", datacol)
        datacol <- gsub("MARINE(.*)T(.*)WIND", "MARINE THUNDERST0RM W1ND", datacol)
        datacol <- str_replace(datacol, "(.*)HIGH(.*)WIND.*", "HIGH W1ND" )
        datacol <- gsub("^HIGH W1ND.*", "HIGH W1ND", datacol)
        datacol <- gsub("^TYP.*|HUR.*", "HURRICANE/TYPHOON", datacol)
        datacol <- gsub("ICE(.*)STORM", "ICE STORM", datacol)
        datacol <- str_replace(datacol, "(.*)LIGHTN", "LIGHTNING" )
        datacol <- gsub("^LIGHTNING.*", "LIGHTNING", datacol)
        datacol <- gsub("^RIP.*", "RIP CURRENT", datacol)
        datacol <- str_replace(datacol, "(.*)SLEET", "SLEET" )
        datacol <- gsub("^SLEET.*", "SLEET", datacol)
        datacol <- gsub("^STORM SURGE.*", "STORM SURGE/TIDE", datacol)
        datacol <- str_replace(datacol, "(.*)STRONG WIND|WIND", "STRONG WIND" )
        datacol <- gsub("^STRONG WIND.*", "STRONG WIND", datacol)
        datacol <- str_replace(datacol, "(.*)THUNDER(.*)WIN.*", "THUNDERST0RM WIND" )
        datacol <- gsub("^THUNDERST0RM WIND.*|^TS(.*)WIND", "THUNDERST0RM WIND", datacol)
        datacol <- str_replace(datacol, "(.*)THUNDERSTORM|(.*)TSTM", "THUNDERST0RM WIND" )
        datacol <- str_replace(datacol, "(.*)WATERSPOUT", "WATERSPOUT" )
        datacol <- gsub("^WATERSPOUT.*", "WATERSPOUT", datacol)
        datacol <- str_replace(datacol, "(.*)TORN", "TORNADO" )
        datacol <- gsub("^TORNADO.*|GUSTNADO.*", "TORNADO", datacol)
        datacol <- gsub("^TROPICAL STORM.*", "TROPICAL STORM", datacol)
        datacol <- gsub("^VOLCANIC.*", "VOLCANIC ASH", datacol)
        datacol <- gsub("WILD(.*)FIRE", "WILDFIRE", datacol)
        datacol <- str_replace(datacol, "(.*)WINTER(.*)STORM", "WINTER STORM" )
        datacol <- gsub("^WINTER STORM.*", "WINTER STORM", datacol)
        datacol <- str_replace(datacol, "(.*)WINTER(.*)WEATHER", "WINTER WEATHER" )
        datacol <- gsub("^WINTER WEATHER.*", "WINTER WEATHER", datacol)
        return(datacol)
        }

final_evtype <- function(datacol){
        datacol <- clean_evtypes(datacol)
        datacol <- str_replace(datacol, "ALT", "ATMOSPHERIC LOW TIDE")
        datacol <- str_replace(datacol, "(.*)DROUGHT", "DROUGHT")
        datacol <- str_replace(datacol, "COASTAL FL00D", "COASTAL FLOOD")
        datacol <- str_replace(datacol, "C0LD/WINDCHILL|WIND(.*)CHILL", "COLD/WINDCHILL")
        datacol <- str_replace(datacol, "FREEZING F0G", "FREEZING FOG")
        datacol <- str_replace(datacol, "(.*)EXCESSIVE HE4T|(.*)EXCESSIVE HEAT", "EXCESSIVE HEAT")
        datacol <- str_replace(datacol, "FLASH FL00D", "FLASH FLOOD")
        datacol <- str_replace(datacol, "LAKESHORE FL00D", "LAKESHORE FLOOD")
        datacol <- str_replace(datacol, "MARINE H4IL", "MARINE HAIL")
        datacol <- str_replace(datacol, "MARINE THUNDERST0RM W1ND", "MARINE THUNDERSTORM WIND")
        datacol <- str_replace(datacol, "HIGH W1ND", "HIGH WIND")
        datacol <- str_replace(datacol, "THUNDERST0RM WIND", "THUNDERSTORM WIND")
        datacol <- str_replace(datacol, "LAKE-EFFECT SN0W", "LAKE-EFFECT SNOW")
        datacol <- str_replace(datacol, "MARINE STRONG W1ND", "MARINE STRONG WIND")
        datacol <- str_replace(datacol, "MARINE HIGH W1ND", "MARINE HIGH WIND")
        return(datacol)
}

clean_names <- c("ATMOSPHERIC LOW TIDE", "AVALANCHE", "BLIZZARD", "COASTAL FLOOD", "COLD/WINDCHILL",
                 "DEBRIS FLOW", "FREEZING FOG", "DENSE FOG", "DENSE SMOKE",
                 "DROUGHT", "DUST DEVIL", "DUST STORM", "EXCESSIVE HEAT", "EXTREME COLD",
                 "FLASH FLOOD", "LAKESHORE FLOOD", "FLOOD", "FROST/FREEZE", "FUNNEL CLOUD",
                 "MARINE HAIL", "HAIL", "HEAT", "HEAVY RAIN", "LAKE-EFFECT SNOW", "SNOW",
                 "HIGH SURF", "MARINE HIGH WIND", "MARINE STRONG WIND", "MARINE THUNDERSTORM WIND",
                 "HIGH WIND", "HURRICANE/TYPHOON", "ICE STORM", "LIGHTNING", "RIP CURRENT",
                 "SLEET", "STORM SURGE/TIDE", "STRONG WIND", "THUNDERSTORM WIND", "WATERSPOUT",
                 "TORNADO", "TROPICAL STORM", "VOLCANIC ASH", "WILDFIRE", "WINTER STORM", "WINTER WEATHER",
                 "SEICHE", "TROPICAL DEPRESSION", "TSUNAMI")

storm_df$EVTYPE <- clean_evtypes(storm_df$EVTYPE)
storm_df$EVTYPE <- final_evtype(storm_df$EVTYPE)

storm_df <- filter(storm_df, EVTYPE %in% clean_names)

all_fatalities <- aggregate(FATALITIES ~ EVTYPE, storm_df, FUN=sum)
all_fatalities <- arrange(all_fatalities, desc(FATALITIES))

all_injuries <- aggregate(INJURIES ~ EVTYPE, storm_df, FUN=sum)
all_injuries <- arrange(all_injuries, desc(INJURIES))

clean_exp <- function(coldata){
        coldata <- gsub("[012345678]", "10", coldata)
        coldata <- gsub("[hH]", "100", coldata)
        coldata <- gsub("[kK]", "1000", coldata)
        coldata <- gsub("[mM]", "1000000", coldata)
        coldata <- gsub("[bB]", "1000000000", coldata)
        coldata <- gsub("\\?", "0", coldata)
        coldata <- gsub("\\+", "1", coldata)
        coldata <- gsub("-", "0", coldata)
        coldata[coldata==""] <- "0"
        return(as.numeric(coldata))
}

storm_df$PROPDMGEXP <- clean_exp(storm_df$PROPDMGEXP)
storm_df$CROPDMGEXP <- clean_exp(storm_df$CROPDMGEXP)
