# Tornado
unique_names <- gsub("^TORN.*|^WATERSPOUT.*|COLD AIR TORNADO", "TORNADO", unique_names)

#Avalanche
unique_names <- gsub("^AVA.*", "AVALANCHE", unique_names)

#Blizzard
unique_names <- gsub("^BLI.*", "BLIZZARD", unique_names)

#Coastal Flood
unique_names <- gsub("^COA.*", "COASTAL FLOOD", unique_names)

#Hurricane/Typhoon
unique_names <- gsub("^TYP.*|HUR.*", "HURRICANE", unique_names)

#Ice Storm
unique_names <- gsub("^ICE.*", "ICE STORM", unique_names)

#Astronomical Low Tide
unique_names <- gsub("^ASTRO.*", "ALT", unique_names)

# COLD/WINDCHILL
unique_names <- gsub("^COLD.*", "COLD/WINDCHILL", unique_names)

# Extreme Cold
unique_names <- gsub("^EXTREME COLD.*", "EXTREME COLD", unique_names)

# LIGHTENING
unique_names <- gsub("^LIGHT.*", "LIGHTNING", unique_names)

# LIGHTENING
unique_names <- gsub("^MARINE(.*)WIND$", "LIGHTNING", unique_names)