all <- survdat::get_species_stock_area(channel = channel)
all_species <- all$data$COMMON_NAME
unique(all_species)

# SILVER HAKE (need split into N/S)
silverhake <- survdat::get_species_stock_area(channel = channel, species = "HAKE, SILVER (WHITING)")
silverhake_strata <- silverhake$data$STRATUM
unique(silverhake_strata)
# "01200" "01210" "01220" "01230" "01240" "01250" "01260" "01270" "01280" "01290" "01300" "01360" "01370" "01380"
# "01390" "01400" "01010" "01020" "01030" "01040" "01050" "01060" "01070" "01080" "01090" "01100" "01110" "01120"
# "01130" "01140" "01150" "01160" "01170" "01180" "01190" "01610" "01620" "01630" "01640" "01650" "01660" "01670"
# "01680" "01690" "01700" "01710" "01720" "01730" "01740" "01750" "01760"

# MACKEREL
mackerel <- survdat::get_species_stock_area(channel = channel, species = "MACKEREL, ATLANTIC")
mackerel_strata <- mackerel$data$STRATUM
unique(mackerel_strata)
# "01010" "01020" "01030" "01040" "01050" "01060" "01070" "01080" "01090" "01100" "01110" "01120" "01130" "01140"
# "01150" "01160" "01170" "01180" "01190" "01200" "01210" "01220" "01230" "01240" "01250" "01260" "01270" "01280"
# "01290" "01300" "01310" "01320" "01330" "01340" "01350" "01351" "01360" "01370" "01380" "01390" "01400" "01410"
# "01420" "01490" "01650" "01700" "01710" "01730" "01750" "03050" "03080" "03200" "03260" "03450" "03560" "03590"
# "03600" "03610" "03640" "03650" "03660" "01610" "01620" "01630" "01640" "01660" "01670" "01680" "01690" "01720"
# "01740" "01760" "03020" "03110" "03140" "03170" "03230" "03290" "03320" "03350" "03380" "03410" "03440" "03460"

# POLLOCK
pollock <- survdat::get_species_stock_area(channel = channel, species = "POLLOCK, ATLANTIC")
pollock_strata <- pollock$data$STRATUM
unique(pollock_strata)
# "01130" "01140" "01150" "01160" "01170" "01180" "01190" "01200" "01210" "01220" "01230" "01240" "01250" "01260"
# "01270" "01280" "01290" "01300" "01360" "01370" "01380" "01390" "01400"
 
# RED HAKE (need split into N/S)
redhake <- survdat::get_species_stock_area(channel = channel, species = "HAKE, ATLANTIC, RED")
redhake_strata <- redhake$data$STRATUM
unique(redhake_strata)
# "01200" "01210" "01220" "01230" "01240" "01250" "01260" "01270" "01280" "01290" "01300" "01360" "01370" "01380"
# "01390" "01400" "01010" "01020" "01030" "01040" "01050" "01060" "01070" "01080" "01090" "01100" "01110" "01120"
# "01130" "01140" "01150" "01160" "01170" "01180" "01190" "01610" "01620" "01630" "01640" "01650" "01660" "01670"
# "01680" "01690" "01700" "01710" "01720" "01730" "01740" "01750" "01760"

# WITCH FLOUNDER
witchflounder <- survdat::get_species_stock_area(channel = channel, species = "FLOUNDER, WITCH (GRAY SOLE)")
witchflounder_strata <- witchflounder$data$STRATUM
unique(witchflounder_strata)
# "04010" "04030" "04060" "04080" "09250" "09260" "09270" "09280" "09290" "09300" "09310" "09320" "09330" "09340"
# "09350" "09360" "01220" "01230" "01240" "01250" "01260" "01270" "01280" "01290" "01300" "01360" "01370" "01380"
# "01390" "01400"

# HALIBUT
halibut <- survdat::get_species_stock_area(channel = channel, species = "HALIBUT, ATLANTIC")
halibut_strata <- halibut$data$STRATUM
unique(halibut_strata)
# "01130" "01140" "01150" "01160" "01170" "01180" "01190" "01200" "01210" "01220" "01230" "01240" "01250" "01260"
# "01270" "01280" "01290" "01300" "01360" "01370" "01380" "01390" "01400"

#########################################
