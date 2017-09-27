
# how to aggregate regions : https://stackoverflow.com/questions/43174769/r-aggregate-county-map-polygons-to-create-custom-borders

# Packages ----------------------------------------------------------------

library('data.table')
library('DBI')


# Construction of the shape file ------------------------------------------

# construction of the correspondance table between departments and district
# connection to the server to get the districtdealers table
source('src/application/parameters.R')
con <- dbConnect(RMySQL::MySQL(),
                 host = connexionSQL$HOST,
                 port = connexionSQL$PORT,
                 user = connexionSQL$USER,
                 password = connexionSQL$PASSWORD,
                 dbname = connexionSQL$DB)
rm(connexionSQL)

district_dealers <- dbSendQuery(con, "select * from districtsDealers")
district_dealers <- dbFetch(district_dealers)
district_dealers <- as.data.table(district_dealers)

dbDisconnect(con)
rm(con)

# create a correspondance table between departement and district
district_dealers <- unique(district_dealers[, .(district_id, department = as.numeric(department))])

# correspondance_departement table is used because in the districtsDealers table the departments are identified by code number and in the shape file by names.
# import and cleaning of the correspondance table so the merge with the shape file is later done properly on cleaned department names
departement_correspondance <- fread('src/application/input/correspodance_departement.csv')
departement_correspondance$departement <- stringi::stri_replace_all_regex(str = departement_correspondance$departement, pattern = '[^a-zA-Z]', replacement = '')
departement_correspondance$departement <- tolower(departement_correspondance$departement)

# correspondance table between department (code and name) and district_id
district_dealers <- merge(x = district_dealers,
                              y = departement_correspondance,
                              by.x = 'department',
                              by.y = 'code',
                              all.y = TRUE)
colnames(district_dealers) <- c('code', 'district_id', 'department')
rm(departement_correspondance)

# some cleaning is needed because some departments belong to several districts and corse is split in the shape file
district_dealers <- setDT(district_dealers)[!(district_id != 1 & department %in% c('oise', 'paris')), ]
district_dealers$department[district_dealers$department == 'corse'] <- 'corsedusud'
district_dealers <- rbind(district_dealers, data.table(code = 20, district_id = 2, department = 'hautecorse'))

# the district_id information is added to the shape file
departments_sdf <- readRDS("src/application/input/FRA_adm2.rds")

# department names are cleaned in the shape file to be merged with the district_department table
departments_sdf@data$NAME_2 <- stringi::stri_replace_all_regex(str = departments_sdf@data$NAME_2, pattern = '(é|è)', replacement = 'e')
departments_sdf@data$NAME_2 <- stringi::stri_replace_all_fixed(str = departments_sdf@data$NAME_2, pattern = 'ô', replacement = 'o')
departments_sdf@data$NAME_2 <- stringi::stri_replace_all_regex(str = departments_sdf@data$NAME_2, pattern = '[^a-zA-Z]', replacement = '')
departments_sdf@data$NAME_2 <- tolower(departments_sdf@data$NAME_2)

# the merge is done without sorting to keep the initial order in the shape file
data <- departments_sdf@data
data <- merge(x = data,
              y = district_dealers,
              by.x = 'NAME_2',
              by.y = 'department',
              all.x = TRUE,
              sort = FALSE)
data$district_id[is.na(data$district_id)] <- 0
departments_sdf@data <- data
rm(data, district_dealers)

# Merge polygons according to district_id
departments_sdf <- rgeos::gUnaryUnion(spgeom = departments_sdf,
                                      id = departments_sdf@data$district_id)
departments_df <- fortify(departments_sdf, region = district_id)
# get centroid of the newly created regions to plot data labels on the map
centroid_district <- as.data.frame(sp::coordinates(departments_sdf))
names(centroid_district) <- c("long", "lat")
centroid_district$district_id <- rownames(centroid_district)
setDT(centroid_district)
rm(departments_sdf)

# plot the map
ggplot(data = departments_df,
       mapping = aes(x = long, y = lat, group = group)) +
  geom_path(colour = 'black') +
  geom_text(data = centroid_district,
       mapping = aes(x = long, y = lat, label = district_id, group = district_id))

saveRDS(departments_df, 'src/application/input/district_shape.rds')
saveRDS(centroid_district, 'src/application/input/centroid_district.rds')
rm(departments_df, centroid_district)
