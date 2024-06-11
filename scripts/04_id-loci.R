# load("data/admin_units.RData")

# northeast
darkloci1 <- data.frame(STATE = c("Arunachal Pradesh", "Meghalaya", "Tripura",
                                  "Assam", 
                                  "Nagaland", "Manipur", "Mizoram"),
                        # when was it identified as a dark locus?
                        ID.DATE = "2022-03-01" %>% as_date(),
                        # when was roadshow or other action executed? (end date)
                        ACTION.DATE = c(rep("2022-06-07", 4), rep("2023-01-31", 3)) %>% 
                          as_date() %>% 
                          floor_date("months")) %>% 
  left_join(admin_unit_mapping, by = "STATE")

# magadha
darkloci2 <- tribble(
  
  ~ STATE, ~ COUNTY,

  # perimeter
  "Uttar Pradesh", "Siddharthnagar",
  "Uttar Pradesh", "Basti",
  "Uttar Pradesh", "Ambedkar Nagar",
  "Uttar Pradesh", "Jaunpur",
  "Uttar Pradesh", "Bhadohi",
  "Uttar Pradesh", "Mirzapur",
  "Uttar Pradesh", "Sonbhadra",
  "Bihar", "Jamui",
  "Bihar", "Lakhisarai",
  "Bihar", "Begusarai",
  "Bihar", "Samastipur",
  "Bihar", "Darbhanga",
  "Bihar", "Madhubani",
  "Bihar", "Sitamarhi",
  "Bihar", "Sheohar",
  "Bihar", "Purba Champaran",
  "Bihar", "Pashchim Champaran",
  "Uttar Pradesh", "Kushinagar",
  "Uttar Pradesh", "Maharajganj",
  
  # within
  "Uttar Pradesh", "Sant Kabir Nagar",
  "Uttar Pradesh", "Gorakhpur",
  "Uttar Pradesh", "Azamgarh",
  "Uttar Pradesh", "Varanasi",
  "Uttar Pradesh", "Chandauli",
  "Bihar", "Kaimur",
  "Bihar", "Rohtas",
  "Uttar Pradesh", "Ghazipur",
  "Uttar Pradesh", "Mau",
  "Uttar Pradesh", "Deoria",
  "Bihar", "Gopalganj",
  "Bihar", "Siwan",
  "Uttar Pradesh", "Ballia",
  "Bihar", "Buxar",
  "Bihar", "Aurangabad",
  "Bihar", "Gaya",
  "Bihar", "Arwal",
  "Bihar", "Jehanabad",
  "Bihar", "Bhojpur",
  "Bihar", "Patna",
  "Bihar", "Saran",
  "Bihar", "Muzaffarpur",
  "Bihar", "Vaishali",
  "Bihar", "Nalanda",
  "Bihar", "Nawada",
  "Bihar", "Sheikhpura"
  # ignoring Chatra, Garhwa, Giridih, Hazaribagh, Kodarma, Palamu, Singrauli this time
) %>% 
  mutate(ID.DATE = "2023-04-01" %>% as_date(),
         ACTION.DATE = "2023-09-01" %>% as_date() %>% floor_date("months")) %>% 
  left_join(admin_unit_mapping, by = c("STATE", "COUNTY"))

# Ghaggar
darkloci3 <- tribble(
  ~ STATE, ~ COUNTY,
  "Haryana", "Ambala",
  "Haryana", "Fatehabad",
  "Haryana", "Hisar",
  "Haryana", "Jind",
  "Haryana", "Kaithal",
  "Haryana", "Karnal",
  "Haryana", "Kurukshetra",
  "Haryana", "Panipat",
  "Haryana", "Sirsa",
  "Haryana", "Yamunanagar",
  "Punjab", "Barnala",
  "Punjab", "Bathinda",
  "Punjab", "Faridkot",
  "Punjab", "Fatehgarh Sahib",
  "Punjab", "Fazilka",
  "Punjab", "Mansa",
  "Punjab", "Moga",
  "Punjab", "Patiala",
  "Punjab", "Sangrur",
  "Punjab", "Sri Muktsar Sahib"
) %>% 
  mutate(ID.DATE = "2023-12-01" %>% as_date(),
         ACTION.DATE = NA) %>% 
  left_join(admin_unit_mapping, by = c("STATE", "COUNTY"))

#

darkloci1 <- dists_sf %>% 
  right_join(darkloci1, by = c("STATE.NAME", "DISTRICT.NAME")) %>% 
  mutate(DL.NO = 1,
         DL.NAME = "Northeast")

darkloci2 <- dists_sf %>% 
  right_join(darkloci2, by = c("STATE.NAME", "DISTRICT.NAME")) %>% 
  mutate(DL.NO = 2,
         DL.NAME = "Magadha")

darkloci3 <- dists_sf %>% 
  right_join(darkloci3, by = c("STATE.NAME", "DISTRICT.NAME")) %>% 
  mutate(DL.NO = 3,
         DL.NAME = "Ghaggar")


darkloci <- bind_rows(darkloci1, darkloci2, darkloci3) %>%
  st_drop_geometry() %>% 
  mutate(AREA = NULL)

darkloci_sf <- bind_rows(darkloci1, darkloci2, darkloci3) %>%
  group_by(DL.NO, DL.NAME) %>% 
  summarise()


# create HTML map of dark loci clusters
darkloci_HTML_map(darkloci)
# darkloci_HTML_map(darkloci %>%
#                     filter(DL.NO == 3))


# writing
save(darkloci1, darkloci2, 
     # darkloci3, 
     darkloci, darkloci_sf,
     file = get_stage_obj_path("data", "id"))

