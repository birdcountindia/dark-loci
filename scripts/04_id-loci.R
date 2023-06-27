# northeast
dl1_list <- data.frame(STATE.NAME = c("Arunachal Pradesh", "Meghalaya", "Tripura",
                                      "Assam", 
                                      "Nagaland", "Manipur", "Mizoram"),
                       # when was it identified as a dark locus?
                       ID.DATE = "2022-03-01",
                       # when was roadshow or other action executed? (end date)
                       ACTION.DATE = c(rep("2022-06-07", 4), 
                                       rep("2023-01-31", 3)))

# magadha
dl2_list <- data.frame(DISTRICT.NAME = c(
  # perimeter
  "Siddharthnagar", "Basti", "Ambedkar Nagar", "Jaunpur", "Bhadohi", "Mirzapur", 
  "Sonbhadra", 
  "Jamui", "Lakhisarai", "Begusarai", "Samastipur", "Darbhanga", "Madhubani", 
  "Sitamarhi", "Sheohar", "Purba Champaran", "Pashchim Champaran", "Kushinagar", 
  "Maharajganj", 
  # within
  "Sant Kabir Nagar", "Gorakhpur", "Azamgarh", "Varanasi", "Chandauli", 
  "Kaimur (bhabua)", "Rohtas", "Ghazipur", "Mau", "Deoria", "Gopalganj", "Siwan", 
  "Ballia", "Buxar", "Aurangabad", "Gaya", "Arwal", "Jehanabad", "Bhojpur", "Patna", 
  "Saran", "Muzaffarpur", "Vaishali", "Nalanda", "Nawada", "Sheikhpura"
  ), 
  # ignoring Chatra, Garhwa, Giridih, Hazaribagh, Kodarma, Palamu, Singrauli this time
  ID.DATE = "2023-04-01",
  ACTION.DATE = NA)

#

darkloci1 <- dists_sf %>% 
  right_join(dl1_list) %>% 
  mutate(DL.NO = 1,
         DL.NAME = "Northeast")

darkloci2 <- dists_sf %>% 
  right_join(dl2_list) %>% 
  filter(!(STATE.NAME == "Maharashtra" & DISTRICT.NAME == "Aurangabad")) %>% 
  mutate(DL.NO = 2,
         DL.NAME = "Magadha")

darkloci <- bind_rows(darkloci1, darkloci2) %>% 
  st_drop_geometry() %>% 
  mutate(AREA = NULL)

darkloci_sf <- bind_rows(darkloci1, darkloci2) %>% 
  group_by(DL.NO, DL.NAME) %>% 
  summarise()

# # to check outline
# ggplot(dists_sf) + 
#   geom_sf() +
#   # dl boundaries
#   geom_sf(data = darkloci_sf, aes(geometry = DISTRICT.GEOM, col = DL.NAME), fill = NA, colour = "red")
