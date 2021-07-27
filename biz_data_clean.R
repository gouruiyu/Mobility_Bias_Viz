bizs<-read.csv('data/all_businessess.csv')%>%
  st_as_sf(coords = c("lon_pc", "lat_pc"), crs = 4326)

# All-businesses company ids
bizs_ids <- bizs$CompanyID

######Sorting Business Data By Sector #####
stores<- bizs %>%
  filter(LicenceDescription %in% c("Retail Merchant - 0 to 2 Employees",
                                   "Retail Merchant - 10 to 19 Employees",
                                   "Retail Merchant - 20 or More Employees",
                                   "Retail Merchant - 3 to 5 Employees",
                                   "Retail Merchant - 6 to 9 Employees",
                                   "Wholesale",
                                   "Flea Market"))%>%
  filter(!(BusinessName==""))



food.and.restaurant<-bizs %>%
  filter(LicenceDescription %in% c("Restaurant-No Alcohol",
                                   "Food Primary-Class B Dining Lounge",
                                   "Food Primary-Class B Dining Room",
                                   "Farm Produce Sales",
                                   "Bakery",
                                   "Bread & Breakfast",
                                   "Caterer",
                                   "Concession Stand"))%>%
  filter(!(BusinessName==""))

alcohol<-bizs %>%
  filter(LicenceDescription %in% c("Liquor Licensee Retail Store",
                                   "Liquor Primary-Class C Cabaret",
                                   "Liquor Primary-Class D Neighbourhood Pub",
                                   "Liquor Primary-Class E Stadium",
                                   "Liquor Primary-Class F Marine Pub",
                                   "Liquor Primary- Class A Pub"))%>%
  filter(!(BusinessName==""))

health_medicine<-bizs %>%
  filter(LicenceDescription %in% c("Medical Laboratory",
                                   "Methadone Dispensary",
                                   "Nursery",
                                   "Professional Practitioner-Chiropractor",
                                   "Professional Practitioner-Dentist",
                                   "Professional Practitioner-Medical",
                                   "Professional Practitioner-Optometrist",
                                   "Professional Practitioner-Psychiatry",
                                   "Professional Practitioner-Veterinarian",
                                   "Counselling Service",
                                   "Dental Lab",
                                   "Denture Clinic",
                                   "Health Care Consultant",
                                   "Part Time Medical Practitioner",
                                   "Fitness Personal Trainer"))%>%
  filter(!(BusinessName==""))

finances<-bizs %>%
  filter(LicenceDescription %in% c("Financial Agent",
                                   "Financial Planning/Consultant",
                                   "Pawn Broker",
                                   "Income Tax Service/Buyer",
                                   "Investment Services",
                                   "Investment Consultant",
                                   "Consultant",
                                   "Sales/Marketing Office",
                                   "Bank",
                                   "Bankruptcy Trustee",
                                   "Project Management",
                                   "Planning Consultant",
                                   "Collection Agent",
                                   "Cheque Cashing Centre",
                                   "Customs Broker",
                                   "Currency Exchange",
                                   "Business Services Office"))%>%
  filter(!(BusinessName==""))

`%notin%` <- purrr::negate(`%in%`)
services<-bizs %>%
  filter(LicenceDescription %notin% c("Liquor Licensee Retail Store",
                                      "Liquor Primary-Class C Cabaret",
                                      "Liquor Primary-Class D Neighbourhood Pub",
                                      "Liquor Primary-Class E Stadium",
                                      "Liquor Primary-Class F Marine Pub",
                                      "Liquor Primary - Class A Pub",
                                      "Restaurant - No Alcohol",
                                      "Food Primary-Class B Dining Lounge",
                                      "Food Primary-Class B Dining Room",
                                      "Farm Produce Sales",
                                      "Bakery",
                                      "Bread & Breakfast",
                                      "Caterer",
                                      "Concession Stand",
                                      "Medical Laboratory",
                                      "Methadone Dispensary",
                                      "Nursery",
                                      "Professional Practitioner-Chiropractor",
                                      "Professional Practitioner-Dentist",
                                      "Professional Practitioner-Medical",
                                      "Professional Practitioner-Optometrist",
                                      "Professional Practitioner-Psychiatry",
                                      "Professional Practitioner-Veterinarian",
                                      "Counselling Service",
                                      "Dental Lab",
                                      "Denture Clinic",
                                      "Health Care Consultant",
                                      "Part Time Medical Practitioner",
                                      "Financial Agent",
                                      "Financial Planning/Consultant",
                                      "Pawn Broker",
                                      "Income Tax Service/Buyer",
                                      "Investment Services",
                                      "Investment Consultant",
                                      "Consultant",
                                      "Sales/Marketing Office",
                                      "Bank",
                                      "Bankruptcy Trustee",
                                      "Project Management",
                                      "Planning Consultant",
                                      "Collection Agent",
                                      "Cheque Cashing Centre",
                                      "Customs Broker",
                                      "Currency Exchange",
                                      "Business Services Office",
                                      "Retail Merchant - 0 to 2 Employees",
                                      "Retail Merchant - 10 to 19 Employees",
                                      "Retail Merchant - 20 or More Employees",
                                      "Retail Merchant - 3 to 5 Employees",
                                      "Retail Merchant - 6 to 9 Employees",
                                      "Wholesale",
                                      "Flea Market"))%>%
  filter(!(BusinessName==""))

restaurantIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/4a90e2/restaurant.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)
storeIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/4a90e2/buy--v1.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)
liquorIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/cotton/64/4a90e2/liquor-shelf--v1.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)

healthIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/4a90e2/stethoscope.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)

bizIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/4a90e2/sell-property.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)

serviceIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/4a90e2/service.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)