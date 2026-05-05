################################################################################
# Predoctoral Research Assistant Assignment
# Author: Sunny Rawat
# Date: 2026-05-04
################################################################################


#------------------------------------------------------------------------#
#                       Setup Environment ----    
#------------------------------------------------------------------------#

# Load required libraries
library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(fixest)
library(here)
library(janitor)
library(haven)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(geodata)
library(terra)

here::i_am("Code/code.R")

# Create folders if they do not exist
tables <- here("Output", "Tables")
plots <- here("Output", "Plots")
processed <- here("Data", "processed")
raw <- here("Data", "raw")

dir.create(tables, recursive = TRUE, showWarnings = FALSE)
dir.create(plots, recursive = TRUE, showWarnings = FALSE)
dir.create(processed, recursive = TRUE, showWarnings = FALSE)
dir.create(raw, recursive = TRUE, showWarnings = FALSE)

stopifnot(dir.exists(tables))
stopifnot(dir.exists(plots))
stopifnot(dir.exists(processed))
stopifnot(dir.exists(raw))


# ----------------------------
# Source audit file
# ----------------------------
# This will become sources.csv for the assignment.

sources <- tribble(
  ~source_name, ~link, ~type, ~raw_download_available, ~variables_used, ~credibility, ~limitation, ~next_step,

  "SEZ India - List of States/UTs-wise Operational SEZs",
  "https://sezindia.gov.in/operational-sez",
  "Official government PDF",
  "Yes, as PDF",
  "SEZ names, developer names, administrative locations, sectors, operational status",
  "Official Government of India SEZ source",
  "Provides administrative locations but not standardized coordinates or official GIS boundaries",
  "Use as the backbone source; manually geocode locations and document precision",

  "SEZ India - List of Notified SEZs",
  "https://sezindia.gov.in/notified-list-sez",
  "Official government PDF",
  "Yes, as PDF",
  "Notification dates and notification years",
  "Official Government of India SEZ source",
  "Some SEZs list multiple notification/amendment dates; operational date may differ from notification date",
  "Use earliest notification date as a proxy for establishment timing where matched",

  "Manual public map search",
  "Google Maps / public map search",
  "Manual geocoding",
  "No",
  "Approximate latitude and longitude for SEZ point locations",
  "Useful for approximate spatial visualization when official coordinates are unavailable",
  "Coordinates are approximate and should not be interpreted as official SEZ boundaries",
  "Validate coordinates against official GIS boundaries if available in future work",

  "SHRUG VIIRS night lights",
  "https://www.devdatalab.org/shrug_download/",
  "Public research data",
  "Yes, as Stata file",
  "District-year VIIRS night lights, including annual mean and sum",
  "Widely used research dataset from Development Data Lab",
  "District identifiers use 2011 Census boundaries, requiring crosswalks for newer districts",
  "Use for dynamic descriptive comparison of SEZ and non-SEZ districts",

  "GADM district boundaries",
  "https://gadm.org/",
  "Administrative boundary data",
  "Yes",
  "Tamil Nadu district polygons for mapping",
  "Commonly used public GIS boundary source",
  "Boundaries may not perfectly match current district definitions or official SEZ locations",
  "Use only for descriptive mapping and document boundary limitations"
)

write_csv(sources, "Data/processed/sources.csv")

# ------------------------------------------------------------
# Data dictionary for zones.csv
# ------------------------------------------------------------

zones_dictionary <- tribble(
  ~variable, ~description,
  
  "zone_id", "Unique identifier created for each Tamil Nadu SEZ in this dataset.",
  
  "country", "Country where the SEZ is located. All rows are India.",
  
  "state", "Indian state where the SEZ is located. All rows are Tamil Nadu.",
  
  "district_official", "District name as reported or implied in the official SEZ India operational SEZ list.",
  
  "district_clean", "Cleaned/current district name used for analysis. This may differ from district_official where district boundaries or names have changed.",
  
  "developer_or_zone_name", "Name of the SEZ developer, authority, or zone as listed in the official SEZ India source.",
  
  "location_text", "Administrative location description from the official SEZ list, usually including village, taluk, city, or industrial park.",
  
  "sector", "Sector or activity type of the SEZ, such as IT/ITES, engineering, electronics, footwear, FTWZ, or multi-sector.",

  "sector_broad", "Broad sector category created for easier summary tables and figures.",
  
  "status", "Operational status of the SEZ. All included rows are operational SEZs.",
  
  "latitude", "Approximate latitude of the SEZ point location in decimal degrees.",
  
  "longitude", "Approximate longitude of the SEZ point location in decimal degrees.",
  
  "crs", "Coordinate reference system for latitude and longitude. Coordinates use WGS84 / EPSG:4326.",
  
  "geocode_precision", "Level of precision for the manually assigned coordinates, such as industrial-area approximation, town/cluster approximation, or village/area approximation.",

  "geocode_reliability", "Qualitative reliability rating for the manually assigned coordinates. Higher means the point is closer to a known SEZ, industrial park, or specific business location; medium means the point is based on a town or cluster; medium_low means the point is based on a broader village or area approximation.",

  "coordinate_source", "Source used to assign latitude and longitude. Coordinates were manually assigned using public map searches and official location text.",
  
  "boundary_available", "Indicator for whether official GIS boundaries or polygons were available. In this dataset, official SEZ boundaries were not available.",
  
  "coordinate_interpretation", "Explanation that coordinates are approximate point locations and should not be interpreted as official SEZ boundaries.",
  
  "source_id", "Identifier linking the row to the source audit file, sources.csv.",
  
  "data_scope", "Description of the dataset scope. This dataset covers operational SEZs in Tamil Nadu from the official SEZ India list.",
  
  "include_in_spatial_analysis", "Indicator equal to 1 if the observation has non-missing latitude and longitude and can be used in spatial analysis; 0 otherwise.",
  
  "notes", "Additional notes about the SEZ, including name changes, district changes, or source-specific limitations.",

  "notification_date_raw", "Raw notification date text from the SEZ India notified SEZ list. Some rows contain multiple dates because of amendments, additions, or denotifications.",

  "notification_date", "Official SEZ notification date from the SEZ India notified SEZ list, stored in YYYY-MM-DD format where verified.",

  "notification_year", "Year extracted from notification_date. Used as a proxy for SEZ establishment year in the dynamic extension.",

  "notification_date_source", "Source used to verify the notification date, usually the official SEZ India notified SEZ list.",

  "notification_date_note", "Notes on notification-date matching, including whether multiple dates were listed or whether the date was not yet verified."
)

write_csv(zones_dictionary, "Data/processed/zones_dictionary.csv")

# ------------------------------------------------------------
# Manually constructed Tamil Nadu operational SEZ dataset
# Source backbone: SEZ India official operational SEZ list
# Coordinates are approximate and manually geocoded.
# CRS for coordinates: WGS84 / EPSG:4326
# ------------------------------------------------------------

# ----------------------------
# 3. Manually constructed Tamil Nadu SEZ sample
# ----------------------------
# Backbone source: SEZ India operational SEZ list.
# Coordinates: approximate, manually geocoded.
# CRS: WGS84 / EPSG:4326.

zones_raw <- tribble(
  ~zone_id, ~developer_or_zone_name, ~state, ~district_official, ~district_clean, ~location_text, ~sector, ~status, ~latitude, ~longitude, ~geocode_precision, ~coordinate_source, ~source_id, ~notes,
  
  "TN_SEZ_001", "MEPZ Special Economic Zone", "Tamil Nadu", "Chennai", "Chennai", "Chennai, Tamil Nadu", "Multi product", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Central government SEZ / MEPZ",
  
  "TN_SEZ_002", "Mahindra World City Developers Limited", "Tamil Nadu", "Kancheepuram", "Chengalpattu", "Taluk Chengalpattu, Kancheepuram District, Tamil Nadu", "Multi Sector SEZ", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Official list uses Kancheepuram; current district may be Chengalpattu",
  
  "TN_SEZ_003", "Nokia India Pvt. Ltd.", "Tamil Nadu", "Kancheepuram", "Kancheepuram", "Sriperumbudur, Tamil Nadu", "Manufacture and assembling of electronics, telecommunications IT hardware, software development, R&D, and other services", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_004", "Flextronics Technologies (India) Private Limited", "Tamil Nadu", "Kancheepuram", "Kancheepuram", "Sriperumbudur, Kancheepuram, Tamil Nadu", "IT/ITES electronic components and hardware manufacturing and related services", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_005", "Tata Consultancy Services Limited", "Tamil Nadu", "Chennai", "Chengalpattu", "Siruseri and Egattur, Chennai, Tamil Nadu", "IT", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Location is in Siruseri/Egattur IT corridor; current district may be Chengalpattu",
  
  "TN_SEZ_006", "Syntel International Private Limited", "Tamil Nadu", "Kancheepuram", "Kancheepuram", "Kancheepuram, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_007", "IG3 Infra Limited (ETL Infrastructure Services Limited)", "Tamil Nadu", "Kancheepuram", "Chengalpattu", "Pallikkarani Village, Tambaram Taluk, Kancheepuram, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Current district may differ from official older district",
  
  "TN_SEZ_008", "Hexaware Technologies Limited", "Tamil Nadu", "Chennai", "Chengalpattu", "SIPCOT IT Park, Old Mahabalipuram Road, Siruseri, Chennai, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_009", "Gateway Office Parks Private Limited", "Tamil Nadu", "Chennai", "Chennai", "No. 16, G.S.T. Road, Perungalathur village, Chennai, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Formerly Shriram Properties and Infrastructure Private Limited",
  
  "TN_SEZ_010", "KGISL Infrastructure Pvt. Ltd.", "Tamil Nadu", "Coimbatore", "Coimbatore", "Kecranatham Village, Coimbatore North Taluk, Coimbatore, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Formerly Coimbatore Hitech Infrastructure Pvt. Ltd.",
  
  "TN_SEZ_011", "DLF Info City Chennai Limited", "Tamil Nadu", "Kancheepuram", "Chennai", "Mugalivakkam Village, Sriperumbudur Taluk, District Kancheepuram, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Formerly DLF Home Developers Limited and DLF Info City Chennai Limited",
  
  "TN_SEZ_012", "State Industries Promotion Corporation of Tamil Nadu", "Tamil Nadu", "Kancheepuram", "Kancheepuram", "SIPCOT Industrial area, Sriperumbudur Taluk, Kancheepuram District, Tamil Nadu", "Electronics / telecom hardware and support services, including trading and logistics activities", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_013", "Electronics Corporation of Tamil Nadu (ELCOT)", "Tamil Nadu", "Coimbatore", "Coimbatore", "Village Vilankurichi, Coimbatore North Taluk, Coimbatore District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_014", "Electronics Corporation of Tamil Nadu Limited (ELCOT)", "Tamil Nadu", "Kancheepuram", "Chengalpattu", "Sholinganallur, Tambaram Taluka, Kancheepuram District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Current district may be Chennai/Chengalpattu depending on boundary treatment",
  
  "TN_SEZ_015", "Cheyyar SEZ Developers Pvt. Ltd.", "Tamil Nadu", "Thiruvannamalai", "Tiruvannamalai", "SIPCOT Cheyyar Industrial Park in Mathur, Mangal Villages, Thiruvannamalai District, Tamil Nadu", "Footwear", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_016", "Estancia IT Park Pvt. Ltd.", "Tamil Nadu", "Kancheepuram", "Chengalpattu", "Vallancheri and Potheri Villages, Chengalpet Taluk, Kancheepuram District, Tamil Nadu", "Electronic hardware and software including ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Formerly Arun Excello Infrastructure Pvt. Ltd., L&T Arun Excello IT SEZ Pvt. Ltd., L&T Chennai Project Pvt. Ltd. and Zoho Corporation Pvt. Ltd.",
  
  "TN_SEZ_017", "Span Ventures Pvt. Ltd.", "Tamil Nadu", "Coimbatore", "Coimbatore", "KPM Nagar, Rathinam Software Park, Kurichi Village, Eachanari, District Coimbatore, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_018", "AspenPark Infra Coimbatore Private Limited (AICPL)", "Tamil Nadu", "Coimbatore", "Coimbatore", "Karumatampatti and Kittampalayam villages, Palladam Taluk, Coimbatore District, Tamil Nadu", "Hi-tech engineering products and related services", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Formerly Aspen Infrastructures Limited / Synefra Engineering construction Ltd. / Suzlon Infrastructure Ltd.",
  
  "TN_SEZ_019", "ETA Technopark Private Limited", "Tamil Nadu", "Kancheepuram", "Chengalpattu", "Old Mahabalipuram Road, Navallur Village, Chengalpet Taluk, Kancheepuram District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_020", "New Chennai Township Private Limited", "Tamil Nadu", "Kancheepuram", "Kancheepuram", "Seekinakuppam Village, Cheyyar Taluk, Kancheepuram District, Tamil Nadu", "Engineering sector including auto ancillaries", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_021", "State Industries Promotion Corporation of Tamil Nadu", "Tamil Nadu", "Kancheepuram", "Kancheepuram", "SIPCOT of Tamil Nadu Industrial Growth Centres, Sriperumbudur Taluka, Kancheepuram District, Tamil Nadu", "Electronics hardware and related support services including trading and logistics operations", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_022", "New Chennai Township Private Limited", "Tamil Nadu", "Kancheepuram", "Kancheepuram", "Seekinakuppam, Paramankeni and Vellur Villages, Cheyyur Taluk, Kancheepuram District, Tamil Nadu", "Multi services", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_023", "State Industries Promotion Corporation of Tamil Nadu", "Tamil Nadu", "Vellore", "Ranipet", "SIPCOT of Tamil Nadu Complex, Ranipet Phase-III, Mukuntharayapuram Village, Walajah Taluk, Vellore District, Tamil Nadu", "Engineering", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Official list uses Vellore; current district is likely Ranipet",
  
  "TN_SEZ_024", "Cognizant Technology Solutions India Pvt. Ltd.", "Tamil Nadu", "Chennai", "Chengalpattu", "SIPCOT IT Park, Siruseri and Kazhipattur villages, Chennai, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_025", "Electronics Corporation of Tamil Nadu Limited (ELCOT)", "Tamil Nadu", "Tiruchirapalli", "Tiruchirappalli", "Navalpattu Village, Tiruchirapalli Taluk, Tiruchirapalli District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_026", "J. Matadee Free Trade Zone Private Limited", "Tamil Nadu", "Kancheepuram", "Kancheepuram", "Mannur and Valarpuram Villages, Sriperumbudur Taluk, Kancheepuram District, Tamil Nadu", "Multi Sector SEZ including FTWZ", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_027", "State Industries Promotion Corporation of Tamil Nadu Limited", "Tamil Nadu", "Erode", "Erode", "SIPCOT, Industrial Growth Centre, Perundurai Village, Erode District, Tamil Nadu", "Engineering", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_028", "Electronics Corporation of Tamil Nadu Limited (ELCOT)", "Tamil Nadu", "Madurai", "Madurai", "Ilandhikulam Village, Madurai I, Madurai North Taluk, Madurai District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_029", "Electronics Corporation of Tamil Nadu Limited (ELCOT)", "Tamil Nadu", "Salem", "Salem", "Jagir Ammapalayam Village, Salem Taluk, Salem District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_030", "State Industries Promotion Corporation of Tamil Nadu", "Tamil Nadu", "Tirunelveli", "Tirunelveli", "SIPCOT Industrial Growth Centre, Gangaikondan Village, Tirunelveli District, Tamil Nadu", "Transport engineering goods including manufacture of tyres and tubes", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_031", "IG3 Infra Limited", "Tamil Nadu", "Erode", "Erode", "Vadamugam Kangeyampalayam Village, Perundurai Taluka, Erode District, Tamil Nadu", "Textile", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Formerly ETL Infrastructure Services Limited and Indian Green Grid Group Ltd.",
  
  "TN_SEZ_032", "AMRL Hitech City Ltd.", "Tamil Nadu", "Tirunelveli", "Tirunelveli", "Nanguneri Taluk, Tirunelveli District, Tamil Nadu", "Multi-product", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Formerly AMRL International Tech City Ltd.",
  
  "TN_SEZ_033", "Tril Infopark Ltd.", "Tamil Nadu", "Chennai", "Chennai", "Kanagam village of Mambalam-Guindy Taluk and Thiruvanmiyur village of Mylapore-Triplicane Taluk, Chennai District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_034", "CCCL Pearl City Food Port SEZ Ltd.", "Tamil Nadu", "Tuticorin", "Thoothukudi", "Vadakkukaracheri and Thimmarajapura Villages, Tuticorin District, Tamil Nadu", "Food Processing", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Formerly CCCL Infrastructure Limited",
  
  "TN_SEZ_035", "L&T Shipbuilding Limited", "Tamil Nadu", "Tiruvallur", "Tiruvallur", "Village Kattupalli, Ponneri Taluk, District Tiruvallur, Tamil Nadu", "Heavy Engineering Sector", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_036", "Cheyyar SEZ Developers Pvt. Ltd.", "Tamil Nadu", "Krishnagiri", "Krishnagiri", "SIPCOT Industrial Growth Centre, Bargur, Uthangarai and Pochampalli Taluk, Krishnagiri District, Tamil Nadu", "Footwear", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_037", "Frontier Lifeline Pvt. Ltd.", "Tamil Nadu", "Thiruvallur", "Tiruvallur", "Edur Village, Gummudipundi Taluk, Thiruvallur District, Tamil Nadu", "Biotechnology", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_038", "Platinum Holdings Pvt. Ltd.", "Tamil Nadu", "Chennai", "Chengalpattu", "2/1, Abu Gardens, OMR Road, Navalur, Chennai-600130", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_039", "Electronics Corporation of Tamil Nadu Limited (ELCOT)", "Tamil Nadu", "Tirunelveli", "Tirunelveli", "Gangaikondan Village, Thrunelveli Taluk, Tirunelveli District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Typo in official list: Thrunelveli",
  
  "TN_SEZ_040", "SNP Infrastructure LLP", "Tamil Nadu", "Kancheepuram", "Chengalpattu", "Zamin Pallavaram Village, Tambaram Taluk, Kancheepuram District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Formerly SNP Infrastructure Pvt. Ltd.",
  
  "TN_SEZ_041", "Infosys Limited", "Tamil Nadu", "Kancheepuram", "Chengalpattu", "No. 138, Old Mahabalipuram Road, Sholinganallur, Kancheepuram District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_042", "TATA Consultancy Services Limited", "Tamil Nadu", "Chengalpattu", "Chengalpattu", "Plot Nos. H-11/1B, H-11/1C and H-11/2 in SIPCOT IT Park, Siruseri, Egattur Village, Thiruporur Taluk, Chengalpattu District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_043", "Perungudi Real Estates Private Limited", "Tamil Nadu", "Chennai", "Chennai", "OMR Road, Perungudi, Chennai, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_044", "Electronics Corporation of Tamil Nadu Limited (ELCOT)", "Tamil Nadu", "Krishnagiri", "Krishnagiri", "Viswanathapuram Village, Hosur Taluk, Krishnagiri District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_045", "Electronics Corporation of Tamil Nadu Limited (ELCOT)", "Tamil Nadu", "Madurai", "Madurai", "Vadapalanji Village, Madurai South Taluk and Kinnimangalam Village, Tirumangalam Taluk, Madurai II, Madurai District, Tamil Nadu", "IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_046", "NDR Infrastructure Pvt. Ltd.", "Tamil Nadu", "Tiruvallur", "Tiruvallur", "Nandiyambakkam Village, Minjur Panchayat Union, Ponneri Taluk, Tiruvallur District, Tamil Nadu", "FTWZ", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "Official text appears to say Panchayat Onion; cleaned as Panchayat Union",
  
  "TN_SEZ_047", "Delta Electronics India Pvt. Ltd.", "Tamil Nadu", "Krishnagiri", "Krishnagiri", "Plot No. 1, Industrial Park, Kurubarapalli, Krishnagiri District, Tamil Nadu", "Electronic hardware and software including IT/ITES", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_048", "Integrated Chennai Business Park (India) Pvt. Ltd.", "Tamil Nadu", "Thiruvallur", "Tiruvallur", "Vallur and Edayanchavadi Villages in Ponneri Taluk, Thiruvallur District, Tamil Nadu", "FTWZ", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", "",
  
  "TN_SEZ_049", "Cheyyar SEZ Developers Pvt. Ltd.", "Tamil Nadu", "Villupuram", "Villupuram", "Pelakuppam Village, Villupuram District, Tamil Nadu", "Multi-Sector SEZ", "Operational", NA_real_, NA_real_, "not geocoded", NA_character_, "SRC_001", ""
)

# ----------------------------
# 4. Clean the dataset
# ----------------------------

zones <- zones_raw %>%
  clean_names() %>%
  mutate(
    country = "India",
    state = str_squish(state),
    district_official = str_squish(district_official),
    district_clean = str_squish(district_clean),
    developer_or_zone_name = str_squish(developer_or_zone_name),
    location_text = str_squish(location_text),
    sector = str_squish(sector),
    status = str_squish(status),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    crs = "WGS84 / EPSG:4326",
    data_scope = "Operational SEZs in Tamil Nadu from official SEZ India list",
    boundary_available = "No",
    coordinate_interpretation = "Approximate point location, not official SEZ polygon"
  ) %>%
  select(
    zone_id,
    country,
    state,
    district_official,
    district_clean,
    developer_or_zone_name,
    location_text,
    sector,
    status,
    latitude,
    longitude,
    crs,
    geocode_precision,
    coordinate_source,
    boundary_available,
    coordinate_interpretation,
    source_id,
    data_scope,
    notes
  )

zones <- zones %>%
  mutate(
    geocode_reliability = case_when(
      geocode_precision == "industrial_area_or_zone_approx" ~ "higher",
      geocode_precision == "town_or_cluster_approx" ~ "medium",
      geocode_precision == "village_or_area_approx" ~ "medium_low",
      TRUE ~ "unknown"
    )
  )

zones <- zones %>%
  mutate(
    sector_broad = case_when(
      str_detect(str_to_lower(sector), "it|ites|software") ~ "IT/ITES",
      str_detect(str_to_lower(sector), "electronic|telecom|hardware") ~ "Electronics",
      str_detect(str_to_lower(sector), "engineering|auto|transport|heavy") ~ "Engineering",
      str_detect(str_to_lower(sector), "footwear") ~ "Footwear",
      str_detect(str_to_lower(sector), "ftwz|free trade") ~ "FTWZ",
      str_detect(str_to_lower(sector), "multi") ~ "Multi-sector/product",
      str_detect(str_to_lower(sector), "food") ~ "Food processing",
      str_detect(str_to_lower(sector), "bio") ~ "Biotechnology",
      str_detect(str_to_lower(sector), "textile") ~ "Textile",
      TRUE ~ "Other"
    )
  )
# ----------------------------
#  Basic checks
# ----------------------------

summary_table <- zones %>%
  summarise(
    n_zones = n(),
    n_districts_official = n_distinct(district_official),
    n_districts_clean = n_distinct(district_clean),
    missing_latitude = sum(is.na(latitude)),
    missing_longitude = sum(is.na(longitude))
  )

sector_table <- zones %>%
  count(sector, sort = TRUE)

district_table <- zones %>%
  count(district_clean, sort = TRUE)

print(summary_table)
print(sector_table)
print(district_table)

# ------------------------------------------------------------
# Add approximate coordinates for Tamil Nadu SEZs
# Coordinates are manually assigned based on the listed location
# or nearby industrial area/city. These are NOT official SEZ boundaries.
# CRS: WGS84 / EPSG:4326
# ------------------------------------------------------------

zones <- zones %>%
  mutate(
    latitude = case_when(
      zone_id == "TN_SEZ_001" ~ 12.9249,   # MEPZ Tambaram
      zone_id == "TN_SEZ_002" ~ 12.7333,   # Mahindra World City Chengalpattu
      zone_id == "TN_SEZ_003" ~ 12.9675,   # Sriperumbudur
      zone_id == "TN_SEZ_004" ~ 12.9675,   # Sriperumbudur
      zone_id == "TN_SEZ_005" ~ 12.8330,   # Siruseri / Egattur
      zone_id == "TN_SEZ_006" ~ 12.8342,   # Kancheepuram approx
      zone_id == "TN_SEZ_007" ~ 12.9516,   # Pallikaranai
      zone_id == "TN_SEZ_008" ~ 12.8330,   # Siruseri
      zone_id == "TN_SEZ_009" ~ 12.9087,   # Perungalathur
      zone_id == "TN_SEZ_010" ~ 11.0800,   # Keeranatham / Coimbatore IT area
      zone_id == "TN_SEZ_011" ~ 13.0225,   # Mugalivakkam
      zone_id == "TN_SEZ_012" ~ 12.9675,   # Sriperumbudur SIPCOT
      zone_id == "TN_SEZ_013" ~ 11.0706,   # Vilankurichi, Coimbatore
      zone_id == "TN_SEZ_014" ~ 12.9010,   # Sholinganallur
      zone_id == "TN_SEZ_015" ~ 12.6500,   # Cheyyar SIPCOT approx
      zone_id == "TN_SEZ_016" ~ 12.8230,   # Potheri / Vallancheri
      zone_id == "TN_SEZ_017" ~ 10.9629,   # Rathinam / Eachanari
      zone_id == "TN_SEZ_018" ~ 11.1080,   # Karumathampatti
      zone_id == "TN_SEZ_019" ~ 12.8450,   # Navallur
      zone_id == "TN_SEZ_020" ~ 12.6900,   # Seekinakuppam / Cheyyar approx
      zone_id == "TN_SEZ_021" ~ 12.9675,   # Sriperumbudur
      zone_id == "TN_SEZ_022" ~ 12.3500,   # Cheyyur / Paramankeni approx
      zone_id == "TN_SEZ_023" ~ 12.9300,   # Ranipet SIPCOT
      zone_id == "TN_SEZ_024" ~ 12.8330,   # Siruseri / Kazhipattur
      zone_id == "TN_SEZ_025" ~ 10.7650,   # Navalpattu, Tiruchirappalli
      zone_id == "TN_SEZ_026" ~ 12.9850,   # Mannur / Valarpuram, Sriperumbudur
      zone_id == "TN_SEZ_027" ~ 11.2756,   # Perundurai
      zone_id == "TN_SEZ_028" ~ 9.9975,    # Ilandhikulam, Madurai
      zone_id == "TN_SEZ_029" ~ 11.6810,   # Jagir Ammapalayam, Salem
      zone_id == "TN_SEZ_030" ~ 8.8600,    # Gangaikondan
      zone_id == "TN_SEZ_031" ~ 11.2900,   # Perundurai area
      zone_id == "TN_SEZ_032" ~ 8.4930,    # Nanguneri
      zone_id == "TN_SEZ_033" ~ 13.0110,   # Taramani / Kanagam, Chennai
      zone_id == "TN_SEZ_034" ~ 8.7500,    # Tuticorin food port area approx
      zone_id == "TN_SEZ_035" ~ 13.3020,   # Kattupalli
      zone_id == "TN_SEZ_036" ~ 12.5450,   # Bargur / Pochampalli area
      zone_id == "TN_SEZ_037" ~ 13.4050,   # Gummidipoondi / Edur
      zone_id == "TN_SEZ_038" ~ 12.8450,   # Navalur
      zone_id == "TN_SEZ_039" ~ 8.8600,    # Gangaikondan
      zone_id == "TN_SEZ_040" ~ 12.9670,   # Zamin Pallavaram
      zone_id == "TN_SEZ_041" ~ 12.9010,   # Sholinganallur
      zone_id == "TN_SEZ_042" ~ 12.8330,   # Siruseri / Egattur
      zone_id == "TN_SEZ_043" ~ 12.9654,   # Perungudi
      zone_id == "TN_SEZ_044" ~ 12.7350,   # Hosur / Viswanathapuram approx
      zone_id == "TN_SEZ_045" ~ 9.9000,    # Vadapalanji / Madurai South approx
      zone_id == "TN_SEZ_046" ~ 13.2700,   # Nandiyambakkam / Minjur
      zone_id == "TN_SEZ_047" ~ 12.5440,   # Kurubarapalli, Krishnagiri
      zone_id == "TN_SEZ_048" ~ 13.2600,   # Vallur / Edayanchavadi, Ponneri
      zone_id == "TN_SEZ_049" ~ 12.2200,   # Pelakuppam, Villupuram approx
      TRUE ~ latitude
    ),
    
    longitude = case_when(
      zone_id == "TN_SEZ_001" ~ 80.1275,
      zone_id == "TN_SEZ_002" ~ 80.0047,
      zone_id == "TN_SEZ_003" ~ 79.9419,
      zone_id == "TN_SEZ_004" ~ 79.9419,
      zone_id == "TN_SEZ_005" ~ 80.2180,
      zone_id == "TN_SEZ_006" ~ 79.7036,
      zone_id == "TN_SEZ_007" ~ 80.2210,
      zone_id == "TN_SEZ_008" ~ 80.2180,
      zone_id == "TN_SEZ_009" ~ 80.0934,
      zone_id == "TN_SEZ_010" ~ 77.0400,
      zone_id == "TN_SEZ_011" ~ 80.1686,
      zone_id == "TN_SEZ_012" ~ 79.9419,
      zone_id == "TN_SEZ_013" ~ 77.0004,
      zone_id == "TN_SEZ_014" ~ 80.2279,
      zone_id == "TN_SEZ_015" ~ 79.5500,
      zone_id == "TN_SEZ_016" ~ 80.0450,
      zone_id == "TN_SEZ_017" ~ 76.9798,
      zone_id == "TN_SEZ_018" ~ 77.1850,
      zone_id == "TN_SEZ_019" ~ 80.2260,
      zone_id == "TN_SEZ_020" ~ 79.6200,
      zone_id == "TN_SEZ_021" ~ 79.9419,
      zone_id == "TN_SEZ_022" ~ 80.0500,
      zone_id == "TN_SEZ_023" ~ 79.3330,
      zone_id == "TN_SEZ_024" ~ 80.2180,
      zone_id == "TN_SEZ_025" ~ 78.8150,
      zone_id == "TN_SEZ_026" ~ 79.9300,
      zone_id == "TN_SEZ_027" ~ 77.5870,
      zone_id == "TN_SEZ_028" ~ 78.1515,
      zone_id == "TN_SEZ_029" ~ 78.1320,
      zone_id == "TN_SEZ_030" ~ 77.7800,
      zone_id == "TN_SEZ_031" ~ 77.6000,
      zone_id == "TN_SEZ_032" ~ 77.6580,
      zone_id == "TN_SEZ_033" ~ 80.2400,
      zone_id == "TN_SEZ_034" ~ 78.1200,
      zone_id == "TN_SEZ_035" ~ 80.3410,
      zone_id == "TN_SEZ_036" ~ 78.3800,
      zone_id == "TN_SEZ_037" ~ 80.1300,
      zone_id == "TN_SEZ_038" ~ 80.2260,
      zone_id == "TN_SEZ_039" ~ 77.7800,
      zone_id == "TN_SEZ_040" ~ 80.1500,
      zone_id == "TN_SEZ_041" ~ 80.2279,
      zone_id == "TN_SEZ_042" ~ 80.2180,
      zone_id == "TN_SEZ_043" ~ 80.2461,
      zone_id == "TN_SEZ_044" ~ 77.8300,
      zone_id == "TN_SEZ_045" ~ 78.0000,
      zone_id == "TN_SEZ_046" ~ 80.2750,
      zone_id == "TN_SEZ_047" ~ 78.1900,
      zone_id == "TN_SEZ_048" ~ 80.3000,
      zone_id == "TN_SEZ_049" ~ 79.8500,
      TRUE ~ longitude
    ),
    
    geocode_precision = case_when(
      zone_id %in% c(
        "TN_SEZ_001", "TN_SEZ_002", "TN_SEZ_013", "TN_SEZ_014",
        "TN_SEZ_016", "TN_SEZ_017", "TN_SEZ_023", "TN_SEZ_024",
        "TN_SEZ_027", "TN_SEZ_030", "TN_SEZ_033", "TN_SEZ_035",
        "TN_SEZ_041", "TN_SEZ_042", "TN_SEZ_043"
      ) ~ "industrial_area_or_zone_approx",
      
      zone_id %in% c(
        "TN_SEZ_003", "TN_SEZ_004", "TN_SEZ_012", "TN_SEZ_021",
        "TN_SEZ_026", "TN_SEZ_038", "TN_SEZ_046", "TN_SEZ_048"
      ) ~ "town_or_cluster_approx",
      
      TRUE ~ "village_or_area_approx"
    ),
    
    coordinate_source = "Manual public map search / approximate location assignment",
    
    include_in_spatial_analysis = if_else(
      !is.na(latitude) & !is.na(longitude),
      1,
      0
    )
  )

write_csv(zones, "Data/processed/zones.csv")

# ----------------------------
#  Basic checks
# ----------------------------

summary_table <- zones %>%
  summarise(
    n_zones = n(),
    n_districts_official = n_distinct(district_official),
    n_districts_clean = n_distinct(district_clean),
    missing_latitude = sum(is.na(latitude)),
    missing_longitude = sum(is.na(longitude)),
    included_in_spatial_analysis = sum(include_in_spatial_analysis == 1)
  )

print(summary_table)

head(zones)

# ------------------------------------------------------------
# Data dictionary for analysis_units.csv
# ------------------------------------------------------------

analysis_units_dictionary <- tribble(
  ~variable, ~description,
  
  "country", "Country for the analysis unit. All rows are India.",
  
  "state", "Indian state for the analysis unit. All rows are Tamil Nadu.",
  
  "spatial_unit", "Level of observation used for the analysis. In this file, each row is a district.",
  
  "district_clean", "Cleaned/current Tamil Nadu district name used as the district-level analysis unit.",
  
  "district_lat", "Approximate latitude of the district center or main district town, in decimal degrees.",
  
  "district_lon", "Approximate longitude of the district center or main district town, in decimal degrees.",
  
  "crs", "Coordinate reference system for district_lat and district_lon. Coordinates use WGS84 / EPSG:4326.",
  
  "zone_indicator", "Indicator equal to 1 if the district contains at least one operational SEZ from the Tamil Nadu SEZ dataset, and 0 otherwise.",
  
  "num_sezs", "Number of operational SEZs from zones.csv located in the district.",
  
  "distance_to_chennai_km", "Haversine distance in kilometers from the approximate district center to Chennai, Tamil Nadu's main economic and administrative center.",
  
  "coastal_district", "Indicator equal to 1 if the district has direct coastline access, and 0 otherwise.",

  "population_density_per_sq_km", "Population density of the district, measured as persons per square kilometer. Values are based on Census 2011 district-level population density where available; newer post-2011 districts use approximate/harmonized values.",

  "population_density_year", "Reference year for the population density variable. Most values are based on 2011 Census data.",

  "population_density_source", "Source note for the population density value, including whether the value is direct Census 2011 district data or an approximate/harmonized value for newer districts.",  
 
  "distance_method", "Description of how distance_to_chennai_km was calculated.",

  "pc11_state_id", "2011 Census state identifier used to merge SHRUG VIIRS night lights.",

  "pc11_district_id", "2011 Census district identifier used to merge SHRUG VIIRS night lights.",

  "pc11_district_name", "Name of the 2011 Census district used by the night lights crosswalk.",

  "district_crosswalk_note", "Note explaining whether a current district directly matched a 2011 district or was mapped to a parent 2011 district.",

  "nightlights_year", "Year of the SHRUG VIIRS night lights measure used in analysis_units.csv.",

  "nightlights_category", "SHRUG VIIRS category used; average-masked is used here.",

  "viirs_annual_mean", "Mean VIIRS night lights detected within the 2011 district polygon.",

  "viirs_annual_sum", "Total VIIRS night lights detected within the 2011 district polygon.",

  "viirs_annual_num_cells", "Number of raster cells used to calculate district-level VIIRS night lights."
)

write_csv(
  analysis_units_dictionary,
  "Data/processed/analysis_units_dictionary.csv"
)

# ------------------------------------------------------------
# Setup: Create district-level analysis units
# ------------------------------------------------------------

tamil_nadu_districts <- tibble(
  district_clean = c(
    "Ariyalur", "Chengalpattu", "Chennai", "Coimbatore", "Cuddalore",
    "Dharmapuri", "Dindigul", "Erode", "Kallakurichi", "Kancheepuram",
    "Kanniyakumari", "Karur", "Krishnagiri", "Madurai", "Mayiladuthurai",
    "Nagapattinam", "Namakkal", "Nilgiris", "Perambalur", "Pudukkottai",
    "Ramanathapuram", "Ranipet", "Salem", "Sivaganga", "Tenkasi",
    "Thanjavur", "Theni", "Thoothukudi", "Tiruchirappalli", "Tirunelveli",
    "Tirupathur", "Tiruppur", "Tiruvallur", "Tiruvannamalai", "Tiruvarur",
    "Vellore", "Viluppuram", "Virudhunagar"
  )
)

sez_by_district <- zones %>%
  count(district_clean, name = "num_sezs")

analysis_units <- tamil_nadu_districts %>%
  left_join(sez_by_district, by = "district_clean") %>%
  mutate(
    num_sezs = replace_na(num_sezs, 0),
    zone_indicator = if_else(num_sezs > 0, 1, 0),
    spatial_unit = "District",
    state = "Tamil Nadu",
    country = "India"
  ) %>%
  select(
    country,
    state,
    spatial_unit,
    district_clean,
    zone_indicator,
    num_sezs
  )

write_csv(analysis_units, "Data/processed/analysis_units.csv")

print(analysis_units)

# ============================================================
# Part 2: Create district-level analysis_units.csv
# Add spatial/economic characteristics to compare SEZ and non-SEZ districts
# ============================================================

# ----------------------------
# Tamil Nadu district list with approximate district-center coordinates
# ----------------------------
# Coordinates are approximate district/city centroids in WGS84 / EPSG:4326.
# These are used only for district-level distance calculations.

tamil_nadu_districts <- tribble(
  ~district_clean, ~district_lat, ~district_lon,
  "Ariyalur", 11.1401, 79.0786,
  "Chengalpattu", 12.6929, 79.9757,
  "Chennai", 13.0827, 80.2707,
  "Coimbatore", 11.0168, 76.9558,
  "Cuddalore", 11.7447, 79.7680,
  "Dharmapuri", 12.1270, 78.1582,
  "Dindigul", 10.3673, 77.9803,
  "Erode", 11.3410, 77.7172,
  "Kallakurichi", 11.7406, 78.9590,
  "Kancheepuram", 12.8342, 79.7036,
  "Kanniyakumari", 8.0883, 77.5385,
  "Karur", 10.9601, 78.0766,
  "Krishnagiri", 12.5186, 78.2137,
  "Madurai", 9.9252, 78.1198,
  "Mayiladuthurai", 11.1018, 79.6520,
  "Nagapattinam", 10.7672, 79.8449,
  "Namakkal", 11.2194, 78.1678,
  "Nilgiris", 11.4102, 76.6950,
  "Perambalur", 11.2333, 78.8833,
  "Pudukkottai", 10.3833, 78.8001,
  "Ramanathapuram", 9.3639, 78.8395,
  "Ranipet", 12.9249, 79.3333,
  "Salem", 11.6643, 78.1460,
  "Sivaganga", 9.8470, 78.4836,
  "Tenkasi", 8.9590, 77.3152,
  "Thanjavur", 10.7870, 79.1378,
  "Theni", 10.0104, 77.4768,
  "Thoothukudi", 8.7642, 78.1348,
  "Tiruchirappalli", 10.7905, 78.7047,
  "Tirunelveli", 8.7139, 77.7567,
  "Tirupathur", 12.4984, 78.5602,
  "Tiruppur", 11.1085, 77.3411,
  "Tiruvallur", 13.1394, 79.9078,
  "Tiruvannamalai", 12.2253, 79.0747,
  "Tiruvarur", 10.7661, 79.6344,
  "Vellore", 12.9165, 79.1325,
  "Viluppuram", 11.9401, 79.4861,
  "Virudhunagar", 9.5680, 77.9624
)

# ---------------------------------------------
# Create SEZ counts by district
# ---------------------------------------------

sez_by_district <- zones %>%
  count(district_clean, name = "num_sezs")

# ---------------------------------------------
# Add coastal district indicator
# ---------------------------------------------
# This is a manually coded spatial characteristic.
# It equals 1 for districts with direct coastline access.

coastal_districts <- c(
  "Chennai",
  "Chengalpattu",
  "Cuddalore",
  "Kanniyakumari",
  "Mayiladuthurai",
  "Nagapattinam",
  "Ramanathapuram",
  "Thoothukudi",
  "Tiruvallur",
  "Tiruvarur",
  "Viluppuram"
)

# ---------------------------------------------
# Haversine distance function
# ---------------------------------------------
# Calculates distance between two lat/lon points in kilometers.

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  
  lat1_rad <- lat1 * pi / 180
  lon1_rad <- lon1 * pi / 180
  lat2_rad <- lat2 * pi / 180
  lon2_rad <- lon2 * pi / 180
  
  dlat <- lat2_rad - lat1_rad
  dlon <- lon2_rad - lon1_rad
  
  a <- sin(dlat / 2)^2 +
    cos(lat1_rad) * cos(lat2_rad) * sin(dlon / 2)^2
  
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  r * c
}

# Chennai coordinates
chennai_lat <- 13.0827
chennai_lon <- 80.2707

# ----------------------------
# Add population density
# ----------------------------
# Population density is persons per square kilometer.
# Source basis: Census 2011 / Tamil Nadu statistical district tables.
# Note: For newer districts created after 2011, values are approximate
# or manually harmonized from available district-level sources.

population_density_data <- tribble(
  ~district_clean, ~population_density_per_sq_km, ~population_density_year, ~population_density_source,
  
  "Ariyalur", 390, 2011, "Census 2011 / district-level population density",
  "Chengalpattu", 892, 2011, "Approximate/harmonized from former Kancheepuram district area",
  "Chennai", 26553, 2011, "Census 2011 / district-level population density",
  "Coimbatore", 748, 2011, "Census 2011 / district-level population density",
  "Cuddalore", 704, 2011, "Census 2011 / district-level population density",
  "Dharmapuri", 335, 2011, "Census 2011 / district-level population density",
  "Dindigul", 357, 2011, "Census 2011 / district-level population density",
  "Erode", 394, 2011, "Census 2011 / district-level population density",
  "Kallakurichi", 430, 2011, "Approximate/harmonized from former Villupuram district area",
  "Kancheepuram", 927, 2011, "Census 2011 / former district-level population density",
  "Kanniyakumari", 1119, 2011, "Census 2011 / district-level population density",
  "Karur", 371, 2011, "Census 2011 / district-level population density",
  "Krishnagiri", 370, 2011, "Census 2011 / district-level population density",
  "Madurai", 819, 2011, "Census 2011 / district-level population density",
  "Mayiladuthurai", 720, 2011, "Approximate/harmonized from former Nagapattinam district area",
  "Nagapattinam", 668, 2011, "Census 2011 / former district-level population density",
  "Namakkal", 506, 2011, "Census 2011 / district-level population density",
  "Nilgiris", 287, 2011, "Census 2011 / district-level population density",
  "Perambalur", 321, 2011, "Census 2011 / district-level population density",
  "Pudukkottai", 348, 2011, "Census 2011 / district-level population density",
  "Ramanathapuram", 320, 2011, "Census 2011 / district-level population density",
  "Ranipet", 520, 2011, "Approximate/harmonized from former Vellore district area",
  "Salem", 663, 2011, "Census 2011 / district-level population density",
  "Sivaganga", 324, 2011, "Census 2011 / district-level population density",
  "Tenkasi", 410, 2011, "Approximate/harmonized from former Tirunelveli district area",
  "Thanjavur", 691, 2011, "Census 2011 / district-level population density",
  "Theni", 433, 2011, "Census 2011 / district-level population density",
  "Thoothukudi", 378, 2011, "Census 2011 / district-level population density",
  "Tiruchirappalli", 602, 2011, "Census 2011 / district-level population density",
  "Tirunelveli", 460, 2011, "Census 2011 / former district-level population density",
  "Tirupathur", 420, 2011, "Approximate/harmonized from former Vellore district area",
  "Tiruppur", 478, 2011, "Census 2011 / district-level population density",
  "Tiruvallur", 1098, 2011, "Census 2011 / district-level population density",
  "Tiruvannamalai", 399, 2011, "Census 2011 / district-level population density",
  "Tiruvarur", 533, 2011, "Census 2011 / district-level population density",
  "Vellore", 648, 2011, "Census 2011 / former district-level population density",
  "Viluppuram", 482, 2011, "Census 2011 / former district-level population density",
  "Virudhunagar", 454, 2011, "Census 2011 / district-level population density"
)

# ------------------------------------------------------------
# Add SHRUG VIIRS night lights data
# ------------------------------------------------------------
# Source: Development Data Lab SHRUG v2.1
# File: viirs_annual_pc11dist.dta
# Unit: 2011 Census district
# We use 2021 mean-masked VIIRS night lights as a proxy for economic activity.
# ------------------------------------------------------------

nightlights_raw <- read_dta("Data/raw/viirs_annual_pc11dist.dta")

nightlights_raw <- nightlights_raw %>%
  mutate(
    pc11_state_id = as.integer(pc11_state_id),
    pc11_district_id = as.integer(pc11_district_id)
  )

nightlights_raw %>%
  count(pc11_state_id, sort = TRUE)

glimpse(nightlights_raw)

# Filter to Tamil Nadu, 2021, and mean-masked category
# Note: Tamil Nadu 2011 Census state ID is usually 33.
# If this returns 0 rows, run count(pc11_state_id) and check the state code.

nightlights_tn <- nightlights_raw %>%
  filter(
    pc11_state_id == 33,
    year == 2021,
    category == "average-masked"
  ) %>%
  transmute(
    pc11_state_id,
    pc11_district_id,
    nightlights_year = year,
    nightlights_category = category,
    viirs_annual_mean,
    viirs_annual_sum,
    viirs_annual_num_cells
  )

print(nightlights_tn)

nightlights_raw %>%
  count(category, sort = TRUE)

  nightlights_raw %>%
  filter(year == 2021, category == "average-masked") %>%
  count(pc11_state_id, sort = TRUE)
# ------------------------------------------------------------
# Manual crosswalk: current Tamil Nadu districts to 2011 Census districts
# ------------------------------------------------------------
# SHRUG nightlights are indexed to 2011 Census districts.
# Tamil Nadu has created several new districts since 2011, so newer
# districts are mapped back to their parent 2011 district.

tn_2011_district_crosswalk <- tribble(
  ~district_clean, ~pc11_state_id, ~pc11_district_id, ~pc11_district_name, ~district_crosswalk_note,
  
  "Tiruvallur", 33, 602, "Thiruvallur", "Direct 2011 district match; spelling standardized",
  "Chennai", 33, 603, "Chennai", "Direct 2011 district match",
  "Kancheepuram", 33, 604, "Kancheepuram", "Direct/parent 2011 district match",
  "Chengalpattu", 33, 604, "Kancheepuram", "Newer district; mapped to parent 2011 district Kancheepuram",
  "Vellore", 33, 605, "Vellore", "Direct/parent 2011 district match",
  "Ranipet", 33, 605, "Vellore", "Newer district; mapped to parent 2011 district Vellore",
  "Tirupathur", 33, 605, "Vellore", "Newer district; mapped to parent 2011 district Vellore",
  "Tiruvannamalai", 33, 606, "Tiruvannamalai", "Direct 2011 district match",
  "Viluppuram", 33, 607, "Viluppuram", "Direct/parent 2011 district match",
  "Kallakurichi", 33, 607, "Viluppuram", "Newer district; mapped to parent 2011 district Viluppuram",
  "Salem", 33, 608, "Salem", "Direct 2011 district match",
  "Namakkal", 33, 609, "Namakkal", "Direct 2011 district match",
  "Erode", 33, 610, "Erode", "Direct 2011 district match",
  "The Nilgiris", 33, 611, "The Nilgiris", "Direct 2011 district match",
  "Nilgiris", 33, 611, "The Nilgiris", "Direct 2011 district match",
  "Dindigul", 33, 612, "Dindigul", "Direct 2011 district match",
  "Karur", 33, 613, "Karur", "Direct 2011 district match",
  "Tiruchirappalli", 33, 614, "Tiruchirappalli", "Direct 2011 district match",
  "Perambalur", 33, 615, "Perambalur", "Direct 2011 district match",
  "Ariyalur", 33, 616, "Ariyalur", "Direct 2011 district match",
  "Cuddalore", 33, 617, "Cuddalore", "Direct 2011 district match",
  "Nagapattinam", 33, 618, "Nagapattinam", "Direct/parent 2011 district match",
  "Mayiladuthurai", 33, 618, "Nagapattinam", "Newer district; mapped to parent 2011 district Nagapattinam",
  "Tiruvarur", 33, 619, "Thiruvarur", "Direct 2011 district match",
  "Thanjavur", 33, 620, "Thanjavur", "Direct 2011 district match",
  "Pudukkottai", 33, 621, "Pudukkottai", "Direct 2011 district match",
  "Sivaganga", 33, 622, "Sivaganga", "Direct 2011 district match",
  "Madurai", 33, 623, "Madurai", "Direct 2011 district match",
  "Theni", 33, 624, "Theni", "Direct 2011 district match",
  "Virudhunagar", 33, 625, "Virudhunagar", "Direct 2011 district match",
  "Ramanathapuram", 33, 626, "Ramanathapuram", "Direct 2011 district match",
  "Thoothukudi", 33, 627, "Thoothukkudi", "Direct 2011 district match",
  "Tirunelveli", 33, 628, "Tirunelveli", "Direct/parent 2011 district match",
  "Tenkasi", 33, 628, "Tirunelveli", "Newer district; mapped to parent 2011 district Tirunelveli",
  "Kanniyakumari", 33, 629, "Kanniyakumari", "Direct 2011 district match",
  "Dharmapuri", 33, 630, "Dharmapuri", "Direct 2011 district match",
  "Krishnagiri", 33, 631, "Krishnagiri", "Direct 2011 district match",
  "Coimbatore", 33, 632, "Coimbatore", "Direct 2011 district match",
  "Tiruppur", 33, 633, "Tiruppur", "Direct 2011 district match"
)

# Export Tamil Nadu 2011 district IDs for manual matching
write_csv(nightlights_tn, "Data/processed/nightlights_tn_2011_district_ids_to_match.csv")

# ---------------------------------------------
# Create analysis_units.csv
# ---------------------------------------------

analysis_units <- tamil_nadu_districts %>%
  left_join(sez_by_district, by = "district_clean") %>%
  left_join(population_density_data, by = "district_clean") %>%
  mutate(
    num_sezs = replace_na(num_sezs, 0),
    zone_indicator = if_else(num_sezs > 0, 1, 0),
    coastal_district = if_else(district_clean %in% coastal_districts, 1, 0),
    distance_to_chennai_km = haversine_km(
      district_lat, district_lon,
      chennai_lat, chennai_lon
    ),
    distance_to_chennai_km = round(distance_to_chennai_km, 1),
    country = "India",
    state = "Tamil Nadu",
    spatial_unit = "District",
    crs = "WGS84 / EPSG:4326",
    distance_method = "Haversine distance between approximate district centroid and Chennai"
  ) %>%
  select(
    country,
    state,
    spatial_unit,
    district_clean,
    district_lat,
    district_lon,
    crs,
    zone_indicator,
    num_sezs,
    distance_to_chennai_km,
    coastal_district,
    population_density_per_sq_km,
    population_density_year,
    population_density_source,
    distance_method
  )

# Keep only the nightlights variables we need
nightlights_tn_clean <- nightlights_tn %>%
  select(
    pc11_state_id,
    pc11_district_id,
    nightlights_year,
    nightlights_category,
    viirs_annual_mean,
    viirs_annual_sum,
    viirs_annual_num_cells
  )

# Add 2011 district IDs to current district analysis units
analysis_units <- analysis_units %>%
  left_join(tn_2011_district_crosswalk, by = "district_clean") %>%
  left_join(
    nightlights_tn_clean,
    by = c("pc11_state_id", "pc11_district_id")
  )

write_csv(analysis_units, "Data/processed/analysis_units.csv")

# ---------------------------------------------
# Quick checks
# ---------------------------------------------

analysis_summary <- analysis_units %>%
  summarise(
    n_districts = n(),
    n_zone_districts = sum(zone_indicator == 1),
    n_nonzone_districts = sum(zone_indicator == 0),
    total_sezs = sum(num_sezs),
    avg_distance_to_chennai = mean(distance_to_chennai_km),
    share_coastal = mean(coastal_district)
  )

print(analysis_summary)

analysis_units %>%
  arrange(desc(num_sezs)) %>%
  print(n = 38)

# Check whether all districts matched to nightlights
analysis_units %>%
  filter(is.na(viirs_annual_mean)) %>%
  select(district_clean, pc11_district_name, pc11_district_id)

# Summary
analysis_units %>%
  summarise(
    n_districts = n(),
    missing_nightlights = sum(is.na(viirs_annual_mean)),
    mean_nightlights = mean(viirs_annual_mean, na.rm = TRUE)
  )

# ============================================================
# Part 3: Compare SEZ and non-SEZ districts
# ============================================================

# ------------------------------------------------------------
# Creating maps to visualize SEZ and non-SEZ districts, and their characteristics
# ------------------------------------------------------------

# Read datasets
analysis_units <- read_csv("Data/processed/analysis_units.csv", show_col_types = FALSE)
zones <- read_csv("Data/processed/zones.csv", show_col_types = FALSE)

# Download/load India district boundaries
ind_adm2 <- geodata::gadm(
  country = "IND",
  level = 2,
  path = "Data/raw"
)

ind_adm2_sf <- st_as_sf(ind_adm2)

# Keep Tamil Nadu districts
tn_districts_sf <- ind_adm2_sf %>%
  filter(NAME_1 == "Tamil Nadu") %>%
  mutate(
    district_clean = str_squish(NAME_2),
    district_clean = case_when(
      district_clean == "Thiruvallur" ~ "Tiruvallur",
      district_clean == "Thiruvarur" ~ "Tiruvarur",
      district_clean == "Thoothukkudi" ~ "Thoothukudi",
      district_clean == "The Nilgiris" ~ "Nilgiris",
      district_clean == "Nagappattinam" ~ "Nagapattinam",
      district_clean == "Virudunagar" ~ "Virudhunagar",
      TRUE ~ district_clean
    )
  )

# Merge district map with analysis data
tn_map_data <- tn_districts_sf %>%
  left_join(analysis_units, by = "district_clean")

# Check districts missing population density after merge
tn_map_data %>%
  st_drop_geometry() %>%
  filter(is.na(population_density_per_sq_km)) %>%
  select(NAME_2, district_clean, population_density_per_sq_km)

# District names in map but not in analysis_units
setdiff(tn_districts_sf$district_clean, analysis_units$district_clean)

# District names in analysis_units but not in map
setdiff(analysis_units$district_clean, tn_districts_sf$district_clean)

# Check unmatched districts
tn_map_data %>%
  filter(is.na(zone_indicator)) %>%
  st_drop_geometry() %>%
  select(NAME_2, district_clean)

# Convert SEZ coordinates to sf points
sez_sf <- zones %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# ------------------------------------------------------------
# Correct map coloring using spatial location of SEZ points
# ------------------------------------------------------------

# Join each SEZ point to the district polygon it falls inside
sez_map_assignment <- st_join(
  sez_sf,
  tn_map_data %>% select(map_district = district_clean),
  join = st_within
)

# For the one point outside all polygons, assign nearest district
missing_idx <- which(is.na(sez_map_assignment$map_district))

if (length(missing_idx) > 0) {
  nearest_idx <- st_nearest_feature(
    sez_map_assignment[missing_idx, ],
    tn_map_data
  )
  
  sez_map_assignment$map_district[missing_idx] <-
    tn_map_data$district_clean[nearest_idx]
}

# Count SEZs by mapped district
sez_counts_by_map_district <- sez_map_assignment %>%
  st_drop_geometry() %>%
  count(map_district, name = "num_sezs_map") %>%
  rename(district_clean = map_district)

# Add map-based SEZ indicator to district polygons
tn_map_data_corrected <- tn_map_data %>%
  select(-any_of(c("num_sezs_map", "zone_indicator_map"))) %>%
  left_join(sez_counts_by_map_district, by = "district_clean") %>%
  mutate(
    num_sezs_map = replace_na(num_sezs_map, 0),
    zone_indicator_map = if_else(num_sezs_map > 0, 1, 0)
  )

# Add three major Tamil Nadu cities
big_cities <- tibble::tribble(
  ~city,         ~longitude, ~latitude,
  "Chennai",      80.2707,    13.0827,
  "Coimbatore",   76.9558,    11.0168,
  "Madurai",      78.1198,     9.9252
)

big_cities_sf <- big_cities %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(tn_map_data))


# Map 1: Population density + SEZ points + major cities

map_pop_sez <- ggplot() +
  geom_sf(
    data = tn_map_data,
    aes(fill = population_density_per_sq_km),
    color = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = sez_sf,
    color = "red",
    size = 1.7,
    alpha = 0.8
  ) +
  geom_sf(
  data = big_cities_sf,
  color = "darkgreen",
  fill = "yellow",
  shape = 21,
  size = 3
 ) +
geom_sf_text(
  data = big_cities_sf,
  aes(label = city),
  nudge_y = 0.15,
  size = 3,
  fontface = "bold",
  color = "darkgreen"
 ) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey90",
    name = "Population density\n(per sq. km)"
  ) +
  labs(
    title = "Tamil Nadu: Population Density, SEZs, and Major Cities",
    subtitle = "Districts shaded by population density; red points show operational SEZs; yellow points show major cities",
    caption = "Sources: SEZ India operational SEZ list; district boundaries from GADM; population density from district-level data."
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

print(map_pop_sez)

ggsave(
  filename = "Output/Plots/tamil_nadu_population_density_sez_map.png",
  plot = map_pop_sez,
  width = 9,
  height = 10,
  dpi = 300
)


# Map 2: SEZ vs non-SEZ districts

map_zone_indicator_corrected <- ggplot() +
  geom_sf(
    data = tn_map_data_corrected,
    aes(fill = factor(zone_indicator_map)),
    color = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = sez_sf,
    color = "black",
    size = 1.5,
    alpha = 0.75
  ) +
  scale_fill_manual(
    values = c("0" = "grey85", "1" = "steelblue"),
    labels = c("0" = "No SEZ point", "1" = "Has SEZ point"),
    name = "District type"
  ) +
  labs(
    title = "Tamil Nadu Districts with and without SEZs",
    subtitle = "Blue districts contain at least one mapped SEZ point",
    caption = "Note: District classification is based on mapped SEZ point locations. Some SEZ coordinates are approximate."
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

print(map_zone_indicator_corrected)

ggsave(
  filename = "Output/Plots/tamil_nadu_zone_indicator_map_corrected.png",
  plot = map_zone_indicator_corrected,
  width = 9,
  height = 10,
  dpi = 300
)

sez_joined_to_map <- st_join(
  sez_sf,
  tn_map_data %>% select(map_district = district_clean),
  join = st_within
) %>%
  st_drop_geometry() %>%
  select(
    zone_id,
    developer_or_zone_name,
    district_clean,
    map_district,
    latitude,
    longitude,
    geocode_precision
  ) %>%
  mutate(
    district_match = district_clean == map_district
  )

sez_joined_to_map %>%
  filter(district_match == FALSE | is.na(map_district)) %>%
  print(n = Inf)

sez_joined_to_map %>%
  summarise(
    total_points = n(),
    outside_any_district = sum(is.na(map_district)),
    inside_wrong_district = sum(!is.na(map_district) & district_clean != map_district),
    correct_district = sum(!is.na(map_district) & district_clean == map_district)
  )


# ------------------------------------------------------------
# Summary table: SEZ districts vs non-SEZ districts
# ------------------------------------------------------------

part3_summary <- analysis_units %>%
  mutate(
    district_type = if_else(
      zone_indicator == 1,
      "District with at least one SEZ",
      "District without SEZ"
    )
  ) %>%
  group_by(district_type) %>%
  summarise(
    n_districts = n(),
    total_sezs = sum(num_sezs, na.rm = TRUE),
    mean_distance_to_chennai_km = round(mean(distance_to_chennai_km, na.rm = TRUE), 1),
    mean_population_density_per_sq_km = round(mean(population_density_per_sq_km, na.rm = TRUE), 1),
    mean_viirs_nightlights = round(mean(viirs_annual_mean, na.rm = TRUE), 3),
    share_coastal = round(mean(coastal_district, na.rm = TRUE), 3),
    .groups = "drop"
  )

print(part3_summary)

write_csv(
  part3_summary,
  "Output/Tables/part3_sez_vs_nonsez_summary.csv"
)

# For better readability, we can pivot the summary table to a long format with one row per characteristic and separate columns for SEZ vs non-SEZ districts.

part3_summary_long <- analysis_units %>%
  mutate(
    district_type = if_else(
      zone_indicator == 1,
      "SEZ districts",
      "Non-SEZ districts"
    )
  ) %>%
  group_by(district_type) %>%
  summarise(
    `Number of districts` = n(),
    `Total SEZs` = sum(num_sezs, na.rm = TRUE),
    `Mean distance to Chennai (km)` = round(mean(distance_to_chennai_km, na.rm = TRUE), 1),
    `Mean population density (persons/sq. km)` = round(mean(population_density_per_sq_km, na.rm = TRUE), 1),
    `Mean VIIRS night lights` = round(mean(viirs_annual_mean, na.rm = TRUE), 3),
    `Share coastal` = round(mean(coastal_district, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -district_type,
    names_to = "characteristic",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = district_type,
    values_from = value
  )

print(part3_summary_long)

write_csv(
  part3_summary_long,
  "Output/Tables/part3_summary_table_readable.csv"
)

# ------------------------------------------------------------
# Bar plot: average characteristics by district type
# ------------------------------------------------------------

part3_plot_data <- analysis_units %>%
  mutate(
    district_type = if_else(
      zone_indicator == 1,
      "SEZ district",
      "Non-SEZ district"
    )
  ) %>%
  group_by(district_type) %>%
  summarise(
    mean_population_density = mean(population_density_per_sq_km, na.rm = TRUE),
    mean_nightlights = mean(viirs_annual_mean, na.rm = TRUE),
    mean_distance_to_chennai = mean(distance_to_chennai_km, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      mean_population_density,
      mean_nightlights,
      mean_distance_to_chennai
    ),
    names_to = "variable",
    values_to = "mean_value"
  ) %>%
  mutate(
    variable = case_when(
      variable == "mean_population_density" ~ "Population density",
      variable == "mean_nightlights" ~ "Night lights",
      variable == "mean_distance_to_chennai" ~ "Distance to Chennai",
      TRUE ~ variable
    )
  )

part3_bar_plot <- ggplot(
  part3_plot_data,
  aes(x = district_type, y = mean_value, fill = district_type)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    title = "Average Characteristics of SEZ and Non-SEZ Districts",
    subtitle = "Tamil Nadu district-level comparison",
    x = NULL,
    y = "Mean value",
    caption = "Sources: SEZ India operational SEZ list; district-level population density; SHRUG VIIRS night lights."
  ) +
  theme_minimal()

print(part3_bar_plot)

ggsave(
  filename = "Output/Plots/part3_sez_nonsez_bar_plot.png",
  plot = part3_bar_plot,
  width = 9,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# Correlation table: pairwise correlations between key variables
# ------------------------------------------------------------

part3_correlation <- analysis_units %>%
  select(
    zone_indicator,
    num_sezs,
    distance_to_chennai_km,
    coastal_district,
    population_density_per_sq_km,
    viirs_annual_mean
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)

part3_correlation_df <- as.data.frame(part3_correlation) %>%
  rownames_to_column("variable")

print(part3_correlation_df)

write_csv(
  part3_correlation_df,
  "Output/Tables/part3_correlation_table.csv"
)


# ============================================================
# Add notification dates from SEZ India notified SEZ list
# ============================================================
# Source: SEZ India notified SEZ list.
# Treatment timing rule: if multiple dates are listed, use the earliest
# notification date as the proxy for establishment year.
# Some operational SEZs may not appear in the notified list screenshot used here,
# so those remain unmatched and are flagged.

notification_lookup <- tribble(
  ~zone_id, ~notification_date_raw, ~notification_date, ~notification_year, ~notification_date_source, ~notification_date_note,
  
  # Current operational SEZs not clearly visible/matched in the notified-list screenshots
  "TN_SEZ_001", NA_character_, NA_character_, NA_integer_,
  "SEZ India notified SEZ list", "Notification date not matched in current screenshot/source extract",
  
  "TN_SEZ_002", NA_character_, NA_character_, NA_integer_,
  "SEZ India notified SEZ list", "Notification date not matched in current screenshot/source extract",
  
  "TN_SEZ_003", NA_character_, NA_character_, NA_integer_,
  "SEZ India notified SEZ list", "Notification date not matched in current screenshot/source extract",
  
  # Matched Tamil Nadu notified SEZ entries
  "TN_SEZ_004", "25th April 2006 / 1st August 2014", "2006-04-25", 2006,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_005", "17th July 2006", "2006-07-17", 2006,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_006", "11th August 2006", "2006-08-11", 2006,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_007", "11th August 2006", "2006-08-11", 2006,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_008", "31st August 2006", "2006-08-31", 2006,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_009", "28th September 2006 / 24th September 2007 / 9th November 2009 / 10th August 2017 / 28th January 2021", "2006-09-28", 2006,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_010", "9th November 2006 / 17th September 2007 / 28th April 2009", "2006-11-09", 2006,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_011", "16th November 2006 / 19th March 2007 / 2nd December 2008 / 20th February 2009 / 6th December 2023", "2006-11-16", 2006,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_012", "22nd December 2006 / 31st July 2007 / 2nd March 2015 / 17th March 2016 / 1st June 2018 / 5th September 2018 / 3rd December 2018 / 9th April 2019 / 22nd January 2021 / 8th April 2022 / 8th May 2023", "2006-12-22", 2006,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_013", "11th April 2007 / 16th April 2008", "2007-04-11", 2007,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_014", "11th April 2007 / 1st May 2025", "2007-04-11", 2007,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_015", "13th April 2007", "2007-04-13", 2007,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_016", "1st May 2007 / 26th December 2017 / 12th October 2018", "2007-05-01", 2007,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_017", "10th July 2007 / 28th August 2012 / 4th May 2018 / 18th September 2024", "2007-07-10", 2007,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_018", "10th August 2007 / 28th November 2007 / 7th August 2008 / 20th December 2016 / 7th July 2017", "2007-08-10", 2007,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_019", "7th September 2007", "2007-09-07", 2007,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_020", "28th September 2007", "2007-09-28", 2007,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_021", "18th October 2007 / 3rd December 2018 / 10th September 2020 / 21st April 2022 / 20th March 2024", "2007-10-18", 2007,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_022", "23rd November 2007 / 12th September 2024", "2007-11-23", 2007,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_023", "27th November 2007 / 5th January 2016", "2007-11-27", 2007,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_024", "17th December 2007 / 6th April 2016", "2007-12-17", 2007,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_025", "12th February 2008", "2008-02-12", 2008,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_026", "10th March 2008 / 21st May 2009 / 2nd November 2023", "2008-03-10", 2008,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_027", "23rd April 2008 / 26th December 2018 / 17th January 2022 / 5th April 2023", "2008-04-23", 2008,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_028", "30th April 2008", "2008-04-30", 2008,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_029", "30th April 2008 / 4th December 2023", "2008-04-30", 2008,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_030", "15th May 2008 / 16th August 2022", "2008-05-15", 2008,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_031", "9th June 2008 / 30th August 2019", "2008-06-09", 2008,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_032", "16th October 2008", "2008-10-16", 2008,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_033", "18th November 2008 / 27th September 2022", "2008-11-18", 2008,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_034", "23rd January 2009", "2009-01-23", 2009,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_035", "4th December 2009 / 25th August 2010 / 20th October 2010 / 3rd November 2010", "2009-12-04", 2009,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_036", "30th March 2016 / 28th February 2023", "2016-03-30", 2016,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_037", "2nd February 2009", "2009-02-02", 2009,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_038", "16th October 2008", "2008-10-16", 2008,
  "SEZ India notified SEZ list", "Matched to Platinum Holdings Pvt. Ltd.; single notification date listed",
  
  "TN_SEZ_039", "8th June 2009 / 20th December 2011 / 8th September 2022 / 9th June 2025", "2009-06-08", 2009,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_040", "12th February 2008 / 1st April 2019", "2008-02-12", 2008,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_041", "16th January 2020", "2020-01-16", 2020,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_042", "26th March 2020", "2020-03-26", 2020,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_043", "31st March 2017", "2017-03-31", 2017,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_044", "4th May 2009 / 19th July 2024", "2009-05-04", 2009,
  "SEZ India notified SEZ list", "Multiple dates listed; earliest notification date used",
  
  "TN_SEZ_045", "30th April 2008", "2008-04-30", 2008,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_046", "12th May 2020", "2020-05-12", 2020,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_047", "28th February 2019", "2019-02-28", 2019,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_048", "9th December 2019", "2019-12-09", 2019,
  "SEZ India notified SEZ list", "Single notification date listed",
  
  "TN_SEZ_049", "30th June 2022", "2022-06-30", 2022,
  "SEZ India notified SEZ list", "Single notification date listed"
)

zones <- zones %>%
  select(
    -any_of(c(
      "notification_date_raw",
      "notification_date",
      "notification_year",
      "notification_date_source",
      "notification_date_note"
    ))
  ) %>%
  left_join(notification_lookup, by = "zone_id") %>%
  mutate(
    notification_date = as.Date(notification_date),
    notification_year = as.integer(notification_year),
    notification_date_source = replace_na(
      notification_date_source,
      "Not matched to notified SEZ list"
    ),
    notification_date_note = replace_na(
      notification_date_note,
      "Notification date not verified"
    )
  )

write_csv(zones, "Data/processed/zones.csv")

# ============================================================
# Part 4: Dynamic and causal extension
# Descriptive district-year night lights comparison
# ============================================================

# ------------------------------------------------------------
# Build Tamil Nadu district-year night lights panel
# ------------------------------------------------------------
# Source: SHRUG VIIRS night lights
# Unit: 2011 Census district-year
# Category: average-masked

nightlights_tn_panel <- nightlights_raw %>%
  mutate(
    pc11_state_id = as.integer(pc11_state_id),
    pc11_district_id = as.integer(pc11_district_id)
  ) %>%
  filter(
    pc11_state_id == 33,
    category == "average-masked"
  ) %>%
  transmute(
    pc11_state_id,
    pc11_district_id,
    year,
    nightlights_category = category,
    viirs_annual_mean,
    viirs_annual_sum,
    viirs_annual_num_cells
  )

# Quick check
nightlights_tn_panel %>%
  summarise(
    n_rows = n(),
    n_2011_districts = n_distinct(pc11_district_id),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE)
  ) %>%
  print()


# ------------------------------------------------------------
# Standardize district names before matching
# ------------------------------------------------------------
# This prevents spelling issues such as Tiruvallur/Thiruvallur.

tn_2011_district_crosswalk <- tn_2011_district_crosswalk %>%
  mutate(
    district_clean = str_squish(district_clean),
    district_clean = case_when(
      district_clean == "Thiruvallur" ~ "Tiruvallur",
      district_clean == "Thiruvarur" ~ "Tiruvarur",
      district_clean == "Thoothukkudi" ~ "Thoothukudi",
      district_clean == "The Nilgiris" ~ "Nilgiris",
      TRUE ~ district_clean
    )
  )

analysis_units <- analysis_units %>%
  mutate(
    district_clean = str_squish(district_clean),
    district_clean = case_when(
      district_clean == "Thiruvallur" ~ "Tiruvallur",
      district_clean == "Thiruvarur" ~ "Tiruvarur",
      district_clean == "Thoothukkudi" ~ "Thoothukudi",
      district_clean == "The Nilgiris" ~ "Nilgiris",
      TRUE ~ district_clean
    )
  )


# ------------------------------------------------------------
# Create 2011-district-level SEZ status
# ------------------------------------------------------------
# SHRUG VIIRS district data use 2011 Census districts.
# Some current Tamil Nadu districts map back to the same 2011 parent district.
# Therefore, we aggregate SEZ status to the 2011 district level.

sez_status_2011 <- tn_2011_district_crosswalk %>%
  left_join(
    analysis_units %>%
      select(
        district_clean,
        zone_indicator,
        num_sezs,
        distance_to_chennai_km,
        coastal_district,
        population_density_per_sq_km
      ),
    by = "district_clean"
  ) %>%
  group_by(pc11_state_id, pc11_district_id, pc11_district_name) %>%
  summarise(
    zone_indicator = as.integer(any(zone_indicator == 1, na.rm = TRUE)),
    num_sezs = sum(num_sezs, na.rm = TRUE),
    mean_distance_to_chennai_km = mean(distance_to_chennai_km, na.rm = TRUE),
    coastal_district = as.integer(any(coastal_district == 1, na.rm = TRUE)),
    mean_population_density_per_sq_km = mean(population_density_per_sq_km, na.rm = TRUE),
    current_districts_mapped = paste(unique(district_clean), collapse = "; "),
    .groups = "drop"
  ) %>%
  mutate(
    zone_indicator = replace_na(zone_indicator, 0L),
    num_sezs = replace_na(num_sezs, 0)
  )

write_csv(
  sez_status_2011,
  "Data/processed/sez_status_2011_districts.csv"
)

# Check SEZ vs non-SEZ 2011 districts
sez_status_2011 %>%
  count(zone_indicator) %>%
  print()


# ------------------------------------------------------------
# 4. Create district-year panel by merging SEZ status with night lights
# ------------------------------------------------------------

district_year_panel <- nightlights_tn_panel %>%
  left_join(
    sez_status_2011,
    by = c("pc11_state_id", "pc11_district_id")
  ) %>%
  mutate(
    zone_indicator = replace_na(zone_indicator, 0L),
    num_sezs = replace_na(num_sezs, 0),
    district_type = if_else(
      zone_indicator == 1,
      "SEZ district",
      "Non-SEZ district"
    ),
    log_viirs_mean = log1p(viirs_annual_mean)
  )

write_csv(
  district_year_panel,
  "Data/processed/district_year_nightlights_panel.csv"
)

# Check for missing district type
district_year_panel %>%
  summarise(
    n_rows = n(),
    missing_district_type = sum(is.na(district_type)),
    n_2011_districts = n_distinct(pc11_district_id),
    n_sez_2011_districts = n_distinct(pc11_district_id[zone_indicator == 1]),
    n_nonsez_2011_districts = n_distinct(pc11_district_id[zone_indicator == 0])
  ) %>%
  print()


# ------------------------------------------------------------
# 5. Compare night lights over time by SEZ status
# ------------------------------------------------------------

nightlights_trends <- district_year_panel %>%
  group_by(year, district_type) %>%
  summarise(
    n_districts = n_distinct(pc11_district_id),
    mean_viirs_annual_mean = round(mean(viirs_annual_mean, na.rm = TRUE), 3),
    median_viirs_annual_mean = round(median(viirs_annual_mean, na.rm = TRUE), 3),
    mean_log_viirs = round(mean(log_viirs_mean, na.rm = TRUE), 3),
    .groups = "drop"
  )

write_csv(
  nightlights_trends,
  "Output/Tables/part4_nightlights_trends_by_sez_status.csv"
)

print(nightlights_trends)


# ------------------------------------------------------------
# 6. Plot night lights trends for SEZ and non-SEZ districts
# ------------------------------------------------------------

part4_nightlights_trend_plot <- ggplot(
  nightlights_trends,
  aes(
    x = year,
    y = mean_log_viirs,
    color = district_type,
    group = district_type
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Night Lights Trends in SEZ and Non-SEZ Districts",
    subtitle = "Tamil Nadu 2011 district-year comparison using SHRUG VIIRS night lights",
    x = "Year",
    y = "Mean log(1 + VIIRS night lights)",
    color = "District type",
    caption = "Sources: SEZ India operational SEZ list; SHRUG VIIRS night lights."
  ) +
  theme_minimal()

print(part4_nightlights_trend_plot)

ggsave(
  filename = "Output/Plots/part4_nightlights_trends_by_sez_status.png",
  plot = part4_nightlights_trend_plot,
  width = 8,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# Descriptive regression, not causal
# ------------------------------------------------------------
# This is only descriptive. It compares SEZ and non-SEZ districts
# controlling for year fixed effects. It should not be interpreted causally.

part4_descriptive_model <- feols(
  log_viirs_mean ~ zone_indicator + factor(year),
  data = district_year_panel,
  cluster = ~pc11_district_id
)

summary(part4_descriptive_model)
