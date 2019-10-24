library(tidyverse)
#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#CCCCCCCCCCCCCCCCC       Clean data       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
# Clean data for NT
# First read in Result2.txt which contains the information for all houses collected from the state 
overheat_result2 <- read_fwf("data/NT/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
        StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
        CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 

# Second read in Result2_error.txt which contains the information for all houses collected from the state
# which has issues in the data 
overheat_result2_error <- read_fwf("data/NT/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
        StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
        CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 
overheat_result2
overheat_result2_error

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

# Now read in the performance data for each house using the existing window model in the state
overheatcsv_orig <- read.csv("data/NT/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

# Clean the performance data just contain those house which do not have issues.
overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NT/Result2_Orig_Clean.csv")

walltype <- overheat_result2 %>% semi_join(validRow, by = "Nvalid") %>%
  select(Nvalid,CZ,Class,WallType) %>% 
  write_csv("res/NT/walltype.csv")

# Now join the overheatorig_clean, and walltype
overheat_houses <- overheatorig_clean %>% inner_join(walltype, by = "Nvalid") %>% 
  mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 
  write_csv("res/NT/Result2_Houses.csv")

overheat_houses_error <- filter(overheat_houses, CZ != NClimateZone ) %>% 
  write_csv("res/NT/error.csv") 
# Assign to a NT data form
overheat_houses_NT <- overheat_houses
overheat_houses_NT

# Clean data for ACT
# First read in Result2.txt which contains the information for all houses collected from the state 
overheat_result2 <- read_fwf("data/ACT/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                            StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                            CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 

# Second read in Result2_error.txt which contains the information for all houses collected from the state
# which has issues in the data 
overheat_result2_error <- read_fwf("data/ACT/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                        StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                        CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 
overheat_result2
overheat_result2_error

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

# Now read in the performance data for each house using the existing window model in the state
overheatcsv_orig <- read.csv("data/ACT/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

# Clean the performance data just contain those house which do not have issues.
overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/ACT/Result2_Orig_Clean.csv")

walltype <- overheat_result2 %>% semi_join(validRow, by = "Nvalid") %>%
  select(Nvalid,CZ,Class,WallType) %>% 
  write_csv("res/ACT/walltype.csv")

# Now join the overheatorig_clean, and walltype
overheat_houses <- overheatorig_clean %>% inner_join(walltype, by = "Nvalid") %>% 
  mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 
  write_csv("res/ACT/Result2_Houses.csv")

overheat_houses_error <- filter(overheat_houses, CZ != NClimateZone ) %>% 
  write_csv("res/ACT/error.csv") 
# Assign to a NT data form
overheat_houses_ACT <- overheat_houses
overheat_houses_ACT

# Clean data for TAS
# First read in Result2.txt which contains the information for all houses collected from the state 
overheat_result2 <- read_fwf("data/TAS/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 

# Second read in Result2_error.txt which contains the information for all houses collected from the state
# which has issues in the data 
overheat_result2_error <- read_fwf("data/TAS/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 
overheat_result2
overheat_result2_error

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

# Now read in the performance data for each house using the existing window model in the state
overheatcsv_orig <- read.csv("data/TAS/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

# Clean the performance data just contain those house which do not have issues.
overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/TAS/Result2_Orig_Clean.csv")

walltype <- overheat_result2 %>% semi_join(validRow, by = "Nvalid") %>%
  select(Nvalid,CZ,Class,WallType) %>% 
  write_csv("res/TAS/walltype.csv")

# Now join the overheatorig_clean, and walltype
overheat_houses <- overheatorig_clean %>% inner_join(walltype, by = "Nvalid") %>% 
  mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 
  write_csv("res/TAS/Result2_Houses.csv")

overheat_houses_error <- filter(overheat_houses, CZ != NClimateZone ) %>% 
  write_csv("res/TAS/error.csv") 
# Assign to a NT data form
overheat_houses_TAS <- overheat_houses
overheat_houses_TAS

# Clean data for SA
# First read in Result2.txt which contains the information for all houses collected from the state 
overheat_result2 <- read_fwf("data/SA/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 

# Second read in Result2_error.txt which contains the information for all houses collected from the state
# which has issues in the data 
overheat_result2_error <- read_fwf("data/SA/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 
overheat_result2
overheat_result2_error

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

# Now read in the performance data for each house using the existing window model in the state
overheatcsv_orig <- read.csv("data/SA/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

# Clean the performance data just contain those house which do not have issues.
overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/SA/Result2_Orig_Clean.csv")

walltype <- overheat_result2 %>% semi_join(validRow, by = "Nvalid") %>%
  select(Nvalid,CZ,Class,WallType) %>% 
  write_csv("res/SA/walltype.csv")

# Now join the overheatorig_clean, and walltype
overheat_houses <- overheatorig_clean %>% inner_join(walltype, by = "Nvalid") %>% 
  mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 
  write_csv("res/SA/Result2_Houses.csv")

overheat_houses_error <- filter(overheat_houses, CZ != NClimateZone ) %>% 
  write_csv("res/SA/error.csv") 
# Assign to a NT data form
overheat_houses_SA <- overheat_houses
overheat_houses_SA

# Clean data for WA
# First read in Result2.txt which contains the information for all houses collected from the state 
overheat_result2 <- read_fwf("data/WA/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 

# Second read in Result2_error.txt which contains the information for all houses collected from the state
# which has issues in the data 
overheat_result2_error <- read_fwf("data/WA/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 
overheat_result2
overheat_result2_error

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

# Now read in the performance data for each house using the existing window model in the state
overheatcsv_orig <- read.csv("data/WA/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

# Clean the performance data just contain those house which do not have issues.
overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/WA/Result2_Orig_Clean.csv")

walltype <- overheat_result2 %>% semi_join(validRow, by = "Nvalid") %>%
  select(Nvalid,CZ,Class,WallType) %>% 
  write_csv("res/WA/walltype.csv")

# Now join the overheatorig_clean, and walltype
overheat_houses <- overheatorig_clean %>% inner_join(walltype, by = "Nvalid") %>% 
  mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 
  write_csv("res/WA/Result2_Houses.csv")

overheat_houses_error <- filter(overheat_houses, CZ != NClimateZone ) %>% 
  write_csv("res/WA/error.csv") 
# Assign to a NT data form
overheat_houses_WA <- overheat_houses
overheat_houses_WA

# Clean data for QLD
# First read in Result2.txt which contains the information for all houses collected from the state 
overheat_result2 <- read_fwf("data/QLD/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 

# Second read in Result2_error.txt which contains the information for all houses collected from the state
# which has issues in the data 
overheat_result2_error <- read_fwf("data/QLD/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 
overheat_result2
overheat_result2_error

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

# Now read in the performance data for each house using the existing window model in the state
overheatcsv_orig <- read.csv("data/QLD/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

# Clean the performance data just contain those house which do not have issues.
overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/QLD/Result2_Orig_Clean.csv")

walltype <- overheat_result2 %>% semi_join(validRow, by = "Nvalid") %>%
  select(Nvalid,CZ,Class,WallType) %>% 
  write_csv("res/QLD/walltype.csv")

# Now join the overheatorig_clean, and walltype
overheat_houses <- overheatorig_clean %>% inner_join(walltype, by = "Nvalid") %>% 
  mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 
  write_csv("res/QLD/Result2_Houses.csv")

overheat_houses_error <- filter(overheat_houses, CZ != NClimateZone ) %>% 
  write_csv("res/QLD/error.csv") 
# Assign to a NT data form
overheat_houses_QLD <- overheat_houses
overheat_houses_QLD

# Clean data for VIC
# First read in Result2.txt which contains the information for all houses collected from the state 
overheat_result2 <- read_fwf("data/VIC/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 

# Second read in Result2_error.txt which contains the information for all houses collected from the state
# which has issues in the data 
overheat_result2_error <- read_fwf("data/VIC/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 
overheat_result2
overheat_result2_error

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

# Now read in the performance data for each house using the existing window model in the state
overheatcsv_orig <- read.csv("data/VIC/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

# Clean the performance data just contain those house which do not have issues.
overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/VIC/Result2_Orig_Clean.csv")


overheatorig_clean2 <- overheatorig_clean %>% filter(StarRating != "*****",StarRating != "0" ) %>%
  filter(StarRating != "0" ) %>%
  mutate(StarRating = as.numeric(StarRating), CertificateLCool = as.numeric(CertificateLCool)) %>% 
  mutate(CertificateSCool = as.numeric(CertificateSCool)) %>%
  mutate(CertificateHeating = as.numeric(CertificateHeating)) %>%  
  mutate(NumberofBedrooms = as.numeric(NumberofBedrooms)) %>%    
  filter(StarRating > 0.1 ) %>%  
  write_csv("res/VIC/Result2_Orig_Clean.csv")

overheatorig_clean <- overheatorig_clean2 %>%  
  write_csv("res/VIC/Result2_Orig_Clean.csv")


walltype <- overheat_result2 %>% semi_join(validRow, by = "Nvalid") %>%
  select(Nvalid,CZ,Class,WallType) %>% 
  write_csv("res/VIC/walltype.csv")

# Now join the overheatorig_clean, and walltype
overheat_houses <- overheatorig_clean %>% inner_join(walltype, by = "Nvalid") %>% 
  mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 
  write_csv("res/VIC/Result2_Houses.csv")

overheat_houses_error <- filter(overheat_houses, CZ != NClimateZone ) %>% 
  write_csv("res/VIC/error.csv") 
# Assign to a NT data form
overheat_houses_VIC <- overheat_houses
overheat_houses_VIC

# Clean data for NSW
# First read in Result2.txt which contains the information for all houses collected from the state 
overheat_result2 <- read_fwf("data/NSW/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                             StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                             CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 

# Second read in Result2_error.txt which contains the information for all houses collected from the state
# which has issues in the data 
overheat_result2_error <- read_fwf("data/NSW/Result2_error.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 
overheat_result2
overheat_result2_error

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% select("Nvalid")
validRow

# Now read in the performance data for each house using the existing window model in the state
overheatcsv_orig <- read.csv("data/NSW/Result2_Orig.csv", header = FALSE, col.names = c(
  "Nvalid", "StateName", "NPostCode", "NYear", "NClimateZone", "StarRating", 
  "Exposure","X", "CertificateHeating", "CertificateSCool", "CertificateLCool","TotalFlArea", 
  "TotFloorArea", "SlabOnGroundArea", "FloorsAboveGround", "SubfloorFloorArea",
  "FloorsAboveNeighbours_100","CeilingsBelowNeighbours","TotalSharedSurfaceArea",
  "FloorHeightmin","FloorHeightmax","NumberofLiving","NumberofBedrooms","MStorey", "NStorey"
))
overheatcsv_orig

# Clean the performance data just contain those house which do not have issues.
overheatorig_clean <- overheatcsv_orig %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NSW/Result2_Orig_Clean.csv")


overheatorig_clean2 <- overheatorig_clean %>% filter(StarRating != "*****",StarRating != "0" ) %>%
  filter(StarRating != "0" ) %>% 
  mutate(StarRating = as.numeric(StarRating), CertificateLCool = as.numeric(CertificateLCool)) %>% 
  mutate(CertificateSCool = as.numeric(CertificateSCool)) %>%
  mutate(CertificateHeating = as.numeric(CertificateHeating)) %>%  
  mutate(NumberofBedrooms = as.numeric(NumberofBedrooms)) %>%   
  filter(NumberofBedrooms != "NA" ) %>%   
  filter(StarRating > 0.1 ) %>%  filter(StarRating < 10.1 ) %>% 
  write_csv("res/NSW/Result2_Orig_Clean.csv")

overheatorig_clean <- overheatorig_clean2 %>%  
  write_csv("res/NSW/Result2_Orig_Clean.csv")


walltype <- overheat_result2 %>% semi_join(validRow, by = "Nvalid") %>%
  select(Nvalid,CZ,Class,WallType) %>% 
  write_csv("res/NSW/walltype.csv")

# Now join the overheatorig_clean, and walltype
overheat_houses <- overheatorig_clean %>% inner_join(walltype, by = "Nvalid") %>% 
  mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 
  write_csv("res/NSW/Result2_Houses.csv")

overheat_houses_error <- filter(overheat_houses, CZ != NClimateZone ) %>% 
  write_csv("res/NSW/error.csv") 
# Assign to a NT data form
overheat_houses_NSW <- overheat_houses
overheat_houses_NSW
#view(overheat_houses_VIC)
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#CCCCCCCCCCCCCCCCCCCC     Merge clean data  ccccccccccccccccccccccccccccccccccccccccccccccccccccc


bindAll <- bind_rows(overheat_houses_ACT, overheat_houses_NSW,
                     overheat_houses_NT, overheat_houses_QLD, 
                     overheat_houses_SA, overheat_houses_TAS,
                     overheat_houses_VIC, overheat_houses_WA) 
#Now combine with BCA climate zone
BCA_CZs <- read.csv("data/BCA_NatHERS_CZs.csv")
BCA_CZs
#view(BCA_CZs)
bindAll2 <- bindAll %>% inner_join(BCA_CZs, by = "CZ")
bindAll <- bindAll2

bindAll
bindAll_Houses <- bindAll %>% 
  write_csv("res/Result2_AllState.csv")

#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#CCCCCCCCCCCCCCCCCCCC          PLOT         ccccccccccccccccccccccccccccccccccccccccccccccccccccc

#Dwelling number in Climatezone 
bindAll_Houses
by_Climate1 <- group_by(bindAll_Houses, NClimateZone) %>%
  summarise(DwellingNo = n()) %>% 
  filter(DwellingNo > 29)
by_Climate2 <- by_Climate1 %>% inner_join(BCA_CZs, by = c("NClimateZone" = "CZ"))
by_Climate <- by_Climate2 %>% 
  write_csv("res/By_ClimateZone.csv")
by_Climate
ClimateZoneSelected <- select(by_Climate,NClimateZone)
#view(by_Climate)


#Dwelling number in States 
library(dplyr)
by_state <- bindAll_Houses %>% semi_join(ClimateZoneSelected, by = "NClimateZone") %>% 
  group_by(StateName) %>%  
  summarise(DwellingNo = n() ) %>% 
  write_csv("res/By_State.csv")
by_state

# Using ggplot2
figure1 <- ggplot(
  data = by_state, 
  mapping = aes(x = StateName, y = DwellingNo,  label = DwellingNo,
                col.lab="red", cex.axis = 3, cex.lab = 4)
) +
  geom_col(fill = "green", width = 0.5) +
  geom_text(aes(label = DwellingNo), colour ="blue", vjust = -0.5, fontface = "bold") +
  labs(title = "Figure 1. Number of dwellings simmulated in each State",
       x = "State",
       y = "Number of Dwellings"
  ) +
  ylim(0, 23000) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))
figure1  
ggsave("fig/Figure_1.png", plot = figure1)

?sort


# Using ggplot2
figure2 <- ggplot(
  data = by_Climate, 
  mapping = aes(x = NClimateZone, y = DwellingNo,  label = BCA_CZ,
                col.lab="red", cex.axis = 3, cex.lab = 4)
) +
  geom_col(fill = "green", width = 0.5) +
  geom_text(aes(label = BCA_CZ), colour = "red", vjust = -0.5, fontface = "bold") +
  labs(title = "Figure 2. Number of dwellings simmulated in climate zone",
       x = "State",
       y = "Number of Dwellings"
  ) +
  ylim(0, 13000) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))
figure2
ggsave("fig/Figure_2.png", plot = figure2)


#Dwelling number in Climatezone and construction
bindAll_Houses
by_walltype <- bindAll_Houses %>% semi_join(ClimateZoneSelected, by = "NClimateZone") %>% 
  group_by(NClimateZone,WallType) %>%
  summarise(DwellingNo = n()) 
by_walltype2 <- by_walltype %>% inner_join(BCA_CZs, by = c("NClimateZone" = "CZ"))
by_walltype <- by_walltype2 %>% 
  write_csv("res/By_Walltype.csv")
by_walltype
#view(by_walltype)

#Dwelling number in Climatezone and Building Class
bindAll_Houses
by_Class <- bindAll_Houses %>% semi_join(ClimateZoneSelected, by = "NClimateZone") %>%
  group_by(NClimateZone,Class) %>%
  summarise(DwellingNo = n()) 
by_Class2 <- by_Class %>% inner_join(BCA_CZs, by = c("NClimateZone" = "CZ"))
by_Class <- by_Class2 %>% 
  write_csv("res/By_BuildingClass.csv")
by_Class
#view(by_Class)

#Dwelling number in State and Star rating
bindAll_Houses
by_StarRating <- bindAll_Houses %>% semi_join(ClimateZoneSelected, by = "NClimateZone") %>%
  group_by(StateName,StarRating) %>%
  summarise(DwellingNo = n()) %>% 
  write_csv("res/By_StarRating.csv")
by_StarRating
by_StarRating2 <-  subset(by_StarRating,select =StarRating)
by_StarRating2
by_StarRating2 <- format(round(by_StarRating2,1)) %>% 
  write_csv("res/By_StarRating2.csv")


by_StarRating
#view(by_StarRating)

