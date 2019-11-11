library(tidyverse)
#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#CCCCCCCCCCCCCCCCC       Clean data       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
# Clean data for NT
# First read in Result2.txt which contains the information for all houses collected from the state 
overheat_result2 <- read_fwf("data/NT/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
        StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
        CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 
?read_fwf

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

# Now read in the performance data for each house for star rating information in the state
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

##############################################################################
# now read in simulation results for Overheating in houses
overheatcsv_rst3 <- read.csv("data/NT/Result3.csv", header = FALSE, col.names = c(
  "Nvalid", "WFR", "Overheat", "Kit", 
  "LivR01", "LivR02", "LivR03","LivR04","LivR05",
  "LivR06", "LivR07", "LivR08","LivR09","LivR10",
  "BedRD01", "BedRD02", "BedRD03","BedRD04","BedRD05",
  "BedRD06", "BedRD07", "BedRD08","BedRD09","BedRD10",
  "BedRN01", "BedRN02", "BedRN03","BedRN04","BedRN05",
  "BedRN06", "BedRN07", "BedRN08","BedRN09","BedRN10"  
))
overheatcsv_rst3

overheatcsv_rst3B <- read.csv("data/NT/Result3B.csv", header = FALSE, col.names = c(
  "Nvalid", "WFRB", "OverheatB", "KitB", 
  "LivR01B", "LivR02B", "LivR03B","LivR04B","LivR05B",
  "LivR06B", "LivR07B", "LivR08B","LivR09B","LivR10B",
  "BedRD01B", "BedRD02B", "BedRD03B","BedRD04B","BedRD05B",
  "BedRD06B", "BedRD07B", "BedRD08B","BedRD09B","BedRD10B",
  "BedRN01B", "BedRN02B", "BedRN03B","BedRN04B","BedRN05B",
  "BedRN06B", "BedRN07B", "BedRN08B","BedRN09B","BedRN10B"  
))
overheatcsv_rst3B

overheatcsv_rst3_clean <- overheatcsv_rst3 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NT/Result3_Clean.csv")
overheatcsv_rst3B_clean <- overheatcsv_rst3B %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NT/Result3B_Clean.csv")

# now read in houses insulation and direction information
overheatcsv_rst4 <- read.csv("data/NT/Result4.csv", header = FALSE, col.names = c(
  "Nvalid", "WallR", "CeilingR", "RoofR", "KitWD",
  "LivWD01", "LivWD02", "LivWD03","LivWD04","LivWD05",
  "LivWD06", "LivWD07", "LivWD08","LivWD09","LivWD10",
  "BedWD01", "BedWD02", "BedWD03","BedWD04","BedWD05",
  "BedWD06", "BedWD07", "BedWD08","BedWD09","BedWD10"
))

overheatcsv_rst4
overheatcsv_rst4_clean <- overheatcsv_rst4 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NT/Result4_Clean.csv")

# Merge the data
overheat_houses_NT
overheatcsv_rst3
overheatcsv_rst3B
overheatcsv_rst4
overheat_houses_join <- overheat_houses_NT %>% 
  inner_join(overheatcsv_rst3, by = "Nvalid") %>%
  inner_join(overheatcsv_rst3B, by = "Nvalid") %>%
  inner_join(overheatcsv_rst4,  by = "Nvalid") %>%
  write_csv("res/NT/Result2_Houses_join.csv")

overheat_houses_join_NT <- overheat_houses_join
overheat_houses_join_NT
#-------------------------------------------------------------------------#



#mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 

# now read in overheating for houses using different temperature criteria
overheatcsv_cool01 <- read.csv("data/NT/Resultcool01.csv", header = FALSE, col.names = c(
  "Nvalid", "tJanNeutral", "OverDayH", "OverNightH","Kit", 
  "LivR01", "LivR02", "LivR03","LivR04","LivR05",
  "LivR06", "LivR07", "LivR08","LivR09","LivR10",
  "BedRD01", "BedRD02", "BedRD03","BedRD04","BedRD05",
  "BedRD06", "BedRD07", "BedRD08","BedRD09","BedRD10",
  "BedRN01", "BedRN02", "BedRN03","BedRN04","BedRN05",
  "BedRN06", "BedRN07", "BedRN08","BedRN09","BedRN10"  
))

overheatcsv_cool01
overheatcsv_cool01_clean <- overheatcsv_cool01 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NT/Resultcool01_Clean.csv")

# now read in overheatnig for houses using different temperature criteria
overheatcsv_cool02 <- read.csv("data/NT/Resultcool02.csv", header = FALSE, col.names = c(
  "Nvalid", "tJanNeutral", "OverDayH", "OverNightH","Kit", 
  "LivR01", "LivR02", "LivR03","LivR04","LivR05",
  "LivR06", "LivR07", "LivR08","LivR09","LivR10",
  "BedRD01", "BedRD02", "BedRD03","BedRD04","BedRD05",
  "BedRD06", "BedRD07", "BedRD08","BedRD09","BedRD10",
  "BedRN01", "BedRN02", "BedRN03","BedRN04","BedRN05",
  "BedRN06", "BedRN07", "BedRN08","BedRN09","BedRN10"  
))

overheatcsv_cool02
overheatcsv_cool02_clean <- overheatcsv_cool02 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NT/Resultcool02_Clean.csv")





##################################################################



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
# Assign to a ACT data form
overheat_houses_ACT <- overheat_houses
overheat_houses_ACT
##############################################################################
# now read in simulation results for Overheating in houses
overheatcsv_rst3 <- read.csv("data/ACT/Result3.csv", header = FALSE, col.names = c(
  "Nvalid", "WFR", "Overheat", "Kit", 
  "LivR01", "LivR02", "LivR03","LivR04","LivR05",
  "LivR06", "LivR07", "LivR08","LivR09","LivR10",
  "BedRD01", "BedRD02", "BedRD03","BedRD04","BedRD05",
  "BedRD06", "BedRD07", "BedRD08","BedRD09","BedRD10",
  "BedRN01", "BedRN02", "BedRN03","BedRN04","BedRN05",
  "BedRN06", "BedRN07", "BedRN08","BedRN09","BedRN10"  
))
overheatcsv_rst3

overheatcsv_rst3B <- read.csv("data/ACT/Result3B.csv", header = FALSE, col.names = c(
  "Nvalid", "WFRB", "OverheatB", "KitB", 
  "LivR01B", "LivR02B", "LivR03B","LivR04B","LivR05B",
  "LivR06B", "LivR07B", "LivR08B","LivR09B","LivR10B",
  "BedRD01B", "BedRD02B", "BedRD03B","BedRD04B","BedRD05B",
  "BedRD06B", "BedRD07B", "BedRD08B","BedRD09B","BedRD10B",
  "BedRN01B", "BedRN02B", "BedRN03B","BedRN04B","BedRN05B",
  "BedRN06B", "BedRN07B", "BedRN08B","BedRN09B","BedRN10B"  
))
overheatcsv_rst3B

overheatcsv_rst3_clean <- overheatcsv_rst3 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/ACT/Result3_Clean.csv")
overheatcsv_rst3B_clean <- overheatcsv_rst3B %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/ACT/Result3B_Clean.csv")

# now read in houses insulation and direction information
overheatcsv_rst4 <- read.csv("data/ACT/Result4.csv", header = FALSE, col.names = c(
  "Nvalid", "WallR", "CeilingR", "RoofR", "KitWD",
  "LivWD01", "LivWD02", "LivWD03","LivWD04","LivWD05",
  "LivWD06", "LivWD07", "LivWD08","LivWD09","LivWD10",
  "BedWD01", "BedWD02", "BedWD03","BedWD04","BedWD05",
  "BedWD06", "BedWD07", "BedWD08","BedWD09","BedWD10"
))

overheatcsv_rst4
overheatcsv_rst4_clean <- overheatcsv_rst4 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/ACT/Result4_Clean.csv")

# Merge the data
overheat_houses_ACT
overheatcsv_rst3
overheatcsv_rst3B
overheatcsv_rst4
overheat_houses_join <- overheat_houses_ACT %>% 
  inner_join(overheatcsv_rst3, by = "Nvalid") %>%
  inner_join(overheatcsv_rst3B, by = "Nvalid") %>%
  inner_join(overheatcsv_rst4,  by = "Nvalid") %>%
  write_csv("res/ACT/Result2_Houses_join.csv")

overheat_houses_join_ACT <- overheat_houses_join
overheat_houses_join_ACT
#-------------------------------------------------------------------------#


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
# Assign to a TAS data form
overheat_houses_TAS <- overheat_houses
overheat_houses_TAS
##############################################################################
# now read in simulation results for Overheating in houses
overheatcsv_rst3 <- read.csv("data/TAS/Result3.csv", header = FALSE, col.names = c(
  "Nvalid", "WFR", "Overheat", "Kit", 
  "LivR01", "LivR02", "LivR03","LivR04","LivR05",
  "LivR06", "LivR07", "LivR08","LivR09","LivR10",
  "BedRD01", "BedRD02", "BedRD03","BedRD04","BedRD05",
  "BedRD06", "BedRD07", "BedRD08","BedRD09","BedRD10",
  "BedRN01", "BedRN02", "BedRN03","BedRN04","BedRN05",
  "BedRN06", "BedRN07", "BedRN08","BedRN09","BedRN10"  
))
overheatcsv_rst3

overheatcsv_rst3B <- read.csv("data/TAS/Result3B.csv", header = FALSE, col.names = c(
  "Nvalid", "WFRB", "OverheatB", "KitB", 
  "LivR01B", "LivR02B", "LivR03B","LivR04B","LivR05B",
  "LivR06B", "LivR07B", "LivR08B","LivR09B","LivR10B",
  "BedRD01B", "BedRD02B", "BedRD03B","BedRD04B","BedRD05B",
  "BedRD06B", "BedRD07B", "BedRD08B","BedRD09B","BedRD10B",
  "BedRN01B", "BedRN02B", "BedRN03B","BedRN04B","BedRN05B",
  "BedRN06B", "BedRN07B", "BedRN08B","BedRN09B","BedRN10B"  
))
overheatcsv_rst3B

overheatcsv_rst3_clean <- overheatcsv_rst3 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/TAS/Result3_Clean.csv")
overheatcsv_rst3B_clean <- overheatcsv_rst3B %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/TAS/Result3B_Clean.csv")

# now read in houses insulation and direction information
overheatcsv_rst4 <- read.csv("data/TAS/Result4.csv", header = FALSE, col.names = c(
  "Nvalid", "WallR", "CeilingR", "RoofR", "KitWD",
  "LivWD01", "LivWD02", "LivWD03","LivWD04","LivWD05",
  "LivWD06", "LivWD07", "LivWD08","LivWD09","LivWD10",
  "BedWD01", "BedWD02", "BedWD03","BedWD04","BedWD05",
  "BedWD06", "BedWD07", "BedWD08","BedWD09","BedWD10"
))

overheatcsv_rst4
overheatcsv_rst4_clean <- overheatcsv_rst4 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/TAS/Result4_Clean.csv")

# Merge the data
overheat_houses_TAS
overheatcsv_rst3
overheatcsv_rst3B
overheatcsv_rst4
overheat_houses_join <- overheat_houses_TAS %>% 
  inner_join(overheatcsv_rst3, by = "Nvalid") %>%
  inner_join(overheatcsv_rst3B, by = "Nvalid") %>%
  inner_join(overheatcsv_rst4,  by = "Nvalid") %>%
  write_csv("res/TAS/Result2_Houses_join.csv")

overheat_houses_join_TAS <- overheat_houses_join
overheat_houses_join_TAS
#-------------------------------------------------------------------------#

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
# Assign to a SA data form
overheat_houses_SA <- overheat_houses
overheat_houses_SA

##############################################################################
# now read in simulation results for Overheating in houses
overheatcsv_rst3 <- read.csv("data/SA/Result3.csv", header = FALSE, col.names = c(
  "Nvalid", "WFR", "Overheat", "Kit", 
  "LivR01", "LivR02", "LivR03","LivR04","LivR05",
  "LivR06", "LivR07", "LivR08","LivR09","LivR10",
  "BedRD01", "BedRD02", "BedRD03","BedRD04","BedRD05",
  "BedRD06", "BedRD07", "BedRD08","BedRD09","BedRD10",
  "BedRN01", "BedRN02", "BedRN03","BedRN04","BedRN05",
  "BedRN06", "BedRN07", "BedRN08","BedRN09","BedRN10"  
))
overheatcsv_rst3

overheatcsv_rst3B <- read.csv("data/SA/Result3B.csv", header = FALSE, col.names = c(
  "Nvalid", "WFRB", "OverheatB", "KitB", 
  "LivR01B", "LivR02B", "LivR03B","LivR04B","LivR05B",
  "LivR06B", "LivR07B", "LivR08B","LivR09B","LivR10B",
  "BedRD01B", "BedRD02B", "BedRD03B","BedRD04B","BedRD05B",
  "BedRD06B", "BedRD07B", "BedRD08B","BedRD09B","BedRD10B",
  "BedRN01B", "BedRN02B", "BedRN03B","BedRN04B","BedRN05B",
  "BedRN06B", "BedRN07B", "BedRN08B","BedRN09B","BedRN10B"  
))
overheatcsv_rst3B

overheatcsv_rst3_clean <- overheatcsv_rst3 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/SA/Result3_Clean.csv")
overheatcsv_rst3B_clean <- overheatcsv_rst3B %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/SA/Result3B_Clean.csv")

# now read in houses insulation and direction information
overheatcsv_rst4 <- read.csv("data/SA/Result4.csv", header = FALSE, col.names = c(
  "Nvalid", "WallR", "CeilingR", "RoofR", "KitWD",
  "LivWD01", "LivWD02", "LivWD03","LivWD04","LivWD05",
  "LivWD06", "LivWD07", "LivWD08","LivWD09","LivWD10",
  "BedWD01", "BedWD02", "BedWD03","BedWD04","BedWD05",
  "BedWD06", "BedWD07", "BedWD08","BedWD09","BedWD10"
))

overheatcsv_rst4
overheatcsv_rst4_clean <- overheatcsv_rst4 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/SA/Result4_Clean.csv")

# Merge the data
overheat_houses_SA
overheatcsv_rst3
overheatcsv_rst3B
overheatcsv_rst4
overheat_houses_join <- overheat_houses_SA %>% 
  inner_join(overheatcsv_rst3, by = "Nvalid") %>%
  inner_join(overheatcsv_rst3B, by = "Nvalid") %>%
  inner_join(overheatcsv_rst4,  by = "Nvalid") %>%
  write_csv("res/SA/Result2_Houses_join.csv")

overheat_houses_join_SA <- overheat_houses_join
overheat_houses_join_SA
#-------------------------------------------------------------------------#




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
# Assign to a WA data form
overheat_houses_WA <- overheat_houses
overheat_houses_WA

##############################################################################
# now read in simulation results for Overheating in houses
overheatcsv_rst3 <- read.csv("data/WA/Result3.csv", header = FALSE, col.names = c(
  "Nvalid", "WFR", "Overheat", "Kit", 
  "LivR01", "LivR02", "LivR03","LivR04","LivR05",
  "LivR06", "LivR07", "LivR08","LivR09","LivR10",
  "BedRD01", "BedRD02", "BedRD03","BedRD04","BedRD05",
  "BedRD06", "BedRD07", "BedRD08","BedRD09","BedRD10",
  "BedRN01", "BedRN02", "BedRN03","BedRN04","BedRN05",
  "BedRN06", "BedRN07", "BedRN08","BedRN09","BedRN10"  
))
overheatcsv_rst3

overheatcsv_rst3B <- read.csv("data/WA/Result3B.csv", header = FALSE, col.names = c(
  "Nvalid", "WFRB", "OverheatB", "KitB", 
  "LivR01B", "LivR02B", "LivR03B","LivR04B","LivR05B",
  "LivR06B", "LivR07B", "LivR08B","LivR09B","LivR10B",
  "BedRD01B", "BedRD02B", "BedRD03B","BedRD04B","BedRD05B",
  "BedRD06B", "BedRD07B", "BedRD08B","BedRD09B","BedRD10B",
  "BedRN01B", "BedRN02B", "BedRN03B","BedRN04B","BedRN05B",
  "BedRN06B", "BedRN07B", "BedRN08B","BedRN09B","BedRN10B"  
))
overheatcsv_rst3B

overheatcsv_rst3_clean <- overheatcsv_rst3 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/WA/Result3_Clean.csv")
overheatcsv_rst3B_clean <- overheatcsv_rst3B %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/WA/Result3B_Clean.csv")

# now read in houses insulation and direction information
overheatcsv_rst4 <- read.csv("data/WA/Result4.csv", header = FALSE, col.names = c(
  "Nvalid", "WallR", "CeilingR", "RoofR", "KitWD",
  "LivWD01", "LivWD02", "LivWD03","LivWD04","LivWD05",
  "LivWD06", "LivWD07", "LivWD08","LivWD09","LivWD10",
  "BedWD01", "BedWD02", "BedWD03","BedWD04","BedWD05",
  "BedWD06", "BedWD07", "BedWD08","BedWD09","BedWD10"
))

overheatcsv_rst4
overheatcsv_rst4_clean <- overheatcsv_rst4 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/WA/Result4_Clean.csv")

# Merge the data
overheat_houses_WA
overheatcsv_rst3
overheatcsv_rst3B
overheatcsv_rst4
overheat_houses_join <- overheat_houses_WA %>% 
  inner_join(overheatcsv_rst3, by = "Nvalid") %>%
  inner_join(overheatcsv_rst3B, by = "Nvalid") %>%
  inner_join(overheatcsv_rst4,  by = "Nvalid") %>%
  write_csv("res/WA/Result2_Houses_join.csv")

overheat_houses_join_WA <- overheat_houses_join
overheat_houses_join_WA
#-------------------------------------------------------------------------#

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
# Assign to a QLD data form
overheat_houses_QLD <- overheat_houses
overheat_houses_QLD
##############################################################################
# now read in simulation results for Overheating in houses
overheatcsv_rst3 <- read.csv("data/QLD/Result3.csv", header = FALSE, col.names = c(
  "Nvalid", "WFR", "Overheat", "Kit", 
  "LivR01", "LivR02", "LivR03","LivR04","LivR05",
  "LivR06", "LivR07", "LivR08","LivR09","LivR10",
  "BedRD01", "BedRD02", "BedRD03","BedRD04","BedRD05",
  "BedRD06", "BedRD07", "BedRD08","BedRD09","BedRD10",
  "BedRN01", "BedRN02", "BedRN03","BedRN04","BedRN05",
  "BedRN06", "BedRN07", "BedRN08","BedRN09","BedRN10"  
))
overheatcsv_rst3

overheatcsv_rst3B <- read.csv("data/QLD/Result3B.csv", header = FALSE, col.names = c(
  "Nvalid", "WFRB", "OverheatB", "KitB", 
  "LivR01B", "LivR02B", "LivR03B","LivR04B","LivR05B",
  "LivR06B", "LivR07B", "LivR08B","LivR09B","LivR10B",
  "BedRD01B", "BedRD02B", "BedRD03B","BedRD04B","BedRD05B",
  "BedRD06B", "BedRD07B", "BedRD08B","BedRD09B","BedRD10B",
  "BedRN01B", "BedRN02B", "BedRN03B","BedRN04B","BedRN05B",
  "BedRN06B", "BedRN07B", "BedRN08B","BedRN09B","BedRN10B"  
))
overheatcsv_rst3B

overheatcsv_rst3_clean <- overheatcsv_rst3 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/QLD/Result3_Clean.csv")
overheatcsv_rst3B_clean <- overheatcsv_rst3B %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/QLD/Result3B_Clean.csv")

# now read in houses insulation and direction information
overheatcsv_rst4 <- read.csv("data/QLD/Result4.csv", header = FALSE, col.names = c(
  "Nvalid", "WallR", "CeilingR", "RoofR", "KitWD",
  "LivWD01", "LivWD02", "LivWD03","LivWD04","LivWD05",
  "LivWD06", "LivWD07", "LivWD08","LivWD09","LivWD10",
  "BedWD01", "BedWD02", "BedWD03","BedWD04","BedWD05",
  "BedWD06", "BedWD07", "BedWD08","BedWD09","BedWD10"
))

overheatcsv_rst4
overheatcsv_rst4_clean <- overheatcsv_rst4 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/QLD/Result4_Clean.csv")

# Merge the data
overheat_houses_QLD
overheatcsv_rst3
overheatcsv_rst3B
overheatcsv_rst4
overheat_houses_join <- overheat_houses_QLD %>% 
  inner_join(overheatcsv_rst3, by = "Nvalid") %>%
  inner_join(overheatcsv_rst3B, by = "Nvalid") %>%
  inner_join(overheatcsv_rst4,  by = "Nvalid") %>%
  write_csv("res/QLD/Result2_Houses_join.csv")

overheat_houses_join_QLD <- overheat_houses_join
overheat_houses_join_QLD
#-------------------------------------------------------------------------#

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


# Third read in Result2_error2.txt which contains the information for all houses collected from the state
# which has issues in star rating
overheat_result2_error2 <- read_fwf("data/VIC/Result2_error2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 
overheat_result2
overheat_result2_error
overheat_result2_error2

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% 
  anti_join(overheat_result2_error2, by = "ScratchName" ) %>%  
  select("Nvalid") %>% 
  write_csv("res/VIC/Nvalid1.csv")
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

validRow <- overheatorig_clean %>%  
  select("Nvalid") %>% 
  write_csv("res/VIC/Nvalid2.csv")

walltype <- overheat_result2 %>% semi_join(validRow, by = "Nvalid") %>%
  select(Nvalid,CZ,Class,WallType) %>% 
  write_csv("res/VIC/walltype.csv")

# Now join the overheatorig_clean, and walltype
overheat_houses <- overheatorig_clean %>% inner_join(walltype, by = "Nvalid") %>% 
  mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 
  write_csv("res/VIC/Result2_Houses.csv")

overheat_houses_error <- filter(overheat_houses, CZ != NClimateZone ) %>% 
  write_csv("res/VIC/error.csv") 
# Assign to a VIC data form
overheat_houses_VIC <- overheat_houses
overheat_houses_VIC

##############################################################################
# now read in simulation results for Overheating in houses
overheatcsv_rst3 <- read.csv("data/VIC/Result3.csv", header = FALSE, col.names = c(
  "Nvalid", "WFR", "Overheat", "Kit", 
  "LivR01", "LivR02", "LivR03","LivR04","LivR05",
  "LivR06", "LivR07", "LivR08","LivR09","LivR10",
  "BedRD01", "BedRD02", "BedRD03","BedRD04","BedRD05",
  "BedRD06", "BedRD07", "BedRD08","BedRD09","BedRD10",
  "BedRN01", "BedRN02", "BedRN03","BedRN04","BedRN05",
  "BedRN06", "BedRN07", "BedRN08","BedRN09","BedRN10"  
))
overheatcsv_rst3

overheatcsv_rst3B <- read.csv("data/VIC/Result3B.csv", header = FALSE, col.names = c(
  "Nvalid", "WFRB", "OverheatB", "KitB", 
  "LivR01B", "LivR02B", "LivR03B","LivR04B","LivR05B",
  "LivR06B", "LivR07B", "LivR08B","LivR09B","LivR10B",
  "BedRD01B", "BedRD02B", "BedRD03B","BedRD04B","BedRD05B",
  "BedRD06B", "BedRD07B", "BedRD08B","BedRD09B","BedRD10B",
  "BedRN01B", "BedRN02B", "BedRN03B","BedRN04B","BedRN05B",
  "BedRN06B", "BedRN07B", "BedRN08B","BedRN09B","BedRN10B"  
))
overheatcsv_rst3B

overheatcsv_rst3_clean <- overheatcsv_rst3 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/VIC/Result3_Clean.csv")
overheatcsv_rst3B_clean <- overheatcsv_rst3B %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/VIC/Result3B_Clean.csv")

# now read in houses insulation and direction information
overheatcsv_rst4 <- read.csv("data/VIC/Result4.csv", header = FALSE, col.names = c(
  "Nvalid", "WallR", "CeilingR", "RoofR", "KitWD",
  "LivWD01", "LivWD02", "LivWD03","LivWD04","LivWD05",
  "LivWD06", "LivWD07", "LivWD08","LivWD09","LivWD10",
  "BedWD01", "BedWD02", "BedWD03","BedWD04","BedWD05",
  "BedWD06", "BedWD07", "BedWD08","BedWD09","BedWD10"
))

overheatcsv_rst4
overheatcsv_rst4_clean <- overheatcsv_rst4 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/VIC/Result4_Clean.csv")

# Merge the data
overheat_houses_VIC
overheatcsv_rst3
overheatcsv_rst3B
overheatcsv_rst4
overheat_houses_join <- overheat_houses_VIC %>% 
  inner_join(overheatcsv_rst3, by = "Nvalid") %>%
  inner_join(overheatcsv_rst3B, by = "Nvalid") %>%
  inner_join(overheatcsv_rst4,  by = "Nvalid") %>%
  write_csv("res/VIC/Result2_Houses_join.csv")

overheat_houses_join_VIC <- overheat_houses_join
overheat_houses_join_VIC
#-------------------------------------------------------------------------#



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
# Second read in Result2_error.txt which contains the information for all houses collected from the state
# which has issues in the data 
overheat_result2_error2 <- read_fwf("data/NSW/Result2_error2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                         StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,
                                                                         CZ = 2, BL = 1,SubDir = 97,  ScrachDM = 8, Class =1, ScratchName = NA)) 

overheat_result2
overheat_result2_error
overheat_result2_error2

# Figure out the those house numbers which do not have issues. 
validRow <- overheat_result2 %>% 
  anti_join(overheat_result2_error, by = "ScratchName" ) %>% 
  anti_join(overheat_result2_error2, by = "ScratchName" ) %>%
  select("Nvalid")  %>% 
  write_csv("res/NSW/Nvalid1.csv")
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

validRow <- overheatorig_clean %>%
  select("Nvalid")  %>% 
  write_csv("res/NSW/validrow2.csv")

walltype <- overheat_result2 %>% semi_join(validRow, by = "Nvalid") %>%
  select(Nvalid,CZ,Class,WallType) %>% 
  write_csv("res/NSW/walltype.csv")

# Now join the overheatorig_clean, and walltype
overheat_houses <- overheatorig_clean %>% inner_join(walltype, by = "Nvalid") %>% 
  mutate(CZ = as.numeric(CZ), NClimateZone = as.numeric(NClimateZone) ) %>% 
  write_csv("res/NSW/Result2_Houses.csv")

overheat_houses_error <- filter(overheat_houses, CZ != NClimateZone ) %>% 
  write_csv("res/NSW/error.csv") 
# Assign to a NSW data form
overheat_houses_NSW <- overheat_houses
overheat_houses_NSW
#view(overheat_houses_NSW)

##############################################################################
# now read in simulation results for Overheating in houses
overheatcsv_rst3 <- read.csv("data/NSW/Result3.csv", header = FALSE, col.names = c(
  "Nvalid", "WFR", "Overheat", "Kit", 
  "LivR01", "LivR02", "LivR03","LivR04","LivR05",
  "LivR06", "LivR07", "LivR08","LivR09","LivR10",
  "BedRD01", "BedRD02", "BedRD03","BedRD04","BedRD05",
  "BedRD06", "BedRD07", "BedRD08","BedRD09","BedRD10",
  "BedRN01", "BedRN02", "BedRN03","BedRN04","BedRN05",
  "BedRN06", "BedRN07", "BedRN08","BedRN09","BedRN10"  
))
overheatcsv_rst3

overheatcsv_rst3B <- read.csv("data/NSW/Result3B.csv", header = FALSE, col.names = c(
  "Nvalid", "WFRB", "OverheatB", "KitB", 
  "LivR01B", "LivR02B", "LivR03B","LivR04B","LivR05B",
  "LivR06B", "LivR07B", "LivR08B","LivR09B","LivR10B",
  "BedRD01B", "BedRD02B", "BedRD03B","BedRD04B","BedRD05B",
  "BedRD06B", "BedRD07B", "BedRD08B","BedRD09B","BedRD10B",
  "BedRN01B", "BedRN02B", "BedRN03B","BedRN04B","BedRN05B",
  "BedRN06B", "BedRN07B", "BedRN08B","BedRN09B","BedRN10B"  
))
overheatcsv_rst3B

overheatcsv_rst3_clean <- overheatcsv_rst3 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NSW/Result3_Clean.csv")
overheatcsv_rst3B_clean <- overheatcsv_rst3B %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NSW/Result3B_Clean.csv")

# now read in houses insulation and direction information
overheatcsv_rst4 <- read.csv("data/NSW/Result4.csv", header = FALSE, col.names = c(
  "Nvalid", "WallR", "CeilingR", "RoofR", "KitWD",
  "LivWD01", "LivWD02", "LivWD03","LivWD04","LivWD05",
  "LivWD06", "LivWD07", "LivWD08","LivWD09","LivWD10",
  "BedWD01", "BedWD02", "BedWD03","BedWD04","BedWD05",
  "BedWD06", "BedWD07", "BedWD08","BedWD09","BedWD10"
))

overheatcsv_rst4
overheatcsv_rst4_clean <- overheatcsv_rst4 %>% semi_join(validRow, by = "Nvalid") %>%
  write_csv("res/NSW/Result4_Clean.csv")

# Merge the data
overheat_houses_NSW
overheatcsv_rst3
overheatcsv_rst3B
overheatcsv_rst4
overheat_houses_join <- overheat_houses_NSW %>% 
  inner_join(overheatcsv_rst3, by = "Nvalid") %>%
  inner_join(overheatcsv_rst3B, by = "Nvalid") %>%
  inner_join(overheatcsv_rst4,  by = "Nvalid") %>%
  write_csv("res/NSW/Result2_Houses_join.csv")

overheat_houses_join_NSW <- overheat_houses_join
overheat_houses_join_NSW
#-------------------------------------------------------------------------#

#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#CCCCCCCCCCCCCCCCCCCC    Merge clean overheating data  cccccccccccccccccccccccccccccccccccccccccc


bindAll_Overheat <- bind_rows(overheat_houses_join_ACT, overheat_houses_join_NSW,
                     overheat_houses_join_NT, overheat_houses_join_QLD, 
                     overheat_houses_join_SA, overheat_houses_join_TAS,
                     overheat_houses_join_VIC, overheat_houses_join_WA) 
#Now combine with BCA climate zone
BCA_CZs <- read.csv("data/BCA_NatHERS_CZs.csv")
BCA_CZs
#view(BCA_CZs)
bindAll_Overheat2 <- bindAll_Overheat %>% inner_join(BCA_CZs, by = "CZ")
bindAll_Overheat <- bindAll_Overheat2

bindAll_Overheat
bindAll_Overheat_Houses <- bindAll_Overheat %>% 
  write_csv("res/Result_Overheat_AllState.csv")

#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#CCCCCCCCCCCCCCCCCCCCCCCCC   Merge clean house data     ccccccccccccccccccccccccccccccccccccccccc


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
#CCCCCCCCCCCCCCCCCCCC          PLOT for house distribution and characteristics        ccccccccccc

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
ClimateZoneSelected

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
  labs(title = "Figure 1. Number of dwellings used in this study in each State",
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

by_walltype_Sprd <- by_walltype %>% spread(key = WallType, value = DwellingNo, fill = "") %>% 
  write_csv("res/By_Walltype_sprd.csv")

by_walltype_Sprd


?spread

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

by_Class_Sprd <- by_Class %>% spread(key = Class, value = DwellingNo, fill = "") %>% 
  write_csv("res/By_BuildingClass_sprd.csv")

by_Class_Sprd

#Dwelling number in State and Star rating
bindAll_Houses
RbindAll_Houses <- bindAll_Houses %>% mutate(StarRating = round(StarRating,0))
by_StarRating <- RbindAll_Houses %>% semi_join(ClimateZoneSelected, by = "NClimateZone") %>%
  group_by(StateName,StarRating) %>%
  summarise(DwellingNo = n()) %>% 
  write_csv("res/By_StarRating.csv")  
by_StarRating

by_StarRating_VIC_NSW <- by_StarRating %>% filter(StateName =="     VIC"|StateName =="     NSW")
by_StarRating_OtherStates <- by_StarRating %>% filter(StateName !="     VIC",StateName !="     NSW")
by_StarRating_VIC_NSW
by_StarRating_OtherStates


figure3a <- ggplot(
  data = by_StarRating_VIC_NSW, 
  mapping = aes(x = StarRating, y = DwellingNo, group = StateName)
) +
  geom_col(fill = "green",width = 0.25) +   
  geom_text(aes(label = DwellingNo), colour = "blue", vjust = -0.15, fontface = "bold", size = 4) +
  labs(title = "Figure 5a. Star rating distributions in NSW and VIC dwellings",
       x = "Star Rating",
       y = "Number of Dwellings"
  ) +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  ylim(0, 10000) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))+
  facet_wrap( ~ StateName)
figure3a
ggsave("fig/Figure_3a.png", plot = figure3a)

?axisTicks

figure3b <- ggplot(
  data = by_StarRating_OtherStates, 
  mapping = aes(x = StarRating, y = DwellingNo, group = StateName)
) +
  geom_col(fill = "green",width = 0.25) +   
  geom_text(aes(label = DwellingNo), colour = "blue", vjust = -0.15, fontface = "bold", size = 4) +
  labs(title = "Figure 5b. Star rating distributions in other states and territories dwellings",
       x = "Star Rating",
       y = "Number of Dwellings"
  ) + 
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  ylim(0, 250) +
  theme(axis.title = element_text(colour = "red", face = "bold", size = 18))+
  facet_wrap( ~ StateName)
figure3b
ggsave("fig/Figure_3b.png", plot = figure3b)

by_StarRating
 
#view(by_StarRating)

#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#CCCCCCCCCCCCCCCCCCCC          PLOT for overheating        ccccccccccc

bindAll_Overheat_Houses
#Dwelling number in Climatezone 
bindAll_Overheat_CZSelected <- bindAll_Overheat_Houses %>% 
  semi_join(ClimateZoneSelected, by = "NClimateZone") %>% 
  write_csv("res/Result_Overheat_AllState_CZSelected.csv")
bindAll_Overheat_CZSelected

OH_select <- select(bindAll_Overheat_CZSelected,maxday,
                    NClimateZone,StarRating,WFR,WallR,CeilingR,RoofR) %>% scale
head(OH_select)
OH_pca <- prcomp(OH_select)
summary(OH_pca)
plot(OH_pca)
OH_select <- select(bindAll_Overheat_CZSelected,maxday,
                    BCA_CZ,StarRating,WFR,WallR,CeilingR,RoofR)
lm(maxday ~ ., data = OH_select) %>% 
  summary()

?lm



mtcars
mtcars_scaled <- select(mtcars, -mpg, -am, -vs) %>% scale
?scale
head(mtcars_scaled)
mtcars_pca <- prcomp(mtcars_scaled)

summary(mtcars_pca)
plot(mtcars_pca)









bindAll_Overheat_CZSelected[,"maxday"] <- apply(bindAll_Overheat_CZSelected[,31:51], 1, max) 
bindAll_Overheat_CZSelected[,"maxnight"] <- apply(bindAll_Overheat_CZSelected[,52:61], 1, max)

bindAll_Overheat_CZSelected %>% 
  write_csv("res/Result_Overheat_AllState_CZSelected2.csv")
#by climate zone
bindAll_Overheat_CZSelected_byCZ <- group_by(bindAll_Overheat_CZSelected,NClimateZone)
bindAll_Overheat_CZSelected_byCZ

bindAll_Overheat_CZSelected_byCZ %>% 
  ggplot(aes(x=StarRating, y = maxday)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap( ~ NClimateZone)

bindAll_Overheat_CZSelected_byCZ %>% 
  ggplot(aes(x=StarRating, y = maxnight)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap( ~ NClimateZone)

bindAll_Overheat_CZSelected_byCZ %>% 
  ggplot(aes(x=Exposure, y = maxday)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap( ~ NClimateZone)

bindAll_Overheat_CZSelected_byCZ %>% 
  ggplot(aes(x=Exposure, y = maxnight)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap( ~ NClimateZone)


OH_by_CZ <- bindAll_Overheat_CZSelected_byCZ %>% summarise(OHDwellingNo = n())  %>% 
  spread(key = "Overheat", value = "OHDwellingNo", fill = 0) %>% 
  mutate(OH_percent = `TRUE`/(`FALSE`+`TRUE`)) %>% 
  inner_join(BCA_CZs, by = c("NClimateZone" = "CZ"))
OH_by_CZ

