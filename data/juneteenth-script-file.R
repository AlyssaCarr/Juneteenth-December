
juneteenth <- read_excel (path = "data-raw/juneteenthforr_excel.xlsx")

juneteenth <- juneteenth %>% 
  clean_names() 

festivals <- juneteenth %>% 
  pivot_longer(cols = other_fest_passport_to_india:other_open_ended,
               names_to = "festivals_attended",
               values_to = "attended") %>% 
  mutate(attended = replace_na(attended, "No")) %>% 
  select(fid, festivals_attended, attended) %>% 
  mutate(festivals_attended = str_remove(festivals_attended, "other_fest_")) %>% 
  mutate(festivals_attended = recode(festivals_attended, "passport_to_india" = "Passport to India",
                                     "cny" = "Lunar New Year",
                                     "amer_indian" = "Native American Festival",
                                     "deaf_culture" = "Deaf Culture Day",
                                     "big_picnic" = "Lawn Party",
                                     "day_of_dead" = "Dia de los Muertos",
                                     "other" = "Other",
                                     "other_open_ended" = "Other Festival Attended"))

activities <- juneteenth %>% 
  pivot_longer(cols = did_watched_a_perform:did_none_of_above,
               names_to = "activities_available",
               values_to = "activities_participated_in") %>% 
  mutate(activities_participated_in = replace_na(activities_participated_in, "No"))%>% 
  select(fid, activities_available, activities_participated_in) %>% 
  mutate(activities_available = str_remove(activities_available, "did_")) %>% 
  mutate(activities_available = recode(activities_available, "watched_a_perform" = "Performance",
                                       "created_something" = "Created",
                                       "shared_memory_or_story" = "Shared",
                                       "interact_w_visitors_not_in_group" = "Interacted",
                                       "saw_demonstration" = "Demonstration",
                                       "attended_story_time" = "Story Time",
                                       "game_activity" = "Game",
                                       "imagined_other_times" = "Imagined",
                                       "reflected_on_significance_of_juneteenth" = "Reflected",
                                       "none_of_above" = "None"))


race_ethinicity <- juneteenth %>% 
  pivot_longer(cols = race_a_a_or_black:missing_prefer_not_to_respond,
               names_to = "race_ethnicity",
               values_to = "race_values") %>% 
  select(fid, race_ethnicity, race_values) %>% 
  mutate(race_ethnicity = str_remove(race_ethnicity, "race_")) %>% 
  mutate(race_ethnicity = recode(race_ethnicity, "a_a_or_black" = "African American/Black",
                                 "american_indian_or_alaskan_native" = "Native American",
                                 "asian" = "Asian",
                                 "white" = "White",
                                 "hispanic_or_latino" = "Latinx",
                                 "native_hawaiian_or_p_i" = "Native Hawaiian",
                                 "poc" = "BIPOC",
                                 "indicated_other" = "Other",
                                 "missing_prefer_not_to_respond" = "Prefer Not to Respond"))


group <- juneteenth %>% 
  pivot_longer(cols = visit_group_alone:visit_group_out_of_town_guests,
               names_to = "visit_group",
               values_to = "groups_visited") %>% 
  mutate(groups_visited = replace_na(groups_visited, "No"))%>% 
  select(fid, visit_group, groups_visited) %>% 
  mutate(visit_group = str_remove(visit_group, "visit_group_")) %>% 
  mutate(visit_group = recode(visit_group, "alone" = "Alone",
                              "spouse_s_o" = "Partner",
                              "child_ren_under_18" = "Children",
                              "other_fam" = "Family",
                              "friend_s" = "Friends",
                              "out_of_town_guests" = "Out of Town"))             

juneteenth_attendance <- juneteenth %>% 
  pivot_longer(cols = juneteenth_past:juneteenth_future,
               names_to = "juneteenth_attendance",
               values_to = "attended") %>% 
  mutate(attended = replace_na(attended, "No"))%>% 
  select(fid, juneteenth_attendance, attended) %>% 
  mutate(juneteenth_attendance = recode(juneteenth_attendance, "juneteenth_past" = "Past",
                                        "attend_juneteenth_events_in_17" = "Present",
                                        "juneteenth_future" = "Future")) 

juneteenth <- juneteenth %>% 
  select("fid":"felt_inspired_uplifted") %>% 
  select(-"other_fest_passport_to_india") %>% 
  select(-"other_fest_cny") %>% 
  select(-"other_fest_amer_indian") %>% 
  select(-"other_fest_deaf_culture") %>% 
  select(-"other_fest_big_picnic") %>% 
  select(-"other_fest_day_of_dead") %>% 
  select(-"other") %>% 
  select(-"other_open_ended") %>% 
  select(-"did_watched_a_perform") %>% 
  select(-"did_created_something") %>% 
  select(-"did_shared_memory_or_story") %>% 
  select(-"did_interact_w_visitors_not_in_group") %>% 
  select(-"did_saw_demonstration") %>% 
  select(-"did_attended_story_time") %>% 
  select(-"did_game_activity") %>% 
  select(-"did_imagined_other_times") %>% 
  select(-"did_reflected_on_significance_of_juneteenth") %>% 
  select(-"did_none_of_above") %>% 
  select(-"race_a_a_or_black") %>% 
  select(-"race_american_indian_or_alaskan_native") %>% 
  select(-"race_asian") %>% 
  select(-"race_white") %>% 
  select(-"race_hispanic_or_latino") %>% 
  select(-"race_native_hawaiian_or_p_i") %>% 
  select(-"race_poc") %>% 
  select(-"race_indicated_other") %>% 
  select(-"missing_prefer_not_to_respond") %>% 
  select(-"visit_group_alone") %>% 
  select(-"visit_group_spouse_s_o") %>% 
  select(-"visit_group_child_ren_under_18") %>% 
  select(-"visit_group_other_fam") %>% 
  select(-"visit_group_friend_s") %>% 
  select(-"visit_group_out_of_town_guests") %>% 
  select(-"juneteenth_past") %>% 
  select(-"attend_juneteenth_events_in_17") %>% 
  select(-"juneteenth_future")


juneteenth <- juneteenth %>% 
  select(-"groups_with_children_under_18") %>% 
  select(-"groups_without_children_under_18")


juneteenth <- left_join(juneteenth,
                        festivals,
                        by = "fid")

juneteenth <- left_join(juneteenth,
                        group,
                        by = "fid")

juneteenth <- left_join(juneteenth,
                        juneteenth_attendance,
                        by = "fid")

juneteenth <- left_join(juneteenth,
                        race_ethinicity,
                        by = "fid")


write_csv(juneteenth,
          file = "data/juneteenth.csv")

write_csv(activities,
          file = "data/activities.csv")

write_csv(festivals,
          file = "data/festivals.csv")

write_csv(group,
          file = "data/group.csv")

write_csv(race_ethinicity,
          file = "data/race_ethnicity.csv")

write_csv(juneteenth_attendance,
          file = "data/juneteenth_attendance.csv")


write_rds(juneteenth,
          file = "data/juneteenth.rds")

write_rds(activities,
          file = "data/activities.rds")

write_rds(festivals,
          file = "data/festivals.rds")

write_rds(group,
          file = "data/group.rds")

write_rds(race_ethinicity,
          file = "data/race_ethnicity.rds")

write_rds(juneteenth_attendance,
          file = "data/juneteenth_attendance.rds")

