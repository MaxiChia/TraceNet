source('global.R')
cat('--- james_stern events sample ---\n')
js <- mc2_df[sapply(mc2_df$parties, function(x) any(grepl('james_stern', x))), ]
print(head(js[, c('datetime','short_name','parties')], 15))
cat('\ntotal james_stern events:', nrow(js), '\n')
cat('\n--- john_windward events sample ---\n')
jw <- mc2_df[sapply(mc2_df$parties, function(x) any(grepl('john_windward', x))), ]
print(head(jw[, c('datetime','short_name','parties')], 15))
cat('\ntotal john_windward events:', nrow(jw), '\n')
