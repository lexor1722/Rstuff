

library("XLConnect")
#ws <- readWorksheetFromFile("prod_sample_banks.xlsx", sheet="prod_sample_banks")
ws <- readWorksheetFromFile("ratings_dummy.xlsx", sheet="Sheet1")


t1 <- table(ws$ext_rating)
df1 <- as.data.frame(t1)
df1 <- data.frame(df1[,-1], row.names = df1[,1])

t2 <- table(ws$rac_overall)
df2 <- as.data.frame(t2)
df2 <- data.frame(df2[,-1], row.names = df2[,1])

ratings <- c('A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D')
df0 <- data.frame(row.names = ratings)
df1 <- merge(df0, df1, by=0, all=TRUE)
df1 <- data.frame(df1[,-1], row.names = df1[,1])

df_final <- merge(df1, df2, by=0, all=TRUE)
df_final <- data.frame(df_final[,-1], row.names = df_final[,1])

result <- barplot(as.matrix(t(df_final)), beside = T)


