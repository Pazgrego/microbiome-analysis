data_raw <- read_csv("Downloads/otu-to-test.csv")


> data_separated <- separate_rows(data_raw, `Microorganism assingee`, sep = ";\\s*")

# מאחדים כל שם של מיוקריבי לאחד ועושים sum לכמות
data_grouped <- data_separated %>%
  +     group_by(`Microorganism assingee`) %>%
  +     summarise(
    +         total_abundance = sum(`Combined Abundance`, na.rm = TRUE)
    +     ) %>%
  +     arrange(desc(total_abundance))


data_sorted <- data_grouped[order(-data_grouped$total_abundance), ]

# מייצרים ליסט של אחוזי המיקרובים מסך כולם
percent <- round(data_sorted$total_abundance / sum(data_sorted$total_abundance[1:15]) * 100, 1)

# מחברים בין שם המיקרובים לבין האחוז
labels <- paste0(data_sorted$`Microorganism assingee`, " (", percent, "%)")

# מייצר פלטת צבעים משתנה עם 15 מרווחים
colors = colorRampPalette(c("lightblue", "blue", "darkblue"))(15)

# מייצרים פי מ15 נתונים הראשונים
pie(data_sorted[1:15,]$total_abundance, labels=labels, col=colors)

barplot(height=data_sorted[1:5,]$total_abundance, names.arg = data_sorted[1:5,]$`Microorganism assingee`, main='Data', cex.names = 0.7, col=colors, xlab='microorganism', ylab='total')
# cex.names = מקטין את גודל הטקסט