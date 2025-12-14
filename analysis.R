barplot(otu_to_test)
otu <- matrix(
c(10, 20, 5,   # Sample1
30, 10, 2,   # Sample2
5,  40, 8),  # Sample3
byrow = TRUE
)
otu <- matrix(
c(10, 20, 5,   # Sample1
30, 10, 2,   # Sample2
5,  40, 8),  # Sample3
nrow = 3,
byrow = TRUE
)
colnames(otu) <- c("Bacteroides", "Prevotella", "Ruminococcus")
rownames(otu) <- c("Sample1", "Sample2", "Sample3")
otu
barplot(t(otu),
beside = TRUE,
col = c("skyblue", "pink", "lightgreen"),
main = "Microbiome OTU Abundance",
ylab = "Reads",
xlab = "Samples")
barplot(t(otu),
col = c("skyblue", "pink", "lightgreen"),
main = "Microbiome OTU Abundance",
ylab = "Reads",
xlab = "Samples")
barplot(t(otu),
beside = TRUE,
col = c("skyblue", "pink", "lightgreen"),
main = "Microbiome OTU Abundance",
ylab = "Reads",
xlab = "Samples")
legend("topleft", legend = colnames(otu),
fill = c("skyblue", "pink", "lightgreen"))
barplot(t(otu),
beside = TRUE,
col = c("skyblue", "pink", "lightgreen"),
main = "Microbiome OTU Abundance",
ylab = "Reads",
xlab = "Samples")
legend("topleft", legend = colnames(otu),
fill = c("skyblue", "pink", "lightgreen"))
View(otu)
View(otu)
library(readr)
otu_to_test <- read_csv("Downloads/otu-to-test.csv")
View(otu_to_test)
library(readr)
otu_to_test <- read_csv("Downloads/otu-to-test.csv")
View(otu_to_test)
otu_matrix <- as.matrix(otu_to_test)
str(otu_matrix)
barplot(colSums(otu_matrix),
las = 2,
main = "Total Microbial Abundance per Sample",
ylab = "Reads")
barplot(colSums(otu_to_test),
las = 2,
main = "Total Microbial Abundance per Sample",
ylab = "Reads")
library(readr)
otu_to_test <- read_csv("Downloads/otu-to-test.csv")
View(otu_to_test)
library(readr)
otu_to_test <- read_csv("Downloads/otu-to-test.csv")
View(otu_to_test)
barplot(otu_matrix,
las = 2,
main = "Total Microbial Abundance per Sample",
ylab = "Reads")
barplot(otu_matrix,
las = 2,
main = "Total Microbial Abundance per Sample",
ylab = "Reads")
barplot(colSums(as.matrix(otu_to_test)),
las = 2,
main = "Total Microbial Abundance per Sample",
ylab = "Reads")
barplot(colSums(as.matrix(otu_to_test)),
las = 2,
main = "Total Microbial Abundance per Sample",
ylab = "Reads")
colSums(otu_matrix)
colSums(as.matrix(otu_matrix))
class(otu_matrix)
mode(otu_matrix)
otu_matrix <- as.matrix(otu_to_test$Combined.Abundance)
View(colSums)
colSums(as.matrix.data.frame(otu_matrix))
colSums(otu_to_test)
colSums(as.data.frame(otu_matrix))
colSums(as.data.frame(otu_to_test))
View(colSums)
colSums(as.complex(otu_matrix))
colSums(as.data.frame.array(otu_matrix))
View(otu_matrix)
View(otu_matrix)
class(otu_matrix)
otu_num <- as.matrix(otu_df[, "Combined.Abundance", drop = FALSE])
otu_num <- as.matrix(otu_matrix[, "Combined.Abundance", drop = FALSE])
View(otu_matrix)
View(otu_matrix)
otu_num <- as.matrix(otu_matrix[, "Combined Abundance", drop = FALSE])
colSums(as.data.frame.array(otu_matrix))
colSums(otu_num)
colSums(as.array(otu_num))
View(otu_num)
View(otu_num)
colSums(as.complex(otu_num))
View(otu_to_test)
library(dplyr)
library(dplyr)
download.packages(dplyr)
install.packages(dplyr)
install.package(dplyr)
install.packages(dplyr)
library(dplyr)
install.packages("dplyr")
library(dplyr)
otu_grouped <- otu %>%
group_by(Microorganism.assingee) %>%
summarise(
total_abundance = sum(Combined.Abundance, na.rm = TRUE)
) %>%
arrange(desc(total_abundance))
library(dplyr)
otu_grouped <- otu %>%
group_by(Microorganism.assingee) %>%
summarise(
total_abundance = sum(Combined.Abundance, na.rm = TRUE)
) %>%
arrange(desc(total_abundance))
library(dplyr)
otu_grouped <- otu %>%
group_by(Microorganism.assingee) %>%
summarise(
total_abundance = sum(Combined.Abundance, na.rm = TRUE)
) %>%
arrange(desc(total_abundance))
library(ggplot2)
install.packages("ggplot2")
ggplot(otu_grouped,
aes(x = reorder(Microorganism.assingee, -total_abundance),
y = total_abundance)) +
geom_bar(stat = "identity") +
labs(
x = "Microorganism assignee",
y = "Total combined abundance",
title = "Total abundance per microorganism assignment"
) +
theme_bw() +
theme(
axis.text.x = element_text(angle = 90, hjust = 1)
)
ggplot2(otu_grouped,
aes(x = reorder(Microorganism.assingee, -total_abundance),
y = total_abundance)) +
geom_bar(stat = "identity") +
labs(
x = "Microorganism assignee",
y = "Total combined abundance",
title = "Total abundance per microorganism assignment"
) +
theme_bw() +
theme(
axis.text.x = element_text(angle = 90, hjust = 1)
)
ggplot(otu_grouped,
aes(x = reorder(Microorganism.assingee, -total_abundance),
y = total_abundance)) +
geom_bar(stat = "identity") +
labs(
x = "Microorganism assignee",
y = "Total combined abundance",
title = "Total abundance per microorganism assignment"
) +
theme_bw() +
theme(
axis.text.x = element_text(angle = 90, hjust = 1)
)
otu_class <- otu %>%
mutate(
# מפרקים את השיוך למרכיבים לפי ";" ולוקחים את הרכיב השלישי (class)
Class = sapply(strsplit(Microorganism.assingee, ";\\s*"), `[`, 3)
) %>%
group_by(Class) %>%
summarise(
total_abundance = sum(Combined.Abundance, na.rm = TRUE)
) %>%
arrange(desc(total_abundance))
barplot(otu_to_test)
barplot(as.matrix(otu_to_test))
install.packages("tidyverse")
df <- as.data.frame(out_matrix)
df <- as.data.frame(otu_matrix)
df
df <- as.data.frame(otu_matrix)
View(df)
View(df)
df_expanded <- df %>%
separate_rows(taxonomy, sep = ";\\s*")
library(tidyverse)
library(tidyverse)
df_expanded <- df %>%
separate_rows(taxonomy, sep = ";\\s*")
df_expanded <- df %>%
separate_rows(`Microorganism assingee`, sep = ";\\s*")
View(df_expanded)
View(df)
df <- as.data.frame(otu_matrix)
View(separate_rows())
View(separate_rows)
df_expanded <- separate_rows(df, `Microorganism assingee`, sep = ";\\s*")
View(df_expanded)
df_expanded <- df %>%
separate_rows(`Microorganism assingee`, sep = ";\\s*")
df_summary <- df %>%
group_by(microorganism_assigned) %>%
summarise(total_abundance = sum(combined_abundance, na.rm = TRUE))
df_summary <- df %>%
group_by(`Microorganism assingee`) %>%
summarise(total_abundance = sum(`Combined Abundance`, na.rm = TRUE))
df_summary <- df_expanded %>%
group_by(`Microorganism assingee`) %>%
summarise(total_abundance = sum(`Combined Abundance`, na.rm = TRUE))
df_summary <- df_expanded %>%
group_by(`Microorganism assingee`) %>%
summarise(total_abundance = sum(`Combined Abundance`, na.rm = TRUE))
df_clean <- df %>%
mutate(`Microorganism assingee` = na_if(`Microorganism assingee`, "")) %>%
drop_na(`Microorganism assingee`)
df_summary <- df_clean %>%
group_by(`Microorganism assingee`) %>%
summarise(total_abundance = sum(`Combined Abundance`, na.rm = TRUE))
df_clean <- df_expanded %>%
mutate(`Microorganism assingee` = na_if(`Microorganism assingee`, "")) %>%
drop_na(`Microorganism assingee`)
df_summary <- df_clean %>%
group_by(`Microorganism assingee`) %>%
summarise(total_abundance = sum(`Combined Abundance`, na.rm = TRUE))
View(df_clean)
df_summary <- df_clean %>%
group_by(`Microorganism assingee`) %>%
summarise(total_abundance = sum(`Combined Abundance`, na.rm = TRUE))
df_summary <- df_expanded %>%
group_by(`Microorganism assingee`) %>%
summarise(total_abundance = sum(`Combined Abundance`, na.rm = TRUE))
View(df)
View(df_expanded)
df_clean <- df_expanded %>%
mutate(`Microorganism assingee` = na_if(`Microorganism assingee`, "")) %>%
drop_na(`Microorganism assingee`)
View(df_clean)
df_summary <- df_clean %>%
group_by(`Microorganism assingee`) %>%
summarise(total_abundance = sum(`Combined Abundance`, na.rm = TRUE))
df <- df %>%
mutate(`Combined Abundance` = as.numeric(`Combined Abundance`))
df_summary <- df_clean %>%
group_by(`Microorganism assingee`) %>%
summarise(total_abundance = sum(`Combined Abundance`, na.rm = TRUE))
df <- df_clean %>%
mutate(`Combined Abundance` = as.numeric(`Combined Abundance`))
df_summary <- df %>%
group_by(`Microorganism assingee`) %>%
summarise(total_abundance = sum(`Combined Abundance`, na.rm = TRUE))
View(df_summary)
barplot(df)
barplot(df_summary)
barplot(as.matrix(df_summary))
as.
View(barplot)
plot(df_summary)
barplot(df_summary)
df_summary = as.matrix(df_summary)
plot(df_summary)\
plot(df_summary)
View(df_summary)
View(df_summary)
pie(df_summary,
main = "My Pie Chart",
col = rainbow(length(values)))
pie(df_summary,
main = "My Pie Chart",)
pie(df_summary,
main = "My Pie Chart")
View(pie)
vals <- df_summary$total_abundance
pie(as.data.frame(df_summary))
df_summary
plot(df_summary)
plot(df_summary[, 1], df_summary[, 2])
df_summary
df_summary[, 1]
df_summary[, 2]
pie(
df$total_abundance,
labels = df$microorganism_assingee,
col = rainbow(nrow(df)),
main = "Microorganism Abundance"
)
pie(
df_pie$total_abundance,
labels = df_pie$`Microorganism assingee`,
col = rainbow(nrow(df_pie)),
main = "Microorganism Abundance"
)
pie(
df_summary$total_abundance,
labels = df_summary$`Microorganism assingee`,
col = rainbow(nrow(df_summary)),
main = "Microorganism Abundance"
)
View(df_summary)
pd_final = as.matrix(pd_summary)
df_final = as.matrix(df_summary)
View(df_final)
View(df_expanded)
View(df_summary)
View(df_final)
View(df_summary)
View(df_final)
View(df_expanded)
class(df_clean)
class(df_final)
as.data.frame(df_final)
df_final = as.data.frame(df_final)
class(df_final)\
class(df_final)
class(df_clean)
plot(df_final)
as.data.frame.table(df_final)
class(as.data.frame.table(df_final))
plot(as.data.frame.table(df_final))
plotbar(as.data.frame.table(df_final))
barplot(as.data.frame.table(df_final))
df_final
class(df_final)
df_final&microorganism_asingee
df_final&microorganism_assingee
df_final$microorganism_assingee
df_final$microorganism-assingee
df_final$microorganismassingee
df_final$microorganism_assingee
View(df_final)
View(df_final)
View(df_final)
df_final
colnames(df_final)[colnames(df_final) == "Microorganism assingee"] <- "Bla"
colnames(df_final)[colnames(df_final) == "Microorganism assingee"] <- "microorganism_assingee"
colnames(df_final)[colnames(df_final) == "bla"] <- "microorganism_assingee"
colnames(df_final)[colnames(df_final) == "Bla"] <- "microorganism_assingee"
df_final$microorganism_assingee
barplot(height=df_final$total_abundance, names.arg = df_final$total_abundance)
barplot(height=df_final$total_abundance, names.arg = df_final$microorganism_assingee)
barplot(height=as.numeric(df_final$total_abundance), names.arg = df_final$microorganism_assingee)
barplot(height=as.numeric(df_final$total_abundance), names.arg = df_final$microorganism_assingee)
View(df_final)
View(df_final)
pie(df_final)
pie(df_final)
df_final$total_abundance <- as.numeric(df_final$total_abundance)
pie(df_final)
df_final$total_abundance <- as.numeric(df_final$total_abundance)
pie(df_final)
summary(df_final$total_abundance)
df_final <- df_final |> drop_na(total_abundance)
pie(df_final)
df_final <- df_final |> drop_na(total_abundance)
pie(df_final$total_abundance, labels = df_final$microorganism_assingee)
View(df)
View(df_clean)
View(df_expanded)
View(otu_num)
View(otu_matrix)
pd_final
df_final
sort(x, decreasing = TRUE)
sort(df_final, decreasing = TRUE)
sort(as_matrix(df_final), decreasing = TRUE)
sort(as.matrix(df_final), decreasing = TRUE)
df_sorted <- df_final[order(df_final$total_abundance), ]
View(df_sorted)
df_sorted <- df_final[order(-df_final$total_abundance), ]
df_sorted[:5]
df_sorted[1:5,]
df_sorted[1:15,]
df_sorted[1:15,df_sorted$microorganism_assingee]
df_sorted[1:15,]
pie(df_sorted[1:15,]$total_abundance, labels=df_sorted[1:15, ]$microorganism_assingee)
legend("topright",
legend = df$Microorganism,
fill = colors)
legend("topright",
legend = df_sorted$microorganism_assingee,
fill = colors)
pie(df_sorted[1:15,]$total_abundance, labels=df_sorted[1:15, ]$microorganism_assingee)
percent <- round(df$Abundance / sum(df$Abundance) * 100, 1)
percent <- round(df_sorted$total_abundance / sum(df_sorted$total_abundance[1:15, ]) * 100, 1)
percent <- round(df_sorted$total_abundance / sum(df_sorted$total_abundance) * 100, 1)
percent
labels <- paste0(df_sorted$microorganism_assingee, " (", percent, "%)")
labels
pie(df_sorted[1:15,]$total_abundance, labels=df_sorted[1:15, ]$microorganism_assingee)
pie(df_sorted[1:15,]$total_abundance, labels=labels[1:15, ]$microorganism_assingee)
pie(df_sorted[1:15,]$total_abundance, labels=labels[1:15, ])
labels
pie(df_sorted[1:15,]$total_abundance, labels=labels)
percent <- round(df_sorted$total_abundance / sum(df_sorted$total_abundance) * 100, 1)
percent
labels <- paste0(df_sorted$microorganism_assingee, " (", percent, "%)")
labels
pie(df_sorted[1:15,]$total_abundance, labels=labels)
history()
history()
savehistory("my_script.R")
barplot(df_sorted)
View(barplot)
barplot(height=df_sorted$total_abundance[1:15,], )
barplot(height=df_sorted$total_abundance[1:15,])
barplot(height=df_sorted$total_abundance[1:15,], labels=labels)
barplot(height=df_sorted$total_abundance, labels=labels)
barplot(height=df_sorted$total_abundance[1:15], labels=labels)
barplot(height=df_sorted$total_abundance[1:15], labels=df_sorted$microorganism_assingee[1:15])
barplot(height=df_sorted$total_abundance[1:15])
pie(df_sorted[1:15,]$total_abundance, labels=labels)
VIew(pie)
View(pie)
pie(df_sorted[1:15,]$total_abundance, labels=labels, density=5)
pie(df_sorted[1:15,]$total_abundance, labels=labels, density=2)
pie(df_sorted[1:15,]$total_abundance, labels=labels, density=1)
pie(df_sorted[1:15,]$total_abundance, labels=labels, density=10)
pie(df_sorted[1:15,]$total_abundance, labels=labels, density=500)
pie(df_sorted[1:15,]$total_abundance, labels=labels, density=3)
pie(df_sorted[1:15,]$total_abundance, labels=labels, density=1)
pie(df_sorted[1:15,]$total_abundance, labels=labels, density=0)
pie(df_sorted[1:15,]$total_abundance, labels=labels, density=30)
pie(df_sorted[1:15,]$total_abundance, labels=labels)
pie(df_sorted[1:15,]$total_abundance, labels=labels, radius=5)
pie(df_sorted[1:15,]$total_abundance, labels=labels, radius=0.3)
pie(df_sorted[1:15,]$total_abundance, labels=labels, radius=1)
pie(df_sorted[1:15,]$total_abundance, labels=labels, radius=1.5)
pie(df_sorted[1:15,]$total_abundance, labels=labels, radius=0.8)
pie(df_sorted[1:15,]$total_abundance, labels=labels)
pie(df_sorted[1:15,]$total_abundance, labels=labels)
pie(df_sorted[1:15,]$total_abundance, labels=labels col=colorRampPalette(c("lightblue", "blue", "darkblue"))(nrow(df_sorted)))
colors = colorRampPalette(c("lightblue", "blue", "darkblue"))(nrow(df_sorted))
colors
pie(df_sorted[1:15,]$total_abundance, labels=labels, col=colors)
colors = colorRampPalette(c("lightblue", "blue", "darkblue"))(15)
pie(df_sorted[1:15,]$total_abundance, labels=labels, col=colors)
colors = colorRampPalette(c("lightblue", "white"))(15)
pie(df_sorted[1:15,]$total_abundance, labels=labels, col=colors)
colors = colorRampPalette(c("lightblue", "white"))(15)
pie(df_sorted[1:15,]$total_abundance, labels=labels, col=colors)
colors = colorRampPalette(c("black", "white"))(15)
pie(df_sorted[1:15,]$total_abundance, labels=labels, col=colors)
colors = colorRampPalette(c("darkblue", "cyan"))(15)
pie(df_sorted[1:15,]$total_abundance, labels=labels, col=colors)
otu_to_test
df_expanded <- separate_rows(df, `Microorganism assingee`, sep = ";\\s*")
otu_grouped <- otu %>%
group_by(Microorganism.assingee) %>%
summarise(
total_abundance = sum(Combined.Abundance, na.rm = TRUE)
otu_grouped <- otu %>%
otu_grouped <- otu %>%
group_by(Microorganism.assingee) %>%
summarise(
total_abundance = sum(Combined.Abundance, na.rm = TRUE)
) %>%
arrange(desc(total_abundance))
data_raw <- read_csv("Downloads/otu-to-test.csv")
data_raw <- read_csv("Downloads/otu-to-test.csv")
data_raw <- read_csv("Downloads/otu-to-test.csv")
df_expanded <- separate_rows(df, `Microorganism assingee`, sep = ";\\s*")
data_separated <- separate_rows(data_raw, `Microorganism assingee`, sep = ";\\s*")
View(data_separated)
View(data_separated)
data_grouped <- data_separated %>%
group_by(`Microorganism assingee`) %>%
summarise(
total_abundance = sum(`Combined Abundance`, na.rm = TRUE)
) %>%
arrange(desc(total_abundance))
View(data_separated)
View(data_grouped)
data_sorted <- data_grouped[order(-data_grouped$total_abundance), ]
percent <- round(data_sorted$total_abundance / sum(data_sorted$total_abundance[1:15, ]) * 100, 1)
percent <- round(data_sorted$total_abundance / sum(data_sorted$total_abundance[1:15]) * 100, 1)
labels <- paste0(data_sorted$microorganism_assingee, " (", percent, "%)")
labels <- paste0(data_sorted$`Microorganism assingee`, " (", percent, "%)")
colors = colorRampPalette(c("lightblue", "blue", "darkblue"))(15)
pie(data_sorted[1:15,]$total_abundance, labels=labels, col=colors)
barplot()
barplot(height=data_sorted[1:15,]$total_abundance)
values = data_sorted[1:15, ]$total_abundance
barplot(data.frame(data_sorted))
barplot(as.matrix(data_sorted))
barplot(height=data_sorted[1:15,]$total_abundance, names.arg = data_sorted$`Microorganism assingee`)
barplot(height=data_sorted[1:15,]$total_abundance, names.arg = data_sorted[1:15]$`Microorganism assingee`)
barplot(height=data_sorted[1:15,]$total_abundance, names.arg = data_sorted[1:15,]$`Microorganism assingee`)
barplot(height=data_sorted[1:4,]$total_abundance, names.arg = data_sorted[1:4,]$`Microorganism assingee`)
barplot(height=data_sorted[1:7,]$total_abundance, names.arg = data_sorted[1:7,]$`Microorganism assingee`)
barplot(height=data_sorted[1:6,]$total_abundance, names.arg = data_sorted[1:6,]$`Microorganism assingee`)
barplot(height=data_sorted[1:6,]$total_abundance, names.arg = data_sorted[1:6,]$`Microorganism assingee`, main='Data', cex.names = 0.6)
barplot(height=data_sorted[1:6,]$total_abundance, names.arg = data_sorted[1:6,]$`Microorganism assingee`, main='Data', cex.names = 0.8)
barplot(height=data_sorted[1:5,]$total_abundance, names.arg = data_sorted[1:5,]$`Microorganism assingee`, main='Data', cex.names = 0.8)
barplot(height=data_sorted[1:5,]$total_abundance, names.arg = data_sorted[1:5,]$`Microorganism assingee`, main='Data', cex.names = 0.7)
barplot(height=data_sorted[1:5,]$total_abundance, names.arg = data_sorted[1:5,]$`Microorganism assingee`, main='Data', cex.names = 0.7, col=colors)
barplot(height=data_sorted[1:5,]$total_abundance, names.arg = data_sorted[1:5,]$`Microorganism assingee`, main='Data', cex.names = 0.7, col=colors, xlab='microorganism', ylab='total')
