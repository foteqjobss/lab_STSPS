setwd("C:/temp/lr_5")

#читання файлу
table <- read.table("data/household_power_consumption.txt", sep = ";", header = TRUE)

#вилучення пропущених значень
table <- table[!(table$Voltage == '?'),]

#читання даних відповідно до дат 2007-02-01 і 2007-02-02
table <- table[table$Date=='1/2/2007' | table$Date=='2/2/2007',]

#перетворення змінних у Date і Time та внесення отриманого значення у колонку Date
table$Date <-strptime(paste(as.Date(table$Date, format = "%d/%m/%Y"), table$Time), format = "%Y-%m-%d %H:%M:%S")

#вилучення колонки Time, оскільки значення із даної колонки уже містяться у колонці Date
table <- table[, !(colnames(table) %in% "Time")]

#перетворення даних у цифровий формат
table$Global_active_power <- as.numeric(table$Global_active_power)

#будування графіку
hist(table$Global_active_power, xlab = "Global active power (kilowatts)", col = "red", main = "Global active power")

#збереження графіка у файл
dev.copy(device = png, filename = "plot1.png", width = 480, height = 480)
dev.off()