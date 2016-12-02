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
table$Global_reactive_power <- as.numeric(table$Global_reactive_power)
table$Voltage <- as.numeric(table$Voltage)
table$Sub_metering_1 <- as.numeric(table$Sub_metering_1)
table$Sub_metering_2 <- as.numeric(table$Sub_metering_2)
table$Sub_metering_3 <- as.numeric(table$Sub_metering_3)

#будування графіку
par(mfrow=c(2,2))
plot(table$Date, table$Global_active_power, type = "l", xlab = "", ylab = "Global active power")
plot(table$Date, table$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
plot(table$Date, table$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
lines(table$Date, table$Sub_metering_2, type = "l", col = "red")
lines(table$Date, table$Sub_metering_3, type = "l", col = "blue")
legend("topright", legend = c("Sub metering 1", "Sub metering 2", "Sub metering 3"), col = c("black", "red", "blue"), pch = NA, lty = c(1, 1, 1), bty = "n")
plot(table$Date, table$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global reactive power")

#збереження графіка у файл
dev.copy(device = png, filename = "plot4.png", width = 480, height = 480)
dev.off()