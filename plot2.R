setwd("C:/temp/lr_5")

#������� �����
table <- read.table("data/household_power_consumption.txt", sep = ";", header = TRUE)

#��������� ���������� �������
table <- table[!(table$Voltage == '?'),]

#������� ����� �������� �� ��� 2007-02-01 � 2007-02-02
table <- table[table$Date=='1/2/2007' | table$Date=='2/2/2007',]

#������������ ������ � Date � Time �� �������� ���������� �������� � ������� Date
table$Date <-strptime(paste(as.Date(table$Date, format = "%d/%m/%Y"), table$Time), format = "%Y-%m-%d %H:%M:%S")

#��������� ������� Time, ������� �������� �� ���� ������� ��� �������� � ������� Date
table <- table[, !(colnames(table) %in% "Time")]

#������������ ����� � �������� ������
table$Global_active_power <- as.numeric(table$Global_active_power)

#��������� �������
plot(table$Date, table$Global_active_power, type = "l", xlab = "", ylab = "Global active power (kilowatts)")

#���������� ������� � ����
dev.copy(device = png, filename = "plot2.png", width = 480, height = 480)
dev.off()