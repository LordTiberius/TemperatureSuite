#!/usr/bin/python

#from Adafruit_BMP085 import BMP085
from time import gmtime, strftime
import time
import Adafruit_DHT
from w1thermsensor import W1ThermSensor


def median(mylist):
    sorts = sorted(mylist)
    length = len(sorts)
    if not length % 2:
        return (sorts[length / 2] + sorts[length / 2 - 1]) / 2.0
    return sorts[length / 2]

def remove_wrong_entries(mylist, tol):
	med = median(mylist)
	newlist = mylist
	for entry in mylist:
		if ((entry > med + tol) or (entry < med - tol)):
			newlist.remove(entry)
	return newlist

def calc_values(mylist):
	minval = 10000
	maxval = -10000
	sumval = 0
	for entry in mylist:
		sumval = sumval + entry
		if entry < minval:
			minval = entry
		if entry > maxval:
			maxval = entry
	return (minval, maxval, (sumval/len(mylist)), len(mylist))
	
	
before = time.time()

# Initialise the BMP085 and use STANDARD mode (default value)
# bmp = BMP085(0x77, debug=True)
#bmp = BMP085(0x77, 3)

# To specify a different operating mode, uncomment one of the following:
# bmp = BMP085(0x77, 0)  # ULTRALOWPOWER Mode
# bmp = BMP085(0x77, 1)  # STANDARD Mode
# bmp = BMP085(0x77, 2)  # HIRES Mode
# bmp = BMP085(0x77, 3)  # ULTRAHIRES Mode

#temp = bmp.readTemperature()

# Read the current barometric pressure level
#pressure = bmp.readPressure()

# To calculate altitude based on an estimated mean sea level pressure
# (1013.25 hPa) call the function as follows, but this won't be very accurate
#altitude = bmp.readAltitude()

# To specify a more accurate altitude, enter the correct mean sea level
# pressure level.  For example, if the current pressure level is 1023.50 hPa
# enter 102350 since we include two decimal places in the integer value
# altitude = bmp.readAltitude(102350)

sensor = Adafruit_DHT.DHT22
pin = 4
DHThumidity, DHTtemperature = Adafruit_DHT.read_retry(sensor, pin)

sensor2 = W1ThermSensor(W1ThermSensor.THERM_SENSOR_DS18B20, "031571a500ff")
temp_sens2 = sensor2.get_temperature()
sensor3 = W1ThermSensor(W1ThermSensor.THERM_SENSOR_DS18B20, "031571c9adff")
temp_sens3 = sensor3.get_temperature()
sensor4 = W1ThermSensor(W1ThermSensor.THERM_SENSOR_DS18B20, "031571ca6fff")
temp_sens4 = sensor4.get_temperature()

after = time.time()

#print "Temperature: %.2f C" % temp
#print "Pressure:    %.2f hPa" % (pressure / 100.0)
#print "Altitude:    %.2f" % altitude
print "Temperature DHT: %.2f C" % DHTtemperature
print "Humidity DHT: %.2f " % DHThumidity
print "Temperature 2: %.2f C" % temp_sens2
print "Temperature 3: %.2f C" % temp_sens3
print "Temperature 4: %.2f C" % temp_sens4
print "Query took: %.1f" % (after-before)

while True:
	seconds = time.localtime(time.time())[5]
	print "%d" % time.localtime(time.time())[5]
	next = 8;
	if seconds > 10:
		time.sleep(60-seconds)
	i = 0
	temparray1 = []
	#pressarray = []
	temparray2 = []
	temparray3 = []
	temparray4 = []
	humidityarray = []
	while i < 6:
		seconds = time.localtime(time.time())[5]
		time.sleep(next-seconds)
		#temp = bmp.readTemperature()
		#pressure = bmp.readPressure()
		#temparray.append(temp)
		#pressarray.append(pressure)
		DHThumidity, DHTtemperature = Adafruit_DHT.read_retry(sensor, pin)
		temperature2 = sensor2.get_temperature()
		temperature3 = sensor3.get_temperature()
		temperature4 = sensor4.get_temperature()
		humidityarray.append(DHThumidity)
		temparray1.append(DHTtemperature)
		temparray2.append(temperature2)
		temparray3.append(temperature3)
		temparray4.append(temperature4)
		i = i + 1
		next = next + 10;
		if next == 60:
			next = 58

	thetime = strftime("%Y-%m-%d %H:%M:%S", gmtime())
	thetimestamp = int(time.time())

	t1min, t1max, t1mean, t1len = calc_values(remove_wrong_entries(temparray1, 2))
	t2min, t2max, t2mean, t2len = calc_values(remove_wrong_entries(temparray2, 2))
	t3min, t3max, t3mean, t3len = calc_values(remove_wrong_entries(temparray3, 2))
	t4min, t4max, t4mean, t4len = calc_values(remove_wrong_entries(temparray4, 2))
	hmin, hmax, hmean, hlen = calc_values(remove_wrong_entries(humidityarray, 5))

	with open("measurements.dat", "a") as DataFile:
		DataFile.write("%d,%.2f,%.2f,%.2f,%d,%.2f,%.2f,%.2f,%d,%.2f,%.2f,%.2f,%d,%.2f,%.2f,%.2f,%d,%.2f,%.2f,%.2f,%d\n" % (thetimestamp, t1min, t1max, t1mean, t1len, t2min, t2max, t2mean, t2len, t3min, t3max, t3mean, t3len, t4min, t4max, t4mean, t4len, hmin, hmax, hmean, hlen))
		DataFile.close()
