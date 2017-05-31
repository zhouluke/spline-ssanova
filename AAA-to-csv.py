# encoding=utf8
import sys, re
reload(sys)
sys.setdefaultencoding('utf8')


spk = sys.argv[1]
fileName = sys.argv[2]


file = open(fileName,'r')
lines = file.readlines()


BASE_TASK_ID = "base"
IMIT_TASK_ID = "imit"


task=BASE_TASK_ID
annotLbl=""
readMyCoords=True


# Table header
print ("Speaker\tTask\tLabel\tAAA-ID\tr\ttheta")


for line in lines:

	line = line.strip()
	line = re.sub(r"(\s)+", r"\1", line)

	if not line:
		continue

	if "*" in line:
		continue
	
	if "Tongue" in line:
		readMyCoords = True
		line = re.sub(r",", r" ", line)
		line = re.sub(r"(\s)+", r"\1", line)
		line = line.split(" ")
		annotLbl = line[1]
		aaaID = line[5]
		continue

	if "Mean" in line:
		readMyCoords = False
		# Flips the task type
		task = IMIT_TASK_ID #if task==BASE_TASK_ID else BASE_TASK_ID

	if readMyCoords:
		line = line.split("\t")
		r = line[0]
		theta = line[1]

		# Skips the extraneous line giving coordinates for the origin point
		if r=="123.472" and theta == "0.000":
			continue

		print ("\t".join([spk,task,annotLbl,aaaID,r,theta]))