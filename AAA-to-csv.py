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
aaaLblRaw=""
tokLbl=""
readMyCoords=True


# Table header
print ("Speaker\tTask\tLabel\tTokNum\tX\tY\tAAAid")


for line in lines:

	line = line.strip()
	line = re.sub(r"(\s)+", r"\1", line)

	if not line:
		continue

	if "*" in line:
		continue
	
	if "Tongue" in line:
		readMyCoords = True
		aaaLblRaw=line
		line = re.sub(r",", r" ", line)
		line = re.sub(r"(\s)+", r"\1", line)
		line = line.split(" ")
		tokLbl = line[1]
		aaaID = line[5]
		continue

	if "Mean" in line:
		readMyCoords = False
		aaaLblRaw=line
		# Flips the task type
		task = IMIT_TASK_ID #if task==BASE_TASK_ID else BASE_TASK_ID

	if "Diff" in line:
		readMyCoords = False
		aaaLblRaw=line
		# Flips the task type
		task = BASE_TASK_ID

	if readMyCoords:
		line = line.split("\t")
		coord1 = line[0]
		coord2 = line[1]

		# Skips the extraneous line giving coordinates for the origin point
		if coord1=="123.472" and coord2 == "0.000":
			continue

		print ("\t".join([spk,task,tokLbl,aaaID,coord1,coord2,aaaLblRaw]))