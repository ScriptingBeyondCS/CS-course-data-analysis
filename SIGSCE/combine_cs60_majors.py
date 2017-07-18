import csv

r = csv.reader(open('cs60majors_cleaned.csv'))
lines = [l for l in r]

for line in lines:
    line.append(line[2][1:])

writer = csv.writer(open('cs60majors_combined.csv', 'w'))
writer.writerows(lines)
