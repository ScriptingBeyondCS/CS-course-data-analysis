import pandas as pd
import numpy as np
from scipy import stats

df = pd.read_csv('fulldata_42.csv', header=0, nrows=1329)

colnames = df.columns.values.tolist()
grid = df.values.tolist()

def gradeStats(coursetitle, dataframe):
    index = dataframe.columns.values.tolist().index(coursetitle)
    
    green_total = 0
    gold_total = 0
    black_total = 0

    green_count = 0
    gold_count = 0
    black_count = 0

    green_values = []
    gold_values = []
    black_values = []

    for row in grid:
        if row[index] == row[index]: \
            #and row[index] != 0: #include to remove pass/fail grades
            if row[0] == 'green':
                green_total += row[index]
                green_count += 1
                green_values += [row[index]]
            elif row[0] == 'gold':
                gold_total += row[index]
                gold_count += 1
                gold_values += [row[index]]
            elif row[0] == 'black':
                black_total += row[index]
                black_count += 1
                black_values += [row[index]]

    try: green_avg = green_total/green_count
    except: green_avg = 0
    try: gold_avg = gold_total/gold_count
    except: gold_avg = 0
    try: black_avg = black_total/black_count
    except: black_avg = 0
    
    green_stdev = np.std(green_values)
    gold_stdev = np.std(gold_values)
    black_stdev = np.std(black_values)

    green_gold_ttest = stats.ttest_ind_from_stats(green_avg, green_stdev, green_count, gold_avg, gold_stdev, gold_count)
    green_black_ttest = stats.ttest_ind_from_stats(green_avg, green_stdev, green_count, black_avg, black_stdev, black_count)
    gold_black_ttest = stats.ttest_ind_from_stats(gold_avg, gold_stdev, gold_count, black_avg, black_stdev, black_count)

    #print('green average grade in %sis ' % coursetitle, green_avg)
    #print('gold  average grade in %sis ' % coursetitle, gold_avg)
    #print('black average grade in %sis ' % coursetitle, black_avg)
    print('green stdev in %sis ' % coursetitle, green_stdev)
    print('gold  stdev in %sis ' % coursetitle, gold_stdev)
    print('black stdev in %sis ' % coursetitle, black_stdev)
    print()
    print('green/gold has p = ', green_gold_ttest[1])
    print('green/black has p = ', green_black_ttest[1])
    print('gold/black has p = ', gold_black_ttest[1])
    print()

    #return green_avg, gold_avg, black_avg

#three examples:
gradeStats('BIOL052  HM ', df)
gradeStats('CSCI060  HM ', df)
gradeStats('CSCI070  HM ', df)

