import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("F:/Databases/ProbabilisticShapeModellingFS22/project-data/distances.csv")
df.plot(kind='scatter', x=' length', y=' width').get_figure().savefig('measurements.png')

# mean length 420.440909, mean width 60.529105
print(df.mean())
# length squared variance 848.037875, width squared variance 18.477001
print(df.var())
