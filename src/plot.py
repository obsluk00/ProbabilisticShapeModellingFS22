import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("../project-data/distances.csv")
df.plot(kind='scatter', x=' length', y=' width').get_figure().savefig('measurements.png')
