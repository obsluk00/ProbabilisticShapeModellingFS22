import pandas as pd
import matplotlib.pyplot as plt

referenceDF = pd.read_csv("/home/damian/Documents/Uni/Probabilistic Shape Modelling/scalismolab/datasets/project-data/distances.csv")
gpDF = pd.read_csv("/home/damian/Documents/Uni/Probabilistic Shape Modelling/scalismolab/datasets/project-data/distancesGPModel.csv")

ax = referenceDF.plot(kind='scatter', x=' length', y=' width', color='Green')
gpDF.plot(ax=ax, kind='scatter', x=' length', y=' width', color='Orange').get_figure().savefig('measurements.png')

# mean length 420.440909, mean width 60.529105
print(referenceDF.mean())
# length squared variance 848.037875, width squared variance 18.477001
print(referenceDF.var())

# mean length 425.530941, 70.087738 mean width
print(gpDF.mean())
# length squared variance 376.879229, width squared variance 5.400647
print(gpDF.var())
