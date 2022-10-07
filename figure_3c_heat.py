import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv('source_data/fig_3c_heat_source_data.txt', sep="\t")
df['bar'] = 1.0
numtraits = len(list(df.iloc[:, 3:]))  #make sure this matched the for loop for iloc positions
fig, axes = plt.subplots(nrows=numtraits, ncols=1, figsize=(15,8))
colors = {1:'gainsboro', 2:'gold'}
count = 0
for (columnName, columnData) in df.iloc[:, 3:].iteritems():
    print(columnName)
    df2 = df.sort_values(by=[columnName])
    ax = df2.plot.bar(x='Taxa', y='bar', ax=axes[count], rot=1, width=1, color=df2['6D-6276646_MAF_split'].apply(lambda x: colors[x]))
    ax.axis('off')
    ax.get_legend().remove()
    ax.set_title(columnName,x=-0.073,y=0.2, fontsize=14, fontweight='bold')
    count += 1
fig.savefig('hibap_heat_new_6Dallelesplit_gold.png', transparent=True)