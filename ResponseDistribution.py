"""Response-distribution figures."""
import os, sys
import pandas as pd, numpy as np, matplotlib as mpl
mpl.use('Agg'); import matplotlib.pyplot as plt
OUT = 'ItemPrep'
PLOT_DIR = os.path.join(OUT, 'RespDistPlot')
os.makedirs(PLOT_DIR, exist_ok=True)
X=pd.read_excel(os.path.join(OUT,'item_prep.xlsx'),sheet_name=None)
cat,dc=X['categories'],X['direction_check']
ORDER=['ATT','SN','PBC','HAB','INT','SA','PA','RBF','NBV','INFRA','MR','CAUS','PENF']
LABEL={'ATT':'Attitude','SN':'Social norms','PBC':'Perceived control','HAB':'Habit','INT':'Intention',
 'SA':'Social acceptance','PA':'Personal acceptance','RBF':'Risky behaviour','NBV':'Parking violations',
 'INFRA':'Infrastructure safety','MR':'Moral responsibility','CAUS':'Perceived causes','PENF':'Perceived enforcement'}
COL=['#d4d9ea','#a6bddb','#6fa3cc','#2b7bb9','#08457e']
mpl.rcParams.update({'font.family':'DejaVu Sans','font.size':8})
for mode in ['Cyclist','Pedestrian','Driver','Motorcyclist']:
    c=cat[(cat['mode']==mode)&cat.role.isin(ORDER)].copy()
    c=c.merge(dc[dc['mode']==mode][['item','item_rest_r']],on='item',how='left')
    c['o']=c.role.map(ORDER.index); c=c.sort_values('o',kind='stable')
    rows=[];y=0;heads=[]
    for f,g in c.groupby('o',sort=True):
        heads.append((y,LABEL[ORDER[f]])); y+=1
        for _,r in g.iterrows(): rows.append((y,r)); y+=1
        y+=0.4
    fig,ax=plt.subplots(figsize=(7.2,0.17*y+1.3))
    for yy,r in rows:
        n=np.array([r[f'n_{k}'] for k in range(1,6)],float); p=n/n.sum()*100; left=0
        for k in range(5):
            ax.barh(yy,p[k],left=left,color=COL[k],edgecolor='white',linewidth=0.4,height=0.8); left+=p[k]
        lab=r['item']+('†' if isinstance(r['merge'],str) else '')+('*' if r['item_rest_r']<.30 else '')
        ax.text(-1.5,yy,lab,ha='right',va='center',fontsize=7)
    for yy,h in heads: ax.text(-1.5,yy,h,ha='right',va='center',fontsize=8,fontweight='bold')
    ax.set_xlim(0,100); ax.set_ylim(y-0.4,-0.8); ax.set_yticks([])
    ax.set_xticks(range(0,101,25)); ax.set_xticklabels([f'{t}%' for t in range(0,101,25)])
    for s in ['top','right','left']: ax.spines[s].set_visible(False)
    ax.xaxis.grid(True,color='#dddddd',linewidth=0.5); ax.set_axisbelow(True)
    n=int(c.iloc[0][[f'n_{k}' for k in range(1,6)]].sum()+c.iloc[0]['missing'])
    ax.set_title(f'{mode}s (n = {n}): response distribution of measurement-model items',loc='left',fontsize=10,fontweight='bold',x=-0.22)
    hs=[mpl.patches.Patch(color=COL[k],label=str(k+1)) for k in range(5)]
    ax.legend(handles=hs,ncol=5,loc='upper left',bbox_to_anchor=(-0.22,-0.035/(0.17*y/8+0.2)*1.2),frameon=False,
              title='Response category (1 = lowest, 5 = highest)',
              title_fontsize=7,alignment='left')
    fig.text(0.01,0.005,'1 = never / unacceptable / strongly disagree / not important / very unlikely;  5 = always / acceptable / strongly agree / very important / very likely\n† category with fewer than 5 answers merged before modelling     * item-rest correlation below .30',fontsize=6.5,color='#444444',linespacing=1.5)
    fig.subplots_adjust(left=0.22,right=0.98,top=1-0.55/(0.17*y+1.3),bottom=1.05/(0.17*y+1.3))
    fig.savefig(os.path.    join(PLOT_DIR, f'fig_distributions_{mode}.png'), dpi=200)
    plt.close(fig)