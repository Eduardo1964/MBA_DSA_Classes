# -*- coding: utf-8 -*-

"""
Análise de Correspondência Simples e Múltipla

Material Complementar MBA DSA (USP ESALQ)

Prof. Wilson Tarantin Junior

"""
#%% Carregar as bibliotecas

# Executar o seguinte comando no console: pip install -r requirements.txt

# Em seguida, importar os pacotes

import pandas as pd
import prince
from scipy.stats import chi2_contingency

#%% Análise de Correspondência Simples (ANACOR)

# Importar o banco de dados
## Fonte: Fávero e Belfiore (2017, Capítulo 11)

perfil = pd.read_excel("Perfil Aplicação.xlsx")

print(perfil)

#%% Analisando os dados do dataset

print(perfil.info())

#%% Analisando as características descritivas das variáveis do dataset

print(perfil.describe())

#%% Criando a tabela de contingência

tabela = pd.crosstab(perfil["Perfil"], perfil["Tipo de Aplicação"])

print(tabela)

#%% Analisando a significância estatística da associação (teste qui²)

chi2, pvalor, df, freq_esp = chi2_contingency(tabela)

print(f"estatística qui²: {chi2}") # estatística qui²
print(f"p-valor da estatística: {pvalor}") # p-valor da estatística
print(f"graus de liberdade: {df}") # graus de liberdade

#%% Elaborando a ANACOR

# Inicializando a instância da Anacor
ca = prince.CA()

#%% Ajustes do dataframe

# Renomeando as linhas e colunas do dataframe

tabela.columns.rename('Investimento', inplace=True)
tabela.index.rename('Perfil', inplace=True)

print(tabela)

#%% Fit do modelo

# Note que o input é a tabela de contingência criada antes

ca = ca.fit(tabela)

#%% Obtendo as coordenadas em linha e coluna

print(ca.row_coordinates(tabela), "\n")
print(ca.column_coordinates(tabela))

#%% Obtendo os eigenvalues

print(ca.eigenvalues_)

#%% Obtendo a inércia principal total

# É a soma dos eigenvalues (também é a divisão: estat. qui² / N)
# Quanto maior a inércia principal total, maior é a associação entre categorias

print(ca.total_inertia_)

#%% Inércia explicada por dimensão

# Indica o percentual da a inércia principal total explicado por cada dimensão

print(ca.explained_inertia_)

#%% Massas em linhas

print(ca.row_masses_)

#%% Massas em colunas

print(ca.col_masses_)

#%% Por fim, podemos plotar o mapa perceptual

ax = ca.plot_coordinates(X=tabela,
                         ax=None,
                         figsize=(10,10),
                         x_component=0,
                         y_component=1,
                         show_row_labels=True,
                         show_col_labels=True)

#%% Plotando o mapa percentual interativo

import plotly.graph_objects as go
import plotly.io as pio

pio.renderers.default='browser'

chart_df = pd.DataFrame({'obs_x':ca.row_coordinates(tabela)[0].values,
                         'obs_y': ca.row_coordinates(tabela)[1].values})

fig = go.Figure(data=go.Scatter(x=chart_df['obs_x'],
                                y=chart_df['obs_y'],
                                name=tabela.index.name,
                                textposition="top center",
                                text=tabela.index,
                                mode="markers+text",))

fig.add_trace(go.Scatter(
    x=ca.column_coordinates(tabela)[0].values,
    mode="markers+text",
    name=tabela.columns.name,
    textposition="top center",
    y=ca.column_coordinates(tabela)[1].values,
    text=ca.column_coordinates(tabela).index
))

fig.update_layout(
    autosize=False,
    width=800,
    height=800,
    title_text='Coordenadas principais'

)

fig.show()

#%% Análise de Correspondência Múltipla (MCA)

# Importar o banco de dados
## Fonte: Fávero e Belfiore (2017, Capítulo 11)

perfil_mca = pd.read_excel("Perfil Aplicação Civil.xlsx")

print(perfil_mca)

#%% Selecionando apenas variável para análise

perfil_mca_select = perfil_mca.drop(columns=['estudante'])

print(perfil_mca_select)

#%% Analisando as tabelas de contingência

tabela_mca_1 = pd.crosstab(perfil_mca["perfil"], perfil_mca["aplicacao"])
tabela_mca_2 = pd.crosstab(perfil_mca["perfil"], perfil_mca["estado_civil"])
tabela_mca_3 = pd.crosstab(perfil_mca["aplicacao"], perfil_mca["estado_civil"])

print(tabela_mca_1)

#%%

print(tabela_mca_2)

#%%

print(tabela_mca_3)

#%% Analisando a significância estatística das associações (teste qui²)

chi2, pvalor, df, freq_esp = chi2_contingency(tabela_mca_1)

print("Associação 1")
print(f"estatística qui²: {chi2}") # estatística qui²
print(f"p-valor da estatística: {pvalor}") # p-valor da estatística
print(f"graus de liberdade: {df} \n") # graus de liberdade

chi2, pvalor, df, freq_esp = chi2_contingency(tabela_mca_2)

print("Associação 2")
print(f"estatística qui²: {chi2}") # estatística qui²
print(f"p-valor da estatística: {pvalor}") # p-valor da estatística
print(f"graus de liberdade: {df} \n") # graus de liberdade

chi2, pvalor, df, freq_esp = chi2_contingency(tabela_mca_3)

print("Associação 3")
print(f"estatística qui²: {chi2}") # estatística qui²
print(f"p-valor da estatística: {pvalor}") # p-valor da estatística
print(f"graus de liberdade: {df}") # graus de liberdade

#%% Elaborando a MCA 

## Utiliza o método da matriz de Burt

mca = prince.MCA()
mca = mca.fit(perfil_mca_select)

#%% Obtendo as coordenadas nas duas dimensões do mapa

print(mca.column_coordinates(perfil_mca_select))

#%% Obtendo as coordenadas de cada um das observações

print(mca.row_coordinates(perfil_mca_select))

#%% Obtendo os eigenvalues

print(mca.eigenvalues_)

#%% Inércia principal total

print(mca.total_inertia_)

#%% Obtendo a variância

print(mca.explained_inertia_)

#%% Plotando o mapa perceptual

mp_mca = mca.plot_coordinates(
             X = perfil_mca_select,
             figsize=(12,12),
             show_row_points = True,
             show_column_points = True,
             show_row_labels=False,
             column_points_size = 100,
             show_column_labels = True,
             legend_n_cols = 2)

#%% Plotando o mapa perceptual interativo

import plotly.graph_objects as go
import plotly.io as pio

pio.renderers.default='browser'

chart_df = pd.DataFrame({'obs_x':mca.row_coordinates(perfil_mca_select)[0],
                         'estudante':perfil['Estudante'],
                         'obs_y': mca.row_coordinates(perfil_mca_select)[1]})

fig = go.Figure(data=go.Scatter(x=chart_df['obs_x'],
                                y=chart_df['obs_y'],
                                mode='markers',
                                name="Estudante",
                                text=chart_df['estudante']))

fig.add_trace(go.Scatter(
    x=mca.column_coordinates(perfil_mca_select)[0],
    mode='markers+text',
    name="Associação",
    marker={'size':12},
    y=mca.column_coordinates(perfil_mca_select)[1],
    textposition="top center",
    text=mca.column_coordinates(perfil_mca_select).index
))

fig.update_layout(
    autosize=False,
    width=800,
    height=800,
    title_text='Coordenadas das linhas e colunas'
)

fig.show()

#%% FIM!