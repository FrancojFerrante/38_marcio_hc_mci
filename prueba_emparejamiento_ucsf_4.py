# -*- coding: utf-8 -*-
"""
Created on Thu May 18 17:40:08 2023

@author: Usach
"""

import pandas as pd
from scipy import stats
from scipy.stats import f_oneway, chi2_contingency, ttest_ind, dunnett
import pandas as pd
import numpy as np

def emparejamiento_estadistico_f1_parejo(df,column_group,variables,tipos,min_p_value,col_conservar = None,n_dif = 5, n_max = None):
    
    def calcular_resultado(lista):
        multiplicacion = 1
        suma = sum(lista)
        cantidad_elementos = len(lista)
    
        for elemento in lista:
            multiplicacion *= elemento
    
        resultado = (multiplicacion / suma) * cantidad_elementos
        return resultado

    grupos = df[column_group]

    p_values = []
    
    if len(np.unique(grupos)) > 2:
        for variable, tipo in zip(variables, tipos):
            if tipo == "continua":
                # Eliminar NaN de la variable antes de calcular el p-value
                variable_sin_nan = df[variable].dropna()
                grupos_sin_nan = grupos[df[variable].notna()]
                p_values.append(f_oneway(*[variable_sin_nan[grupos_sin_nan == g] for g in np.unique(grupos_sin_nan)])[1])
            if tipo == "categorica":
                # Eliminar NaN de la variable antes de calcular el p-value
                variable_sin_nan = df[variable].dropna()
                grupos_sin_nan = grupos[df[variable].notna()]
                tabla_contingencia = pd.crosstab(grupos_sin_nan, variable_sin_nan)
                _, p_value_categorica, _, _ = chi2_contingency(tabla_contingencia)
                p_values.append(p_value_categorica)
    else:
        for variable, tipo in zip(variables, tipos):
            if tipo == "continua":
                variable_sin_nan = df[variable].dropna()
                grupos_sin_nan = grupos[df[variable].notna()]
                p_values.append(stats.ttest_ind(*[variable_sin_nan[grupos_sin_nan == g] for g in np.unique(grupos_sin_nan)])[1])
            if tipo == "categorica":
                grupos_sin_nan = grupos[df[variable].notna()]
                variable_sin_nan = df[variable].dropna()
                tabla_contingencia = pd.crosstab(grupos_sin_nan, variable_sin_nan)
                _, p_value_categorica, _, _ = chi2_contingency(tabla_contingencia)
                p_values.append(p_value_categorica)


    iteracion = 0
    # df_emparejado = pd.DataFrame()
    while any(valor < min_p_value for valor in p_values) or (((df[column_group].value_counts().max()) - (df[column_group].value_counts().min())) > n_dif)\
    or ((n_max is not None) and (df[column_group].value_counts().max() > n_max)):
    
        print(str(iteracion) + ")")

        grupo_mayor = df[column_group].value_counts().idxmax()        
        
        puntaje_p = 0
        puntaje_p_mayor = 0
        participante_mayor = 0
        # Crear un bucle for para iterar sobre cada participante del grupo mayor
        for participante in df[df[column_group] == grupo_mayor].index:
            # Eliminar el participante del DataFrame copia
            
            if (col_conservar is not None) and (df.loc[participante][col_conservar] == True):
                continue
            
            df_copia = df.drop(participante)
            
            grupos = df_copia[column_group]
            
            p_values = []
            
            if len(np.unique(grupos)) > 2:
                for variable, tipo in zip(variables, tipos):
                    if tipo == "continua":
                        # Eliminar NaN de la variable antes de calcular el p-value
                        variable_sin_nan = df_copia[variable].dropna()
                        grupos_sin_nan = grupos[df_copia[variable].notna()]
                        p_values.append(f_oneway(*[variable_sin_nan[grupos_sin_nan == g] for g in np.unique(grupos_sin_nan)])[1])
                    if tipo == "categorica":
                        # Eliminar NaN de la variable antes de calcular el p-value
                        variable_sin_nan = df_copia[variable].dropna()
                        grupos_sin_nan = grupos[df_copia[variable].notna()]
                        tabla_contingencia = pd.crosstab(grupos_sin_nan, variable_sin_nan)
                        _, p_value_categorica, _, _ = chi2_contingency(tabla_contingencia)
                        p_values.append(p_value_categorica)
            else:
                for variable, tipo in zip(variables, tipos):
                    if tipo == "continua":
                        variable_sin_nan = df_copia[variable].dropna()
                        grupos_sin_nan = grupos[df_copia[variable].notna()]
                        p_values.append(stats.ttest_ind(*[variable_sin_nan[grupos_sin_nan == g] for g in np.unique(grupos_sin_nan)])[1])
                    if tipo == "categorica":
                        grupos_sin_nan = grupos[df_copia[variable].notna()]
                        variable_sin_nan = df_copia[variable].dropna()
                        tabla_contingencia = pd.crosstab(grupos_sin_nan, variable_sin_nan)
                        _, p_value_categorica, _, _ = chi2_contingency(tabla_contingencia)
                        p_values.append(p_value_categorica)
            
            puntaje_p = calcular_resultado(p_values)
            
            if puntaje_p >= puntaje_p_mayor:
                puntaje_p_mayor = puntaje_p
                participante_mayor = participante
               
        df = df.drop(participante_mayor)
        iteracion += 1
        grupos = df[column_group]

        p_values = []
        if len(np.unique(grupos)) > 2:
            for variable, tipo in zip(variables, tipos):
                if tipo == "continua":
                    # Eliminar NaN de la variable antes de calcular el p-value
                    variable_sin_nan = df[variable].dropna()
                    grupos_sin_nan = grupos[df[variable].notna()]
                    p_values.append(f_oneway(*[variable_sin_nan[grupos_sin_nan == g] for g in np.unique(grupos_sin_nan)])[1])
                if tipo == "categorica":
                    # Eliminar NaN de la variable antes de calcular el p-value
                    variable_sin_nan = df[variable].dropna()
                    grupos_sin_nan = grupos[df[variable].notna()]
                    tabla_contingencia = pd.crosstab(grupos_sin_nan, variable_sin_nan)
                    _, p_value_categorica, _, _ = chi2_contingency(tabla_contingencia)
                    p_values.append(p_value_categorica)
        else:
            for variable, tipo in zip(variables, tipos):
                if tipo == "continua":
                    variable_sin_nan = df[variable].dropna()
                    grupos_sin_nan = grupos[df[variable].notna()]
                    p_values.append(stats.ttest_ind(*[variable_sin_nan[grupos_sin_nan == g] for g in np.unique(grupos_sin_nan)])[1])
                if tipo == "categorica":
                    grupos_sin_nan = grupos[df[variable].notna()]
                    variable_sin_nan = df[variable].dropna()
                    tabla_contingencia = pd.crosstab(grupos_sin_nan, variable_sin_nan)
                    _, p_value_categorica, _, _ = chi2_contingency(tabla_contingencia)
                    p_values.append(p_value_categorica)

    return df

df = pd.read_csv("C:/Franco/Doctorado/Laboratorio/38_marcio_hc_mci/demographic_data.csv")

column_group = 'group'

# df.loc[df['Zac\'s DX'] == 'NC', 'MMSE'] = np.nan

variables = ["age","sex","education"]
tipos = ["continua","categorica","continua"]
min_p_value = 0.2

df_emparejado = emparejamiento_estadistico_f1_parejo(df,column_group,variables,tipos,min_p_value)

# df_emparejado = df_emparejado.merge(df_copy_mmse)
# df_emparejado = df_emparejado.drop(columns=["MMSE"],axis=1)

str_p_value = str(min_p_value).replace(".","-")
df_emparejado.to_csv("D:/Franco/Doctorado/Laboratorio/17_aphasia_verbal_fluency_ucsf/mayor_a_2/emparejamiento/demografia_emparejada_mmse_age_fluency.csv",index=False)

