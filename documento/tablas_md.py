# Genera las tablas del manuscrito (markdown) a partir de las salidas de R.
import csv, re, sys, statistics as st
R = '/home/claude/repo_cambio_partido/'
def rd(p, sep=','): return list(csv.DictReader(open(R + p, encoding='utf-8'), delimiter=sep))
def f(x, k=2):
    return ('−' if x < 0 else '') + f'{abs(x):.{k}f}'.replace('.', ',')
def stars(p):
    return '***' if p < 0.001 else '**' if p < 0.01 else '*' if p < 0.05 else ''

out = {}
# Tabla 1: mediana por partido y período
per = {'2014-2018': 2, '2018-2022': 18, '2022-2026': 34, '2026-2030': 50}
def med(suf):
    rows = rd(f'data/processed/panel_trimestral{suf}.csv')
    res = {}
    for r in rows:
        t = int(r['t']); p = '2014-2018' if t <= 17 else '2018-2022' if t <= 33 else '2022-2026' if t <= 49 else '2026-2030'
        res.setdefault((p, r['partido']), {'z': [], 'id': set()})
        res[(p, r['partido'])]['z'].append(float(r['z'])); res[(p, r['partido'])]['id'].add(r['diputado_id'])
    return {k: (st.median(v['z']), len(v['id'])) for k, v in res.items()}
m1, m2 = med(''), med('_omega1')
partidos = ['PC', 'PS', 'PPD', 'DC', 'PDG', 'EVOP', 'RN', 'UDI', 'PSC', 'PNL', 'PREP']
t1 = ['| Partido | 2014–2018 | 2018–2022 | 2022–2026 | 2026–2030 |', '|---|---|---|---|---|']
for pa in partidos:
    cells = []
    for p in per:
        a = m1.get((p, pa)); b = m2.get((p, pa))
        cells.append(f'{f(a[0])} ({f(b[0])})' if a and a[1] >= 4 else '–')
    t1.append(f'| {pa} | ' + ' | '.join(cells) + ' |')
out[1] = ('**Tabla 1.** Mediana de la posición de los diputados por partido y período legislativo',
          t1, 'Posición estandarizada dentro de cada trimestre; valores negativos a la izquierda y positivos a la derecha. El primer valor corresponde a la especificación más suavizada (ω² = 0,1) y el valor entre paréntesis, a la menos suavizada (ω² = 1). Se omiten los partidos con menos de cuatro diputados en el período. Las siglas se detallan en el Anexo C. Elaboración propia con datos de la Cámara de Diputadas y Diputados de Chile (s. f.).')

# Tabla 2: modelos de riesgo
mods = [('m1_toda_salida', 'Todas las salidas'), ('m3_hacia_radical', 'Hacia la derecha radical'),
        ('m5_radical_rez', 'Hacia la derecha radical, rezago de 2 a 4 trimestres'),
        ('m7_radical_distrito', 'Hacia la derecha radical, con variables electorales'),
        ('m4_otras_conv', 'Otras salidas de la derecha convencional')]
filas = [('dist_der', 'Distancia hacia la derecha'), ('dist_izq', 'Distancia hacia la izquierda'),
         ('I(demanda_c/10)', 'Demanda de derecha radical (10 puntos)'),
         ('I(cuota_propia/10)', 'Votación personal (10 puntos)'), ('sin_reeleccion', 'Sin reelección posible')]
t2 = ['| | (1) | (2) | (3) | (4) | (5) |', '|---|---|---|---|---|---|']
for suf, nom in (('', 'Especificación más suavizada (ω² = 0,1)'), ('_omega1', 'Especificación menos suavizada (ω² = 1)')):
    rows = rd(f'output/tablas/04_modelos_riesgo{suf}.csv')
    t2.append(f'| *{nom}* | | | | | |')
    for key, lab in filas:
        cells = []
        for m, _ in mods:
            k2 = key + '24' if m == 'm5_radical_rez' and key in ('dist_der', 'dist_izq') else key
            r = [x for x in rows if x['modelo'] == m and x['termino'] == k2]
            if not r: cells.append(''); continue
            r = r[0]; c, e, p = float(r['coef']), float(r['ee']), float(r['p'])
            if m == 'm4_otras_conv' and key == 'dist_der' and abs(c) > 10: cells.append('n. e.'); continue
            cells.append(f'{f(c)}{stars(p)} ({f(e)})')
        t2.append(f'| {lab} | ' + ' | '.join(cells) + ' |')
    nrow = []; erow = []
    for m, _ in mods:
        r = [x for x in rows if x['modelo'] == m][0]
        nrow.append(f"{int(float(r['n'])):,}".replace(',', '.')); erow.append(str(int(float(r['eventos']))))
    t2.append('| Diputado-trimestres | ' + ' | '.join(nrow) + ' |')
    t2.append('| Salidas | ' + ' | '.join(erow) + ' |')
out[2] = ('**Tabla 2.** Modelos de riesgo de salida de partidos de derecha, 2014–2026', t2,
          'Coeficientes de modelos de tiempo discreto con enlace logarítmico complementario; errores estándar agrupados por diputado entre paréntesis. Todos los modelos incluyen efectos fijos de período legislativo. La distancia es la diferencia entre la posición del diputado y la mediana de los demás militantes de su partido en el trimestre anterior (en el modelo 3, promedio de los trimestres t − 4 a t − 2), separada según su dirección. Los modelos 1 a 3 y 5 usan el período 2014–2026; el modelo 4, el período desde 2018, para el que existen las variables electorales. Los modelos 2 a 5 se restringen a militantes de la derecha convencional. Con enlace logit, los coeficientes del modelo 2 son {LOGIT}. La sigla n. e. indica un coeficiente no estimable por separación completa. \\* p < 0,05; \\*\\* p < 0,01; \\*\\*\\* p < 0,001. Elaboración propia con datos de la Cámara de Diputadas y Diputados de Chile (s. f.), Servicio Electoral de Chile (s. f.) y fuentes del Anexo A.')

lg = [float([x for x in rd(f'output/tablas/04_modelos_riesgo{suf}.csv') if x['modelo'] == 'm6_radical_logit' and x['termino'] == 'dist_der'][0]['coef']) for suf in ('', '_omega1')]
out[2] = (out[2][0], out[2][1], out[2][2].replace('{LOGIT}', f'{f(lg[0])} y {f(lg[1])}'))

# Tabla 3: pruebas de proceso
rs = rd('output/tablas/08_resumen_pruebas.csv')
t3 = ['| Grupo | Salidas | P, aro | P, pistola | O, aro | O, pistola | C, aro | C, pistola |',
      '|---|---|---|---|---|---|---|---|']
nomg = {'Hacia la derecha radical': 'Hacia la derecha radical', 'Otras salidas de la derecha convencional': 'Otras salidas de la derecha convencional',
        'Salidas de la derecha radical': 'Salidas de partidos de derecha radical'}
for r in rs:
    t3.append(f"| {nomg[r['grupo']]} | {r['n']} | {r['aro_prog']} | {r['pistola_prog']} | {r['aro_org']} | {r['pistola_org']} | {r['aro_carr']} | {r['pistola_carr']} |")
out[3] = ('**Tabla 3.** Número de salidas que pasan cada prueba de proceso, por grupo', t3,
          'Prueba de aro (condición necesaria) y de pistola humeante (condición suficiente) para cada explicación, donde P corresponde a la explicación programática, O a la organizacional y C a la de carrera; los criterios se describen en la sección 4.5 y el detalle por caso en el Anexo B. Las pruebas programáticas no se aplican a las tres salidas de partidos con menos de tres diputados. Elaboración propia con datos de la Cámara de Diputadas y Diputados de Chile (s. f.), Servicio Electoral de Chile (s. f.) y fuentes del Anexo A.')

# Anexo A: salidas
MED = {'biobiochile': 'BioBioChile', 'elmostrador': 'El Mostrador', 'emol': 'Emol', 'eldinamo': 'El Dínamo', 'cnnchile': 'CNN Chile', 'df.cl': 'Diario Financiero', 'bcn.cl': 'BCN',
       'latercera': 'La Tercera', 'theclinic': 'The Clinic', 'elmostrador': 'El Mostrador', 'cooperativa': 'Cooperativa',
       'chilevision': 'Chilevisión', 'pauta': 'Pauta'}
MOT = {'PROG': 'programático', 'CONDUC': 'conducción del partido', 'DISC': 'disciplina', 'CARRERA': 'carrera',
       'PERS': 'personal', 'ESC': 'investigación judicial'}
def medio(u):
    for k, v in MED.items():
        if k in u: return v
    return u
sal = rd('data/fuentes/salidas_partido.csv', ';')
mes = ['ene.', 'feb.', 'mar.', 'abr.', 'may.', 'jun.', 'jul.', 'ago.', 'sep.', 'oct.', 'nov.', 'dic.']
def fecha(s):
    y, m, d = s.split('-'); return f'{int(d)} {mes[int(m) - 1]} {y}'
tA = ['| Diputado | Partido de origen | Fecha de salida | Destino | Motivo declarado | Fuentes |', '|---|---|---|---|---|---|']
for r in sal:
    fu = sorted({medio(r['fuente_fecha']), medio(r['fuente_motivo'])} - {''})
    mot = MOT.get(r['motivo_principal'], '') + (f", {MOT[r['motivo_secundario']]}" if r['motivo_secundario'] in MOT else '')
    dest = r['destino'] + (' (bloque radical)' if r['destino'] == 'IND' and r['destino_bloque'] == 'radical' else '')
    tA.append(f"| {r['nombre']} | {r['partido_origen']} | {fecha(r['fecha_salida'])} | {dest} | {mot} | {', '.join(fu)} |")
out['A'] = ('**Tabla A1.** Salidas de partidos de derecha durante el mandato, 2014–2026', tA,
            'La fecha es la del anuncio público. El destino es el partido al que se incorporó después, e IND (bloque radical) indica que, sin militar, se integró al comité o a la lista de un partido de derecha radical. Motivo declarado según las fuentes indicadas. Las direcciones electrónicas de cada fuente, el detonante y el fundamento del destino están en data/fuentes/salidas_partido.csv y data/fuentes/detonantes.csv del repositorio.')

# Tabla 4: percentil previo por área temática
rt = rd('output/tablas/12_rangos_por_tema.csv')
AREA = [('todas', 'Todas las votaciones'), ('orden', 'Orden, seguridad y migración'), ('valorico', 'Género, sexualidad y familia'), ('economia', 'Economía')]
nv = {}
for r in rd('output/tablas/12_percentil_por_tema.csv'):
    nv.setdefault(r['area'], {})[r['periodo']] = int(float(r['n_votos']))
t4 = ['| Área | Votaciones | Hacia la derecha radical | p | Otras salidas de la derecha convencional | p |', '|---|---|---|---|---|---|']
for a, lab in AREA:
    h = [x for x in rt if x['area'] == a and x['grupo'] == 'Hacia la derecha radical'][0]
    o = [x for x in rt if x['area'] == a and x['grupo'] != 'Hacia la derecha radical'][0]
    nvt = f"{sum(nv[a].values()):,}".replace(',', '.')
    t4.append(f"| {lab} | {nvt} | {f(float(h['percentil_medio']))} (n = {h['n']}) | {f(float(h['p_derecha']), 3)} | {f(float(o['percentil_medio']))} (n = {o['n']}) | {f(float(o['p_izquierda']), 3)} |")
out[4] = ('**Tabla 4.** Percentil medio de la posición previa a la salida dentro del partido de origen, por área temática', t4,
          'Posiciones estimadas con un modelo IRT estático por período legislativo y área, en el que cada diputado que sale aporta una posición antes y otra después de su salida. El percentil se calcula dentro de los militantes del partido de origen en el trimestre previo a la salida; 0,5 es el valor esperado si la posición no se relacionara con la salida. Para las salidas hacia la derecha radical, p es la probabilidad de observar un percentil medio igual o mayor bajo la hipótesis de distribución uniforme; para las demás salidas, la de observar uno igual o menor (100.000 simulaciones). Votaciones es el total de votaciones usadas en los tres períodos. Las áreas se asignan con un diccionario aplicado al título de cada proyecto, y una votación puede pertenecer a más de un área. El período 2026–2030 no se incluye porque tiene una sola salida. Elaboración propia con datos de la Cámara de Diputadas y Diputados de Chile (s. f.).')

# Tabla 5: Senado
ts5 = rd('output/tablas/13_senado_salidas.csv')
ssen = {(r['nombre'], r['fecha_salida']): r for r in rd('data/fuentes/salidas_senado.csv', ';')}
GR = {'Hacia la derecha radical': 'hacia la derecha radical', 'Otras salidas de la derecha convencional': 'otra, derecha convencional',
      'Salidas de la derecha radical': 'desde la derecha radical'}
def num(x, k=2):
    return '–' if x in ('', 'NA', 'NaN') else f(float(x), k)
t5 = ['| Senador | Partido de origen | Fecha | Grupo | Motivo declarado | Percentil previo | Cambio posterior | Fuentes |', '|---|---|---|---|---|---|---|---|']
for r in ts5:
    z = ssen[(r['nombre'], r['fecha_salida'])]
    mot = MOT.get(z['motivo_principal'], '') + (f", {MOT[z['motivo_secundario']]}" if z['motivo_secundario'] in MOT else '')
    fu = sorted({medio(z['fuente_fecha']), medio(z['fuente_motivo'])} - {''})
    pc = '–' if r['percentil_w01'] in ('', 'NA') else f"{num(r['percentil_w01'])} / {num(r['percentil_w1'])}"
    cb = '–' if r['cambio_w01'] in ('', 'NA', 'NaN') else f"{num(r['cambio_w01'])} / {num(r['cambio_w1'])}"
    nombre = r['nombre'] + (' (suspensión)' if r['tipo'] == 'suspension' else '')
    t5.append(f"| {nombre} | {r['partido_origen']} | {fecha(r['fecha_salida'])} | {GR[r['grupo']]} | {mot} | {pc} | {cb} | {', '.join(fu)} |")
out[5] = ('**Tabla 5.** Salidas de partidos de derecha en el Senado, 2014–2026', t5,
          'La fecha es la del anuncio público o, en el caso de Alejandro Kusanovic, la de la suspensión de su militancia por el partido. El percentil previo es el de la distancia promedio a la mediana de los demás militantes del partido de origen en los cuatro trimestres previos, o de su bloque si el partido tenía menos de tres senadores; el cambio posterior es la variación de esa distancia entre los cuatro trimestres previos y los disponibles hasta cuatro trimestres después, en desviaciones estándar. Ambos se presentan en las especificaciones más suavizada y menos suavizada. El guion indica que no hay datos suficientes, porque el grupo de referencia tenía menos de tres senadores o porque el senador no volvió a votar. Elaboración propia con datos del Senado de Chile (s. f.) y las fuentes indicadas; las direcciones electrónicas están en data/fuentes/salidas_senado.csv del repositorio.')

# Anexo B: matriz por caso
mz = rd('output/tablas/08_matriz_pruebas.csv')
tB = ['| Diputado (salida) | Percentil previo | Detonante | Elección siguiente | Aro | Pistola |', '|---|---|---|---|---|---|']
DET = {'NINGUNO': 'sin detonante documentado', 'CONFLICTO': 'conflicto', 'SANCION': 'sanción', 'CANDIDATURA': 'candidatura', 'ESCANDALO': 'investigación'}
for r in mz:
    pc = '–' if r['percentil_w01'] in ('', 'NA') else f"{f(float(r['percentil_w01']))} / {f(float(r['percentil_w1']))}"
    aro = ', '.join(x for x, k in (('P', 'aro_prog'), ('O', 'aro_org'), ('C', 'aro_carr')) if r[k] == 'TRUE') or 'ninguna'
    pis = ', '.join(x for x, k in (('P', 'pistola_prog'), ('O', 'pistola_org'), ('C', 'pistola_carr')) if r[k] == 'TRUE') or 'ninguna'
    sig = r['siguiente'].replace('no compite', 'no compite').replace('sin elección aún', 'sin elección aún')
    sig = re.sub(r'\(ind\.\)', '(independiente)', sig)
    tB.append(f"| {r['nombre']} ({r['partido_origen']}, {r['fecha_salida'][:4]}) | {pc} | {DET.get(r['tipo_detonante'], '')} | {sig} | {aro} | {pis} |")
out['B'] = ('**Tabla B1.** Evidencia y pruebas de proceso por salida', tB,
            'El percentil previo es el de la distancia promedio a la mediana del partido en los cuatro trimestres previos, en las especificaciones más suavizada y menos suavizada. La elección siguiente es la candidatura en la primera elección parlamentaria o presidencial posterior a la salida (Servicio Electoral de Chile, s. f.). En las pruebas, P corresponde a la explicación programática, O a la organizacional y C a la de carrera.')

# Anexo C: siglas
tC = ['| Sigla | Partido |', '|---|---|']
SIG = [('AMPL', 'Amplitud'), ('PDC', 'Partido Demócrata Cristiano'), ('EVOP', 'Evolución Política (Evópoli)'),
       ('FA', 'Frente Amplio'), ('IND', 'Independiente'), ('PCCh', 'Partido Comunista de Chile'),
       ('PCC', 'Partido Conservador Cristiano'), ('PDG', 'Partido de la Gente'), ('PNL', 'Partido Nacional Libertario'),
       ('PPD', 'Partido por la Democracia'), ('PRCh', 'Partido Republicano'), ('PS', 'Partido Socialista de Chile'),
       ('PSC', 'Partido Social Cristiano'), ('RN', 'Renovación Nacional'), ('UDI', 'Unión Demócrata Independiente')]
for s_, n in sorted(SIG, key=lambda x: x[0].upper()):
    tC.append(f'| {s_} | {n} |')
out['C'] = ('**Tabla C1.** Siglas de partidos', tC, None)

def disp(t):
    # siglas de presentación (los datos usan PREP, DC y PC)
    t = re.sub(r'\bPREP\b', 'PRCh', t)
    t = re.sub(r'\bDC\b', 'PDC', t)
    return re.sub(r'\bPC\b', 'PCCh', t)

def bloque(k):
    tit, rows, nota = out[k]
    s = tit + '\n\n' + '\n'.join(disp(r) for r in rows) + '\n'
    if nota: s += '\n*Nota.* ' + nota + '\n'
    return s

src = open(R + 'documento/manuscrito.md', encoding='utf-8').read()
for k in (1, 2, 3, 4, 5):
    src = src.replace(f'[[TABLA:{k}]]', bloque(k).strip())
for k in ('A', 'B', 'C'):
    src = src.replace(f'[[ANEXO:{k}]]', bloque(k).strip())
# bootstrap
try:
    b = rd('output/tablas/04b_bootstrap_resumen.csv')[0]
    rep = {'{BOOT_N}': b['replicas'], '{BOOT_POS}': str(round(float(b['coef_positivo']) * int(b['replicas']))),
           '{BOOT_MED}': f(float(b['coef_mediana'])), '{BOOT_P05}': f(float(b['coef_p05'])), '{BOOT_P95}': f(float(b['coef_p95'])),
           '{BOOT_PCT}': f(float(b['pct_mediana'])) + f", con un intervalo del 90% entre {f(float(b['pct_p05']))} y {f(float(b['pct_p95']))}"}
    for k, v in rep.items(): src = src.replace(k, v)
except FileNotFoundError:
    print('sin bootstrap')
open(R + 'documento/manuscrito_final.md', 'w', encoding='utf-8').write(src)
print('ok')
