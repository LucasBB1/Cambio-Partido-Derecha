# -*- coding: utf-8 -*-
# Arma el Documento de trabajo N.° 5 a partir del N.° 4 (plantilla) y el manuscrito en markdown.
import docx, copy, re, sys
from docx.shared import Pt, Inches
from docx.oxml.ns import qn
from docx.oxml import OxmlElement

W = '{http://schemas.openxmlformats.org/wordprocessingml/2006/main}'
TPL = '/home/claude/entregables/Bottinelli-Baltra_2026_Fragmentacion-Congreso.docx'
OUT = sys.argv[1] if len(sys.argv) > 1 else '/home/claude/entregables/Bottinelli-Baltra_2026_Cambio-partido-derecha.docx'
REPO = 'https://github.com/LucasBB1/Cambio-Partido-Derecha'
FIGDIR = '/home/claude/repo_cambio_partido/output/figuras/'
MD = open('/home/claude/repo_cambio_partido/documento/manuscrito_final.md', encoding='utf-8').read()
TITULO = MD.split('\n', 1)[0].lstrip('# ').strip()

d = docx.Document(TPL)
body = d.element.body
P = d.paragraphs

from docx.opc.constants import RELATIONSHIP_TYPE as RT

def separar_url(parts):
    # separa las direcciones electrónicas para convertirlas en vínculos activos
    out = []
    for txt, f in parts:
        for tok in re.split(r'(https?://\S+)', txt):
            if not tok: continue
            if tok.startswith('http'):
                m = re.match(r'(.*?)([.,;:)]*)$', tok)
                out.append((m.group(1), dict(f, url=m.group(1))))
                if m.group(2): out.append((m.group(2), f))
            else:
                out.append((tok, f))
    return out

def set_text(p, parts):
    runs = p.runs
    base = copy.deepcopy(runs[0]._r) if runs else None
    for r in runs: r._r.getparent().remove(r._r)
    for txt, f in separar_url(parts):
        r = copy.deepcopy(base) if base is not None else OxmlElement('w:r')
        for c in list(r):
            if c.tag != W + 'rPr': r.remove(c)
        rp = r.find(W + 'rPr')
        if rp is not None:
            for tag in ('b', 'bCs', 'i', 'iCs', 'vertAlign'):
                for x in rp.findall(W + tag): rp.remove(x)
        t = OxmlElement('w:t'); t.text = txt; t.set(qn('xml:space'), 'preserve'); r.append(t)
        if f.get('url'):
            h = OxmlElement('w:hyperlink')
            h.set(qn('r:id'), p.part.relate_to(f['url'], RT.HYPERLINK, is_external=True))
            h.append(r); p._p.append(h)
        else:
            p._p.append(r)
        run = docx.text.run.Run(r, p)
        if f.get('b'): run.bold = True
        if f.get('i'): run.italic = True
        if f.get('url'):
            run.font.color.rgb = docx.shared.RGBColor(0x05, 0x63, 0xC1); run.font.underline = True

def inline(s):
    parts = []
    s = s.replace('\\*', '\x00')
    for tok in re.split(r'(\*\*[^*]+\*\*|\*[^*]+\*)', s):
        if not tok: continue
        tok = tok.replace('\x00', '*')
        if tok.startswith('**'): parts.append((tok[2:-2], {'b': 1}))
        elif tok.startswith('*'): parts.append((tok[1:-1], {'i': 1}))
        else: parts.append((tok, {}))
    return parts

tpl = {'h1': P[39]._p, 'h2': P[47]._p, 'body': P[40]._p, 'ref': P[140]._p, 'lab': P[80]._p,
       'tit': P[81]._p, 'nota': P[82]._p, 'draw': P[85]._p, 'li': P[28]._p}
tpl = {k: copy.deepcopy(v) for k, v in tpl.items()}

def newp(kind, parts, anchor):
    el = copy.deepcopy(tpl[kind])
    for h in el.findall(W + 'hyperlink'): el.remove(h)
    anchor.addnext(el)
    p = docx.text.paragraph.Paragraph(el, d._body)
    if kind != 'draw': set_text(p, parts)
    return p, el

def seccion(nombre):
    m = re.search(r'## ' + nombre + r'\n\n(.+?)\n\n## ', MD, re.S)
    return m.group(1).strip()

# ---------- portada y preliminares ----------
set_text(P[1], [('Documento de trabajo N.° 5', {})])
set_text(P[4], [(TITULO, {'b': 1})])
set_text(P[12], [(f'Datos y código: {REPO}. Todas las cifras provienen de datos oficiales de la Cámara de Diputadas y Diputados, el Senado, el SERVEL y la Biblioteca del Congreso Nacional, y de fuentes de prensa identificadas en el Anexo A y en la Tabla 5, y son reproducibles con el código publicado.', {'i': 1})])

def bloque(nombre, clave):
    txt = seccion(nombre)
    paras, kw = txt.split('\n\n**' + clave + ':** ')
    return [x.strip() for x in paras.split('\n\n')], kw.strip()
res, pc = bloque('Resumen', 'Palabras clave')
abs_, kw = bloque('Abstract', 'Keywords')
assert len(res) == 3 and len(abs_) == 3
for k in range(3): set_text(P[18 + k], inline(res[k]))
set_text(P[21], [('Palabras clave:', {'i': 1}), (' ' + pc, {})])
for k in range(3): set_text(P[23 + k], inline(abs_[k]))
set_text(P[26], [('Keywords:', {'i': 1}), (' ' + kw, {})])
hall = [l[2:].strip() for l in seccion('Hallazgos').split('\n') if l.startswith('- ')]
assert len(hall) == 6, len(hall)
for i, h in enumerate(hall): set_text(P[28 + i], inline(h))
set_text(P[35], [('Bottinelli Baltra, L. (2026). ', {}), (TITULO, {'i': 1}), (' (Documento de trabajo N.° 5). https://doi.org/10.5281/zenodo.24026531', {})])
set_text(P[37], [(f'Los datos procesados y el código en R (scripts 00 a 13) están disponibles en {REPO} bajo licencia MIT (código) y CC BY 4.0 (datos, tablas, figuras y texto). Las votaciones nominales de la Cámara y las fichas de los proyectos se descargan de su servicio de datos abiertos con los scripts 01 y 01b, las del Senado con el script 01c y los resultados presidenciales, del sitio del SERVEL. El registro de militancias, las bases de salidas verificadas de ambas cámaras con sus fuentes y los detonantes codificados se incluyen en la carpeta data/fuentes del repositorio. El código y los datos de esta versión están archivados en Zenodo: https://doi.org/10.5281/zenodo.24026991', {})])
set_text(P[38], [(TITULO, {'b': 1})])

el = P[39]._p
while el is not None and el.tag != W + 'sectPr':
    nxt = el.getnext(); body.remove(el); el = nxt
anchor = P[38]._p

# ---------- tablas ----------
def border(tcPr, sides):
    b = OxmlElement('w:tcBorders')
    for s in ('top', 'left', 'bottom', 'right'):
        e = OxmlElement(f'w:{s}')
        if s in sides: e.set(qn('w:val'), 'single'); e.set(qn('w:sz'), '6'); e.set(qn('w:color'), '000000')
        else: e.set(qn('w:val'), 'nil')
        b.append(e)
    tcPr.append(b)

def make_table(rows, anchor, size=10, widths=None, left_cols=1):
    ncol = len(rows[0]); total = 9360
    if widths is None:
        lens = [max(min(len(r[j]), 50) for r in rows) + 4 for j in range(ncol)]
        widths = [int(total * l / sum(lens)) for l in lens]
    tbl = OxmlElement('w:tbl'); tp = OxmlElement('w:tblPr')
    tw = OxmlElement('w:tblW'); tw.set(qn('w:w'), str(total)); tw.set(qn('w:type'), 'dxa'); tp.append(tw)
    lay = OxmlElement('w:tblLayout'); lay.set(qn('w:type'), 'fixed'); tp.append(lay)
    cm = OxmlElement('w:tblCellMar')
    for s, v in (('left', 70), ('right', 70)):
        e = OxmlElement(f'w:{s}'); e.set(qn('w:w'), str(v)); e.set(qn('w:type'), 'dxa'); cm.append(e)
    tp.append(cm); tbl.append(tp)
    g = OxmlElement('w:tblGrid')
    for w in widths:
        c = OxmlElement('w:gridCol'); c.set(qn('w:w'), str(w)); g.append(c)
    tbl.append(g)
    for i, row in enumerate(rows):
        tr = OxmlElement('w:tr'); trPr = OxmlElement('w:trPr')
        if i == 0: trPr.append(OxmlElement('w:tblHeader'))
        trPr.append(OxmlElement('w:cantSplit')); tr.append(trPr)
        panel = i > 0 and all(c == '' for c in row[1:]) and row[0].startswith('*')
        celdas = [row[0]] if panel else row
        for j, txt in enumerate(celdas):
            tc = OxmlElement('w:tc'); tcPr = OxmlElement('w:tcPr')
            w_ = OxmlElement('w:tcW'); w_.set(qn('w:w'), str(total if panel else widths[j])); w_.set(qn('w:type'), 'dxa'); tcPr.append(w_)
            if panel:
                gs = OxmlElement('w:gridSpan'); gs.set(qn('w:val'), str(ncol)); tcPr.append(gs)
            sides = set()
            if i == 0: sides |= {'top', 'bottom'}
            if i == len(rows) - 1: sides.add('bottom')
            border(tcPr, sides)
            va = OxmlElement('w:vAlign'); va.set(qn('w:val'), 'center'); tcPr.append(va)
            tc.append(tcPr)
            p = OxmlElement('w:p'); pPr = OxmlElement('w:pPr')
            if len(rows) <= 20 or i == 0: pPr.append(OxmlElement('w:keepNext'))
            sp = OxmlElement('w:spacing'); sp.set(qn('w:before'), '30'); sp.set(qn('w:after'), '30'); sp.set(qn('w:line'), '240'); sp.set(qn('w:lineRule'), 'auto'); pPr.append(sp)
            ind = OxmlElement('w:ind'); ind.set(qn('w:firstLine'), '0'); pPr.append(ind)
            jc = OxmlElement('w:jc'); jc.set(qn('w:val'), 'left' if j < left_cols or panel else 'center'); pPr.append(jc)
            p.append(pPr)
            para = docx.text.paragraph.Paragraph(p, d._body)
            tc.append(p)
            for txt_, f in inline(txt):
                r = para.add_run(txt_); r.font.size = Pt(size)
                if f.get('i'): r.italic = True
                if f.get('b'): r.bold = True
            tr.append(tc)
        tbl.append(tr)
    anchor.addnext(tbl)
    return tbl

def parse_table(lines):
    rows = []
    for l in lines:
        cells = [c.strip() for c in l.strip().strip('|').split('|')]
        if all(re.fullmatch(r'-+', c) for c in cells if c): 
            if all(c for c in cells): continue
        rows.append(cells)
    return rows

def add_figure(n, f, tit, nota, anchor):
    p, a = newp('lab', [(f'Figura {n}', {'b': 1})], anchor); p.paragraph_format.keep_with_next = True
    p, a = newp('tit', [(tit, {'i': 1})], a); p.paragraph_format.keep_with_next = True
    p, a = newp('draw', [], a)
    for r in list(p.runs): r._r.getparent().remove(r._r)
    p.add_run().add_picture(FIGDIR + f, width=Inches(6.3))
    p.paragraph_format.keep_with_next = True
    p, a = newp('nota', [('Nota.', {'i': 1}), (' ', {})] + inline(nota), a)
    return a

def heading_kind(txt):
    m = re.match(r'^(\d+(?:\.\d+)*)\.\s+(.*)$', txt)
    if not m: return 'h1', txt
    num, rest = m.groups()
    return ('h1' if num.count('.') == 0 else 'h2'), f'{num}. {rest}'

def widths_for(rows):
    h = rows[0]; n = len(h)
    if h[0] == 'Partido': return [1360] + [2000] * 4, 1
    if n == 6 and h[1] == '(1)': return [2860] + [1300] * 5, 1
    if h[0] == 'Grupo': return [2300] + [1000] + [1010] * 6, 1
    if h[0] == 'Diputado': return [2350, 900, 1250, 1300, 1900, 1660], 1
    if h[0].startswith('Diputado ('): return [2700, 1150, 1350, 2260, 950, 950], 1
    if h[0] == 'Sigla': return [1600, 7760], 2
    return None, 1

def emit(text, anchor):
    lines = text.split('\n'); i = 0; in_refs = False; pending = None; anexo = False
    while i < len(lines):
        l = lines[i]
        if not l.strip(): i += 1; continue
        if l.startswith('#'):
            txt = l.lstrip('#').strip()
            if txt == 'Referencias' or txt.startswith('Anexo'):
                p, anchor = newp('h1', [(txt, {})], anchor); p.paragraph_format.page_break_before = True
                in_refs = txt == 'Referencias'; anexo = txt.startswith('Anexo')
                i += 1; continue
            kind, t2 = heading_kind(txt)
            p, anchor = newp(kind, [(t2, {})], anchor); i += 1; continue
        if l.startswith('[[FIG:'):
            f, n, tit, nota = l[6:-2].split('|', 3)
            anchor = add_figure(n, f, tit, nota, anchor); i += 1; continue
        m = re.match(r'^\*\*Tabla ([A-Z]?\d+)\.\*\* (.+)$', l)
        if m:
            pending = (f'Tabla {m.group(1)}', m.group(2).strip()); i += 1; continue
        if l.startswith('|'):
            j = i
            while j < len(lines) and lines[j].startswith('|'): j += 1
            rows = parse_table(lines[i:j])
            if pending:
                p, anchor = newp('lab', [(pending[0], {'b': 1})], anchor); p.paragraph_format.keep_with_next = True
                p, anchor = newp('tit', [(pending[1], {'i': 1})], anchor); p.paragraph_format.keep_with_next = True
                pending = None
            w, lc = widths_for(rows)
            size = 8.5 if (anexo or len(rows[0]) >= 6) else 9.5
            anchor = make_table(rows, anchor, size=size, widths=w, left_cols=lc)
            i = j; continue
        if l.startswith('*Nota.*'):
            rest = l[len('*Nota.*'):].strip()
            p, anchor = newp('nota', [('Nota.', {'i': 1}), (' ', {})] + inline(rest), anchor)
            i += 1; continue
        if l.startswith('- '):
            p, anchor = newp('li', inline(l[2:].strip()), anchor); i += 1; continue
        p, anchor = newp('ref' if in_refs else 'body', inline(l.strip()), anchor)
        i += 1
    return anchor

cuerpo = MD[MD.index('## 1. Introducción'):]
anchor = emit(cuerpo, anchor)
d.core_properties.title = TITULO
d.core_properties.author = 'Lucas Bottinelli Baltra'
d.save(OUT)
print('ok', OUT)
