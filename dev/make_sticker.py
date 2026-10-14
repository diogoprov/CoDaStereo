"""Generate the CoDa Stereo v3 hex sticker (solid colours only).

Usage: python3 dev/make_sticker.py inst/logo/codastreo_logo.svg
Then copy the SVG to www/logo/ and render the PNG with
rsvg::rsvg_png("inst/logo/codastreo_logo.svg", "inst/logo/codastreo_logo.png", width = 1360)
"""
import sys

INK    = "#141414"   # vinyl black
GROOVE = "#2E2E2E"
CREAM  = "#F3EAD3"
ORANGE = "#E69F00"   # Okabe-Ito
BLUE   = "#56B4E9"
GREEN  = "#009E73"

HEX = "340,30 570,163 570,430 340,563 110,430 110,163"
# Simplex (pointy-top) and record centred on its centroid
A, B, C = (198, 352), (482, 352), (340, 106)
cx, cy = (A[0] + B[0] + C[0]) / 3, (A[1] + B[1] + C[1]) / 3

def bary(a, b, c):
    """Barycentric (a: left, b: right, c: top) -> x, y."""
    s = a + b + c
    a, b, c = a / s, b / s, c / s
    return (a * A[0] + b * B[0] + c * C[0], a * A[1] + b * B[1] + c * C[1])

clusters = {
    ORANGE: [(.70, .12, .18), (.64, .10, .26), (.74, .14, .12), (.62, .16, .22), (.68, .06, .26)],
    BLUE:   [(.22, .30, .48), (.18, .36, .46), (.28, .28, .44), (.24, .40, .36), (.16, .30, .54)],
    GREEN:  [(.10, .78, .12), (.14, .72, .14), (.08, .82, .10), (.16, .76, .08), (.06, .74, .20)],
}

out = []
w = out.append
w('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 680 620" width="680" height="620" role="img">')
w('<title>CoDa Stereo logo</title>')
w('<desc>Hexagonal sticker: a ternary simplex with three groups of compositions '
  'over vinyl-record grooves; the name set as an album title with stereo level bars.</desc>')
w(f'<defs><clipPath id="hexClip"><polygon points="{HEX}"/></clipPath></defs>')
w(f'<polygon points="{HEX}" fill="{INK}"/>')
w('<g clip-path="url(#hexClip)">')
# vinyl grooves
for r in range(26, 250, 11):
    w(f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="{r}" fill="none" stroke="{GROOVE}" stroke-width="2.2"/>')
w('</g>')
# simplex: solid cream triangle outline + inner grid
w(f'<polygon points="{A[0]},{A[1]} {B[0]},{B[1]} {C[0]},{C[1]}" fill="none" '
  f'stroke="{CREAM}" stroke-width="6" stroke-linejoin="round"/>')
for f in (1/3, 2/3):
    p1, p2 = bary(f, 1 - f, 0), bary(f, 0, 1 - f)
    q1, q2 = bary(1 - f, f, 0), bary(0, f, 1 - f)
    r1, r2 = bary(1 - f, 0, f), bary(0, 1 - f, f)
    for (x1, y1), (x2, y2) in ((p1, p2), (q1, q2), (r1, r2)):
        w(f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" '
          f'stroke="{GROOVE}" stroke-width="2"/>')
# data points
for col, pts in clusters.items():
    for p in pts:
        x, y = bary(*p)
        w(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="9" fill="{col}" stroke="{INK}" stroke-width="2.5"/>')

# album-title name
FONT_HEAVY = "'Arial Black', 'Helvetica Neue', Helvetica, Arial, sans-serif"
w(f'<text x="341" y="440" text-anchor="middle" font-family="{FONT_HEAVY}" '
  f'font-weight="900" font-size="74" letter-spacing="2" fill="{CREAM}">CoDa</text>')
w(f'<text x="344.5" y="484" text-anchor="middle" font-family="{FONT_HEAVY}" '
  f'font-weight="900" font-size="25" letter-spacing="9" fill="{ORANGE}">STEREO</text>')
# stereo level bars (L and R channels), mirrored
heights = (10, 18, 26)
for side in (-1, 1):
    for k, h in enumerate(heights):
        x = 340 + side * (98 + k * 12)
        w(f'<rect x="{x - 3.5:.1f}" y="{476 - h}" width="7" height="{h}" fill="{(BLUE, GREEN, ORANGE)[k]}"/>')

# solid border on top
w(f'<polygon points="{HEX}" fill="none" stroke="{ORANGE}" stroke-width="12" stroke-linejoin="round"/>')
w('</svg>')
open(sys.argv[1], "w").write("\n".join(out) + "\n")
