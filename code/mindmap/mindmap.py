"""Draws the research mind map from mindmap.json.  Usage: python3 mindmap.py
Edit mindmap.json to add, move or rename papers; thumbnails live in thumbs/.
Change SEED for a different set of wobbles."""
import json, textwrap, os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.patches import FancyBboxPatch
from PIL import Image
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
D = json.load(open(os.path.join(HERE, "mindmap.json"), encoding="utf8"))
SEED = 7
rng = np.random.default_rng(SEED)

W, H = 2800, 1760                      # canvas in pixels
X1, X2 = 250, 520                      # typical x of main branches and sub-branches (from centre)
FAN = 215                              # distance from a sub-branch to its papers
ROW = 66                               # vertical space per paper
GAP_SUB, GAP_MAIN = 26, 74             # extra space between sub-branches / main branches
WRAP = 54                              # characters per label line
THUMB_H = 54
GREY = "#555555"

fig = plt.figure(figsize=(W / 100, H / 100), dpi=100)
ax = fig.add_axes([0, 0, 1, 1]); ax.set_xlim(-W / 2, W / 2); ax.set_ylim(-H / 2, H / 2); ax.axis("off")

def branch(p0, p1, color, lw0, lw1, wobble):
    """A tapering, slightly wobbly curve from p0 to p1."""
    (x0, y0), (x1, y1) = p0, p1
    a, b = rng.uniform(0.35, 0.6), rng.uniform(0.4, 0.65)
    c1 = (x0 + a * (x1 - x0), y0 + rng.uniform(-0.12, 0.12) * (y1 - y0))
    c2 = (x1 - b * (x1 - x0), y1)
    t = np.linspace(0, 1, 60)[:, None]
    P = ((1 - t) ** 3 * np.array(p0) + 3 * (1 - t) ** 2 * t * np.array(c1)
         + 3 * (1 - t) * t ** 2 * np.array(c2) + t ** 3 * np.array(p1))
    tt = t[:, 0]
    P[:, 1] += wobble * np.sin(np.pi * tt) * np.sin(2 * np.pi * rng.uniform(0.6, 1.4) * tt + rng.uniform(0, 6.3))
    segs = np.stack([P[:-1], P[1:]], axis=1)
    ax.add_collection(LineCollection(segs, colors=color, linewidths=np.linspace(lw0, lw1, len(segs)),
                                     capstyle="round", zorder=2))

def layout(side):
    y = 0
    for bi, b in enumerate(side):
        if bi: y -= GAP_MAIN
        for si, s in enumerate(b["children"]):
            if si: y -= GAP_SUB
            for leaf in s["children"]:
                leaf["y"] = y; y -= ROW
            s["y"] = float(np.mean([l["y"] for l in s["children"]]))
        b["y"] = float(np.mean([s["y"] for s in b["children"]]))
    return -y - ROW

def draw(side, sign):
    used = layout(side); shift = used / 2 - 40
    for b in side:
        col = b["color"]
        bx, by = sign * (X1 + rng.uniform(-30, 45)), b["y"] + shift + rng.uniform(-25, 25)
        branch((0, 0), (bx, by), col, 15, 9, 10)
        n = len(b["children"])
        for si, s in enumerate(b["children"]):
            # sub-branches sit on a loose arc around the main node rather than in a column
            arc = 55 * np.sin(np.pi * (si + 0.5) / n) if n > 1 else 30
            sx, sy = sign * (X2 - 45 + arc + rng.uniform(-22, 22)), s["y"] + shift + rng.uniform(-9, 9)
            branch((bx, by), (sx, sy), col, 8, 4.5, 7)
            m = len(s["children"])
            for li, leaf in enumerate(s["children"]):
                ly = leaf["y"] + shift
                dy = ly - sy
                lx = sx + sign * (np.sqrt(max(FAN ** 2 - dy ** 2, 90 ** 2)) + rng.uniform(-14, 14))
                branch((sx, sy), (lx, ly), col, 3.6, 2.0, 4)
                ax.plot(lx, ly, marker="8", ms=13, mfc="#E3E3E6", mec="#C9C9CE", mew=2, zorder=4)
                x = lx + sign * 20
                if leaf.get("thumb"):
                    im = Image.open(os.path.join(HERE, "thumbs", leaf["thumb"])).convert("RGB")
                    w = THUMB_H * im.size[0] / im.size[1]
                    x0, x1 = (x, x + w) if sign > 0 else (x - w, x)
                    ax.imshow(np.asarray(im), extent=(x0, x1, ly - THUMB_H / 2, ly + THUMB_H / 2), zorder=3, aspect="auto")
                    x += sign * (w + 12)
                ax.text(x, ly, textwrap.fill(leaf["name"], WRAP - (10 if leaf.get("thumb") else 0)),
                        ha="left" if sign > 0 else "right", va="center", fontsize=13.5, color=GREY, linespacing=1.15, zorder=5)
            ax.plot(sx, sy, "o", ms=18, color=col, zorder=4)
            up = sy >= by
            ax.text(sx - sign * 8, sy + (21 if up else -21), s["name"], ha="center", va="bottom" if up else "top",
                    fontsize=14.5, style="italic", color="#333333", rotation=sign * rng.uniform(-4, 4), zorder=6,
                    bbox=dict(fc="white", ec="none", alpha=0.75, pad=1.5))
        ax.plot(bx, by, "o", ms=25, color=col, zorder=4)
        up = by >= 0
        ax.text(bx, by + (30 if up else -30), b["name"], ha="center", va="bottom" if up else "top", fontsize=17,
                style="italic", weight="bold", color="#333333", rotation=rng.uniform(-3, 3), zorder=6,
                bbox=dict(fc="white", ec="none", alpha=0.75, pad=1.5))

ax.set_autoscale_on(False)
draw(D["left"], -1); draw(D["right"], +1)
ax.add_patch(FancyBboxPatch((-112, -36), 224, 72, boxstyle="round,pad=4,rounding_size=14", fc="#B8B8BB", ec="none", zorder=7))
ax.text(0, 0, D["root"], ha="center", va="center", fontsize=17, style="italic", zorder=8)
ax.text(0, H / 2 - 48, D["title"], ha="center", va="center", fontsize=27, weight="bold")
fig.savefig(os.path.join(HERE, "kmm.png"), dpi=100, facecolor="white")
print("wrote kmm.png", W, "x", H)
