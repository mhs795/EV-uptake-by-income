"""Download generalised LGA (NSW, QLD) and postcode (VIC) boundaries from the
ABS ArcGIS REST service for the map dashboard. Writes processed/*.geojson.

Run:  python3 fetch_boundaries.py
"""
import json
import urllib.parse
import urllib.request
from pathlib import Path

import yaml

HERE = Path(__file__).resolve().parent
CFG = yaml.safe_load(open(HERE / "config.yaml"))
OUT = HERE / CFG["paths"]["processed"]
OUT.mkdir(exist_ok=True)
M = CFG["map"]


def query(url, where, fields, offset_deg):
    feats, start = [], 0
    while True:
        params = {
            "where": where, "outFields": fields, "returnGeometry": "true",
            "outSR": "4326", "f": "geojson", "maxAllowableOffset": offset_deg,
            "geometryPrecision": M["coord_decimals"], "resultOffset": start,
            "orderByFields": "objectid",
        }
        with urllib.request.urlopen(url + "?" + urllib.parse.urlencode(params), timeout=300) as r:
            page = json.load(r)
        got = page.get("features", [])
        feats += got
        if not got or not page.get("exceededTransferLimit") and not page.get("properties", {}).get("exceededTransferLimit"):
            break
        start += len(got)
    return {"type": "FeatureCollection", "features": [f for f in feats if f.get("geometry")]}


def point_in_ring(x, y, ring):
    inside = False
    j = len(ring) - 1
    for i in range(len(ring)):
        xi, yi = ring[i][:2]
        xj, yj = ring[j][:2]
        if (yi > y) != (yj > y) and x < (xj - xi) * (y - yi) / (yj - yi) + xi:
            inside = not inside
        j = i
    return inside


def point_in_feature(x, y, geom):
    polys = geom["coordinates"] if geom["type"] == "MultiPolygon" else [geom["coordinates"]]
    for poly in polys:
        if point_in_ring(x, y, poly[0]) and not any(point_in_ring(x, y, h) for h in poly[1:]):
            return True
    return False


def postcode_suburbs(poa):
    """Name each VIC postcode by the ABS suburbs/localities (SAL 2021) whose
    representative point falls inside it, smallest (most specific) locality first."""
    pts = query(M["sal_point_service"], "state_code_2021 = '2'", "sal_name_2021,area_albers_sqkm", 0)
    boxes = []
    for f in poa["features"]:
        g = f["geometry"]
        polys = g["coordinates"] if g["type"] == "MultiPolygon" else [g["coordinates"]]
        xs = [c[0] for p in polys for c in p[0]]
        ys = [c[1] for p in polys for c in p[0]]
        boxes.append((min(xs), max(xs), min(ys), max(ys), f))
    names = {}
    for pt in pts["features"]:
        g = pt["geometry"]
        coords = g["coordinates"] if g["type"] == "MultiPoint" else [g["coordinates"]]
        x, y = coords[0][:2]
        nm = pt["properties"]["sal_name_2021"].replace(" (Vic.)", "")
        area = pt["properties"].get("area_albers_sqkm") or 0
        for x0, x1, y0, y1, f in boxes:
            if x0 <= x <= x1 and y0 <= y <= y1 and point_in_feature(x, y, f["geometry"]):
                names.setdefault(f["properties"]["poa_code_2021"], []).append((area, nm))
                break
    rows = ["postcode,suburbs"]
    for pc, lst in sorted(names.items()):
        # smallest-area localities first: in cities they are the named suburbs people know
        lst = [n for _, n in sorted(lst)]
        label = ", ".join(lst[:M["suburb_names_per_postcode"]]) + (f" +{len(lst) - M['suburb_names_per_postcode']}"
                                                                   if len(lst) > M["suburb_names_per_postcode"] else "")
        rows.append(f'{pc},"{label}"')
    (OUT / "vic_postcode_suburbs.csv").write_text("\n".join(rows) + "\n")
    print("VIC postcodes named:", len(names))


def main():
    lga = query(M["lga_service"], "state_code_2021 IN ('1','3')", "lga_code_2023,lga_name_2023,state_code_2021", M["lga_offset_deg"])
    (OUT / "lga_boundaries.geojson").write_text(json.dumps(lga, separators=(",", ":")))
    print("LGA features:", len(lga["features"]))
    v = CFG["vic"]
    poa = query(M["poa_service"], f"poa_code_2021 >= '{v['postcode_min']}' AND poa_code_2021 <= '{v['postcode_max']}'",
                "poa_code_2021", M["poa_offset_deg"])
    (OUT / "vic_postcode_boundaries.geojson").write_text(json.dumps(poa, separators=(",", ":")))
    print("VIC postcode features:", len(poa["features"]))
    postcode_suburbs(poa)
    ste = query(M["ste_service"], "state_code_2021 IN ('1','2','3','4','5','6','7','8')", "state_code_2021,state_name_2021",
                M["ste_offset_deg"])
    for f in ste["features"]:
        f["properties"] = {"name": f["properties"]["state_name_2021"]}
    (OUT / "australia_states.geojson").write_text(json.dumps(ste, separators=(",", ":")))
    print("State features:", len(ste["features"]))


if __name__ == "__main__":
    main()
