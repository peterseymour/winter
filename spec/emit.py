from ast import named_types, poly_types
from itertools import chain


for typename, typ in chain(poly_types.items(), sorted(named_types.items(), key=lambda item: item[1].depth)):
    defn = typ.emit()

    if isinstance(defn, str):
        defn = [defn]
    else:
        defn = list(defn)

    print(f"{typename:>{max(32, len(typename))}} ::= {defn[0]}")
    for line in defn[1:]:
        print(f"{'':>{max(32, len(typename))}}     {line}")
    print()
