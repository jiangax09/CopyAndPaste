from graphviz import Digraph

from value import Value
from draw import draw_cfg

x = Value(1.0)
y = (x * 2 + 1)
y.backward()
draw_cfg(y)
