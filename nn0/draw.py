from graphviz import Digraph

def trace(root):
    nodes, edges = set(), set()
    def dfs(node):
        if node in nodes:
            return
        nodes.add(node)
        for child in node._prev:
            edges.add((child, node))
            dfs(child)
    dfs(root)
    return nodes, edges

def draw_cfg(root):
    nodes, edges = trace(root)

    print("1234 nodes.size", len(nodes))
    cfg = Digraph("ControlFlow", format="jpg")
    cfg.attr(rankdir="TB", size="8")

    for node in nodes:
        cfg.node(name=str(id(node)), label="{ data %.4f | grad %.4f }" % (node.data, node.grad), shape='record')
        if node._op != '':
            cfg.node(name=str(id(node)) + node._op, label=node._op)
            cfg.edge(str(id(node)) + node._op, str(id(node)))

    #for n1, n2 in edges:
    #    cfg.edge(str(id(n1)), str(id(n2)) + n2._op)

    for n1, n2 in edges:
        if n2._op != "":
            op_name = str(id(n2)) + n2._op
            cfg.edge(str(id(n1)), op_name)
        else:
            cfg.edge(str(id(n1)), str(id(n2)))

    cfg.render("cfg_example", view=True)

    return cfg
