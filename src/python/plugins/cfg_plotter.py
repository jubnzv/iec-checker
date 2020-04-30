from typing import List
from pygraphviz import AGraph

from .. import om


class CFGPlotter:

    def __init__(self, cfgs: List[om.Cfg]):
        self.cfgs = cfgs
        self.graph = self.generate_graph()

    def generate_graph(self) -> AGraph:
        graph = AGraph(directed=True)
        for cfg in self.cfgs:
            for bb in cfg.basic_blocks:
                for succ in bb.succs:
                    graph.add_edge(bb.id, succ)
                for pred in bb.preds:
                    graph.add_edge(pred, bb.id)
        return graph

    def save_file(self, filepath: str):
        self.graph.layout()
        self.graph.draw(filepath)
