from typing import List
from pygraphviz import AGraph

from .. import om  # noqa


class CFGPlotter:

    def __init__(self, cfgs: List[om.Cfg]):
        self.cfgs = cfgs
        self.graph = self.generate_graph()

    def generate_graph(self) -> AGraph:
        graph = AGraph(directed=True, splines='curved', overlap='vpsc')
        for cfg in self.cfgs:
            for bb in cfg.basic_blocks:
                style = {}
                if bb.type == 'BBExit':
                    style = dict(style='filled', color='#665c54')
                if bb.type == 'BBEntry':
                    style = dict(style='filled', color='#458588')
                graph.add_node(
                    n=bb.id, label=f'bb={bb.id} stmt={bb.stmt_id}', **style)

                for succ in bb.succs:
                    graph.add_edge(bb.id, succ)
                for pred in bb.preds:
                    graph.add_edge(pred, bb.id)
        return graph

    def save_file(self, filepath: str):
        self.graph.layout()
        self.graph.draw(filepath)
