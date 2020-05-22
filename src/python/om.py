from dataclasses import dataclass
from typing import List, Dict, Set


@dataclass
class Tok_info:
    id: int
    linenr: int
    col: int

    @classmethod
    def from_dict(cls, values):
        args = {}
        args['id'] = values.get('id', -1)
        args['linenr'] = values.get('linenr', -1)
        args['col'] = values.get('col', -1)
        return Tok_info(**args)


@dataclass
class Statement:
    ty: str
    nested: List

    def detect_ty(values):
        if not isinstance(values, list):
            return 'Opaque'
        if len(values) == 0:
            return 'Opaque'
        return values[0]

    def get_nested(ty, values):
        if len(values) < 2:
            return []
        if ty == 'While':
            return Statement.from_dict(values[1])
        return []

    @classmethod
    def from_dict(cls, values):
        args = {}
        args['ty'] = cls.detect_ty(values)
        args['nested'] = cls.get_nested(args['ty'], values)

        return Statement(**args)


@dataclass
class Variable:
    name: str

    @classmethod
    def from_dict(cls, values):
        pass


@dataclass
class VarDecl:
    var: Variable
    # spec:
    # qual
    # dir

    @classmethod
    def from_dict(cls, values):
        pass


@dataclass
class Function:
    name: str
    ti: Tok_info
    is_std: bool
    # return_ty:
    variables: List[VarDecl]
    statements: List[Statement]

    @classmethod
    def from_dict(cls, values):
        args = {}

        id_ = values.get('id')
        if id_:
            args['name'] = id_.get('name', '')
            args['ti'] = Tok_info.from_dict(id_.get('ti'))
            args['is_std'] = id_.get('is_std', False)
        else:
            args['name'] = ''
            args['ti'] = None
            args['is_std'] = False

        args['variables'] = [Variable.from_dict(
            i) for i in values.get('variables', [])]
        args['statements'] = [Statement.from_dict(
            i) for i in values.get('statements', [])]
        return Function(**args)


@dataclass
class FunctionBlock:
    name: str


@dataclass
class Program:
    name: str
    is_retain: bool
    variables: List[VarDecl]
    statements: List[Statement]

    @classmethod
    def from_dict(cls, values):
        args = {}
        args['name'] = values.get('name', '')
        args['is_retain'] = values.get('is_retain', False)
        args['variables'] = [Variable.from_dict(
            i) for i in values.get('variables', [])]
        args['statements'] = [Statement.from_dict(
            i) for i in values.get('statements', [])]
        return Program(**args)


@dataclass
class Configuration:
    name: str


@dataclass
class Type:
    name: str
    type: str

    @classmethod
    def from_dict(cls, values):
        args = {}
        args['name'] = values.get('name', '')
        args['type'] = values.get('type', '')
        return Type(**args)


@dataclass
class Environment:
    name: str


@dataclass
class BasicBlock:
    """Basic block of intraprocedural control flow graph."""
    id: int
    type: str
    preds: Set[int]
    succs: Set[int]
    stmt_ids: List[int]

    @classmethod
    def from_dict(cls, values):
        args = {}
        args['id'] = values.get('id', -1)
        args['type'] = values.get('type', [])
        if len(args['type']) > 0:
            args['type'] = args['type'][0]
        args['preds'] = set(values.get('preds', []))
        args['succs'] = set(values.get('succs', []))
        args['stmt_ids'] = values.get('stmt_ids', [])
        return BasicBlock(**args)


@dataclass
class Cfg:
    """Intraprocedural control flow graph."""
    entry_bb_id: int
    basic_blocks: List[BasicBlock]
    pou_id: int

    @classmethod
    def from_dict(cls, values):
        args = {}
        args['entry_bb_id'] = values.get('entry_bb_id', -1)
        args['basic_blocks'] = [BasicBlock.from_dict(
            bb) for bb in values.get('basic_blocks')]
        args['pou_id'] = values.get('pou_id', -1)
        return Cfg(**args)


@dataclass
class Scheme:
    version: str
    functions: List[Function]
    function_blocks: List[FunctionBlock]
    programs: List[Program]
    configurations: List[Configuration]
    types: List[Type]
    environments: List[Environment]
    cfgs: List[Cfg]

    @classmethod
    def from_dict(cls, values: Dict):
        args = {}
        args['version'] = values.get('version', '0')
        args['functions'] = [Function.from_dict(
            i) for i in values.get('functions', [])]
        # args['function_blocks'] = [FunctionBlock.from_dict(
        #     i) for i in values.get('function_blocks', [])]
        args['function_blocks'] = []
        args['programs'] = [Program.from_dict(
            i) for i in values.get('programs', [])]
        # args['configurations'] = [Configuration.from_dict(
        #     i) for i in values.get('configurations', [])]
        args['configurations'] = []
        args['types'] = [Type.from_dict(i) for i in values.get('types', [])]
        # args['environments'] = [Environment.from_dict(
        #     i) for i in values.get('environments', [])]
        args['environments'] = []
        args['cfgs'] = [Cfg.from_dict(i) for i in values.get('cfgs', [])]
        return Scheme(**args)


@dataclass
class Warning:
    """Warning found by OCaml core."""
    linenr: int
    column: int
    id: str
    msg: str
    type: str

    @classmethod
    def from_dict(cls, values):
        args = {}
        args['linenr'] = values.get('linenr', -1)
        args['column'] = values.get('column', -1)
        args['id'] = values.get('id', -1)
        args['msg'] = values.get('msg', '')
        args['type'] = values.get('type', 'Inspection')
        return Warning(**args)

    def __str__(self):
        if self.linenr == 0 and self.column == 0:
            return f"[{self.id}] {self.msg}"
        return f"[{self.id}] {self.linenr}:{self.column} {self.msg}"
