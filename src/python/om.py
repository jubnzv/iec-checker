from dataclasses import dataclass
from typing import List, Dict


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
    type: str
    ti: Tok_info

    @classmethod
    def from_dict(cls, values):
        pass


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


@dataclass
class Configuration:
    name: str


@dataclass
class Type:
    name: str


@dataclass
class Environment:
    name: str


@dataclass
class Scheme:
    version: str
    functions: List[Function]
    function_blocks: List[FunctionBlock]
    programs: List[Program]
    configurations: List[Configuration]
    types: List[Type]
    environments: List[Environment]

    @classmethod
    def from_dict(cls, values: Dict):
        args = {}
        args['version'] = values.get('version', '0')
        args['functions'] = [Function.from_dict(
            i) for i in values.get('functions', [])]
        # args['function_blocks'] = [FunctionBlock.from_dict(
        #     i) for i in values.get('function_blocks', [])]
        args['function_blocks'] = []
        # args['programs'] = [Program.from_dict(
        #     i) for i in values.get('program', [])]
        args['programs'] = []
        # args['configurations'] = [Configuration.from_dict(
        #     i) for i in values.get('configurations', [])]
        args['configurations'] = []
        # args['types'] = [Type.from_dict(
        #     i) for i in values.get('types', [])]
        args['types'] = []
        # args['environments'] = [Environment.from_dict(
        #     i) for i in values.get('environments', [])]
        args['environments'] = []
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
        return f"[{self.id}] {self.linenr}:{self.column}: {self.msg}"


@dataclass
class Error:
    """Error produced when OCaml core fails."""
    msg: str
