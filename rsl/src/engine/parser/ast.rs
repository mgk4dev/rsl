use std::fmt::Debug;
use std::sync::Arc;

#[derive(Debug, Default)]
pub struct ModuleDefinition<'a> {
    pub name: Identifier<'a>,
    pub statements: Vec<ModuleStatement<'a>>,
    pub return_expression: Option<Expression<'a>>,
}

#[derive(Debug, Default)]
pub enum ModuleStatement<'a> {
    #[default]
    Unknown,
    Import(ImportStatement<'a>),
    Function(Function<'a>),
    Structure(Structure<'a>),
    Impl(Implementation<'a>),
    Enum(Enumeration<'a>),
    Trait(Trait<'a>),
    VariableDeclaration(VariableDeclarationStatement<'a>),
    Assignment(AssignmentStatement<'a>),
    Expression(Expression<'a>),
    If(IfStatement<'a>),
    For(ForStatement<'a>),
    Module(ModuleDefinition<'a>),
}

#[derive(Debug, Default)]
pub struct ImportStatement<'a> {
    pub module_name: Identifier<'a>,
    pub alias: Option<&'a str>,
}

#[derive(Debug, Default)]
pub struct Function<'a> {
    pub name: Identifier<'a>,
    pub signature: FunctionSignature<'a>,
    pub scope: Scope<'a>,
}

#[derive(Debug, Default)]
pub struct Structure<'a> {
    pub name: Identifier<'a>,
    pub fields: Vec<IndentifierAndType<'a>>,
}

#[derive(Debug, Default)]
pub struct Implementation<'a> {
    pub name: Identifier<'a>,
    pub fields: Vec<IndentifierAndType<'a>>,
}

#[derive(Debug, Default)]
pub struct Enumeration<'a> {
    pub name: Identifier<'a>,
    pub fields: Vec<TypeExpression<'a>>,
}

#[derive(Debug, Default)]
pub struct Trait<'a> {
    pub name: Identifier<'a>,
    pub functions: Vec<TraitFunction<'a>>,
}

#[derive(Debug)]
pub enum TraitFunction<'a> {
    Static(FunctionSignature<'a>),
    Member(FunctionSignature<'a>),
}

#[derive(Debug, Default)]
pub struct GenericTypeInstance<'a> {
    pub name: Identifier<'a>,
    pub args: Vec<TypeExpression<'a>>,
}

#[derive(Debug, Default)]
pub enum TypeExpression<'a> {
    #[default]
    Unknown,
    Unit,
    Concrete(Identifier<'a>),
    Generic {
        name: Identifier<'a>,
        args: Vec<TypeExpression<'a>>,
    },
    Path(Vec<TypeExpression<'a>>),
    Tuple(Vec<TypeExpression<'a>>),
}

#[derive(Debug)]
pub enum TypeConstructArg<'a> {
    Push(Expression<'a>),
    Set(Identifier<'a>, Expression<'a>),
}

#[derive(Debug, Default)]
pub struct TypeConstruction<'a> {
    pub type_: TypeExpression<'a>,
    pub args: Vec<TypeConstructArg<'a>>,
}

#[derive(Debug, Default)]
pub struct IndentifierAndType<'a> {
    pub name: Identifier<'a>,
    pub type_: TypeExpression<'a>,
}
#[derive(Debug, Default)]
pub struct FunctionSignature<'a> {
    pub arguments: Vec<IndentifierAndType<'a>>,
    pub return_type: Option<TypeExpression<'a>>,
}

#[derive(Debug, Default)]
pub struct FunctionCall<'a> {
    pub name: Identifier<'a>,
    pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, Default)]
pub struct VariableDeclarationStatement<'a> {
    pub name: Identifier<'a>,
    pub type_: TypeExpression<'a>,
    pub value: Expression<'a>,
}

#[derive(Debug, Default)]
pub struct Scope<'a> {
    pub statements: Vec<FunctionStatement<'a>>,
    pub return_expression: Option<Expression<'a>>,
}

#[derive(Debug, Default)]
pub struct ConditionalScope<'a> {
    pub condition: Expression<'a>,
    pub scope: Scope<'a>,
}

#[derive(Debug, Default)]
pub struct IfStatement<'a> {
    pub main_condition: ConditionalScope<'a>,
    pub elif_conditions: Vec<ConditionalScope<'a>>,
    pub else_scope: Option<Scope<'a>>,
}

#[derive(Debug, Default)]
pub struct ForStatement<'a> {
    pub var: Identifier<'a>,
    pub expression: Expression<'a>,
    pub scope: Scope<'a>,
}

#[derive(Debug, Default)]
pub struct BreakStatement<'a> {
    pub expression: Option<Expression<'a>>,
}

#[derive(Debug, Default)]
pub struct ReturnStatement<'a> {
    pub expression: Option<Expression<'a>>,
}

#[derive(Debug, Default)]
pub struct AssignmentStatement<'a> {
    pub left: Expression<'a>,
    pub right: Expression<'a>,
}

#[derive(Debug, Default)]
pub enum FunctionStatement<'a> {
    #[default]
    Unknown,
    Var(VariableDeclarationStatement<'a>),
    If(IfStatement<'a>),
    For(ForStatement<'a>),
    Expression(Expression<'a>),
    Assignment(AssignmentStatement<'a>),
    Break(BreakStatement<'a>),
    Return(ReturnStatement<'a>),
}

#[derive(Default)]
pub struct Identifier<'a>(pub &'a str);

impl Debug for Identifier<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id('{}')", self.0)
    }
}

#[derive(Debug, Default)]
pub enum Expression<'a> {
    #[default]
    Unknown,
    Boolean(bool),
    Int(i128),
    Real(f64),
    String(&'a str),
    Identifier(Identifier<'a>),
    FunctionCall(FunctionCall<'a>),
    GenericTypeInstance(GenericTypeInstance<'a>),
    TypeConstruction(TypeConstruction<'a>),
    ExpressionPath(ExpressionPath<'a>),
    Logical(LogicalExpression<'a>),
    Equality(EqualityExpression<'a>),
    Relational(RelationalExpression<'a>),
    Additive(AdditiveExpression<'a>),
    Multiplicative(MultiplicativeExpression<'a>),
    Unary(UnaryExpression<'a>),
    Cast(CastExpression<'a>),
    TypeTest(TypeTestExpression<'a>),
    Range(RangeExpression<'a>),
    Tuple(Vec<Expression<'a>>),
}

#[derive(Debug)]
pub enum ExpressionPathItem<'a> {
    Property(Identifier<'a>),
    FunctionCall(FunctionCall<'a>),
    GenericTypeInstance(GenericTypeInstance<'a>),
}

#[derive(Debug)]
pub enum RangeExpression<'a> {
    Closed(Arc<Expression<'a>>, Arc<Expression<'a>>),
    Open(Arc<Expression<'a>>, Arc<Expression<'a>>),
}

#[derive(Debug, Default)]
pub struct ExpressionPath<'a> {
    pub items: Vec<ExpressionPathItem<'a>>,
}

#[derive(Debug, Default)]
pub enum UnaryOperator {
    Not,
    #[default]
    Plus,
    Minus,
}

#[derive(Debug, Default)]
pub enum MultiplicativeOperator {
    #[default]
    Multiply,
    Divide,
}

#[derive(Debug, Default)]
pub enum AdditiveOperator {
    #[default]
    Plus,
    Minus,
}

#[derive(Debug, Default)]
pub enum LogicalOperator {
    #[default]
    And,
    Or,
    Is,
}

#[derive(Debug, Default)]
pub enum EqualityOperator {
    #[default]
    Equal,
    NotEqual,
}

#[derive(Debug, Default)]
pub enum RelationalOperator {
    #[default]
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Default)]
pub struct UnaryExpression<'a> {
    pub operator: Option<UnaryOperator>,
    pub expression: Arc<Expression<'a>>,
}
#[derive(Debug, Default)]
pub struct BinaryExpression<'a, Operator> {
    pub left: Arc<Expression<'a>>,
    pub right: Vec<(Operator, Expression<'a>)>,
}

impl<'a, Operator> BinaryExpression<'a, Operator> {
    pub fn new(left: Expression<'a>, right: Vec<(Operator, Expression<'a>)>) -> Self {
        Self {
            left: Arc::new(left),
            right,
        }
    }
}

pub type MultiplicativeExpression<'a> = BinaryExpression<'a, MultiplicativeOperator>;
pub type AdditiveExpression<'a> = BinaryExpression<'a, AdditiveOperator>;
pub type RelationalExpression<'a> = BinaryExpression<'a, RelationalOperator>;
pub type EqualityExpression<'a> = BinaryExpression<'a, EqualityOperator>;
pub type LogicalExpression<'a> = BinaryExpression<'a, LogicalOperator>;

#[derive(Debug, Default)]
pub struct CastExpression<'a> {
    pub expression: Arc<Expression<'a>>,
    pub type_: TypeExpression<'a>,
}

#[derive(Debug, Default)]
pub struct TypeTestExpression<'a> {
    pub expression: Arc<Expression<'a>>,
    pub type_: TypeExpression<'a>,
}
