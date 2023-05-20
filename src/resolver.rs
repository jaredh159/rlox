use crate::err::*;
use crate::expr::*;
use crate::stmt::{Class, FnStmt, IfStmt, Stmt, WhileStmt};
use crate::tok::Token;
use crate::visit::*;
use std::borrow::BorrowMut;
use std::collections::HashMap;

pub fn resolve(stmts: &mut Vec<Stmt>) -> Result<()> {
  Resolver {
    scopes: Vec::new(),
    current_fn: FunctionType::None,
    current_class: ClassType::None,
  }
  .resolve_stmts(stmts)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FunctionType {
  None,
  Function,
  Initializer,
  Method,
}

impl FunctionType {
  fn from_method(method: &mut FnStmt) -> Self {
    if method.name.lexeme() == "init" {
      FunctionType::Initializer
    } else {
      FunctionType::Method
    }
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ClassType {
  None,
  Class,
}

struct Resolver {
  scopes: Vec<HashMap<String, bool>>,
  current_fn: FunctionType,
  current_class: ClassType,
}

pub trait Resolvable {
  fn name(&self) -> &Token;
  fn set_distance(&mut self, distance: usize);
  fn get_distance(&self) -> Option<usize>;
}

impl Resolver {
  fn resolve_stmts(&mut self, stmts: &mut Vec<Stmt>) -> Result<()> {
    for stmt in stmts {
      stmt.accept(self)?;
    }
    Ok(())
  }

  fn resolve_stmt(&mut self, stmt: &mut Stmt) -> Result<()> {
    stmt.accept(self)
  }

  fn resolve_expr(&mut self, expr: &mut Expr) -> Result<()> {
    expr.accept(self)
  }

  fn begin_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  fn begin_scope_with(&mut self, name: String, value: bool) {
    let mut scope = HashMap::new();
    scope.insert(name, value);
    self.scopes.push(scope);
  }

  fn end_scope(&mut self) {
    self.scopes.pop();
  }

  fn declare(&mut self, name: &Token) -> Result<()> {
    if let Some(scope) = self.scopes.last_mut() {
      if scope.contains_key(name.lexeme()) {
        return Err(LoxErr::Resolve {
          line: name.line(),
          message: format!(
            "already a variable with the name `{}` in this scope",
            name.lexeme()
          ),
        });
      }
      scope.insert(name.lexeme().to_string(), false);
    }
    Ok(())
  }

  fn define(&mut self, name: &Token) {
    if let Some(scope) = self.scopes.last_mut() {
      scope.insert(name.lexeme().to_string(), true);
    }
  }

  fn resolve_local(&self, resolvable: &mut impl Resolvable) {
    for (index, scope) in self.scopes.iter().enumerate().rev() {
      if scope.contains_key(resolvable.name().lexeme()) {
        resolvable.set_distance(self.scopes.len() - 1 - index);
        return;
      }
    }
  }

  fn resolve_fn(&mut self, fn_stmt: &mut FnStmt, fn_type: FunctionType) -> Result<()> {
    let enclosing_fn = self.current_fn;
    self.current_fn = fn_type;
    self.begin_scope();
    for param in &fn_stmt.params {
      self.declare(&param)?;
      self.define(&param);
    }
    self.resolve_stmts(&mut fn_stmt.body)?;
    self.end_scope();
    self.current_fn = enclosing_fn;
    Ok(())
  }
}

impl StmtVisitor for Resolver {
  type Result = Result<()>;

  fn visit_expression(&mut self, expr: &mut Expr) -> Self::Result {
    self.resolve_expr(expr)
  }

  fn visit_print(&mut self, expr: &mut Expr) -> Self::Result {
    self.resolve_expr(expr)
  }

  fn visit_var(&mut self, name: &Token, initializer: Option<&mut Expr>) -> Self::Result {
    self.declare(name)?;
    if let Some(initializer) = initializer {
      self.resolve_expr(initializer)?;
    }
    self.define(name);
    Ok(())
  }

  fn visit_block(&mut self, stmts: &mut Vec<Stmt>) -> Self::Result {
    self.begin_scope();
    self.resolve_stmts(stmts)?;
    self.end_scope();
    Ok(())
  }

  fn visit_if(&mut self, if_stmt: &mut IfStmt) -> Self::Result {
    self.resolve_expr(&mut if_stmt.condition)?;
    self.resolve_stmt(&mut if_stmt.then_branch)?;
    if let Some(else_branch) = if_stmt.else_branch.borrow_mut() {
      self.resolve_stmt(else_branch)?;
    }
    Ok(())
  }

  fn visit_while(&mut self, while_stmt: &mut WhileStmt) -> Self::Result {
    self.resolve_expr(&mut while_stmt.condition)?;
    self.resolve_stmt(&mut while_stmt.body)
  }

  fn visit_fn(&mut self, fn_stmt: &mut FnStmt) -> Self::Result {
    self.declare(&fn_stmt.name)?;
    self.define(&fn_stmt.name);
    self.resolve_fn(fn_stmt, FunctionType::Function)?;
    Ok(())
  }

  fn visit_return(&mut self, keyword: &Token, value: Option<&mut Expr>) -> Self::Result {
    if self.current_fn == FunctionType::None {
      Err(LoxErr::Resolve {
        line: keyword.line(),
        message: "can't return from top-level code".to_string(),
      })
    } else {
      value.map_or_else(
        || Ok(()),
        |value| {
          if self.current_fn == FunctionType::Initializer {
            Err(LoxErr::Resolve {
              line: keyword.line(),
              message: "can't return a value from an initializer".to_string(),
            })
          } else {
            self.resolve_expr(value)
          }
        },
      )
    }
  }

  fn visit_class(&mut self, class: &mut Class) -> Self::Result {
    let enclosing_class = self.current_class;
    self.current_class = ClassType::Class;
    self.declare(&class.name)?;
    self.define(&class.name);

    if class.superclass.is_some() {
      let superclass = class.superclass.as_mut().unwrap();
      match superclass {
        Expr::Variable(variable) => {
          if variable.name.lexeme() == class.name.lexeme() {
            return Err(LoxErr::Resolve {
              line: variable.name.line(),
              message: "a class can't inherit from itself".to_string(),
            });
          }
        }
        _ => panic!("unreachable"),
      }
      self.resolve_expr(superclass)?;
    }

    self.begin_scope_with("this".to_string(), true);
    for mut method in class.methods.iter_mut() {
      let fn_type = FunctionType::from_method(method);
      self.resolve_fn(&mut method, fn_type)?;
    }
    self.end_scope();
    self.current_class = enclosing_class;
    Ok(())
  }
}

impl ExprVisitor for Resolver {
  type Result = Result<()>;

  fn visit_assign(&mut self, assign: &mut Assign) -> Self::Result {
    self.resolve_expr(&mut assign.value)?;
    self.resolve_local(assign);
    Ok(())
  }

  fn visit_binary(&mut self, binary: &mut Binary) -> Self::Result {
    self.resolve_expr(&mut binary.left)?;
    self.resolve_expr(&mut binary.right)
  }

  fn visit_grouping(&mut self, grouping: &mut Grouping) -> Self::Result {
    self.resolve_expr(&mut grouping.expr)
  }

  fn visit_literal(&mut self, _literal: &mut Literal) -> Self::Result {
    Ok(())
  }

  fn visit_logical(&mut self, logical: &mut Logical) -> Self::Result {
    self.resolve_expr(&mut logical.left)?;
    self.resolve_expr(&mut logical.right)
  }

  fn visit_unary(&mut self, unary: &mut Unary) -> Self::Result {
    self.resolve_expr(&mut unary.right)
  }

  fn visit_variable(&mut self, variable: &mut Variable) -> Self::Result {
    if let Some(scope) = self.scopes.last() {
      if scope.get(variable.name.lexeme()) == Some(&false) {
        return Err(LoxErr::Resolve {
          line: variable.name.line(),
          message: format!(
            "can't read local variable `{}` in it's own initializer",
            variable.name.lexeme()
          ),
        });
      }
    }
    self.resolve_local(variable);
    Ok(())
  }

  fn visit_call(&mut self, call: &mut Call) -> Self::Result {
    self.resolve_expr(&mut call.callee)?;
    for arg in call.args.iter_mut() {
      self.resolve_expr(arg)?;
    }
    Ok(())
  }

  fn visit_get(&mut self, get: &mut Get) -> Self::Result {
    self.resolve_expr(&mut get.object)
  }

  fn visit_set(&mut self, set: &mut Set) -> Self::Result {
    self.resolve_expr(&mut set.value)?;
    self.resolve_expr(&mut set.object)
  }

  fn visit_this(&mut self, this: &mut This) -> Self::Result {
    if self.current_class == ClassType::None {
      return Err(LoxErr::Resolve {
        line: this.keyword.line(),
        message: "can't use `this` outside of a class".to_string(),
      });
    }
    self.resolve_local(this);
    Ok(())
  }
}
