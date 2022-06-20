//   Copyright (C) 2022 Ashwin Godbole
//
//   This file is part of coloff.
//
//   coloff is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   coloff is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with coloff. If not, see <https://www.gnu.org/licenses/>.

package tools

import (
	"bytes"
)

type Node interface {
	TokenLiteral() string
	String() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}

type Identifier struct {
	Token Token
}

type IntegerLiteral struct {
	Token Token
	Value int64
}

type FloatingLiteral struct {
	Token Token
	Value float64
}

type BooleanLiteral struct {
	Token Token
	Value bool
}

type StringLiteral struct {
	Token Token
}

type CharacterLiteral struct {
	Token Token
    Value byte
}

type VarStatement struct {
	Token Token
	Name  *Identifier
	Value Expression
}

type ReturnStatement struct {
	Token       Token
	ReturnValue Expression
}

type ExpressionStatement struct {
	Token      Token
	Expression Expression
}

type PrefixExpression struct {
	Token           Token
	RightExpression Expression
}

type InfixExpression struct {
	Token           Token
	LeftExpression  Expression
	RightExpression Expression
}

type IfExpression struct {
	Token     Token
	Condition Expression
	IfBody    *Block
	ElseBody  *Block
}

type Block struct {
	Token      Token
	Statements []Statement
}

type FunctionExpression struct {
	Token    Token
	Params   []*Identifier
	FuncBody *Block
}

type FunctionCallExpression struct {
	Token     Token
	Function  Expression
	Arguments []Expression
}

type LoopExpression struct {
	Token     Token
	Condition Expression
	LoopBody  *Block
}

type Array struct {
	Token    Token
	Elements []Expression
}

type ArrayIndexExpression struct {
	Token Token
	LeftExpression Expression
	Index Expression
}

// ----------------------------------------------------------------------------

func (d *Identifier) expressionNode() {}
func (d *ArrayIndexExpression) expressionNode() {}
func (d *Array) expressionNode() {}
func (d *FunctionCallExpression) expressionNode() {}
func (d *FunctionExpression) expressionNode() {}
func (d *IfExpression) expressionNode() {}
func (d *InfixExpression) expressionNode() {}
func (d *PrefixExpression) expressionNode() {}
func (d *IntegerLiteral) expressionNode() {}
func (d *FloatingLiteral) expressionNode() {}
func (d *BooleanLiteral) expressionNode() {}
func (d *StringLiteral) expressionNode() {}
func (d *CharacterLiteral) expressionNode() {}
func (d *LoopExpression) expressionNode() {}

func (d *Block) statementNode() {}
func (d *VarStatement) statementNode() {}
func (d *ReturnStatement) statementNode() {}
func (d *ExpressionStatement) statementNode() {}

// ----------------------------------------------------------------------------

func (e *ExpressionStatement) TokenLiteral() string {
	return e.Token.Literal
}

func (pe *PrefixExpression) TokenLiteral() string {
	return pe.Token.Literal
}

func (ie *InfixExpression) TokenLiteral() string {
	return ie.Token.Literal
}

func (ife *IfExpression) TokenLiteral() string {
	return ife.Token.Literal
}

func (b *Block) TokenLiteral() string {
	return b.Token.Literal
}

func (f *FunctionExpression) TokenLiteral() string {
	return f.Token.Literal
}

func (fc *FunctionCallExpression) TokenLiteral() string {
	return fc.Token.Literal
}

func (l *LoopExpression) TokenLiteral() string {
	return l.Token.Literal
}

func (a *Array) TokenLiteral() string {
	return a.Token.Literal
}

func (ain *ArrayIndexExpression) TokenLiteral() string {
	return ain.Token.Literal
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

func (i *Identifier) TokenLiteral() string {
	return i.Token.Literal
}

func (b *BooleanLiteral) TokenLiteral() string {
	return b.Token.Literal
}

func (f *FloatingLiteral) TokenLiteral() string {
	return f.Token.Literal
}

func (i *IntegerLiteral) TokenLiteral() string {
	return i.Token.Literal
}

func (b *StringLiteral) TokenLiteral() string {
	return b.Token.Literal
}

func (b *CharacterLiteral) TokenLiteral() string {
	return b.Token.Literal
}

func (v *VarStatement) TokenLiteral() string {
	return v.Token.Literal
}

func (r *ReturnStatement) TokenLiteral() string {
	return r.Token.Literal
}

// ----------------------------------------------------------------------------

func (p *Program) String() string {
	var str bytes.Buffer
    str.WriteString("Program: {\n")
	for _, s := range p.Statements {
		str.WriteString("\t" + s.String() + "\n")
	}
    str.WriteString("}")
	return str.String()
}

func (i *Identifier) String() string {
    return "{Ident: " + i.Token.Literal + "}"
}

func (i *IntegerLiteral) String() string {
    return "{Int: " + i.Token.Literal + "}"
}

func (b *BooleanLiteral) String() string {
    return "{Bool: " + b.Token.Literal + "}"
}

func (f *FloatingLiteral) String() string {
    return "{Float: " + f.Token.Literal + "}"
}
func (b *StringLiteral) String() string {
    return "{String: " + b.Token.Literal + "}"
}

func (b *CharacterLiteral) String() string {
    return "{Char: " + b.Token.Literal + "}"
}

func (v *VarStatement) String() string {
	var str bytes.Buffer
	str.WriteString(v.TokenLiteral() + " " + v.Name.String() + " = ")
	if v.Value != nil {
		str.WriteString(v.Value.String())
	}
	return str.String()
}

func (r *ReturnStatement) String() string {
	var str bytes.Buffer
	str.WriteString(r.TokenLiteral() + " ")
	if r.ReturnValue != nil {
		str.WriteString(r.ReturnValue.String())
	}
	return str.String()
}

func (e *ExpressionStatement) String() string {
	if e.Expression != nil {
		return e.Expression.String()
	}
	return ""
}

func (pe *PrefixExpression) String() string {
	var str bytes.Buffer
	str.WriteString("(" + pe.Token.Literal + " " + pe.RightExpression.String() + ")")
	return str.String()
}

func (ie *InfixExpression) String() string {
	var str bytes.Buffer
	str.WriteString("(" + ie.LeftExpression.String() + " " + ie.Token.Literal + " " + ie.RightExpression.String() + ")")
	return str.String()
}

func (ife *IfExpression) String() string {
	var str bytes.Buffer
	str.WriteString("If ( " + ife.Condition.String() + " )")
	str.WriteString(ife.IfBody.String())
	str.WriteString("\n}")
	if ife.ElseBody != nil {
		str.WriteString("Else {\n")
		str.WriteString(ife.ElseBody.String())
	}
	return str.String()
}

func (b *Block) String() string {
	var str bytes.Buffer
    str.WriteString("{\n")
	for _, v := range b.Statements {
		str.WriteString("\t" + v.String() + "\n")
	}
    str.WriteString("}")
	return str.String()
}

func (f *FunctionExpression) String() string {
	var str bytes.Buffer
	str.WriteString("func " + f.Token.Literal + " ( ")
	for _, v := range f.Params {
		str.WriteString(v.String())
	}
	str.WriteString(" )\n")
	str.WriteString(f.FuncBody.String())
	return str.String()
}

func (fc *FunctionCallExpression) String() string {
	var str bytes.Buffer
	str.WriteString(fc.Function.String())
	str.WriteString(" Args ( ")
	for _, v := range fc.Arguments {
		str.WriteString(v.String() + ", ")
	}
    str.WriteString(" )\n")
	return str.String()
}

func (l *LoopExpression) String() string {
	var str bytes.Buffer
	str.WriteString("loop ( " + l.Condition.String() + " )")
	str.WriteString(l.LoopBody.String())
	return str.String()
}

func (a *Array) String() string {
	var str bytes.Buffer
	str.WriteString("Array : [ ")
	for _, v := range a.Elements {
		str.WriteString(v.String() + ", ")
	}
	str.WriteString(" ]")
	return str.String()
}

func (ain *ArrayIndexExpression) String() string {
	var str bytes.Buffer
    str.WriteString("(")
	str.WriteString(ain.LeftExpression.String())
	str.WriteString(ain.Index.String())
	return str.String()
}
