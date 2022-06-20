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
	"fmt"
	"strconv"
)

// loc : To store the contents of the source file. For better error reporting
var loc []string

// defining a couple of function types.
type (
	prefixFunc func() Expression
	infixFunc  func(Expression) Expression
)

// Operator precedence / Binding power values
const (
	LOWEST       int = iota
	LOGICAL          // All relational operators have equal precedence (and no short-circuiting) [&, |]
	COMPARISON       // All relational operators have equal precendence [==, >=, <=, >, <]
	SIMPLEARITH      // Addition and Subtraction have equal precedence [+, -]
	COMPLEXARITH     // Multiplication, Division and Remainder have equal precedence [*, /, %]
	PREFIX           // Unary prefix operators have the equal precedence [!, -]
	FCALL            // Function calls
	INDEX            // Array indexing operation has the highest preference because array elements may be functions.
)

var precedenceTable = map[TokenType]int{
	// array indexing operator
	LSB: INDEX,

	// function call operator
	LPR: FCALL,

	// logical operators
	LND: LOGICAL,
	LOR: LOGICAL,

	// relational operators
	EQL: COMPARISON,
	NEQ: COMPARISON,
	LST: COMPARISON,
	GRT: COMPARISON,
	LSE: COMPARISON,
	GRE: COMPARISON,

	// arithmetic operators
	PLS: SIMPLEARITH,
	MIN: SIMPLEARITH,
	DIV: COMPLEXARITH,
	PRD: COMPLEXARITH,
	REM: COMPLEXARITH,
}

// Parser : Current state of the parser
type Parser struct {
	lexer           *Lexer
	currentToken    Token
	peekToken       Token
	errors          []string
	prefixFunctions map[TokenType]prefixFunc
	infixFunctions  map[TokenType]infixFunc
}

// CreateParserState : to create a new parser state and initialize it
func CreateParserState(lexer *Lexer) *Parser {
	p := &Parser{lexer: lexer, errors: []string{}}
	p.advanceToken()
	p.advanceToken()

	p.prefixFunctions = make(map[TokenType]prefixFunc)
	p.infixFunctions = make(map[TokenType]infixFunc)

	// registering all the valid PREFIX tokens
	p.registerPrefixFunc(IDN, p.parseIdentifier)
	p.registerPrefixFunc(INT, p.parseIntegerLiteral)
	p.registerPrefixFunc(CHR, p.parseCharacterLiteral)
	p.registerPrefixFunc(FLT, p.parseFloatingLiteral)
	p.registerPrefixFunc(BOL, p.parseBooleanLiteral)
	p.registerPrefixFunc(STR, p.parseStringLiteral)
	p.registerPrefixFunc(MIN, p.parsePrefixExpression)
	p.registerPrefixFunc(LNT, p.parsePrefixExpression)
	p.registerPrefixFunc(LPR, p.parseGroupedExpression)
	p.registerPrefixFunc(IFK, p.parseIfExpression)
    p.registerPrefixFunc(LPK, p.parseLoopStatement)
    p.registerPrefixFunc(FNK, p.parseFunctionExpression)
	p.registerPrefixFunc(LSB, p.parseArray)

	// registering all the valid INFIX tokens
	p.registerInfixFunc(EQL, p.parseInfixExpression)
	p.registerInfixFunc(NEQ, p.parseInfixExpression)
	p.registerInfixFunc(LSE, p.parseInfixExpression)
	p.registerInfixFunc(GRE, p.parseInfixExpression)
	p.registerInfixFunc(LST, p.parseInfixExpression)
	p.registerInfixFunc(GRT, p.parseInfixExpression)
	p.registerInfixFunc(PLS, p.parseInfixExpression)
	p.registerInfixFunc(MIN, p.parseInfixExpression)
	p.registerInfixFunc(PRD, p.parseInfixExpression)
	p.registerInfixFunc(DIV, p.parseInfixExpression)
	p.registerInfixFunc(REM, p.parseInfixExpression)
	p.registerInfixFunc(POW, p.parseInfixExpression)
	p.registerInfixFunc(LND, p.parseInfixExpression)
	p.registerInfixFunc(LOR, p.parseInfixExpression)
	p.registerInfixFunc(LPR, p.parseFunctionCall)
	p.registerInfixFunc(LSB, p.parseArrayIndexExpression)

	return p
}

func (p *Parser) advanceToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.lexer.NextToken()
    // fmt.Println(" advtok : ", p.currentToken.Literal, p.peekToken.Literal)
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) Parse() *Program {
	program := &Program{}
	program.Statements = []Statement{}

	for p.currentToken.TokType != EOF {
		statement := p.parseStatement()
		if statement != nil {
			program.Statements = append(program.Statements, statement)
		}
		p.advanceToken()
	}

	return program
}

func (p *Parser) parseStatement() Statement {
	switch p.currentToken.TokType {
	case VAR:
		return p.parseVarStatement()
	case RET:
		return p.parseReturnStatement()
	case EOL:
		return nil
    case IDN:
        return p.parseReassignmentStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseVarStatement() *VarStatement {
	statement := &VarStatement{Token: p.currentToken}

	if !p.advanceTokenIfPeekTokIs(IDN) {
		return nil
	}
	statement.Name = &Identifier{Token: p.currentToken}
	if !p.advanceTokenIfPeekTokIs(ASN) {
		return nil
	}
	p.advanceToken()

	statement.Value = p.parseExpression(LOWEST)
	// println(p.parseExpression(LOWEST))
	if p.peekTokIs(EOL) {
		p.advanceToken()
	}

	return statement
}

func (p *Parser) parseReassignmentStatement() *VarStatement {
	statement := &VarStatement{}
	statement.Name = &Identifier{Token: p.currentToken}
	if !p.advanceTokenIfPeekTokIs(ASN) {
		return nil
	}
	p.advanceToken()
	statement.Value = p.parseExpression(LOWEST)
	if p.peekTokIs(EOL) {
		p.advanceToken()
	}

	return statement
}

func (p *Parser) parseReturnStatement() *ReturnStatement {
	statement := &ReturnStatement{Token: p.currentToken}
	p.advanceToken()
	statement.ReturnValue = p.parseExpression(LOWEST)
	if p.peekTokIs(EOL) {
		p.advanceToken()
	}
	return statement
}

func (p *Parser) parseExpressionStatement() *ExpressionStatement {
	statement := &ExpressionStatement{Token: p.currentToken}
	statement.Expression = p.parseExpression(LOWEST)
	// NOTE(aelobdog): include the EOL requirement here too?
	// if p.peekTokIs(EOL) { p.advanceToken() }
	return statement
}

func (p *Parser) parseIdentifier() Expression {
	return &Identifier{Token: p.currentToken}
}

func (p *Parser) parseCharacterLiteral() Expression {
	charLit := &CharacterLiteral{Token: p.currentToken, Value: p.currentToken.Literal[0]}
	return charLit
}

func (p *Parser) parseIntegerLiteral() Expression {
	intLit := &IntegerLiteral{Token: p.currentToken}
	value, err := strconv.ParseInt(p.currentToken.Literal, 0, 64)
	if err != nil {
		p.WrongTypeError(p.currentToken.Literal, "an integer")
		return nil
	}
	intLit.Value = value
	return intLit
}

func (p *Parser) parseFloatingLiteral() Expression {
	fltLit := &FloatingLiteral{Token: p.currentToken}
	value, err := strconv.ParseFloat(p.currentToken.Literal, 64)
	if err != nil {
		p.WrongTypeError(p.currentToken.Literal, "a real number")
		return nil
	}
	fltLit.Value = value
	return fltLit
}

func (p *Parser) parseBooleanLiteral() Expression {
	boolLit := &BooleanLiteral{Token: p.currentToken}
	value, err := strconv.ParseBool(p.currentToken.Literal)
	if err != nil {
		p.WrongTypeError(p.currentToken.Literal, "a boolean")
	}
	boolLit.Value = value
	return boolLit
}

func (p *Parser) parseStringLiteral() Expression {
	return &StringLiteral{Token: p.currentToken}
}

func (p *Parser) parseExpression(precedence int) Expression {
	prefix := p.prefixFunctions[p.currentToken.TokType]
	if prefix == nil {
		p.PrefixDoesntExistError(p.currentToken.TokType)
		return nil
	}
	leftExpression := prefix()

	for !p.peekTokIs(EOL) && precedence < p.peekPrecedence() {
		infix := p.infixFunctions[p.peekToken.TokType]
		if infix == nil {
			return leftExpression
		}
		p.advanceToken()
		leftExpression = infix(leftExpression)
	}
	return leftExpression
}

func (p *Parser) parsePrefixExpression() Expression {
	expression := &PrefixExpression{Token: p.currentToken}
	p.advanceToken()
	expression.RightExpression = p.parseExpression(PREFIX)

	return expression
}

func (p *Parser) parseInfixExpression(leftExpression Expression) Expression {
	expression := &InfixExpression{
		Token:          p.currentToken,
		LeftExpression: leftExpression,
	}
	cPrecedence := p.currPrecedence()
	p.advanceToken()
	expression.RightExpression = p.parseExpression(cPrecedence)
	return expression
}

func (p *Parser) parseGroupedExpression() Expression {
	p.advanceToken()
	expression := p.parseExpression(LOWEST)
	if !p.advanceTokenIfPeekTokIs(RPR) {
		p.ClosedParenMissingError()
		return nil
	}
	return expression
}

func (p *Parser) parseIfExpression() Expression {
	expression := &IfExpression{Token: p.currentToken}
	if !p.advanceTokenIfPeekTokIs(LPR) {
		return nil
	}
	expression.Condition = p.parseGroupedExpression()
	if !p.advanceTokenIfPeekTokIs(LCB) {
		return nil
	}
	expression.IfBody = p.parseBlock()

	if p.peekTokIs(ELK) {
		p.advanceToken()
		if !p.advanceTokenIfPeekTokIs(LCB) {
			return nil
		}
		expression.ElseBody = p.parseBlock()
	} else {
		p.removeExtraNewLines()
		if p.peekTokIs(ELK) {
			p.advanceToken()
			if !p.advanceTokenIfPeekTokIs(LCB) {
				return nil
			}
			expression.ElseBody = p.parseBlock()
		}
	}
	return expression
}

func (p *Parser) parseBlock() *Block {
	block := &Block{ Token: p.currentToken }
	p.advanceToken()
	block.Statements = []Statement{}

	for p.currentToken.TokType != RCB && p.currentToken.TokType != EOF {
		statement := p.parseStatement()
		if statement != nil {
			block.Statements = append(block.Statements, statement)
		}
		p.advanceToken()
	}
	if p.peekTokIs(EOL) {
		p.advanceToken()
	}
	return block
}

func (p *Parser) parseFunctionExpression() Expression {
	expression := &FunctionExpression{ Token: p.currentToken }
	if !p.advanceTokenIfPeekTokIs(LPR) {
		return nil
	}
	expression.Params = p.parseFunctionParameters()
	// fmt.Println("parsing a function ", expression.Params)
	if !p.advanceTokenIfPeekTokIs(LCB) {
		return nil
	}
	expression.FuncBody = p.parseBlock()
	return expression
}

func (p *Parser) parseFunctionParameters() []*Identifier {
	params := []*Identifier{}

	if p.peekTokIs(RPR) {
		p.advanceToken()
		return params
	}
	p.advanceToken()

	param := &Identifier{ Token: p.currentToken }
	params = append(params, param)

	for p.peekTokIs(COM) {
        println("here 1")
        p.advanceToken()
		p.advanceToken()
		param := &Identifier{ Token: p.currentToken }
		params = append(params, param)
	}

	if !p.advanceTokenIfPeekTokIs(RPR) {
		return nil
	}
	return params
}

func (p *Parser) parseFunctionCall(leftExpr Expression) Expression {
	expression := &FunctionCallExpression{ Token:    p.currentToken, Function: leftExpr }
	expression.Arguments = p.parseFunctionCallArguments()
	return expression
}

func (p *Parser) parseFunctionCallArguments() []Expression {
	args := []Expression{}
	if p.peekTokIs(RPR) {
		p.advanceToken()
		return args
	}
	p.advanceToken()
	args = append(args, p.parseExpression(LOWEST))
	for p.peekTokIs(COM) {
		p.advanceToken()
		p.advanceToken()
		args = append(args, p.parseExpression(LOWEST))
	}
	if !p.advanceTokenIfPeekTokIs(RPR) {
		return nil
	}
	return args
}

func (p *Parser) parseLoopStatement() Expression {
	loopExpression := &LoopExpression{ Token: p.currentToken }
	if !p.advanceTokenIfPeekTokIs(LPR) {
		return nil
	}
	loopExpression.Condition = p.parseGroupedExpression()
	if !p.advanceTokenIfPeekTokIs(LCB) {
		return nil
	}
	loopExpression.LoopBody = p.parseBlock()
	return loopExpression
}

func (p *Parser) parseArray() Expression {
	array := &Array{ Token: p.currentToken, Elements: []Expression{} }
	if p.peekTokIs(RSB) {
		p.advanceToken()
		return array
	}
	p.advanceToken()
	array.Elements = append(array.Elements, p.parseExpression(LOWEST))
	for p.peekTokIs(COM) {
		p.advanceToken()
		p.advanceToken()
		array.Elements = append(array.Elements, p.parseExpression(LOWEST))
	}
	if !p.advanceTokenIfPeekTokIs(RSB) {
		return nil
	}
	return array
}

func (p *Parser) parseArrayIndexExpression(leftExpr Expression) Expression {
	arrIndExp := &ArrayIndexExpression{
		Token:          p.currentToken,
		LeftExpression: leftExpr,
	}
	p.advanceToken() // skipping over the [ Token
	arrIndExp.Index = p.parseExpression(LOWEST)
	if !p.advanceTokenIfPeekTokIs(RSB) {
		return nil
	}
	return arrIndExp
}

func (p *Parser) currTokIs(tokentype TokenType) bool {
	return p.currentToken.TokType == tokentype
}

func (p *Parser) peekTokIs(tokentype TokenType) bool {
	return p.peekToken.TokType == tokentype
}

func (p *Parser) advanceTokenIfPeekTokIs(t TokenType) bool {
	if p.peekTokIs(t) {
		p.advanceToken()
		return true
	}
	p.peekTokenMismatchError(t)
	return false
}

func (p *Parser) currPrecedence() int {
	if p, ok := precedenceTable[p.currentToken.TokType]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedenceTable[p.peekToken.TokType]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) removeExtraNewLines() {
	for p.currentToken.TokType != EOF && !p.peekTokIs(EOF) && p.peekTokIs(EOL) {
		p.advanceToken()
	}
}

func (p *Parser) registerPrefixFunc(ype TokenType, prefixFunction prefixFunc) {
	p.prefixFunctions[ype] = prefixFunction
}

func (p *Parser) registerInfixFunc(ype TokenType, infixFunction infixFunc) {
	p.infixFunctions[ype] = infixFunction
}

func (p *Parser) peekTokenMismatchError(tokentype TokenType) {
	err := fmt.Sprintf(
		"\nerror (line %d) expected token of type %s but got %s",
		p.peekToken.Line, tokentype.String(),
		p.peekToken.TokType.String())

	p.errors = append(p.errors, err)
}

func (p *Parser) ClosedParenMissingError() {
	err := fmt.Sprintf(
		"\nError on line %d : Closing parenthesis ')' expected but not found.",
		p.currentToken.Line)
	p.errors = append(p.errors, err)
}

func (p *Parser) WrongTypeError(literal, target string) {
	err := fmt.Sprintf(
		"\nerror (line %d) %q cannot be parsed as %q",
		p.currentToken.Line, literal, target)
	p.errors = append(p.errors, err)
}

func (p *Parser) PrefixDoesntExistError(t TokenType) {
	err := fmt.Sprintf(
		"\nerror (line %d) %q is not a valid 'prefix' expression/n.",
		p.currentToken.Line, t.String())
	p.errors = append(p.errors, err)
}

// WrongDataTypeWithOperatorError : happens when an operator is used with operands that the operator does not operate on
// func (p *Parser) WrongDataTypeWithOperatorError(expected, operator string) {
// 	err := fmt.Sprintf("\nError on line %d : Operator %q can be used with operands of type %s only.", p.ns[p.currentToken].Line, operator, expected)
// 	err += "\n\n\t" + loc[p.ns[p.currentToken].Line]
// 	p.errors = append(p.errors, err)
// }

// formatting error messages
// func (p *Parser) ReportErrors() []string {
// 	if len(p.errors) > 0 {
// 		report := []string{}
// 		for _, v := range p.errors {
// 			report = append(report, v)
// 		}
// 		// report = append(report, "\n")
// 		return report
// 	}
// 	return []string{"None"}
// }
