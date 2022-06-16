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
	p.registerPrefixFunc(FLT, p.parseFloatingLiteral)
	p.registerPrefixFunc(BOL, p.parseBooleanLiteral)
	p.registerPrefixFunc(STR, p.parseStringLiteral)
	p.registerPrefixFunc(MIN, p.parsePrefixExpression)
	p.registerPrefixFunc(LNT, p.parsePrefixExpression)
	// p.registerPrefixFunc(LPR, p.parseGroupedExpression)

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
	// p.registerInfixFunc(LPR, p.parseFunctionCall)
	// p.registerInfixFunc(LSB, p.parseArrayIndexExpression)

	return p
}

func (p *Parser) advanceToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.lexer.NextToken()
    // fmt.Println(p.currentToken.Literal, p.peekToken.Literal)
}

// returns a list of errors
func (p *Parser) Errors() []string {
	return p.errors
}

// parse a program
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

// parses a statemnt
func (p *Parser) parseStatement() Statement {
	switch p.currentToken.TokType {
	case VAR:
		return p.parseVarStatement()
	case RET:
		return p.parseReturnStatement()
	case EOL:
		return nil
	default:
		return p.parseExpressionStatement()
	}
}

// parses a 'def' statement
func (p *Parser) parseVarStatement() *VarStatement {
	statement := &VarStatement{Token: p.currentToken}

	if !p.advanceTokenIfPeekTokIs(IDN) { return nil }
	statement.Name = &Identifier{ Token: p.currentToken }
	if !p.advanceTokenIfPeekTokIs(ASN) { return nil }
	p.advanceToken()

	statement.Value = p.parseExpression(LOWEST)
    // println(p.parseExpression(LOWEST))
	if p.peekTokIs(EOL) { p.advanceToken() }

	return statement
}

func (p *Parser) parseReturnStatement() *ReturnStatement {
	statement := &ReturnStatement{Token: p.currentToken}
	p.advanceToken()
	p.advanceToken()
	statement.ReturnValue = p.parseExpression(LOWEST)
	if p.peekTokIs(EOL) { p.advanceToken() }
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
	return &Identifier{ Token: p.currentToken }
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
	return &StringLiteral{ Token: p.currentToken }
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
	expression := &PrefixExpression{ Token: p.currentToken }
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

// func (p *Parser) parseGroupedExpression() Expression {
// 	p.advanceToken()
// 	expression := p.parseExpression(LOWEST)
// 	if !p.NextTokenIs(RPR) {
// 		p.ClosedParenMissingError()
// 		return nil
// 	}
// 	return expression
// }

// func (p *Parser) parseIfExpression() Expression {
// 	expression := &IfExpression{
// 		Token: p.ns[p.currentToken],
// 	}
// 	if !p.NextTokenIs(LPR) {
// 		return nil
// 	}
// 	expression.Condition = p.parseGroupedExpression()
// 	// if !p.NextTokenIs(BLK) {
// 	// 	return nil
// 	// }
// 	// expression.IfBody = p.parseBlock(IFE)
// 
// 	// ---------------------------------------------------------------------------------
// 	// to take care of cases where else starts on the same line that if ends on :
// 	// if p.peekTokIs(ELB) {
// 	// 	p.advanceToken()
// 	// 	if !p.NextTokenIs(BLK) {
// 	// 		return nil
// 	// 	}
// 	// 	expression.ElseBody = p.parseBlock(ELE)
// 	// 	// ---------------------------------------------------------------------------------
// 	// } else {
// 	// 	// to remove the extra EOLs that may exist after if's end & else's start
// 	// 	p.removeExtraNewLines()
// 	// 	// if there is an else after a bunch of new lines, it needs to be handled
// 	// 	if p.peekTokIs(ELB) {
// 	// 		p.advanceToken()
// 	// 		if !p.NextTokenIs(BLK) {
// 	// 			return nil
// 	// 		}
// 	// 		expression.ElseBody = p.parseBlock(ELE)
// 	// 	}
// 	// }
// 	// ---------------------------------------------------------------------------------
// 	return expression
// }
// 
// func (p *Parser) parseBlock(endToken TokenType) *Block {
// 	block := &Block{
// 		Token: p.ns[p.currentToken],
// 	}
// 	p.advanceToken()
// 	block.Statements = []Statement{}
// 
// 	for p.ns[p.currentToken].TokType != endToken && p.ns[p.currentToken].TokType != EOF {
// 		statement := p.parseStatement()
// 		if statement != nil {
// 			block.Statements = append(block.Statements, statement)
// 		}
// 		p.advanceToken()
// 	}
// 	if p.peekTokIs(EOL) {
// 		p.advanceToken()
// 	}
// 	return block
// }
// 
// func (p *Parser) parseFunctionExpression() Expression {
// 	expression := &FunctionExpression{
// 		Token: p.ns[p.currentToken],
// 	}
// 	if !p.NextTokenIs(LPR) {
// 		return nil
// 	}
// 	expression.Params = p.parseFunctionParameters()
// 	// fmt.Println(p.parseFunctionParameters())
// 	// if !p.NextTokenIs(BLK) {
// 	// 	return nil
// 	// }
// 	// expression.FuncBody = p.parseBlock(FNE)
// 	return expression
// }
// 
// func (p *Parser) parseFunctionParameters() []*Identifier {
// 	params := []*Identifier{}
// 
// 	// current n is "(" So checking if the next n is ")" , denoting that function has no params
// 	if p.peekTokIs(RPR) {
// 		p.advanceToken() // current n is now ")" and the next n should be ":"
// 		return params
// 	}
// 	p.advanceToken()
// 
// 	// Extracting the first parameter
// 	param := &Identifier{
// 		Token: p.ns[p.currentToken],
// 		Value: p.ns[p.currentToken].Literal,
// 	}
// 	params = append(params, param)
// 
// 	// Extracting any other parameters if present
// 	for p.peekTokIs(COM) {
// 		// skipping over the comma
// 		p.advanceToken()
// 		p.advanceToken()
// 		param := &Identifier{
// 			Token: p.ns[p.currentToken],
// 			Value: p.ns[p.currentToken].Literal,
// 		}
// 		params = append(params, param)
// 	}
// 	if !p.NextTokenIs(RPR) {
// 		return nil
// 	}
// 	return params
// }
// 
// func (p *Parser) parseFunctionCall(leftExpr Expression) Expression {
// 	expression := &FunctionCallExpression{
// 		Token:    p.ns[p.currentToken],
// 		Function: leftExpr,
// 	}
// 	expression.Arguments = p.parseFunctionCallArguments()
// 	return expression
// }
// 
// func (p *Parser) parseFunctionCallArguments() []Expression {
// 	args := []Expression{}
// 	if p.peekTokIs(RPR) {
// 		p.advanceToken()
// 		return args
// 	}
// 	p.advanceToken()
// 	args = append(args, p.parseExpression(LOWEST))
// 	for p.peekTokIs(COM) {
// 		p.advanceToken()
// 		p.advanceToken()
// 		args = append(args, p.parseExpression(LOWEST))
// 	}
// 	if !p.NextTokenIs(RPR) {
// 		return nil
// 	}
// 	return args
// }
// 
// func (p *Parser) parseLoopStatement() Expression {
// 	loopExpression := &LoopExpression{
// 		Token: p.ns[p.currentToken],
// 	}
// 	if !p.NextTokenIs(LPR) {
// 		return nil
// 	}
// 	loopExpression.Condition = p.parseGroupedExpression()
// 	// if !p.NextTokenIs(BLK) {
// 	// 	return nil
// 	// }
// 	// loopExpression.LoopBody = p.parseBlock(LPE)
// 	return loopExpression
// }
// 
// func (p *Parser) parseArray() Expression {
// 	array := &Array{
// 		Token:    p.ns[p.currentToken],
// 		Elements: []Expression{},
// 	}
// 	if p.peekTokIs(RSB) {
// 		p.advanceToken()
// 		return array
// 	}
// 	p.advanceToken()
// 	array.Elements = append(array.Elements, p.parseExpression(LOWEST))
// 	for p.peekTokIs(COM) {
// 		p.advanceToken()
// 		p.advanceToken()
// 		array.Elements = append(array.Elements, p.parseExpression(LOWEST))
// 	}
// 	if !p.NextTokenIs(RSB) {
// 		return nil
// 	}
// 	return array
// }
// 
// func (p *Parser) parseArrayIndexExpression(leftExpr Expression) Expression {
// 	arrIndExp := &ArrayIndexExpression{
// 		Token:          p.ns[p.currentToken],
// 		LeftExpression: leftExpr,
// 	}
// 	p.advanceToken() // skipping over the [ Token
// 	arrIndExp.Index = p.parseExpression(LOWEST)
// 	if !p.NextTokenIs(RSB) {
// 		return nil
// 	}
// 	return arrIndExp
// }

func (p *Parser) currTokIs(tokentype TokenType) bool {
    return p.currentToken.TokType == tokentype
}

func (p *Parser) peekTokIs(tokentype TokenType) bool {
    return p.peekToken.TokType == tokentype
}

// returns true if token advenced, false otherwise
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
// 
// func (p *Parser) removeExtraNewLines() {
// 	for p.currentToken < len(p.ns)-1 &&
// 		!p.peekTokIs(EOF) &&
// 		p.peekTokIs(EOL) {
// 		p.advanceToken()
// 	}
// }

/* --------------------------------------------------------------------------
						Operator registration functions
  --------------------------------------------------------------------------- */

// registerPrefixFunc : adds an entry to the prefix functions map
func (p *Parser) registerPrefixFunc(ype TokenType, prefixFunction prefixFunc) {
	p.prefixFunctions[ype] = prefixFunction
}

// registerInfixFunc : adds an entry to the infix functions map
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

// // ClosedParenMissingError : happens when an expression is missing a right parenthesis
// func (p *Parser) ClosedParenMissingError() {
// 	err := fmt.Sprintf("\nError on line %d : Closing parenthesis ')' expected but not found.", p.ns[p.currentToken].Line)
// 	err += "\n\n\t" + loc[p.ns[p.currentToken].Line]
// 	p.errors = append(p.errors, err)
// }

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
