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

// TokenType : type of token
type TokenType int

// valid token types in COLON
const (
	INT TokenType = iota // INTEGER
	FLT                  // FLOAT
	STR                  // STRING
    CHR                  // CHARACTER
	BOL                  // BOOLEAN

	LPR                  // LEFT PARENTHESIS
	RPR                  // RIGHT PARENTHESIS
	LSB                  // LEFT SQUARE BRACKET
	RSB                  // RIGHT SQUARE BRACKET
	LCB                  // LEFT CURLY BRACKET
	RCB                  // RIGHT CURLY BRACKET

	PLS                  // PLUS
	MIN                  // MINUS
	PRD                  // PRODUCT
	DIV                  // DIVISION
	REM                  // REMAINDER
	POW                  // POWER

	EQL                  // EQUAL
	NEQ                  // NOT EQUAL
	GRT                  // GREATER
	LST                  // LESSER
	GRE                  // GEATER OR EQUAL
	LSE                  // LESS OR EQUAL

	ASN                  // ASSIGNMENT
	COM                  // COMMA

	LND                  // LOGICAL_AND
	LOR                  // LOGICAL_OR
	LNT                  // LOGICAL_NOT

	IDN                  // IDENTIFIER

	VAR                  // VARIABLE
	IFK                  // IF
	ELK                  // ELSE
	LPK                  // LOOP
	FNK                  // FUNCTION
	RET                  // RETURN

	EOL                  // END OF LINE
	EOF                  // END OF FILE
	ILG                  // ILLEGAL CHARACTER
)

// String [from tokentype]
func (t TokenType) String() string {
	switch t {
	case INT:
		return "INTEGER"
	case FLT:
		return "FLOATING"
	case STR:
		return "STRING"
	case BOL:
		return "BOOLEAN"
	case LPR:
		return "LEFT PARENTHESES"
	case RPR:
		return "RIGHT PARENTHESES"
	case PLS:
		return "PLUS"
	case MIN:
		return "MINUS"
	case PRD:
		return "PRODUCT"
	case DIV:
		return "DIVISION"
	case REM:
		return "REMAINDER"
	case POW:
		return "POWER"
	case EQL:
		return "EQUAL"
	case NEQ:
		return "NOT_EQUAL"
	case GRT:
		return "GREATER_THAN"
	case LST:
		return "LESS_THAN"
	case GRE:
		return "GREATER_EQUAL"
	case LSE:
		return "LESSER_EQUAL"
	case ASN:
		return "ASSIGNMENT"
	case LND:
		return "LOGICAL_AND"
	case LOR:
		return "LOGICAL_OR"
	case LNT:
		return "LOGICAL_NOT"
	case IDN:
		return "IDENTIFIER"
	case VAR:
		return "VARIABLE"
	case IFK:
		return "IF"
	case ELK:
		return "ELSE"
	case LPK:
		return "LOOP"
	case FNK:
		return "FUNCTION"
	case RET:
		return "RETURN"
	case COM:
		return "COMMA"
	case LSB:
		return "LEFT SQUARE BRACKET"
	case RSB:
		return "RIGHT SQUARE BRACKET"
	case LCB:
		return "LEFT CURLY BRACKET"
	case RCB:
		return "RIGHT CURLY BRACKET"
	case EOF:
		return "EOF"
	case EOL:
		return "EOL"
	}
	return "ILLEGAL_TOKEN"
}

// Keywords map
var Keywords map[string]TokenType = map[string]TokenType {
	"def"   : VAR,
	"if"    : IFK,
	"else"  : ELK,
	"loop"  : LPK,
	"func"  : FNK,
	"return": RET,
}

// Token : properties
type Token struct {
	TokType TokenType
	Literal string
	Line    int
}

// NewToken : to assemble a token 'object'
func NewToken(tokenType TokenType, lit string, line int) Token {
	return Token{TokType: tokenType, Literal: lit, Line: line}
}

// IsDigit : to check if the current character is a digit
func IsDigit(val byte) bool {
	return val >= '0' && val <= '9'
}

// IsLetter : to check if the current character is a letter
func IsLetter(val byte) bool {
	return (val >= 'a' && val <= 'z') || (val >= 'A' && val <= 'Z') || (val == '_')
}

// IsKeyword : to check if the given string is a keyword or not
func IsKeyword(word string) bool {
	_, ok := Keywords[word];
    return ok
}
