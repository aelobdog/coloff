package main

import (
	"coloff/tools"
	"fmt"
    "os"
    "io/ioutil"
)

func main() {
	if len(os.Args) != 2 {
		return
	}
	code, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println("Error reading file : " + os.Args[1])
		return
	}

    lexer := tools.CreateLexerState(string(code))
	parser := tools.CreateParserState(lexer)

	program := parser.Parse()

    for _, i := range(program.Statements) {
        fmt.Println(i.String())
    }

	// parseErrors := parser.Errors()
    // for i := range(parseErrors) {
    //     fmt.Println(i)
    // }

	// PARSE-ERROR CHECKING
	// if parseErrors[0] != "None" {
	// 	for _, v := range parseErrors {
	// 		fmt.Println(v)
	// 	}
	// 	os.Exit(22)
	// }
}
