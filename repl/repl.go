package repl

import (
	"bufio"
	"fmt"
	"io"

	"gomonkey/evaluator"
	"gomonkey/lexer"
	"gomonkey/parser"
)

const PROMPT = ">> "

const MONKEY_FACE = `

`

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		fmt.Print(PROMPT)
		scanned := scanner.Scan()

		if !scanned {
			return
		}

		// REPL Commands...
		line := scanner.Text()
		if line == "[quit]" {
			fmt.Println("[!] Quitting REPL...")
			break
		}

		l := lexer.NewLexer(line)
		p := parser.NewParser(l)

		program := p.ParseProgram()
		if len(p.Errors()) != 0 {
			printParserErrors(out, p.Errors())
			continue
		}

		evaluated := evaluator.Eval(program)
		if evaluated != nil {
			io.WriteString(out, evaluated.Inspect())
			io.WriteString(out, "\n")
		}

	}
}

func printParserErrors(out io.Writer, errors []string) {
	for _, msg := range errors {
		io.WriteString(out, "\t"+msg+"\n")
	}
}
