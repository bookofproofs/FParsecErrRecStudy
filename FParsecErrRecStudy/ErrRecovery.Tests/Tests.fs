namespace ErrRecovery.Tests
open FParsec
open ErrRecovery
open ParserExamples
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestErrRecovery () =
    /// a helper function to replace any whitespace from the string representation of AST
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestAst00 () =
        // in this test, we just run the parser on an syntactically correct input and return an AST
        let inputAst00 = "begin run {a,c };run{a, b} end begin run{a,b,c}; run{a,b} end"
        let result = run globalParser inputAst00
        let actual = sprintf "%O" result
        let expected = """Success: Ast
  [Block (RunSequence [Run (Sequence [A; C]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTryParseNoError () =
        // in this test, we run the parser on an syntactically correct input, 
        // returning an AST, and expect no diagnostics
        let inputNoError = "begin run {a,c };run{a, b} end begin run{a,b,c}; run{a,b} end"
        let result = tryParse globalParser "This should return the ast, since there is no error" ad inputNoError
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A; C]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTryParseNoErrorDiag () =
        // in this test, we run the parser on an syntactically correct input, 
        // returning an AST, and expect no diagnostics
        let inputNoError = "begin run {a,c };run{a, b} end begin run{a,b,c}; run{a,b} end"
        ad.Clear()
        let result = tryParse globalParser "This should return the ast, since there is no error" ad inputNoError
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEcharSequence01 () =
        // In this use case, the first charSequence contains a 'd' but expects 'a'|'b'|'c'.
        let inputTestEcharSequence01 = "begin run {a,c,d,a };run{a, b} end begin run{a,b,c}; run{a,b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence01
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A; C; Escape; A]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEcharSequence01Diag () =
        // In this use case, the first charSequence contains a 'd' but expects 'a'|'b'|'c'.
        let inputTestEcharSequence01 = "begin run {a,c,d,a };run{a, b} end begin run{a,b,c}; run{a,b} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence01
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 16),
   DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)


    [<TestMethod>]
    member this.TestEcharSequence02 () =
        // In this use case, the first charSequence contains a ',' that is not followed by some 'a'|'b'|'c'.
        let inputTestEcharSequence02 = "begin run {a,c,b, };run{a, b} end begin run{a,b,c}; run{a,b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence02
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block
     (RunSequence
        [Run (Sequence [A; C; B; Escape]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEcharSequence02Diag () =
        // In this use case, the first charSequence contains a ',' that is not followed by some 'a'|'b'|'c'.
        let inputTestEcharSequence02 = "begin run {a,c,b, };run{a, b} end begin run{a,b,c}; run{a,b} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence02
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 19),
   DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEcharSequence03 () =
        // In this use case, the second charSequence ends by ',' that is not followed another charChoice, missing an '}' token.
        // Escaping the error is still possible, because a third, properly closed charSequence occurs.
        let inputTestEcharSequence03 = " begin run {a};run {a, b, ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence03
        let actual = sprintf "%O" result
        let expected = """Ast [Block (RunSequence [Run (Sequence [A]); Run (Sequence [A; B; Escape; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEcharSequence03Diag () =
        // In this use case, the second charSequence ends by ',' that is not followed another charChoice, missing an '}' token.
        // Escaping the error is still possible, because a third, properly closed charSequence occurs.
        let inputTestEcharSequence03 = " begin run {a};run {a, b, ;run{a, b} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence03
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 27),
   DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEcharSequence04 () =
        // In this use case, the second run block is contains a comma instead of a charSequence.
        let inputTestEcharSequence04 = "begin run {a,c,a};run { , } ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence04
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block
         (RunSequence
            [Run (Sequence [A; C; A]); Run (Sequence [Escape; Escape]);
             Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEcharSequence04Diag () =
        // In this use case, the second run block is contains a comma instead of a charSequence.
        ad.Clear()
        let inputTestEcharSequence04 = "begin run {a,c,a};run { , } ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence04
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 25),
   DiagnosticMessage "charChoice a|b|c expected")
Diagnostic
  (Parser, Error, (Ln: 1, Col: 27),
   DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEcharSequence05 () =
        // In this use case, the second run block does close properly and has no valid charSequence.
        let inputTestEcharSequence05 = "begin run {a};run { ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence05
        let actual = sprintf "%O" result
        let expected = """Ast [Block (RunSequence [Run (Sequence [A]); Run (Sequence [Escape; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEcharSequence05Diag () =
        // In this use case, the second run block does close properly and has no valid charSequence.
        ad.Clear()
        let inputTestEcharSequence05 = "begin run {a};run { ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence05
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 21),
       DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)
        
    [<TestMethod>]
    member this.TestEcharSequence06 () =
        // In this use case, the second run block does close properly and has two opening curly brackets.
        let inputTestEcharSequence06 = "begin run {a};run {{ ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence06
        let actual = sprintf "%O" result
        let expected = """Ast [Block (RunSequence [Run (Sequence [A]); Run (Sequence [Escape; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEcharSequence06Diag () =
        // In this use case, the second run block does close properly and has two opening curly brackets.
        ad.Clear()
        let inputTestEcharSequence06 = "begin run {a};run {{ ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcharSequence06
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 20),
       DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunSequence01 () =
        // In this use case, the second runBlock is not started properly.
        let inputErunSequence01 = "begin run {a};xxx {a, b};run{a, b} end begin run{a,b,c}; run{a,b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence01
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A]); Escape; Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunSequence01Diag () =
        // In this use case, the second runBlock is not started properly.
        let inputErunSequence01 = "begin run {a};xxx {a, b};run{a, b} end begin run{a,b,c}; run{a,b} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence01
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 15), DiagnosticMessage "run block expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunSequence02 () =
        // In this use case, there is a third runBlock missing before the 'end' token.
        let inputErunSequence02 = "begin run {a};run {a, b}; end begin run{a,b,c}; run{a,b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence02
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block (RunSequence [Run (Sequence [A]); Run (Sequence [A; B]); Escape]);
       Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunSequence02Diag () =
        // In this use case, there is a third runBlock missing before the 'end' token.
        let inputErunSequence02 = "begin run {a};run {a, b}; end begin run{a,b,c}; run{a,b} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence02
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 27), DiagnosticMessage "run block expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunSequence03 () =
        // In this use case, the second runSequence ends by ';' that is not followed another runBlock, missing an 'end' token.
        // Escaping the error is still possible, because a third, properly closed runSequence occurs.
        let inputErunSequence03 = "begin run {a};run {a, b}; begin run{a,b,c}; run{a,b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence03
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block
     (RunSequence
        [Run (Sequence [A]); Run (Sequence [A; B]); Escape;
         Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunSequence03Diag () =
        // In this use case, the second runSequence ends by ';' that is not followed another runBlock, missing an 'end' token.
        // Escaping the error is still possible, because a third, properly closed runSequence occurs.
        let inputErunSequence03 = "begin run {a};run {a, b}; begin run{a,b,c}; run{a,b} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence03
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 27), DiagnosticMessage "run block expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunSequence04 () =
        // In this use case, the second begin end block is contains a semicolon instead of a runSequence.
        let inputErunSequence04 = "begin run {a} end begin ; end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence04
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block (RunSequence [Run (Sequence [A])]);
       Block (RunSequence [Escape; Escape])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunSequence04Diag () =
        // In this use case, the second begin end block is contains a semicolon instead of a runSequence.
        let inputErunSequence04 = "begin run {a} end begin ; end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence04
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 25), DiagnosticMessage "run block expected")
    Diagnostic
      (Parser, Error, (Ln: 1, Col: 27), DiagnosticMessage "run block expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunSequence05 () =
        // In this use case, the second begin end block is contains a semicolon instead of a runSequence.
        let inputErunSequence05 = "begin run {a} end begin begin run{b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence05
        let actual = sprintf "%O" result
        let expected = """Ast [Block (RunSequence [Run (Sequence [A])]); Block (RunSequence [Escape])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunSequence05Diag () =
        // In this use case, the second begin end block does close properly and has no valid runSequence.
        let inputErunSequence05 = "begin run {a} end begin begin run{b} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence05
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 25), DiagnosticMessage "run block expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunSequence06 () =
        // In this use case, the second begin end block does close properly but has two opening 'begin' tokens.
        let inputErunSequence06 = "begin run {a} end begin begin end begin run{b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence06
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block (RunSequence [Run (Sequence [A])]); Block (RunSequence [Escape]);
       Block (RunSequence [Run (Sequence [B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunSequence06Diag () =
        // In this use case, the second begin end block does close properly but has two opening 'begin' tokens.
        let inputErunSequence06 = "begin run {a} end begin begin end begin run{b} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputErunSequence06
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 25), DiagnosticMessage "run block expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunBlock01 () =
        // In this use case, the second run block is missing an opening "{".
        let inputTestErunBlock01 = "begin run {a};run a, b };run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock01
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block
         (RunSequence
            [Run (Sequence [A]); Run (Sequence [A; B]); Run (Sequence [A; B])])]""" 
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunBlock01Diag () =
        // In this use case, the second run block is missing an opening "{".
        let inputTestErunBlock01 = "begin run {a};run a, b };run{a, b} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock01
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 19), DiagnosticMessage "missing opening {")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunBlock02 () =
        // In this use case, the second run block is missing both, an opening "{", and a closing "}".
        let inputTestErunBlock02 = "begin run {a,c,a};run a,c ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock02
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block
     (RunSequence
        [Run (Sequence [A; C; A]); Run (Sequence [A; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunBlock02Diag () =
        // In this use case, the second run block is missing both, an opening "{", and a closing "}".
        ad.Clear()
        let inputTestErunBlock02 = "begin run {a,c,a};run a,c ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock02
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 23), DiagnosticMessage "missing opening {")
    Diagnostic
      (Parser, Error, (Ln: 1, Col: 27), DiagnosticMessage "missing closing }")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunBlock03 () =
        // In this use case, the second run block is empty.
        let inputTestErunBlock03 = "begin run {a,c,a};run { } ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock03
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block
     (RunSequence
        [Run (Sequence [A; C; A]); Run Empty; Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunBlock03Diag () =
        // In this use case, the second run block is empty.
        ad.Clear()
        let inputTestErunBlock03 = "begin run {a,c,a};run { } ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock03
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunBlock04 () =
        // In this use case, the second run block is missing a closing "}".
        let inputTestErunBlock04 = "begin run {a};run { a, b ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock04
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block
         (RunSequence
            [Run (Sequence [A]); Run (Sequence [A; B]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunBlock04Diag () =
        // In this use case, the second run block is missing a closing "}".
        ad.Clear()
        let inputTestErunBlock04 = "begin run {a};run { a, b ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock04
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 19), DiagnosticMessage "missing closing }")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunBlock05 () =
        // In this use case, the second run block is missing an opening "{" and a charSequence.
        let inputTestErunBlock05 = "begin run {a};run };run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock05
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block
         (RunSequence
            [Run (Sequence [A]); Run (Sequence [Escape]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunBlock05Diag () =
        // In this use case, the second run block is missing an opening "{" and a charSequence.
        ad.Clear()
        let inputTestErunBlock05 = "begin run {a};run };run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock05
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 19),
       DiagnosticMessage "charChoice a|b|c expected")
    Diagnostic
      (Parser, Error, (Ln: 1, Col: 19), DiagnosticMessage "missing opening {")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErunBlock06 () =
        // In this use case, the second run block contains only an opening "{".
        let inputTestErunBlock06 = "begin run {a};run { ;run{b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock06
        let actual = sprintf "%O" result
        let expected = """Ast [Block (RunSequence [Run (Sequence [A]); Run (Sequence [Escape])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErunBlock06Diag () =
        // In this use case, the second run block contains only an opening "{".
        ad.Clear()
        let inputTestErunBlock06 = "begin run {a};run { ;run{b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErunBlock06
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 21),
       DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEbeginEndBlock01 () =
        // In this use case, the second beginEnd block is missing an opening "begin".
        let inputTestEbeginEndBlock01 = "begin run {a} end run {b} end begin run{c} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock01
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block (RunSequence [Run (Sequence [A])]);
       Block (RunSequence [Run (Sequence [B])]);
       Block (RunSequence [Run (Sequence [C])])]""" 
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbeginEndBlock01Diag () =
        // In this use case, the second beginEnd block is missing an opening "begin".
        let inputTestEbeginEndBlock01 = "begin run {a} end run {b} end begin run{c} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock01
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 19), DiagnosticMessage "missing opening begin")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEbeginEndBlock02 () =
        // In this use case, the second beginEnd block is missing both, an opening "begin", and a closing "end".
        let inputTestEbeginEndBlock02 = "begin run {a} end run {b} begin run{c} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock02
        let actual = sprintf "%O" result
        let expected = """Error""" 
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbeginEndBlock02Diag () =
        // In this use case, the second beginEnd block is missing both, an opening "begin", and a closing "end".
        let inputTestEbeginEndBlock02 = "begin run {a} end run {b} begin run{c} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock02
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 19), DiagnosticMessage "missing opening begin")
Diagnostic
  (Parser, Error, (Ln: 1, Col: 27), DiagnosticMessage "missing closing end")
Diagnostic
  (Parser, Error, (Ln: 1, Col: 27),
   DiagnosticMessage
     "parser could not recover from errors; Error in Ln: 1 Col: 27
begin run {a} end run {b} begin run{c} end
                          ^
Expecting: end of input, whitespace or ';'
")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEbeginEndBlock03 () =
        // In this use case, the second beginEnd block is empty.
        let inputTestEbeginEndBlock03 = "begin run {a} end begin end begin run{c} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock03
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block (RunSequence [Run (Sequence [A])]); Block Empty;
       Block (RunSequence [Run (Sequence [C])])]""" 
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbeginEndBlock03Diag () =
        // In this use case, the second beginEnd block is empty.
        let inputTestEbeginEndBlock03 = "begin run {a} end begin end begin run{c} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock03
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEbeginEndBlock04 () =
        // In this use case, the second beginEnd block is missing a closing "end".
        let inputTestEbeginEndBlock04 = "begin run {a} end begin run {b} begin run{c} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock04
        let actual = sprintf "%O" result
        let expected = """Error""" 
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbeginEndBlock04Diag () =
        // In this use case, the second beginEnd block is missing a closing "end".
        let inputTestEbeginEndBlock04 = "begin run {a} end begin run {b} begin run{c} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock04
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 19), DiagnosticMessage "missing closing end")
    Diagnostic
      (Parser, Error, (Ln: 1, Col: 33),
       DiagnosticMessage
         "parser could not recover from errors; Error in Ln: 1 Col: 33
    begin run {a} end begin run {b} begin run{c} end
                                    ^
    Expecting: end of input, whitespace or ';'
    ")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEbeginEndBlock05 () =
        // In this use case, the second beginEnd block is missing an opening "begin" and a runSequence.
        let inputTestEbeginEndBlock05 = "begin run {a} end end begin run{c} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock05
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block (RunSequence [Run (Sequence [A])]); Block (RunSequence [Escape]);
       Block (RunSequence [Run (Sequence [C])])]""" 
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbeginEndBlock05Diag () =
        // In this use case, the second beginEnd block is missing an opening "begin" and a runSequence.
        let inputTestEbeginEndBlock05 = "begin run {a} end end begin run{c} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock05
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 19), DiagnosticMessage "run block expected")
    Diagnostic
      (Parser, Error, (Ln: 1, Col: 19), DiagnosticMessage "missing opening begin")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEbeginEndBlock06 () =
        // In this use case, the second beginEnd block only contains an opening "begin".
        let inputTestEbeginEndBlock06 = "begin run {a} end begin begin run{c} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock06
        let actual = sprintf "%O" result
        let expected = """Ast [Block (RunSequence [Run (Sequence [A])]); Block (RunSequence [Escape])]""" 
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbeginEndBlock06Diag () =
        // In this use case, the second beginEnd block only contains an opening "begin".
        let inputTestEbeginEndBlock06 = "begin run {a} end begin begin run{c} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock06
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 25), DiagnosticMessage "run block expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEbeginEndBlock07 () =
        // In this use case, the second block does not start properly.
        let inputTestEbeginEndBlock07 = "begin run {a};run{b} end xxxbegin run{b,c} end begin run{c}; run{c} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock07
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block (RunSequence [Run (Sequence [A]); Run (Sequence [B])]);
       Block (RunSequence [Escape]);
       Block (RunSequence [Run (Sequence [C]); Run (Sequence [C])])]""" 
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbeginEndBlock07Diag () =
        // In this use case, the second block does not start properly.
        let inputTestEbeginEndBlock07 = "begin run {a};run{b} end xxxbegin run{b,c} end begin run{c}; run{c} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEbeginEndBlock07
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 26), DiagnosticMessage "run block expected")
    Diagnostic
      (Parser, Error, (Ln: 1, Col: 26), DiagnosticMessage "missing opening begin")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)
