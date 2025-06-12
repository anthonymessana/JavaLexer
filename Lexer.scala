import java.io.RandomAccessFile
import scala.collection.immutable.*
import scala.Option

enum Token:
    case Keyword(value: String)
    case Identifier(value: String)
    case Literal(value: String)
    case Operator(value: String)
    case Seperator(value: String)
    case Comment(value: String)
    case Invalid
    case EOF

class Lexer(filename: String):
    val source = RandomAccessFile(filename, "r")
    private def nextChar = if source.getFilePointer < source.length then Some(source.read().toChar) else None
    
    def isOctal(c: Char) = 0 <= '7' - c && '7' - c <= 7
    def isHex(c: Char) = c.isDigit || 0 <= 'F' - c.toUpper && 'F' - c.toUpper <= 5
    def isOperator(c: Char) = 
        c == '+' || c == '-' || c == '/' || c == '*' || c == '%' ||
        c == '<' || c == '>' || c == '&' || c == '|' || c == '!' ||
        c == '=' || c == '^'
    
    def isSingleOp(c: Char) = c == '!' || c == '^' || c == '*' || c == '/' || c == '%'

    def identifier(lexeme: String): (Token, Long) =
        nextChar match
            case Some(c) if c.isLetterOrDigit || c == '_' || c == '$' => identifier(lexeme + c)
            case Some(c) if c.isWhitespace => (Token.Identifier(lexeme), source.getFilePointer)
            case Some(e) => 
                println(s"ERROR: Identifier contains illegal character $e")
                (Token.Identifier(lexeme), source.getFilePointer)
            case None => (Token.Identifier(lexeme), source.getFilePointer)

    def decLiteral(lexeme: String): (Token, Long) = 
        nextChar match
            case Some(c) if c.isDigit => decLiteral(lexeme + c)
            case Some(c) => (Token.Literal(lexeme), source.getFilePointer)
            case None => (Token.Literal(lexeme), source.getFilePointer)

    def octLiteral(lexeme: String): (Token, Long) =
        nextChar match
            case Some(c) if isOctal(c) => octLiteral(lexeme + c)
            case Some(c) if c.isWhitespace => (Token.Literal(lexeme), source.getFilePointer)
            case Some(_) => (Token.Invalid, source.getFilePointer)
            case None => (Token.Literal(lexeme), source.getFilePointer)
        
    def binLiteral(lexeme: String): (Token, Long) =
        nextChar match 
            case Some(c) if c == '1' || c == '0' => binLiteral(lexeme + c)
            case Some(c) if c.isWhitespace => (Token.Literal(lexeme), source.getFilePointer)
            case Some(_) => (Token.Invalid, source.getFilePointer)
            case None => (Token.Literal(lexeme), source.getFilePointer)

    def hexLiteral(lexeme: String): (Token, Long) =
        nextChar match
            case Some(c) if isHex(c) => hexLiteral(lexeme + c)
            case Some(c) if c.isWhitespace => (Token.Literal(lexeme), source.getFilePointer)
            case Some(_) => (Token.Invalid, source.getFilePointer)
            case None => (Token.Literal(lexeme), source.getFilePointer)

    def stringLiteral(lexeme: String): (Token, Long) = 
        nextChar match
            case Some(c) if c == '"' => (Token.Literal(lexeme), source.getFilePointer)
            case Some(c) if c == '\\' => 
                nextChar match
                    case Some(la) => 
                        if la == 't' || la == 'b' || la == 'n' || la == 'r' ||
                            la == 'f' || la == '\'' || la == '"' || la == '\\' 
                        then
                            stringLiteral(lexeme + s"\\$la".translateEscapes)
                        else
                            stringLiteral(lexeme + la)
                    case None => stringLiteral(lexeme)
            case Some(c) => stringLiteral(lexeme + c)
            case None => println("ERROR: Unclosed string literal"); (Token.Invalid, source.getFilePointer)
 
    def operator(lexeme: String): (Token, Long) =
        nextChar match
            case Some(c) 
                if lexeme.length == 1 && isOperator(c) && c == '=' || 
                lexeme.charAt(0) == c && !isSingleOp(c)
                => operator(lexeme + c)
            case Some('=') if lexeme == ">>" || lexeme == "<<" => operator(lexeme + '=')
            case Some(c) if !isOperator(c) => (Token.Operator(lexeme), source.getFilePointer)
            case Some(_) => (Token.Invalid, source.getFilePointer)
            case None => (Token.Operator(lexeme), source.getFilePointer)

    def nextToken(pos: Long): (Token, Long) = 
        source.seek(pos)
        nextChar match
            case Some(c) =>
                val curLexeme = "" + c
                if c.isWhitespace then
                    nextToken(pos + 1) 
                else if c.isLetter || c == '_' || c == '$' then 
                    identifier(curLexeme)
                else if c.isDigit then
                    (c, nextChar) match 
                        case ('0', Some(la)) if la == 'x' => hexLiteral(curLexeme + la) 
                        case ('0', Some(la)) if la == 'b' => binLiteral(curLexeme + la)
                        case ('0', Some(la)) if la.isDigit => octLiteral(curLexeme + la)
                        case (_, Some(la)) if la.isDigit => decLiteral(curLexeme + la)
                        case (_, Some(la)) if la.isWhitespace => (Token.Literal(curLexeme), source.getFilePointer)
                        case (_, None) => (Token.Literal(curLexeme), source.getFilePointer)
                        case (_, Some(_)) => println("ERROR: Malformed Int literal"); (Token.Invalid, source.getFilePointer)
                else if c == '"' then
                    stringLiteral(curLexeme)
                else if isOperator(c) then
                    operator(curLexeme)
                else
                    println("ERROR: Unrecognized lexeme"); (Token.Invalid, source.getFilePointer())
            case None => println("ERROR: EOF"); (Token.EOF, source.length)