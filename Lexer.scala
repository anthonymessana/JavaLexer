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
    
    private val isDec: (Char => Boolean) = _.isDigit
    private def isOctal(c: Char) = 0 <= '7' - c && '7' - c <= 7
    private def isHex(c: Char) = c.isDigit || 0 <= 'F' - c.toUpper && 'F' - c.toUpper <= 5
    private def isBin(c: Char) = c == '0' || c == '1'
    private def isOperator(c: Char) = 
        c == '+' || c == '-' || c == '/' || c == '*' || c == '%' ||
        c == '<' || c == '>' || c == '&' || c == '|' || c == '!' ||
        c == '=' || c == '^'
    // these are operators that do not form another operator if repeated twice
    private def isSingleOp(c: Char) = c == '!' || c == '^' || c == '*' || c == '/' || c == '%'
    private def isEscapeChar(c: Char) = 
        c == 't' || c == 'b' || c == 'n' || c == 'r' ||
        c == 'f' || c == '\'' || c == '"' || c == '\\' 

    def identifier(lexeme: String): (Token, Long) =
        nextChar match
            case Some(c) if c.isLetterOrDigit || c == '_' || c == '$' => identifier(lexeme + c)
            case Some(c) if c.isWhitespace => (Token.Identifier(lexeme), source.getFilePointer)
            case Some(e) => 
                println(s"ERROR: Identifier contains illegal character $e")
                (Token.Identifier(lexeme), source.getFilePointer)
            case None => (Token.Identifier(lexeme), source.getFilePointer)

    def intLiteral(lexeme: String)(f: Char => Boolean): (Token, Long) = 
        nextChar match
            case Some(c) if f(c) => intLiteral(lexeme + c)(f)
            case Some(c) if c.isLetterOrDigit => (Token.Invalid, source.getFilePointer)
            case Some(c) if c == '.'  => 
                if f == isDec then floatLiteral(lexeme + c) 
                else (Token.Invalid, source.getFilePointer)
            case Some(_) => (Token.Literal(lexeme), source.getFilePointer)
            case None => (Token.Literal(lexeme), source.getFilePointer)

    def floatLiteral(lexeme: String): (Token, Long) =
        nextChar match
            case Some(c) if c.isDigit => floatLiteral(lexeme + c)
            case Some('F') | Some('f') => (Token.Literal(lexeme), source.getFilePointer)
            case Some('d') => (Token.Literal(lexeme), source.getFilePointer)
            case Some(c) if c.isLetter => (Token.Invalid, source.getFilePointer)
            case Some(_) => (Token.Literal(lexeme), source.getFilePointer)
            case None if lexeme.charAt(lexeme.length-1) == '.' => (Token.Invalid, source.getFilePointer)
            case None => (Token.Literal(lexeme), source.getFilePointer)

    def stringLiteral(lexeme: String): (Token, Long) = 
        (lexeme, nextChar) match
            case (_, Some(c)) if c == '"' => (Token.Literal(lexeme), source.getFilePointer)
            case (s"$l\\", Some(c)) if isEscapeChar(c) => stringLiteral((lexeme + c).translateEscapes)
            case (_, Some(c)) => stringLiteral(lexeme + c)
            case (_, None) => println("ERROR: Unclosed string literal"); (Token.Invalid, source.getFilePointer)
 
    def operator(lexeme: String): (Token, Long) =
        nextChar match
            case Some(c) 
                if lexeme.length == 1 && isOperator(c) && c == '=' || lexeme.charAt(0) == c && !isSingleOp(c)
                => operator(lexeme + c)
            case Some('=') if lexeme == ">>" || lexeme == "<<" => operator(lexeme + '=')
            case Some(c) if !isOperator(c) => (Token.Operator(lexeme), source.getFilePointer)
            case Some(_) => (Token.Invalid, source.getFilePointer)
            case None => (Token.Operator(lexeme), source.getFilePointer)

    def nextToken(pos: Long): (Token, Long) = 
        source.seek(pos)
        nextChar match
            case Some(c) =>
                val curLexeme = c.toString
                if c.isWhitespace then
                    nextToken(pos + 1) 
                else if c.isLetter || c == '_' || c == '$' then 
                    identifier(curLexeme)
                else if c.isDigit then
                    (c, nextChar) match 
                        case ('0', Some(la)) if la == 'x' => intLiteral(curLexeme + la)(isHex)
                        case ('0', Some(la)) if la == 'b' => intLiteral(curLexeme + la)(isBin)
                        case ('0', Some(la)) if la.isDigit => intLiteral(curLexeme + la)(isOctal)
                        case (_, Some(la)) if la.isDigit => intLiteral(curLexeme + la)(isDec)
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