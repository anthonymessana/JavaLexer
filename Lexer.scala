import java.io.RandomAccessFile
import scala.collection.immutable.*
import scala.Option
import scala.compiletime.ops.float
import scala.compiletime.ops.string

enum Token:
    case Keyword(value: String)
    case Identifier(value: String)
    case Literal(value: String)
    case Operator(value: String)
    case Separator(value: String)
    case Comment(value: String)
    case Invalid
    case EOF
/* 
TODO: 
    Use peek + skipChar instead of nextChar, as nextChar causes separators at the end of literals 
    and identifiers to be skipped in the subsequent call to nextToken.
*/
class Lexer(filename: String):
    val source = RandomAccessFile(filename, "r")
    def index = source.getFilePointer
    def skipChar = source.seek(index + 1)
    def ERROR(message: String = "") = 
        if message.nonEmpty then println(message)
        (Token.Invalid, index)

    private def nextChar = if index < source.length then Some(source.read().toChar) else None
    private def peek = 
        nextChar match
            case Some(value) => source.seek(index - 1); Some(value)
            case None => None
    
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
    
    private def isSeparator(c: Char) = 
        c == '{' || c == '}' || c == ';' || c == ',' ||
        c == '(' || c == ')' || c == '[' || c == ']' ||
        c == '.' || c == ':' || c == '@' 


    val keywords: Set[String] = Set(
    "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const",
    "continue", "default", "do", "double", "else", "enum", "extends", "final", "finally", "float",
    "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long",
    "native", "new", "package", "private", "protected", "public", "return", "short", "static",
    "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient",
    "try", "void", "volatile", "while"
    )

    def identifier(lexeme: String): (Token, Long) =
        nextChar match
            case Some(c) if c.isLetterOrDigit || c == '_' || c == '$' => identifier(lexeme + c)
            case Some(c) if isSeparator(c) || c.isWhitespace => (Token.Identifier(lexeme), index)
            case Some(e) => 
                ERROR("Identifier contains illegal character $e")
            case None => (Token.Identifier(lexeme), index)

    def intLiteral(lexeme: String)(f: Char => Boolean): (Token, Long) = 
        nextChar match
            case Some(c) if f(c) => intLiteral(lexeme + c)(f)
            case Some('l') | Some('L') => skipChar; (Token.Literal(lexeme), index)
            case Some(c) if c.isLetterOrDigit => ERROR()
            case Some('_') =>
                peek match
                    case Some(c) if c.isDigit => intLiteral(lexeme)(f)
                    case Some(_) | None => ERROR() 
            case Some('.') => if f == isDec then floatLiteral(lexeme + '.') else ERROR()
            case Some(c) if isSeparator(c) || isOperator(c) || c.isWhitespace => (Token.Literal(lexeme), index)
            case Some(_) => ERROR()
            case None if lexeme.nonEmpty => (Token.Literal(lexeme), index)
            case None => ERROR()


    def floatLiteral(lexeme: String): (Token, Long) =
        nextChar match
            case Some(c) if c.isDigit => floatLiteral(lexeme + c)
            case Some('F') | Some('f') => skipChar; (Token.Literal(lexeme), index)
            case Some('d') => (Token.Literal(lexeme), index)
            case Some('.') => ERROR()
            case Some(c) if c.isLetter => ERROR()
            case Some(_) => (Token.Literal(lexeme), index)
            case None if lexeme.charAt(lexeme.length-1) == '.' => ERROR()
            case None => (Token.Literal(lexeme), index)

    def stringLiteral(lexeme: String): (Token, Long) = 
        nextChar match
            case Some('"') => (Token.Literal(lexeme), index)
            case Some('\\') => 
                nextChar match
                    case Some(c) => 
                        if isEscapeChar(c) then stringLiteral(lexeme + ("\\" + c).translateEscapes)
                        else stringLiteral(lexeme + c)
                    case None => ERROR("Unclosed string literal")
            case Some(c) => stringLiteral(lexeme + c)
            case None => ERROR("Unclosed string literal")
 
    def operator(lexeme: String): (Token, Long) =
        nextChar match
            case Some(c) 
                if lexeme.length == 1 && isOperator(c) && c == '=' || lexeme.charAt(0) == c && !isSingleOp(c)
                => operator(lexeme + c)
            case Some('=') if lexeme == ">>" || lexeme == "<<" => operator(lexeme + '=')
            case Some(c) if !isOperator(c) => (Token.Operator(lexeme), index)
            case Some(_) => ERROR()
            case None => (Token.Operator(lexeme), index)

    def nextToken(pos: Long): (Token, Long) = 
        source.seek(pos)
        println(source.length)
        println(s"Getting next token starting from positon $pos")
        nextChar match
            case Some(c) =>
                val curLexeme = c.toString
                if c.isWhitespace then
                    nextToken(pos + 1) 
                else if c.isLetter || c == '_' || c == '$' then 
                    identifier(curLexeme) match
                        case (Token.Identifier(id), i) if keywords contains id => (Token.Keyword(id), i)
                        case t => t
                else if c == '0' then
                    peek match
                        case Some('b') => skipChar; intLiteral("")(isBin)
                        case Some('x') => skipChar; intLiteral("")(isHex)
                        case Some(la) if la.isDigit => intLiteral(curLexeme + la)(isOctal)
                        case _ => intLiteral(curLexeme)(isDec)
                else if c.isDigit then
                    intLiteral(curLexeme)(isDec)
                else if c == '.' then
                    floatLiteral(curLexeme)
                else if c == '"' then
                    stringLiteral(curLexeme)
                else if isOperator(c) then
                    operator(curLexeme)
                else if isSeparator(c) then
                    println("SEPARATOR")
                    (c, peek) match
                        case (':', Some(':')) => skipChar; (Token.Separator("::"), index)
                        case _ => (Token.Separator(curLexeme), index)
                else
                    ERROR("Unrecognized lexeme")
            case None => println("ERROR(): EOF"); (Token.EOF, source.length)