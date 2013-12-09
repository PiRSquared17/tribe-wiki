package cz.gelion.tribewiki.parser

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers
import java.io.Serializable

class TracWikiParser extends RegexParsers {

  override def skipWhitespace = false
  
  def nl:Parser[String] = """\s*(\r\n|\n)""".r ^^ {_.toString}
  
  def ascii ="""[A-Za-z0-9]""".r
  
  def url = (ascii*) ~ "://" ~ (ascii*);
  
  def unicode = """\p{L}""".r
    
  def text = """[\p{L}0-9\s\.]*""".r
    
  def h1 = """=""" ~ ("""\s+""".r?) ~> text <~  ("""\s+""".r?) ~ """=""" ~ nl
  
  def it = """''""" ~> text <~ """''"""

  def inline_element = it | text
  
  def inline_text:Parser[String] = (inline_element ~ inline_text) ^^ {_.toString}
  
  def line_of_text = inline_text ~ nl
  
  def lines_of_text:Parser[String] = (line_of_text  | lines_of_text) ^^ {_.toString}
  
  def paragraph = (h1?) ~ (lines_of_text?) | lines_of_text
  
  
  def article = h1 ~ paragraph
  
}
  


object TracWikiParser extends TracWikiParser with Application {
  
  println("Jedeme...")
  
  
  val t = scala.io.Source.fromFile("page1.wiki").mkString
  
  println("parsing:\n" + t)
  
  println(parseAll(article, t)) 
    
  
  /*
  parseAll(body, t) match {
    case Success(lup,_) => {
      println(lup)
      
    }
    case x => println(x)
  }
  */
  
}