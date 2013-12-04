package cz.gelion.tribewiki.parser

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

class TracWikiParser extends RegexParsers {

  def beginLine: Parser[String] = """^""".r ^^ {
    case s => {
      println("beginLine: " + s)
      s
    }
  }
  
  def endLine: Parser[String] = """(.*)$""".r
  
  def text : Parser[String] = """.*""".r
  
  def h1 = (beginLine ~ "=" ~ text ~ "=")  
  
  
  def it : Parser[String] = """''.*''""".r ^^ {
    case s =>  {
      println(String.format("italic:'%s'", s))
      s
    }
  } 
  
  
  def body = (h1 | it | text)*
  
  
}


object TracWikiParser extends TracWikiParser with Application {
  
  println("Jedeme...")
  
  
  val t = scala.io.Source.fromFile("page1.wiki").mkString
  
  println("parsing:\n" + t)
  
  parseAll(body, t) match {
    case Success(lup,_) => {
      println(lup)
      
    }
    case x => println(x)
  }
  
  
}