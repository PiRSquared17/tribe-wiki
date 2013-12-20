package cz.gelion.tribewiki.parser

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers
import java.io.Serializable

class TracWikiParser extends RegexParsers {

  override def skipWhitespace = false
  

  
  def word:Parser[Element] = {
    println("word")
    """[\p{L}0-9_]+""".r }^^ {s => TextElement(s)} 
  
  def space:Parser[Element] ={
    println("space")
    """[ \t]+|\r\n(?!(?:[ \t]*+\r\n))++""".r  } ^^ (s => SpaceElement())
 
  
  def punct:Parser[Element] = """[\.,\-:;!?"'`/<>+\(\)\]\[]""".r ^^ {s=>PunctElement(s)}
  
  def nl:Parser[Element] = {
    println("nl")
    """".+""".r} ^^ (s=>SpaceElement()) 
  
    
  
  def par:Parser[Element] = (( word|space|nl|punct)*)  <~ ("""\r\n(?:[ \t]*+\r\n)++""".r) ^^ {s=>ParElement(s)}

  def article = (par*)
  
}
  

trait Element {
  
}


case class TextElement(s:String) extends Element 

case class SpaceElement extends Element 

case class ParElement(l:List[Element]) extends Element

case class PunctElement(s:String) extends Element

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