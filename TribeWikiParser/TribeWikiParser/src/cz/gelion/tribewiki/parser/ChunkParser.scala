package cz.gelion.tribewiki.parser

import scala.util.parsing.combinator.RegexParsers


/**
 * http://tristanjuricek.com/knockoff/docs/src/main/scala/com/tristanhunt/knockoff/MarkdownParsing.scala.html
 * 
 */
class ChunkParser extends RegexParsers {

	override def skipWhitespace = false
	
	def text = textBlockWithBreak | textBlock | emptyLines | emptySpace
	
	def textLine : Parser[ Chunk ] =
    	"""[\t ]*\S[^\n]*\n?""".r ^^ { str => TextChunk(str) }
  
	
	def emptyLines : Parser[ Chunk ] =
		rep1( emptyLine ) ^^ ( str => NestedChunk(str)  )
	
	
	def emptySpace : Parser[ Chunk ] =
    	"""[\t ]*""".r ^^ ( str => TextChunk(str) )
		
	def emptyLine : Parser[ Chunk ] =
    	"""[\t ]*\r?\n""".r ^^ ( str => TextChunk( str ) )	
		
	def textBlock : Parser[ Chunk ] =
		rep1( textLine ) ^^ { seq => NestedChunk(seq)  }
	
	def textBlockWithBreak : Parser[ Chunk ] =
		rep( textLineWithEnd ) ~ hardBreakTextLine ^^ { case seq ~ break => NestedChunk(seq ::: List(break))  }
	
	def textLineWithEnd : Parser[Chunk] =
		"""[\t ]*\S[^\n]*[^ \n][ ]?\n""".r ^^ { str => TextChunk(str) }

	 def hardBreakTextLine : Parser[Chunk] =
		"""[\t ]*\S[^\n]*[ ]{2}\n""".r ^^ { s => TextChunk(s) }
	
}


object ChunkParser extends ChunkParser with Application {
  println(parseAll(text, 
"""dafafaf
      jedna dva tøi
      dafadf       
      UUUU
      
aa"""))      
      
      
}



case class TextChunk(s:String) extends Chunk {
  
}

case class NestedChunk(l:List[Chunk]) extends Chunk {
  
}

trait Chunk {
  
  
}