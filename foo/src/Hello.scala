package com.example

import scala.util.matching.Regex
import scala.collection.mutable
import scala.io.StdIn


object Hello extends App {

  val line = StdIn.readLine();
  val caption_with_label = raw"""\[(.*?)\]"(.*?)"""".r
  val caption_with_label(label, caption) = line

  val keyword_sentence = raw" *(for |while |if |else|elsif |end|return |let |procedure |function )?(.*?)(//.*)?".r
  val definition_state = raw"(.*?)\((.*?)\)".r
  val block_start = Set("for ", "while ", "if ", "procedure ", "function ")
  var stack = new mutable.Stack[String]()
  println(
raw"""
\begin{algorithm}[H]
\caption{$caption}\label{$label} 
\centering
\begin{algorithmic}[1]
""")
  do {
    val line = StdIn.readLine();
    def commentsCmd(comments: String) = {
      if(comments != "") raw"\Comment {${comments.tail.tail}}" else ""
    }
    var keyword_sentence(key0, state0, comments0) = line
    
    if(key0 == null) key0 = ""
    if(state0 == null) state0 = ""
    if(comments0 == null) comments0 = ""

    (key0, state0, comments0) match{
      case ("" | null, state, comments) => {
        println(raw"\State $$$state$$ ${commentsCmd(comments)}")
      }
      case ("elsif ", state, comments) => {
        println(raw"\ElsIf { $$$state$$ } ${commentsCmd(comments)}")
      }
      case ("else", state, comments) => {
        println(raw"\Else${commentsCmd(comments)}")
      }
      case ("end", state, comments) => {
        val key = stack.pop()
        println(raw"\End$key ${commentsCmd(comments)}")
      }
      case (key @ ("procedure " | "function "), state, comments) => {
        val keyword = key.init.capitalize
        if(block_start.contains(key)){
          stack.push(keyword)
        }
        val definition_state(ident, args) = state

        println(raw"\$keyword { $ident } {$$$args$$} ${commentsCmd(comments)}")
      }
      case ("return ", state, comments) => {
        println(raw"\State \Return $$$state$$ ${commentsCmd(comments)}")
      }
      case ("let ", state, comments) => {
        println(raw"\State \textbf{let} $$$state$$ ${commentsCmd(comments)}")
      }
      case (key, state, comments) => {
        val keyword = key.init.capitalize
        if(block_start.contains(key)){
          stack.push(keyword)
        }
        println(raw"\$keyword { $$$state$$ } ${commentsCmd(comments)}")
      }
      case _ => {
        println("error reading line")
      }
    }
  } while(stack.nonEmpty)
  println(raw"""
\end{algorithmic}
\end{algorithm}
""")
}