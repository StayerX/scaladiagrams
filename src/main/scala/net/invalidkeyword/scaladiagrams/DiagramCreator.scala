package net.invalidkeyword.scaladiagrams

import java.io.File
import org.rogach.scallop._
import java.io.OutputStream
import java.io.OutputStreamWriter

/*

    at scala.collection.mutable.ArrayOps$ofRef.map(ArrayOps.scala:186)
        at net.invalidkeyword.scaladiagrams.DiagramCreator$.getNodesFromFiles(DiagramCreator.scala:48)
        at net.invalidkeyword.scaladiagrams.DiagramCreator$.selectNodes(DiagramCreator.scala:23)
        at net.invalidkeyword.scaladiagrams.DiagramCreator$.main(DiagramCreator.scala:11)
        at net.invalidkeyword.scaladiagrams.DiagramCreator.main(DiagramCreator.scala)

 */
object DiagramCreator { 
  
  def main(args:Array[String]) = {
    outputDot(selectNodes(args))
  }
  
  def selectNodes(args : Array[String]) : Iterable[TYPE]= {
    object Config extends ScallopConf(args) {
      val extension = opt[String]("extension", default=Some(".scala"))
      val source = opt[String]("source", default=Some("."), descr = "location of source files")
      val linked = opt[Boolean]("linked", descr = "only output types that extend other types")
      val parent = opt[String]("parent", descr = "only output parents of a particular class")
    }
    
    val files = new InputFinder().files(Config.source(),Config.extension())
    System.out.println("Prior1")
    val allNodes = getNodesFromFiles(files)
    System.out.println("Posterior1")
    val ns = new NodeSelector(allNodes)
    
    if(!Config.parent.isEmpty)
      parentNodes(ns,Config.parent())
    else if(Config.linked())
      ns.nodesWithParentsOrChildren
    else
      allNodes
  }
  
  def outputDot(nodes : Iterable[TYPE]) = {
    println ("digraph diagram {")
    nodes.foreach(println _)
    println ("}")  
  }
  
  def fileToString(file : File) = {scala.io.Source.fromFile(file).mkString}
  
  def parseFile(file : File) = {
    val result = ScalaSourceParser.run(fileToString(file))
    ScalaSourceParser.filter(result.get)
  }
  
  def getNodesFromFiles(files: Array[File]) = {
    System.out.println("Prior2")
    files.map(parseFile(_)).flatten.toList
  }
  
  def parentNodes(ns : NodeSelector, name : String) : Iterable[TYPE] = {
    val root = ns.findNode(name)
    if(root.isDefined) ns.selectChildNodes(root.get)
    else List[TYPE]()
  }
}