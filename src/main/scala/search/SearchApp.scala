package search

/**
 * @author Marcin Milewski
 * @since 2014-10-12
 */

import search.types.{DocumentTitle, Word}

object SearchApp extends App {
  require(args.length > 0, "Usage: As an argument you should pass the path to the folder containing documents (program will search through subdirectories")

  val topNResults: Int = 10

  val extractWords: (String) => Seq[String] = s => s.split("\\W+").map(_.toLowerCase).filterNot(_.trim.isEmpty)

  val documentLoader: Iterator[(DocumentTitle, List[Word])] = {
    import java.io.File
    def recursiveListFiles(f: File): Array[File] = {
      // src: http://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala
      val these = f.listFiles
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }

    val files = recursiveListFiles(new File(args.head)).filter(_.isFile)
    println(s"Found ${files.length} documents")

    files zip Stream.from(1) map { case (f, i) =>
      if (i % 200 == 0) println(s"Loaded $i documents so far")
      (f.getName, io.Source.fromFile(f).getLines().flatMap(extractWords).toList)
    } toIterator

    // test data
    //    List(
    //      ("doc1", List("welcome", "to", "scala", "labs")),
    //      ("doc2", List("welcome", "to", "Toronto")),
    //      ("doc3", List("introduce", "scala", "and", "enjoy", "scala")),
    //      ("doc4", List("hello", "scala"))
    //    )
  }

  println("Indexing, please wait...")
  val indexedDocuments = Indexer.indexDocuments(documentLoader)
  println("Indexing... DONE")

  while (true) {
    println()
    println("Enter a sentence or 'q' to quit")
    val input: String = Console.in.readLine()
    if (input == "q")
      System.exit(0)
    println(s"Querying...")
    val topMatches = indexedDocuments.search(extractWords(input), topNResults)
    if (topMatches.isEmpty)
      println("Couldn't find any relevant documents")
    else {
      println(s"Top results:")
      topMatches.foreach(println)
    }
  }
}
