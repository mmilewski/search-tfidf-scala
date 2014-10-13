package search

/**
 * @author Marcin Milewski
 * @since 2014-10-12
 */

package object types {
  type Word = String
  type DocumentTitle = String
  type SearchResult = DocumentTitle
}

import search.types.{DocumentTitle, SearchResult, Word}

import scala.collection.mutable.ListBuffer

/**
 * Representation of a document as mapping from a word to number of instances in the document.
 * When you create and ad-hoc vector, which doesn't have document title assigned you can use None
 * as document title.
 */
case class WordVector(private val sentence: Traversable[Word],
                      documentTitle: Option[DocumentTitle]) extends Traversable[(Word, Int)] {
  private val wordCount: Map[Word, Int] = sentence.groupBy(identity).mapValues(_.size)

  val totalWordCount: Int = wordCount map (_._2) sum

  def countOccurrencesOf(word: Word): Int = wordCount.getOrElse(word, 0)

  override def foreach[U](f: ((Word, Int)) => U): Unit = wordCount.foreach(f)
}

case class TfidfVector(private val data: Map[Word, Double],
                       documentTitle: Option[DocumentTitle]) {
  val length: Double = Math.sqrt(data.values.map(a => a * a).sum)

  def apply(word: Word): Double = data.getOrElse(word, 0)

  def commonWordsWith(other: TfidfVector) = data.keySet.intersect(other.data.keySet)
}

class IndexedDocuments(documentsSpace: Seq[WordVector],
                       docsContainingWord: Map[Word, Set[DocumentTitle]]
                        ) {
  val docCount = documentsSpace.size

  /**
   * Think about a corpus as a space where every document is represented as a vector. This space has
   * M dimensions, where M is the number of unique words in all documents. Naturally, the name/label
   * of the vector is the title of the document.
   *
   * We need named vectors because at the end of the calculations we need to show the titles
   * of the documents to the user.
   */
  val documentsAsTfidfSpace: Seq[TfidfVector] = documentsSpace map wordVectorToTfIdfVector toSeq

  def wordVectorToTfIdfVector(wordVector: WordVector): TfidfVector = {
    val data: Map[Word, Double] = wordVector map {
      case (word, _) => (word, tfidf(word, wordVector))
    } toMap

    TfidfVector(data, wordVector.documentTitle)
  }

  def tfidf(word: Word, wordVector: WordVector): Double = {
    if (documentsSpace.isEmpty) {
      0
    }
    else {
      val occurrencesInDoc: Double = wordVector.countOccurrencesOf(word).toDouble
      val tf = occurrencesInDoc / wordVector.totalWordCount
      val numDocsContainingWord = docsContainingWord.getOrElse(word, Seq.empty).size
      if (numDocsContainingWord == 0) {
        0
      } else {
        val idf = docCount / numDocsContainingWord.toDouble
        val tfidf = tf * Math.log(idf)
        tfidf
      }
    }
  }

  /**
   * Calculates cosine similarity between two documents
   */
  def compareWithQuery(vectorFromUser: TfidfVector)(vectorFromCorpus: TfidfVector): (DocumentTitle, Double) = {
    val vec = vectorFromCorpus

    val commonWords = vectorFromUser.commonWordsWith(vec)
    val numerator = commonWords map (word => vectorFromUser(word) * vec(word)) sum
    val denominator = vectorFromUser.length * vec.length
    (vec.documentTitle.get, numerator / denominator)
  }

  /**
   * @param sentence Has to be normalized (e.g. lowercased)
   */
  def search(sentence: Traversable[Word], topN: Int): Seq[SearchResult] = {
    require(topN > 0, s"Top N has to be greater than 0 but was $topN")

    val queryTfidfVector = wordVectorToTfIdfVector(WordVector(sentence, None))
    val scoredDocuments: Seq[(DocumentTitle, Double)] = documentsAsTfidfSpace.map(compareWithQuery(queryTfidfVector))
    scoredDocuments
      .sortWith(_._2 > _._2) // by score descending
      .filterNot(_._2.isNaN)
      .filterNot(_._2 < 0.000001)
      .take(topN)
      .map(_._1) // keep only title
  }

}

object Indexer {

  def indexDocuments(documents: Iterator[(DocumentTitle, List[Word])]): IndexedDocuments = {
    import scala.collection.mutable

    val docToWordVector = new ListBuffer[WordVector]
    val docsContainingWord = new mutable.HashMap[Word, Set[DocumentTitle]]

    documents.foreach {
      case (title, words) =>
        docToWordVector += WordVector(words, Some(title))
        words.foreach { word =>
          val currentDocsWithThisWord = docsContainingWord.getOrElse(word, Set.empty)
          docsContainingWord += word -> (currentDocsWithThisWord + title)
        }
    }

    new IndexedDocuments(
      docToWordVector.toList,
      docsContainingWord.toMap
    )
  }

}


