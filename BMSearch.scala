object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val reversePattern = "ドワンゴ".toSeq.reverse
  val skipCountMap = reversePattern.distinct.zipWithIndex.map(e => e._1 -> (if (e._2 == 0) 1 else e._2)).toMap
  println(s"reversePattern: ${reversePattern}, skipCountMap: ${skipCountMap}")

  private def getSkipCountOrNoneIfMatches(slicedTextChars: Seq[Char]): Option[Int] = {
    val skipCount = reversePattern.reverse.zip(slicedTextChars)
      .filter(e => e._1 != e._2)
      .headOption
      .map(e => skipCountMap.getOrElse(e._1, reversePattern.length))
      println(s"slicedTextChars: ${slicedTextChars}, skipCount: ${skipCount}")
      skipCount
  }

  private def loop(index: Int, acc: Seq[Int]): Seq[Int] = {
    val remainTextChars = text.drop(index)
    println(s"index: ${index}, acc: ${acc}, remainTextChars: ${remainTextChars}")

    if (remainTextChars.length < reversePattern.length) acc
    else getSkipCountOrNoneIfMatches(remainTextChars.take(reversePattern.length)) match {
      case Some(i) => loop(index + i, acc)
      //case None => loop(index + reversePattern.length, index +: acc)
      case None => loop(index + 1, index +: acc)
    }
  }

  loop(0, Nil)
}
