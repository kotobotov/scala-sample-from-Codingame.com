
val seqWords = "tekst proba naprimer".toUpperCase.split(" ").map { word =>
  word.toCharArray.map(_.toInt).sum / word.size
}
seqWords.map(_.toChar).foreach(println)