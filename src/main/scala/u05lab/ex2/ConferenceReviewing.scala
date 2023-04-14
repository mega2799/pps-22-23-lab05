package u05lab.ex2

enum Question:
  case RELEVANCE
  case SIGNIFICANCE
  case CONFIDENCE
  case FINAL

trait ConferenceReviewing:

  /**
   * @param article
   * @param scores
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Int, scores : Map[Question, Int]) : Unit

  /**
   * @param article
   * @param relevance
   * @param significance
   * @param confidence
   * @param fin
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article : Int, relevance : Int, significance: Int, confidence : Int, fin : Int) : Unit

  /**
   * @param article
   * @param question
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article : Int, question : Question ) : List[Int]

  /**
   * @param article
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore (article : Int) : Double

  /**
   * An article is considered accept if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles() : Set[Int]


  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles() : List[Pair[Int, Double]]

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   *         Note: this method is optional in this exam
   */

  def averageWeightedFinalScoreMap() : Map[Int, Double]

case class Conference(/*article: Int, scores: Map[Question, Int]*/) extends ConferenceReviewing:
  import Question.*
  var reviews: List[Pair[Int,Map[Question, Int]]] = List()
  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews = reviews :+ Pair(article, scores)

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    reviews = reviews :+ Pair(article , Map(
      RELEVANCE -> relevance,
      SIGNIFICANCE -> significance,
      CONFIDENCE -> confidence,
      FINAL -> fin
    ))

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews filter(_.x == article) map(_.y.getOrElse(question, throw NoSuchFieldError())) sortWith(_<_)
  override def averageFinalScore(article: Int): Double =
    (reviews.filter(_.x == article).map(_.y(FINAL)).sum[Int] : Double) / reviews.filter(_.x == article).length


  override def acceptedArticles(): Set[Int] =
    (reviews filter((p) => averageFinalScore(p.x) > 5.0 && p.y.getOrElse(RELEVANCE, throw NoSuchFieldError()) >= 8) map(_.x)).toSet

  override def sortedAcceptedArticles(): List[Pair[Int, Double]] =
    (acceptedArticles() map(p => Pair(p, averageFinalScore(p)))).toList sortWith(_.y < _.y)

  def averageWeightedFinalScore(index : Int) : Double =
    ((reviews filter(_.x == index) map(p => p.y.getOrElse(FINAL, throw NoSuchFieldError()) * p.y.getOrElse(CONFIDENCE, throw NoSuchFieldError()) / 10.0)).sum / reviews.filter(_.x == index).length)
  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    (reviews map(p => p.x -> averageWeightedFinalScore(p.x))).toMap