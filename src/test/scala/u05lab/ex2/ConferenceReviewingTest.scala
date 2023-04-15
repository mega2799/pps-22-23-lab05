package u05lab.ex2

import org.junit.{Before, Test, Assert}
import org.junit.Assert.*
import u05lab.ex2.*
class ConferenceReviewingTest:
  val conferenceReviewing = Conference()
  @Before def init() =
    val map: Map[Question, Int] = Map(
      Question.RELEVANCE -> 8,
      Question.SIGNIFICANCE -> 8,
      Question.CONFIDENCE -> 7,
      Question.FINAL -> 8,
    )
    //  conferenceReviewing loadReview()
    conferenceReviewing loadReview(1, 8, 8, 6, 8); // 4.8 è il voto finale pesato (usato da averageWeightedFinalScoreMap)
    conferenceReviewing loadReview(1, 9, 9, 6, 9); // 5.4
    conferenceReviewing loadReview(2, 9, 9, 10, 9); // 9.0
    conferenceReviewing loadReview(2, 4, 6, 10, 6); // 6.0
    conferenceReviewing loadReview(3, 3, 3, 3, 3); // 0.9
    conferenceReviewing loadReview(3, 4, 4, 4, 4); // 1.6
    conferenceReviewing loadReview(4, 6, 6, 6, 6); // 3.6
    conferenceReviewing loadReview(4, 7, 7, 8, 7); // 5.6
    conferenceReviewing loadReview(4, map)
    conferenceReviewing loadReview(5, 6, 6, 6, 10); // 6.0
    conferenceReviewing loadReview(5, 7, 7, 7, 10); // 7.0
    //  println(conferenceReviewing.reviews)
//    conferenceReviewing.reviews foreach (println(_))

  @Test def isPopulatedInit() =
     assertEquals(11,conferenceReviewing.reviews.length)

  @Test def testOrderedScores() =
    // l'articolo 2 ha preso su RELEVANCE i due voti 4,9
    assertEquals(conferenceReviewing orderedScores(2, Question.RELEVANCE), List(4, 9));
    assertEquals(conferenceReviewing orderedScores(4, Question.CONFIDENCE), List(6, 7, 8));
    assertEquals(conferenceReviewing orderedScores(5, Question.FINAL), List(10, 10));

  @Test def testAverageFinalScore() =
    // l'articolo 1 ha preso voto medio su FINAL pari a 8.5, con scarto massimo 0.01
    assertEquals(8.5, conferenceReviewing averageFinalScore(1), 0.01);
    assertEquals(7.5, conferenceReviewing averageFinalScore(2), 0.01);
    assertEquals(3.5, conferenceReviewing averageFinalScore(3), 0.01);
    assertEquals(7.0, conferenceReviewing averageFinalScore(4), 0.01);
    assertEquals(10.0,conferenceReviewing averageFinalScore(5),  0.01);

  @Test def testAcceptedArticles() =
    // solo gli articoli 1,2,4 vanno accettati, avendo media finale >=5 e almeno un voto su RELEVANCE >= 8
    assertEquals(conferenceReviewing.acceptedArticles() ,Set(1, 2, 4));

  @Test def testSortedAcceptedArticles() =
    // articoli accettati, e loro voto finale medio
    assertEquals(conferenceReviewing.sortedAcceptedArticles() , List(Pair(4, 7.0), Pair(2, 7.5), Pair(1, 8.5)));

  @Test def optionalTestAverageWeightedFinalScoreNoMap() =
    assertEquals((4.8 + 5.4) / 2, conferenceReviewing averageWeightedFinalScore(1),  0.01);
  @Test def optionalTestAverageWeightedFinalScore() =
    // l'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1, con scarto massimo 0.01
    assertEquals((4.8 + 5.4) / 2,conferenceReviewing.averageWeightedFinalScoreMap().get(1).get,  0.01);
    assertEquals((9.0 + 6.0) / 2,conferenceReviewing.averageWeightedFinalScoreMap().get(2).get,  0.01);
    assertEquals((0.9 + 1.6) / 2,conferenceReviewing.averageWeightedFinalScoreMap().get(3).get,  0.01);
    assertEquals((3.6 + 5.6 + 5.6) / 3, conferenceReviewing.averageWeightedFinalScoreMap().get(4).get,  0.01);
    assertEquals((6.0 + 7.0) / 2, conferenceReviewing.averageWeightedFinalScoreMap().get(5).get,  0.01);
    assertEquals(5, conferenceReviewing.averageWeightedFinalScoreMap().size);