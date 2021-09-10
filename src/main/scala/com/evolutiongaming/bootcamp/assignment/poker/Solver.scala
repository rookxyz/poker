package com.evolutiongaming.bootcamp.assignment.poker


object Solver {
  type Card = (Char, Char)

  abstract class PokerGame(hands: List[String]) {
    def splitCards(cards: String): List[Card] = {
      def splitter(cards: List[Char], accum: List[Card]): List[Card] = {
        cards match {
          case x :: y :: xs => splitter(xs, List((x,y)) ::: accum)
          case x :: Nil =>
          {
            print("Invalid input") //TODO: need to refine this scenario
            accum
          }
          case Nil => accum
          case _ => accum
        }
      }
      val acc: List[Card] = Nil
      if (!cards.isEmpty) splitter(cards.toList, acc)
      else acc
    }

    def getCombinations(t: List[Card], h: List[Card], n: Int): List[Set[Card]] = (t ::: h).combinations(n).map(x => x.toSet).toList

    def playerCombinations(board: String, hands: List[String]): List[List[Set[Card]]] = {
      val t = splitCards(board.toLowerCase)
      hands.map(h => getCombinations(t, splitCards(h.toLowerCase), 5))
    }

    def getCombinationType(hand: Set[Card]): (Int, List[Int]) = {
      var cards = Array.ofDim[Int](4, 14)
      val rankMap = Map("b"->0, "2"->1, "3"->2, "4"->3, "5"->4, "6"->5, "7"->6, "8"->7, "9"->8, "t"->9, "j"->10, "q"->11, "k"->12, "a"->13)
      val suitMap = Map("d"->0, "c"-> 1, "h"->2, "s"->3)

      def fillCardsList(c: Card): Unit = {
        val i = suitMap.getOrElse(c._2.toString,0)
        val j = rankMap.getOrElse(c._1.toString,0)
        cards(i)(j) = 1
        if (c._1 == 'a') {
          fillCardsList(('b', c._2))
        }
      }
      def seqCounter(l: List[Int], accum: Int):Int ={
        l match {
          case x :: y :: xs => if(x == y) {seqCounter(y::xs, accum+1)} else accum
          case _ => accum
        }
      }

      def seqProcessor(l: List[Int]): List[Int] = {
        if(!l.isEmpty){
          if (l.head > 0) { List(seqCounter(l, 1)) ::: seqProcessor(l.tail)}
          else
            List(0) ::: seqProcessor(l.tail)
        }
        else Nil

      }

      def getColumnSum(cards: Array[Array[Int]], c: Int): Int = {
        assert(cards.length>0)
        assert(cards(0).length > c)
        val s = for {
          i <- 0 until cards.length
        } yield cards(i)(c)
        s.sum
      }
      def getMaxRowSum(cards: Array[Array[Int]]): Int = {
        val s = for { i <- 0 until cards.length} yield cards(i).toList.tail.sum
        s.max
      }

      def getOfaKind(): List[Int] = {
        // 0 until 14 foreach{i => println(getColumnSum(cards, i))}
        for { i <- (0 until 14).toList} yield getColumnSum(cards, i)


      }
      //fill the cards 2D array
      hand.map(x => fillCardsList(x))


      def getXofaKindIdx(l: List[Int], n: Int): List[Int] = l.zipWithIndex.filter(pair => pair._1 == n).map(pair => pair._2)
      def getPairsIdx(l:List[Int]):List[Int] = getXofaKindIdx(l, 2).filter(x => x != 0) //filter != 0 to remove ace with value 1 //getXofaKindIdx(l, 2)
      def getThreeOfaKindIdx(l:List[Int]):List[Int] = getXofaKindIdx(l, 3).filter(x => x != 0)
      def getFourOfaKindIdx(l:List[Int]):List[Int] = getXofaKindIdx(l, 4).filter(x => x != 0)
      def getStraightIdx(l:List[Int]):List[Int] = l.zipWithIndex.filter(pair => pair._1 == 5).map(pair => pair._2)
      def getHighCardIdx(l:List[Int]):List[Int] = l.zipWithIndex.filter(pair => pair._1 > 0).filter(x => x._2 != 0).map(x => x._2) //.maxBy(pair => pair._2)._2

      def getHasStraight(sIdx: List[Int]): Boolean = !sIdx.isEmpty
      def getHasFlush(c: Array[Array[Int]]): Boolean = getMaxRowSum(c) == 5
      def getHasStraightFlush(s: List[Int], c: Array[Array[Int]]): Boolean = getHasStraight(s) & getHasFlush(c)
      def getHasFourOfaKind(f:List[Int]): Boolean = !f.isEmpty
      def getHasThreeOfaKind(t:List[Int]): Boolean = !t.isEmpty
      def getHasTwoOfaKind(t:List[Int]): Boolean = !t.isEmpty
      def getHasFullHouse(th:List[Int], tw:List[Int]): Boolean = getHasTwoOfaKind(tw) & getHasThreeOfaKind(th)
      def getHasTwoPairs(t:List[Int]): Boolean = t.length  == 2

      val k = getOfaKind()  //fill list of cards of each rank
      val s = seqProcessor(k) //fill sequencial cards list


      def nextCombinationCheck(comb: Int): (Int, List[Int]) = {
        comb match {
          case 23 => {
            val sIdx = getStraightIdx(s)
            if (getHasFlush(cards) & getHasStraight(sIdx)) (23, sIdx)
            else nextCombinationCheck(19)
          }
          case 19 => {
            val fkIdx = getFourOfaKindIdx(k)
            if(getHasFourOfaKind(fkIdx)) (19, getHighCardIdx(k)) //fkIdx:::getHighCardIdx(k))
            else nextCombinationCheck(17)
          }
          case 17 => {
            val thIdx = getThreeOfaKindIdx(k)
            val twIdx = getPairsIdx(k)
            if (getHasFullHouse(thIdx, twIdx))  (17, getHighCardIdx(k)) //List(thIdx(0)*100 + twIdx(0))) //(17, List(thIdx(0), twIdx(0)))
            else nextCombinationCheck(13)
          }
          case 13 => {
            if (getHasFlush(cards)) (13, getHighCardIdx(k))
            else nextCombinationCheck(11)
          }
          case 11 => {
            val sIdx = getStraightIdx(s)
            if (getHasStraight(sIdx)) {
              //   println(s)
              //   for {i<- 0 until cards.length} {
              //     for{j<- 0 until 14} print(cards(i)(j))
              //     println()
              //   }

              (11, sIdx)}
            else nextCombinationCheck(7)

          }
          case 7 => {
            val thIdx = getThreeOfaKindIdx(k)
            if (getHasThreeOfaKind(thIdx)) (7, getHighCardIdx(k):::thIdx) //thIdx:::getHighCardIdx(k))
            else nextCombinationCheck(5)
          }
          case 5 => {
            val twIdx = getPairsIdx(k)
            if (getHasTwoPairs(twIdx)) (5, getHighCardIdx(k):::List(twIdx.sum)) //List(twIdx.sum, getHighCardIdx(k))) //Todo need to verify this works
            else nextCombinationCheck(3)
          }
          case 3 => {
            val twIdx = getPairsIdx(k)
            if (getHasTwoOfaKind(twIdx)) (3, getHighCardIdx(k):::twIdx) //twIdx:::getHighCardIdx(k))
            else nextCombinationCheck(1)
          }
          case 1 => {
            (1, getHighCardIdx(k))
          }
        }
      }
      nextCombinationCheck(23)
    }
    def sortHandByPower(p1: (String, (Set[Card], (Int, List[Int]))), p2:(String, (Set[Card], (Int, List[Int])))): Boolean = {
      val result = p2._2._2._1 - p1._2._2._1
      if (result > 0) {true}
      else if(result < 0) {false}
      // else
      //   p1._2._2._2.sum < p2._2._2._2.sum
      else {

        val pp1 = p1._2._2._2.zipWithIndex
        val pp2 = p2._2._2._2.zipWithIndex
        // println(pp1.map(x => x._1*scala.math.pow(10,x._2)).sum)
        // println(p1)
        // println(pp2.map(x => x._1*scala.math.pow(10,x._2)).sum)
        // println(p2)
        pp1.map(x => x._1*scala.math.pow(10,x._2)).sum < pp2.map(x => x._1*scala.math.pow(10,x._2)).sum
        // p1._2._2.sum < p2._2._2.sum
      }
    }
    def sortCombinationsByPower(p1: (Set[Card], (Int, List[Int])), p2:(Set[Card], (Int, List[Int]))): Boolean = {
      val result = p2._2._1 - p1._2._1
      if (result > 0) {true}
      else if(result < 0) {false}
      else {

        val pp1 = p1._2._2.zipWithIndex
        val pp2 = p2._2._2.zipWithIndex
        // println(pp1.map(x => x._1*10^x._2).sum)
        pp1.map(x => x._1*scala.math.pow(10,x._2)).sum < pp2.map(x => x._1*scala.math.pow(10,x._2)).sum
        // p1._2._2.sum < p2._2._2.sum
      }
    }

    def printHands(h: List[(String, (Set[Card], (Int, List[Int])))]): String = {
      h match {
        case x :: y :: xs => {x._1 + {if((x._2._2._1 == y._2._2._1) & (x._2._2._2.zipWithIndex.map(x => x._1*scala.math.pow(10,x._2)).sum == y._2._2._2.zipWithIndex.map(x => x._1*scala.math.pow(10,x._2)).sum)) "=" else " "} + printHands(y :: xs)}
        case x :: Nil => x._1
        case Nil => ""
      }
    }
  }
  class TexasHoldemGame(board: String, hands: List[String]) extends PokerGame(hands){
    val combPower = playerCombinations(board, hands) //For all hands get all combinations
    val combined = hands zip combPower
    val playerBestCombination = for ((hand, combinations) <- combined) yield
      {
        val pwr = combinations map (x => (x, getCombinationType(x)))
        (hand, pwr.sortWith((p1, p2) => sortCombinationsByPower(p1, p2)).last)
      }

    // val handOrder = playerBestCombination.sortBy(_._1).sortWith((p1, p2) => sortHandByPower(p1, p2))
    // // println(handOrder)
    // printHands(playerBestCombination.sortBy(x => x._1).sortWith((p1, p2) => sortHandByPower(p1, p2)))
    def printOutput(): String = printHands(playerBestCombination.sortBy(x => x._1).sortWith((p1, p2) => sortHandByPower(p1, p2)))

  }
  class FiveCardDrawGame(hands: List[String]) extends PokerGame(hands){
    val combPower = playerCombinations("", hands) //For all hands get all combinations
    val combined = hands zip combPower
    val playerBestCombination = for ((hand, combinations) <- combined) yield
      {
        val pwr = combinations map (x => (x, getCombinationType(x)))
        (hand, pwr.sortWith((p1, p2) => sortCombinationsByPower(p1, p2)).last)
      }

    // val handOrder = playerBestCombination.sortBy(_._1).sortWith((p1, p2) => sortHandByPower(p1, p2))
    // println(handOrder)
    // printHands(playerBestCombination.sortBy(x => x._1).sortWith((p1, p2) => sortHandByPower(p1, p2)))
    def printOutput(): String = printHands(playerBestCombination.sortBy(x => x._1).sortWith((p1, p2) => sortHandByPower(p1, p2)))

  }
  class OmahaHoldemGame(board: String, hands: List[String]) extends PokerGame(hands){

    override def getCombinations(t: List[Card], h: List[Card], n: Int): List[Set[Card]] = {
      val tt = t.combinations(3).toList
      val hh = h.combinations(2).toList

      val f = tt.map(x => for{y <- hh} yield (x:::y).toSet)

      f.flatten

    }

    // def playerCombinations(board: String, hands: List[String]): List[List[Set[Card]]] = {
    //   val t = splitCards(board.toLowerCase)
    //   hands.map(h => getCombinations(t, splitCards(h.toLowerCase), 5))
    // }

    val combPower = playerCombinations(board, hands) //For all hands get all combinations
    val combined = hands zip combPower
    val playerBestCombination = for ((hand, combinations) <- combined) yield
      {
        val pwr = combinations map (x => (x, getCombinationType(x)))
        // println(pwr)
        // println(pwr.sortWith((p1, p2) => sortCombinationsByPower(p1, p2)).last)
        // (hand, pwr.filter(x => x._2._1 == pwr.maxBy(_._2._1)._2._1).map (x => (x._1, (x._2._1, x._2._2))).maxBy(x => x._2._2.sum))
        (hand, pwr.sortWith((p1, p2) => sortCombinationsByPower(p1, p2)).last)
      }
    // print(playerBestCombination.sortBy(_._1))
    // printHands(if (playerBestCombination.length>1)
    // playerBestCombination.sortBy(x => x._1).sortWith((p1, p2) => sortHandByPower(p1, p2))
    // else playerBestCombination)
    def printOutput(): String = printHands(playerBestCombination.sortBy(x => x._1).sortWith((p1, p2) => sortHandByPower(p1, p2)))

  }
  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.toLowerCase.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands   => {val g= new TexasHoldemGame(board, hands); g.printOutput()}
      case "omaha-holdem" :: board :: hands   => {
          val g = new OmahaHoldemGame(board, hands); g.printOutput()
        }
      case "five-card-draw" :: hands          => {val g = new FiveCardDrawGame(hands); g.printOutput()}
      case x :: _                             => ErrorPrefix + "Unrecognized game type"
      case _                                  => ErrorPrefix + "Invalid input"
    }
  }
}
