package repls

class Bodmas(){
    val ranking: Array[String] = Array[String]("","","^","/", "*", "+", "-")
}

class rankStack(rankList: List[Int], bracketLevel: Int) {

    def this() {
        this(rankStack.initList, 0)
    }

    def this(rankList: List[Int], nextOp: Int, bracketLevel: Int){
        this(rankList :+ nextOp, bracketLevel)
    }

    val orderedList: List[Int] =  rankList
    val currentBL = bracketLevel
    //val bracketActive = inBracket

    def getRankLast(): Int = {
        if(orderedList.size > 0) return orderedList(orderedList.size - 1)
        else return 10
    }
    def getRankList(): List[Int] = {
        if (orderedList.size > 0) return orderedList
        else return List(10)
    }

}

object rankStack{
   val initList = List()
}

class PostFix(preList: List[String]){


    def this(){
        this(PostFix.init)

    }

    def this(preList: List[String], element: String){
       this(preList :+ element)
    }

    val orderedList: List[String] = preList

    def getPostFix(): List[String] ={
        return orderedList
    }


}

object PostFix {
    val init = List()
}

class IntREPL extends REPLBase {
    // Have a REPL of type Int
    type Base = Int
    override val replName: String = "" // TODO: name me!

    override def readEval(command: String): String = {
        val elements = command.split(" ") // split string based on whitespace

        val operators: Map[String, (Int, Int) => Int] = {
            Map(("+", _ + _),
                ("-", _ - _),
                ("*", _ * _),
                    ("/", _ / _))
        }

        println(elements.mkString(" "))
        return postFix(elements)


    }

    def postFix(inputArray: Array[String] ): String ={
        val bodmasList = new Bodmas()
        var postFixString = new PostFix()


        var rankStack = new rankStack()
        for(i <- 0 until inputArray.size) {

            if (isDouble(inputArray(i))) postFixString = new PostFix(postFixString.getPostFix(), inputArray(i))

            else{

                val rank = bodmas(inputArray(i))

                 if (rank == 0 || rankStack.orderedList.isEmpty) rankStack = new rankStack(rankStack.getRankList(),rank, bracketLevel(rankStack.currentBL, rank)._1)

                else if (rank == 1){
                    while(rankStack.getRankLast() != 0){
                        println(println(rankStack.getRankLast()))
                        postFixString = new PostFix(postFixString.orderedList,rankStack.getRankLast().toString)
                        rankStack = new rankStack(rankStack.getRankList().dropRight(1),bracketLevel(rankStack.currentBL, rank)._1)
                    }
                     rankStack = new rankStack(rankStack.getRankList().dropRight(1),bracketLevel(rankStack.currentBL, rank)._1)
                }
<<<<<<< HEAD
                else if(rank < rankStack.getRankLast()) postFixString = new PostFix(postFixString.getPostFix(), inputArray(i))
=======

                else if(rank <= rankStack.getRankLast()) postFixString = new PostFix(postFixString.getPostFix(), inputArray(i))
>>>>>>> linux
                else {
                    //postFixString = new PostFix(postFixString.getPostFix(),bodmasList.ranking(rank - 1))
                    rankStack = new rankStack(rankStack.getRankList().dropRight(1),rank,rankStack.currentBL)
                }
            }
        }

        while (rankStack.getRankLast() != 10){
            postFixString =  new PostFix(postFixString.getPostFix(),bodmasList.ranking(rankStack.getRankLast()))
            rankStack = new rankStack(rankStack.getRankList().dropRight(1),bracketLevel(rankStack.currentBL, rankStack.getRankLast())._1)
        }
        return postFixString.getPostFix().mkString(" ")

    }

    def isDouble(num: String): Boolean ={
        try{Some(num.toDouble);return true;}
        catch {case _ => return false }
    }

    def bodmas(rank: String): Int ={
          rank match {
              case "(" => 0
              case ")" => 1
              case "^" => 2
              case "/" => 3
              case "*" => 4
              case "+" => 5
              case "-" => 6
              case _ => 0
          }
    }

    def bracketLevel(level: Int, rank: Int): Tuple2[Int, Boolean] = {
        if (rank == 0) return (level + 1, true)
        else if (rank == 1) {
            if (level > 0) return  (level - 1, true)
            else return ((level- 1), false)
        }
        else return  (level,false )
    }

    // TODO: Implement any further functions that are specifically for an IntREPL
}
