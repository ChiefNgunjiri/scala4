package repls

class Bodmas(){
    val ranking: Array[String] = Array[String]("","","^","/", "*", "+", "-")
}

class rankStack(rankList: List[Int]) {

    def this() {
        this(List())
    }

    def this(rankList: List[Int], nextOp: Int){
        this(rankList :+ nextOp)
    }

    val orderedList: List[Int] =  rankList

    //val bracketActive = inBracket

    def getRankLast(): Int = {

        return orderedList(orderedList.size - 1)


    }
    def getRankList(): List[Int] = {
        return orderedList
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


                if (rank == 0 || rankStack.orderedList.size == 0){ rankStack = new rankStack(rankStack.getRankList(), rank); println("adding" + rank);println(rankStack.orderedList); }
                else if (rank == 1){
                    println(rankStack.orderedList)
                    println("running")

                    while(rankStack.getRankLast() != 0){
                        println(println(rankStack.getRankLast()))
                        postFixString = new PostFix(postFixString.orderedList,bodmasList.ranking(rankStack.getRankLast()))
                        rankStack = new rankStack(rankStack.getRankList().dropRight(1))
                    }
                    rankStack = new rankStack(rankStack.getRankList().dropRight(1))

                }
                else if(rank < rankStack.getRankLast()) postFixString = new PostFix(postFixString.getPostFix(), inputArray(i))
                else {

                    rankStack = new rankStack(rankStack.getRankList(),rank)
                }
                println("ranklist is " + rankStack.orderedList)
            }
        }

        while (rankStack.orderedList.size != 0){
            postFixString =  new PostFix(postFixString.getPostFix(),bodmasList.ranking(rankStack.getRankLast()))
            rankStack = new rankStack(rankStack.getRankList().dropRight(1))
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
