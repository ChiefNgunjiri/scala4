package repls

import scala.collection.mutable.Stack

class Bodmas(){
    val ranking: Array[String] = Array[String]("","","^","/", "*", "+", "-")
}

class CalcStack(stackCal: Stack[String]){

    def this(){this(Stack())}

    def this(stackCal: Stack[String], pushValue: String){this(stackCal.push(pushValue))}

    val orderedStack = stackCal;



}

class rankStack(rankList: List[Int]) {
    def this() {this(List())}

    def this(rankList: List[Int], nextOp: Int){this(rankList :+ nextOp)}

    val orderedList: List[Int] =  rankList

    def getRankLast(): Int = {return orderedList(orderedList.size - 1)}

    def getRankList(): List[Int] = {return orderedList}
}

class PostFix(preList: List[String]){

    def this(){this(List())}

    def this(preList: List[String], element: String){this(preList :+ element)}

    val orderedList: List[String] = preList

    def getPostFix(): List[String] ={return orderedList}
}


class IntREPL extends REPLBase {
    // Have a REPL of type Int
    type Base = Int
    override val replName: String = "" // TODO: name me!

    override def readEval(command: String): String = {
        val elements = command.split(" ") // split string based on whitespace

        println(elements.mkString(" "))
        println(postFix(elements))

        return polishCalc(postFix(elements))

    }

    def postFix(inputArray: Array[String] ): List[String] ={
        val bodmasList = new Bodmas()
        var postFixString = new PostFix()
        var rankStack = new rankStack()

        for(i <- 0 until inputArray.size) {

            if (isDouble(inputArray(i))) postFixString = new PostFix(postFixString.getPostFix(), inputArray(i))

            else{
                val rank = bodmas(inputArray(i))

                if (rank == 0 || rankStack.orderedList.size == 0) rankStack = new rankStack(rankStack.getRankList(), rank)

                else if (rank == 1){
                    while(rankStack.getRankLast() != 0){
                        postFixString = new PostFix(postFixString.orderedList,bodmasList.ranking(rankStack.getRankLast()))
                        rankStack = new rankStack(rankStack.getRankList().dropRight(1))
                    }
                    rankStack = new rankStack(rankStack.getRankList().dropRight(1))
                }

                else if(rank < rankStack.getRankLast()) postFixString = new PostFix(postFixString.getPostFix(), inputArray(i))

                else {
                    rankStack = new rankStack(rankStack.getRankList(),rank)
                }
            }
        }

        while (rankStack.orderedList.size != 0){

            postFixString =  new PostFix(postFixString.getPostFix(),bodmasList.ranking(rankStack.getRankLast()))
            rankStack = new rankStack(rankStack.getRankList().dropRight(1))
        }
        return postFixString.getPostFix()
    }

    def polishCalc(polishList: List[String]): String ={

        var polishStack = new CalcStack(listToStack(polishList))
        var finalString =""
        println(polishStack.orderedStack)

        while(polishStack.orderedStack.size != 0){

            val popVal1 = polishStack.orderedStack.pop()
            polishStack = new CalcStack(polishStack.orderedStack.dropRight(1))
            if (!polishStack.orderedStack.isEmpty){
                val popVal2 = polishStack.orderedStack.pop()
                polishStack = new CalcStack(polishStack.orderedStack.dropRight(1))
                if (!polishStack.orderedStack.isEmpty) {

                    val popVal3 = polishStack.orderedStack.pop()
                    polishStack = new CalcStack(polishStack.orderedStack.dropRight(1))

                    if(bodmas(popVal3) != 10){
                        println("enterning calulation")
                        println("solution" + operations(popVal3, popVal1, popVal1))
                        polishStack = new CalcStack(polishStack.orderedStack, operations(popVal3, popVal1, popVal1))
                    }

                }
                else {
                    println("second level")
                    finalString += (popVal1 + " " + popVal2)
                }
                }
            else finalString += popVal1

            println(finalString)

        }

        return polishStack.orderedStack.mkString(" ")
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
            case _ => 10
        }
    }

    def listToStack(list: List[String]): Stack[String] ={
        var stackList = new CalcStack()
        println("list size is " + list.size)

        for(i <-((list.size- 1) to 0 by -1)){
            println(stackList.orderedStack + " adiing " + list(i))
            stackList = new CalcStack(stackList.orderedStack, list(i))
            println(stackList.orderedStack + " added " + list(i))
        }
        return stackList.orderedStack
    }

    def operations(operand: String, op1: String, op2: String): String ={

        if (isDouble(op1) && isDouble(op2)){
            if (operand.equals("+"))return (op1.toInt + op2.toInt).toString
            else if (operand.equals("-"))return (op1.toInt - op2.toInt).toString
            else if (operand.equals("/"))return (op1.toInt / op2.toInt).toString
            else if (operand.equals("/"))return (op1.toInt / op2.toInt).toString
            else  return (op1.toInt / op2.toInt).toString
        }
        else return (op1 + " " + operand + " "+ op2 )
    }

    // TODO: Implement any further functions that are specifically for an IntREPL
}
