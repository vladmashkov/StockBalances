import scala.io.Source._
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
trait Stock {
  def CargoReception(id: String): List[String]
  def RegularCargoWeight: Double
  def ValuableCargoWeight: Double
  def TotalWeight: Double
}
class StockBalances extends Stock {
  def CargoReception(id: String): List[String] = {
    val readId = Future {
      fromFile("stock.txt").getLines().map { line =>
        line.split(";").zipWithIndex.map { case (weight, id) => id -> weight }.toMap.get(0)
      }.toList.flatten
    }
     val readWeight = Future {
       fromFile("stock.txt").getLines().map { line =>
         line.split(";").zipWithIndex.map { case (weight, id) => id -> weight }.toMap.get(1)
       }.toList.flatten
     }
    val Id = Await.result(readId, 5.second)
    val Weight = Await.result(readWeight, 5.second)
    (Id zip Weight).groupBy(_._1).mapValues(_.map(_._2)).toMap.get(id).toList.flatten
  }
  def RegularCargoWeight: Double = {
    CargoReception("1").map(weight => weight.toDouble * 1.1).sum
  }
  def FinalPacking(weight: Double): Double = {
    weight * Math.pow(1.1, weight.toInt / 5)
  }
  def ValuableCargoWeight: Double = {
    CargoReception("2").map(weight => ValuableCargoPacking(weight.toDouble * 1.2, FinalPacking(weight.toDouble * 1.2))).sum
  }
  def ValuableCargoPacking(mainPacking: Double, finalPacking: Double): Double = {
    if ( Rechecking(mainPacking, finalPacking) == 0) finalPacking
    else ValuableCargoPacking(finalPacking, finalPacking * Math.pow(1.1, Rechecking(mainPacking, finalPacking)))
  }
  def Rechecking (mainPacking: Double, finalPacking: Double): Int = {
    finalPacking.toInt / 5 - mainPacking.toInt / 5
  }
  def TotalWeight: Double = {
    RegularCargoWeight + ValuableCargoWeight
  }
}
object Main extends App {
  def Info: String = {
    scala.io.StdIn.readLine("Нажмите цифру 1 для получения веса обычного груза\nНажмите цифру 2 для получения веса ценного груза\nНажмите цифру 3 для получения веса всего груза\nНажмите цифру 4 для выхода\n")
  }
  val Stock = new StockBalances
  def Menu: Any = Info match {
    case "1" => print("Вес обычного груза: ") -> println(Math.round(Stock.RegularCargoWeight)) -> Menu
    case "2" => print("Вес ценного груза: ") -> println(Math.round(Stock.ValuableCargoWeight)) -> Menu
    case "3" => print("Вес всего груза: ") -> println(Math.round(Stock.TotalWeight)) -> Menu
    case "4" => System.exit(0)
    case _ => println("Команда не распознана! Повторите попытку!") -> Menu
  }
  Menu
}



