object ShoppingBasket extends App{
  val items: Array[String] = scala.io.StdIn.readLine().split(" ")
  val basket1 = new Basket()
  basket1.showTotalPrice(items)
}
trait PriceList{
  val prices = Map("Soup" -> 0.65, "Bread" -> 0.80, "Milk" -> 1.30, "Apples" -> 1.00)
  def getPrice(item:String): Double = {
    val price: Double = prices.getOrElse(item, -1)
    return price
  }
}
trait Discounts{
  val discounts = Map("Apples" -> 0.10, "SoupAndBread" -> 0.50)
  def getDiscount(item:String): Double = {
    val discount: Double = discounts.getOrElse(item, -1)
    return discount
  }
}

class Basket() extends PriceList with Discounts{
  var subTotal: Double = 0.00
  def showTotalPrice(items:Array[String]) = {
    for(item <- items){
      var itemPrice = getPrice(item)
      //handle invalid item
      subTotal = subTotal + itemPrice
    }
    println(f"Subtotal: £$subTotal%.2f")
    var discount = applyDiscount(items)
    var finalPrice = subTotal - discount
    println(f"Total price: £$finalPrice%.2f")
  }
  def applyDiscount(items:Array[String]): Double = {
    var noOfApples = items.count(_ == "Apples")
    var noOfSoups = items.count(_ == "Soup")
    var noOfBreads = items.count(_ == "Bread")
    var appleDiscount = noOfApples * getPrice("Apples") * getDiscount("Apples")
    var soupAndBreadDiscount = 0.0
    if((noOfSoups / 2) <= noOfBreads){
      soupAndBreadDiscount = (noOfSoups / 2) * getPrice("Bread") * getDiscount("SoupAndBread")
    }
    else{
      soupAndBreadDiscount = noOfBreads * getPrice("Bread") * getDiscount("SoupAndBread")
    }
    var totalDiscount = appleDiscount + soupAndBreadDiscount
    if(appleDiscount != 0.0){
      println(f"Apple 10%% off: £$appleDiscount%.2f")
    }
    if(soupAndBreadDiscount != 0.0){
      println(f"Bread 50%% off: £$soupAndBreadDiscount%.2f")
    }
    if(totalDiscount == 0.0){
      println("(No offers available)")
    }
    return totalDiscount
  }
}