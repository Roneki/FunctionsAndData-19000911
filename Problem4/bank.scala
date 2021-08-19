object MyApp extends App{
  val a1 = new Account("283848382v",12345,2000.00)
  val a2 = new Account("574623983v",54683,3000.00)
  val a3 = new Account("589230912v",34981,5000.00)
  val a4 = new Account("789623012v",65738,-6000.00)
  val a5 = new Account("643490124v",23173,4000.00)

  var bank:List[Account]=List(a1,a2,a3,a4,a5)

  //4.1List of Accounts with negative balances
  val overdraft = (b:List[Account])=>b.filter(x=>x.balance<0)
  println(s"Negative accounts :- ${overdraft(bank)}")

  //Total of all account balances
  val balance  = (b:List[Account])=>(b.map(x=>x.balance)).reduce((x,y)=>x+y)
  println(s"\nSum of all account balances = ${balance(bank)}")

  val interest = (b:List[Account])=>b.map((x)=>(x.nic,x.accno,if(x.balance > 0)(x.balance + (x.balance * 0.5))
                                                                      else (x.balance + (x.balance * 0.1))))
  println(s"\nFinal account balances :- ${interest(bank)}")

  class Account(id:String, n:Int, b:Double){
    var nic:String = id
    var accno:Int = n
    var balance:Double = b

    override def toString = s"[${nic} : ${accno} : ${balance}]"
  }
}
