//Problem 3 - Transferring money from an account to another

object MyApp extends App{
  val a1 = new Account("997850696v", 13835, 2000.56)
  val a2 = new Account("564858348v",47436,3000.00)
  a1.transfer(a2,1000.00)
  println(a1)
  println(a2)
  class Account(id:String, n:Int, b:Double){
    var nic:String = id
    var accno:Int = n
    var balance:Double = b

    def withdraw(a:Double)={
      this.balance = this.balance - a
    }

    def deposit(a:Double)={
      this.balance = this.balance + a
    }
    def transfer(a:Account,b:Double)={
      this.withdraw(b)
      a.deposit(b)
    }
    override def toString = s"[${nic} : ${accno} : ${balance}]"

  }
}
