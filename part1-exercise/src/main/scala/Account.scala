import exceptions.NoSufficientFundsException
import exceptions.IllegalAmountException

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {

  var balance: Double = initialBalance

  def withdraw(amount: Double): Unit = {
    // this.synchronized locks this object so that other threads cannot access it
    this.synchronized {
      if (amount < 0) throw new IllegalAmountException(s"Cannot withdraw negative amount $amount")
      else if (this.balance - amount < 0) {
        throw new NoSufficientFundsException(s"Balance is $this.balance")
      } else {
        this.balance -= amount
      }
    }
  }

  def deposit(amount: Double): Unit = {
    // this.synchronized locks this object so that other threads cannot access it
    this.synchronized {
      if (amount < 0) throw new IllegalAmountException(s"Cannot deposit negative amount $amount")
      else this.balance += amount
    }
  }

  def getBalanceAmount: Double = {
    this.balance
  }

}