import exceptions.IllegalAmountException
import exceptions.NoSufficientFundsException

import scala.annotation.tailrec

object Bank {
  
  private var idCounter: Int = 0
  
  def transaction(from: Account, to: Account, amount: Double): Unit = {
    if (amount < 0) throw new IllegalAmountException(s"Cannot transfer negative amount $amount")
    else {
      from.withdraw(amount)
      to.deposit(amount)
    }
  }
  // TODO make this thread safe
  @tailrec def getUniqueId: Int = {
    idCounter += 1
    idCounter
    getUniqueId
  }
}