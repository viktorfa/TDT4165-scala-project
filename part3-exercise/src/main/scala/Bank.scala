import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

  val accountCounter = new AtomicInteger(1000)

  def createAccount(initialBalance: Double): ActorRef = {
    // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
    BankManager.createAccount(accountCounter.addAndGet(1).toString, this.bankId, initialBalance)
  }

  def findAccount(accountId: String): Option[ActorRef] = {
    // Use BankManager to look up an account with ID accountId
    // An option is either None or the type it is supposed to be, here we just cast it
    try {
      Option[ActorRef](BankManager.findAccount(this.bankId, accountId))
    } catch {
      case _: NoSuchElementException => None
    }
  }

  def findOtherBank(bankId: String): Option[ActorRef] = {
    // Use BankManager to look up a different bank with ID bankId
    // An option is either None or the type it is supposed to be, here we just cast it
    try {
      Option[ActorRef](BankManager.findBank(bankId))
    } catch {
      case _: NoSuchElementException => None
    }
  }

  override def receive = {
    case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance) // Create a new account
    case GetAccountRequest(id) => sender ! findAccount(id) // Return account
    case IdentifyActor => sender ! this
    case t: Transaction => processTransaction(t)

    case t: TransactionRequestReceipt => {
      // Forward receipt
      val account: ActorRef = findAccount(t.toAccountNumber).get
      account ! t
    }

    case msg => sender ! msg
  }

  def processTransaction(t: Transaction): Unit = {
    implicit val timeout = new Timeout(5 seconds)
    // If the transaction is from an external bank and the destination account belongs to this bank, it must be
    // treated as an internal transaction further down
    val isFromExternal = t.to.length > 4 && t.to.substring(0, 4).equals(this.bankId)
    val isInternal = t.to.length <= 4

    val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
    val toAccountId = if (isInternal) t.to else t.to.substring(4)
    val transactionStatus = t.status

    // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
    // HINT: Make use of the variables that have been defined above.
    if (isInternal || isFromExternal) {
      // This is where we treat an external transaction as an internal
      val toAccount: Option[ActorRef] = findAccount(toAccountId)
      if (toAccount.isDefined) {
        toAccount.get ! t
      } else {
        t.status = TransactionStatus.FAILED
        if (isInternal) {
          findAccount(t.from.substring(4)).get ! new TransactionRequestReceipt(t.to, t.id, t)
        } else {
          findOtherBank(t.from.substring(0, 4)).get ! t
        }
      }
    } else {
      val toBank: Option[ActorRef] = findOtherBank(toBankId)
      if (toBank.isDefined) {
        toBank.get ! t
      } else {
        t.status = TransactionStatus.FAILED
        findAccount(t.from.substring(4)).get ! new TransactionRequestReceipt(t.to, t.id, t)
      }
    }
  }
}