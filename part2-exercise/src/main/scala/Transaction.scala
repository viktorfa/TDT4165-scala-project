
import exceptions._
import scala.collection.mutable
import scala.collection.mutable.Queue

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  val transactions = Queue[Transaction]

  // Remove and return the first element from the queue
  def pop: Transaction = {

    transactions.dequeue()
  }

  // Return whether the queue is empty
  def isEmpty: Boolean = ???

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = {
    transactions.enqueue(t)
  }

  // Return the first element from the queue without removing it
  def peek: Transaction = {
    
  }

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = ???
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {


      def doTransaction() = {
        from withdraw amount
        to deposit amount
      }

    //Unsure on how from.uid and to.uid will change during each iteration

    for(i <- 1 to allowedAttemps) {

      if (from.uid < to.uid) from synchronized {
        to synchronized {
          doTransaction
        }
      } else to synchronized {
        from synchronized {
          doTransaction
        }
      }

    }

    //after "allowedattempts" number of iteration, if status doesnt reach SUCCESS it gets labelled as a failure

    if(status != TransactionStatus.SUCCESS){
      status = TransactionStatus.FAILED
    }


    //Extend this method to satisfy new requirements.

  }
}
