package main

object Main extends App {

  val random = new scala.util.Random

  val msftBook = new OrderBook("MSFT")

  msftBook.transactionObserver = (response) => {
    response match {
      case resp => println("==> Transaction: " + resp)
    }
  }

  msftBook.marketDataObserver = response => {
    response match {
      case resp => println("==> Market event: " + resp)
    }
  }

  // one bid, only bidQ should be populated
  val order1 = NewOrder(1, "1", "MSFT", 100, true, Some(50))
  msftBook.processOrderBookRequest(order1)
  assert(!msftBook.bidsQ.isEmpty, "bidsQ is empty")
  assert(msftBook.offersQ.isEmpty, "offersQ is not empty")

  // execute 50 shares of the order in bidsQ
  val order2 = NewOrder(1, "2", "MSFT", 50, false, Some(50))
  msftBook.processOrderBookRequest(order2)
  assert(msftBook.bidsQ.peek.qty == 50, "BidsQ peak is not 50")
  assert(msftBook.offersQ.isEmpty, "offersQ is not empty")

  // offer shares at a price here both bid and offer queues are populated with 50 shares
  val order3 = NewOrder(1, "3", "MSFT", 50, false, Some(51))
  msftBook.processOrderBookRequest(order3)
  assert(msftBook.bidsQ.peek.qty == 50, "bidsQ hasn't 50 shares")
  assert(msftBook.offersQ.peek.qty == 50, "offersQ hasn't 50 shares")

  val order4 = Amend(1, order3, Some(52), Some(50))
  msftBook.processOrderBookRequest(order4)

  println("Best offer: " + msftBook.bestOffer)
  println("Best bid: " + msftBook.bestBid)
}

