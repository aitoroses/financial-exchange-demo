package main

import java.util.{ Comparator, PriorityQueue }

class OrderBook(symbol: String) {
  case class Order(timestamp: Long, tradeID: String, symbol: String, var qty: Long, isBuy: Boolean, var price: Option[Double], newOrderEvent: NewOrder)

  val bidOrdering = Ordering.by { order: Order => (order.timestamp, order.price.get) }
  val offerOrdering = bidOrdering.reverse

  val bidComparator = new Comparator[Order] {
    def compare(o1: Order, o2: Order): Int = bidOrdering.compare(o1, o2)
  }

  val offerComparator = new Comparator[Order] {
    def compare(o1: Order, o2: Order): Int = offerOrdering.compare(o1, o2)
  }

  val bidsQ = new PriorityQueue[Order](5, bidComparator)
  val offersQ = new PriorityQueue[Order](5, offerComparator)

  var bestBid: Option[Order] = None
  var bestOffer: Option[Order] = None
  var volume: Long = 0

  var transactionObserver: (OrderBookResponse) => Unit = (OrderBookEvent => ())
  var marketDataObserver: (MarketDataEvent) => Unit = (MarketDataEvent => ())

  def processOrderBookRequest(request: OrderBookRequest): Unit = request match {
    case order: NewOrder => {

      val currentTime = System.currentTimeMillis

      val (isOk, message) = validateOrder(order)

      if (!isOk) this.transactionObserver(Rejected(currentTime, message.getOrElse("N/A"), order))
      else {
        this.transactionObserver(Acknowledged(currentTime, order))

        val orderBookOrder = Order(order.timestamp, order.tradeID, order.symbol, order.qty, order.isBuy, order.price, order)
        processNewOrder(orderBookOrder)
      }
    }

    case cancel: Cancel => {
      val order = cancel.order
      val orderQ = if (order.isBuy) bidsQ else offersQ
      val isRemoved = orderQ.remove(order)

      if (isRemoved) {
        this.transactionObserver(Acknowledged(System.currentTimeMillis(), cancel))
        updateBBO()
      }

      else this.transactionObserver(Rejected(System.currentTimeMillis(), "Order not found", cancel))
    }

    case amend: Amend => {
      val order = amend.order
      val orderBookOrder = Order(order.timestamp, order.tradeID, order.symbol, order.qty, order.isBuy, order.price, order)

      val orderQ = if (order.isBuy) bidsQ else offersQ

      if (!orderQ.remove(orderBookOrder)) {
        this.transactionObserver(Rejected(System.currentTimeMillis(), "Order not found", amend))
      }
      else {

        if (amend.newQty.isDefined) orderBookOrder.qty = amend.newQty.get
        if (amend.newPrice.isDefined) orderBookOrder.price = amend.newPrice

        orderQ.add(orderBookOrder)
        this.transactionObserver(Acknowledged(System.currentTimeMillis(), amend))
        updateBBO()
      }
    }
  }

  def processNewOrder(orderBookOrder: Order) = {
    val currentTime = System.currentTimeMillis

    val orderQ = if (orderBookOrder.isBuy) bidsQ else offersQ
    val oppositeQ = if (orderBookOrder.isBuy) offersQ else bidsQ

    if (orderBookOrder.price.isDefined) {
      // =======LIMIT ORDER=======

      if (oppositeQ.isEmpty || !isLimitOrderExecutable(orderBookOrder, oppositeQ.peek)) {
        orderQ.add(orderBookOrder)
        updateBBO()
      }
      else matchOrder(orderBookOrder, oppositeQ)
    }
    else {
      // =====Market order=====
      // TODO: what if order was already partially executed, replace reject with partial cancel?
      if (oppositeQ.isEmpty) this.transactionObserver(Rejected(currentTime, "No oppossing orders", orderBookOrder.newOrderEvent))
      else matchOrder(orderBookOrder, oppositeQ)
    }
  }

  private def matchOrder(order: Order, oppositeQ: PriorityQueue[Order]): Unit = {
    val oppositeOrder = oppositeQ.peek
    val currentTime = System.currentTimeMillis

    if (order.qty < oppositeOrder.qty) {
      oppositeOrder.qty = oppositeOrder.qty - order.qty

      this.volume += order.qty

      this.transactionObserver(Filled(currentTime, order.price.get, order.qty, Array(order.newOrderEvent, oppositeOrder.newOrderEvent)))
      this.marketDataObserver(LastSalePrice(currentTime, order.symbol, order.price.get, order.qty, volume))
      updateBBO()
    }
    else if (order.qty > oppositeOrder.qty) {
      oppositeQ.poll
      val reducedQty = order.qty - oppositeOrder.qty
      order.qty = reducedQty

      this.volume = order.qty

      this.transactionObserver(Filled(currentTime, order.price.get, order.qty, Array(order.newOrderEvent, oppositeOrder.newOrderEvent)))
      this.marketDataObserver(LastSalePrice(currentTime, order.symbol, order.price.get, order.qty, volume))
      updateBBO()

      processNewOrder(order)
    }
    else {
      oppositeQ.poll

      this.volume += order.qty

      this.transactionObserver(Filled(currentTime, order.price.get, order.qty, Array(order.newOrderEvent, oppositeOrder.newOrderEvent)))
      this.marketDataObserver(LastSalePrice(currentTime, order.symbol, order.price.get, order.qty, volume))
      updateBBO()
    }
  }

  private def validateOrder(order: NewOrder): (Boolean, Option[String]) = (true, None)

  private def updateBBO() = {
    val bidHead = Option(bidsQ.peek)
    val offerHead = Option(offersQ.peek)

    if (bidHead != bestBid || offerHead != bestOffer) {
      bestBid = bidHead
      bestOffer = offerHead

      var bidPrice: Option[Double] = None
      var bidQty: Option[Long] = None
      var offerPrice: Option[Double] = None
      var offerQty: Option[Long] = None

      if (bestBid.isDefined) {
        bidPrice = bestBid.get.price
        bidQty = Some(bestBid.get.qty)
      }

      if (bestOffer.isDefined) {
        offerPrice = bestOffer.get.price
        offerQty = Some(bestOffer.get.qty)
      }

    }
  }

  private def isLimitOrderExecutable(order: Order, oppositeOrder: Order): Boolean = {
    if (order.isBuy) order.price.get >= oppositeOrder.price.get
    else order.price.get <= oppositeOrder.price.get
  }
}
