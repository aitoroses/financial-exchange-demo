package main

sealed trait MarketDataEvent
case class LastSalePrice(timestamp: Long, symbol: String, price: Double, qty: Long, volume: Long) extends MarketDataEvent
case class BBOChange(timestamp: Long, symbol: String, bidPrice: Option[Double], bidQty: Option[Long], offerPrice: Option[Double], offerQty: Option[Long]) extends MarketDataEvent
