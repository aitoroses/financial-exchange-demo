package main

sealed trait OrderBookRequest
case class NewOrder(timestamp: Long, tradeID: String, symbol: String, qty: Long, isBuy: Boolean, price: Option[Double]) extends OrderBookRequest
case class Cancel(timestamp: Long, order: NewOrder) extends OrderBookRequest
case class Amend(timestamp: Long, order: NewOrder, newPrice: Option[Double], newQty: Option[Long])
