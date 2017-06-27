package main

sealed trait OrderBookResponse
case class Filled(timestamp: Long, price: Double, qty: Long, order: Array[NewOrder]) extends OrderBookResponse
case class Acknowledged(timestamp: Long, request: OrderBookRequest) extends OrderBookResponse
case class Rejected(timestamp: Long, error: String, request: OrderBookRequest) extends OrderBookResponse
case class Canceled(timestamp: Long, reason: String, order: NewOrder) extends OrderBookResponse
