package com.lightbend.training.scalatrain

import play.api.libs.json.{JsNumber, JsObject, JsValue}

import scala.util.Try

object Time {
  def fromMinutes(mins: Int): Time = {
    val hours = mins / 60
    val minutes = mins % 60
    Time(hours, minutes)
  }

  def fromJson(js: JsValue): Option[Time] = {
    for {
      hours <- Try((js \ "hours").as[Int])
      minutes <- Try((js \ "minutes").as[Int]).recover { case _=> 0 }
    } yield Time(hours, minutes)
  }.toOption
}

case class Time(hours: Int = 0, minutes: Int = 0) extends Ordered[Time] {
  require(hours <= 23 && hours >= 0, "hours should be within 0 and 23")
  require(minutes <= 59 && minutes >= 0, "minutes should be within 0 and 59")

  val asMinutes: Int = hours * 60 + minutes

  def minus(that: Time): Int = this.asMinutes - that.asMinutes

  def -(that: Time): Int = this.minus(that)

  override lazy val toString: String = f"$hours%02d:$minutes%02d"

  override def compare(that: Time): Int = this.asMinutes - that.asMinutes

  def toJson: JsValue = JsObject(Map("hours" -> JsNumber(hours), "minutes" -> JsNumber(minutes)))
}
