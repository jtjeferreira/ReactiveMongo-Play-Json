import play.api.libs.json.{ Json, JsNumber, JsString, JsObject, __ }

import reactivemongo.bson.{ BSONArray, BSONDocument }
import reactivemongo.play.json._

class JsonSpec extends org.specs2.mutable.Specification {
  "JSON" title

  sequential

  val pack = Package(
    Expeditor("Amazon"),
    List(Item("A Game of Thrones", "Book", 1)),
    20
  )

  "ReactiveMongo Plugin" should {
    "convert an empty JSON and give an empty BSON doc" in {
      val json = Json.obj()
      val bson = BSONFormats.toBSON(json).get.asInstanceOf[BSONDocument]
      bson.isEmpty mustEqual true
      val json2 = BSONFormats.toJSON(bson)
      json2.as[JsObject].value.size mustEqual 0
    }

    "convert a simple JSON to BSON and vice versa" in {
      val json = Json.obj("coucou" -> JsString("jack"))
      val bson = JsObjectWriter.write(json)
      val json2 = JsObjectReader.read(bson)
      json.toString mustEqual json2.toString
    }

    "convert a simple JSON array to BSON and vice versa" in {
      val json = Json.arr(JsString("jack"), JsNumber(9.1))
      val bson = BSONFormats.toBSON(json).get.asInstanceOf[BSONArray]

      json.toString mustEqual BSONFormats.toJSON(bson).toString
    }

    "convert a JSON doc containing an array and vice versa" in {
      val json = Json.obj(
        "name" -> JsString("jack"),
        "contacts" -> Json.arr(Json.toJsFieldJsValueWrapper(Json.obj("email" -> "jack@jack.com")))
      )
      val bson = JsObjectWriter.write(json)

      json.toString mustEqual JsObjectReader.read(bson).toString
    }

    "format a JsPath for Mongo CRUD" in {
      import play.api.libs.functional.syntax._
      import Writers._

      case class Limit(low: Option[Int], high: Option[Int])
      case class App(limit: Option[Limit])

      val lowWriter = (__ \ "low").writeNullable[Int]
      val highWriter = (__ \ "high").writeNullable[Int]
      val limitWriter = (lowWriter and highWriter)(unlift(Limit.unapply))
      val appWriter = (__ \ "limit").writemongo[Limit](limitWriter)

      appWriter.writes(Limit(Some(1), None)) mustEqual
        Json.obj("limit.low" -> 1)
      appWriter.writes(Limit(Some(1), Some(2))) mustEqual
        Json.obj("limit.low" -> 1, "limit.high" -> 2)
      appWriter.writes(Limit(None, None)) mustEqual
        Json.obj()
    }
  }
}

case class Expeditor(name: String)
case class Item(name: String, description: String, occurrences: Int)
case class Package(
  expeditor: Expeditor,
  items: List[Item],
  price: Float
)
