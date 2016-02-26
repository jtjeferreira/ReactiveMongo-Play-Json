import reactivemongo.core.errors.DetailedDatabaseException

object JSONAggregationFrameworkSpec extends org.specs2.mutable.Specification {
  "JSON collection" title

  sequential

  import Common._
  import play.api.libs.json.{ JsObject, _ }
  import reactivemongo.play.json._
  import reactivemongo.play.json.collection.{ JSONCollection, JSONQueryBuilder }
  import reactivemongo.api.commands.WriteResult
  import reactivemongo.api.{ FailoverStrategy, ReadPreference }

  case class Thing(
    _id: Option[String] = None,
    name: String,
    count: Int
  )

  implicit val thingFormat = Json.format[Thing]

  lazy val collectionName = "reactivemongo_test_thing"
  lazy val collection = db.collection[JSONCollection](collectionName)

  "JSONAggregationFramework" should {
    "sum things" in {

      import reactivemongo.play.json.commands.JSONAggregationFramework._

      collection.remove(Json.obj())
        .aka("remove") must beLike[WriteResult] {
          case result => result.ok must beTrue
        }.await(timeoutMillis)

      collection.save(Thing(Some("1"), "xxx", 2))
        .aka("save") must beLike[WriteResult] {
          case result => result.ok must beTrue
        }.await(timeoutMillis)

      collection.save(Thing(Some("2"), "xxx", 3))
        .aka("save") must beLike[WriteResult] {
          case result => result.ok must beTrue
        }.await(timeoutMillis)

      val z = collection.aggregate(
        Match(Json.obj("name" -> "xxx")),
        List(GroupField("name")("total" -> SumField("count")))
      )
        .map(_.firstBatch)
        .map(_.headOption match {
          case Some(x) => (x \ "total").as[Int]
          case None    => -51
        })
        .aka("aggregate")

      z must beEqualTo(5).await(timeoutMillis)

    }
  }
}
