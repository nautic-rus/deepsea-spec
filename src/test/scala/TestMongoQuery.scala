import com.mongodb.BasicDBObject
import deepsea.esp.EspManager.PipeEspObject
import deepsea.pipe.PipeManager.{Material, MaterialTranslation, PipeSeg}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

class TestMongoQuery  extends AnyFunSuite{


  val query4="db.getCollection('esp-objects-n002-pipe').aggregate([{$match :{elements:{$gt:[]}}},{$group : {_id: '$docNumber',date: { $max : '$date' },doc: { '$first': '$$ROOT'}}},{$replaceRoot:{newRoot:'$doc'}}])"
  val query8 = "[{$match :{elements:{$gt:[]}}},{$group : {_id: '$docNumber',date: { $max : '$date' },doc: { '$first': '$$ROOT'}}},{$replaceRoot:{newRoot:'$doc'}}]"


  val query: List[BasicDBObject] ={
    val buff=ListBuffer.empty[BasicDBObject]
    buff+= BasicDBObject.parse("{$match :{elements:{$gt:[]}}}")
    buff+= BasicDBObject.parse("{$sort:{date:-1}}")
    buff+= BasicDBObject.parse("{$group : {_id: '$docNumber',date: { $max : '$date' },doc: { '$first': '$$ROOT'}}}")
    buff+= BasicDBObject.parse("{$replaceRoot:{newRoot:'$doc'}}")
    buff.toList
  }

  getAllPipes()
  private def getAllPipes(): List[PipeEspObject] = {
    import org.mongodb.scala.bson.codecs.Macros._
    val mongoDatabase: MongoDatabase = {
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[PipeEspObject],
        classOf[PipeSeg], classOf[Material], classOf[MaterialTranslation]), DEFAULT_CODEC_REGISTRY)
      //val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      val mongoClient: MongoClient = MongoClient("mongodb://localhost:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }
    val collectionWorkShopMaterial: MongoCollection[PipeEspObject] = mongoDatabase.getCollection("esp-objects-n002-pipe")
    val allelems = collectionWorkShopMaterial.aggregate(query).allowDiskUse(true).toFuture()
    //val allelems = collectionWorkShopMaterial.find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    val list: List[PipeEspObject] = allelems.value.get.getOrElse(Seq.empty[PipeEspObject]).toList
    list
  }

}
