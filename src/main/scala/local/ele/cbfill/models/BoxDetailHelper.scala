package local.ele.cbfill.models

import local.common.Codecs
import local.ele.cbfill.models.Cases.CBStuff
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait BoxDetailHelper extends Codecs {

  private def collectioneleCBBXCableBoxModules(): MongoCollection[CableBoxModule] = mongoDatabase().getCollection("eleCBBXCableBoxModules")

  private def collectioneleCBBXSealModules(): MongoCollection[SealModule] = mongoDatabase().getCollection("eleCBBXSealModules")

  private def collectioneleCBBXCompressionBlocks(): MongoCollection[CompressionBlock] = mongoDatabase().getCollection("eleCBBXCompressionBlocks")

  private def collectioneleCBBXAnkerPlates(): MongoCollection[AnkerPlate] = mongoDatabase().getCollection("eleCBBXAnkerPlates")


  def cableBoxModules(): List[CableBoxModule] = {
    val allelems = collectioneleCBBXCableBoxModules().find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    allelems.value.get.getOrElse(ListBuffer.empty[CableBoxModule].toSeq).toList
  }

  def sealModules(): List[SealModule] = {
    val allelems = collectioneleCBBXSealModules().find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    allelems.value.get.getOrElse(ListBuffer.empty[SealModule].toSeq).toList
  }

  def compressionBlocks(): List[CompressionBlock] = {
    val allelems = collectioneleCBBXCompressionBlocks().find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    allelems.value.get.getOrElse(ListBuffer.empty[CompressionBlock].toSeq).toList
  }

  def ankerPlates(): List[AnkerPlate] = {
    val allelems = collectioneleCBBXAnkerPlates().find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    allelems.value.get.getOrElse(ListBuffer.empty[AnkerPlate].toSeq).toList
  }

  def genCBStuff():CBStuff=CBStuff(cableBoxModules(),sealModules(),compressionBlocks(),ankerPlates())

}
