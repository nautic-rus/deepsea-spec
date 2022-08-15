package local.qrutil

import com.google.zxing.{BarcodeFormat, EncodeHintType}
import com.google.zxing.common.BitMatrix
import com.google.zxing.qrcode.QRCodeWriter
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import local.jdxf.{DXFDocument, DXFGraphics}

import java.util

trait QrDxfHelper {


  def url2qrDXF(targetUrl: String, width: Int, height: Int):String={
    val dxfDocument: DXFDocument = new DXFDocument("QR")
    val graphics: DXFGraphics = dxfDocument.getGraphics

    val hintMap: util.Hashtable[EncodeHintType, Object] = new util.Hashtable[EncodeHintType, Object]()
    hintMap.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.L)
    val qrCodeWriter = new QRCodeWriter()

    val byteMatrix: BitMatrix = qrCodeWriter.encode(targetUrl, BarcodeFormat.QR_CODE, width, height, hintMap)
    val CrunchifyWidth = byteMatrix.getWidth
    (0 until CrunchifyWidth).foreach(i => {
      (0 until CrunchifyWidth).foreach(j => {
        if (byteMatrix.get(i, j)) {
          graphics.fillRect(i, j, 1, 1);
        }
      })
    })
    dxfDocument.toDXFString
  }

}
