package com.dhbikoff.simpleserver

import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.io.OutputStream
import java.net.ServerSocket
import java.net.Socket
import java.net.SocketException

import scala.io.BufferedSource
import scala.io.Codec

import android.app.Activity
import android.os.Bundle

class NetTask extends Runnable {
  def run(): Unit = {
    val serverSocket = new ServerSocket(8080)
    while (true) {
      val clientSocket = serverSocket.accept
      val resp = new Thread(new ConResponse(clientSocket))
      resp.start()
    }
    serverSocket.close
  }
}

class ConResponse(clientSocket: Socket) extends Runnable {
  class ResponseHeader(
    status: String,
    date: String = "Date: " + new java.util.Date,
    server: String = "Server: SSWS/0.01 (Simple Scala Web Server)",
    content: String = "Content-Type: text/html; charset=UTF-8",
    connection: String = "Connection: close",
    empty: String = "") {

    def toList: List[String] = List(status, date, server, content, connection, empty)
    override def toString = this.toList.map(x => x + "\r\n").mkString
    def getBytes: Array[Byte] = this.toString.getBytes
  }

  def contentType(path: String): String = {
    val ext = (path split ('.')).last
    val ct = "Content-Type: "
    val typeName = ext match {
      case "css" => "text/" + ext
      case "js" => "text/javascript"
      case "gif" | "png" => "image/" + ext
      case "jpg" => "image/jpeg"
      case "pdf" => "application/" + ext
      case "mp4" | "avi" | "mov" | "m4v" |
        "ogv" | "flv" | "webm" | "asf" => "video/" + ext
      case "txt" => "text/plain"
      case _ => "text/html; charset=UTF-8"
    }
    ct + typeName
  }

  def header(fileName: String, found: Boolean): ResponseHeader = {
    val header = {
      if (found) {
        val ctype = contentType(fileName)
        new ResponseHeader(status = "HTTP/1.0 200 OK", content = ctype)
      } else {
        new ResponseHeader(status = "HTTP/1.0 404 Not Found")
      }
    }
    header
  }

  def router(input: String): (ResponseHeader, BufferedSource) = {
    val route = {
      val s = input.split(" ")(1)
      if (s == "/") "/index.html"
      else s
    }
    response(route)
  }

  def response(filename: String) = {
    val path = "/sdcard/public/" + filename
    val found = new File(path).exists
    val head = header(filename, found)
    val src = {
      if (found) {
        val in = new FileInputStream(path)
        new BufferedSource(in)(Codec.ISO8859)
      } else {
        val in = new FileInputStream("/sdcard/public/error.html")
        new BufferedSource(in)(Codec.ISO8859)
      }
    }
    (head, src)
  }

  def sendResponseBody(fileStream: BufferedSource, outStream: OutputStream) = {
    try {
      fileStream foreach { outStream write _ }
    } catch {
      case e: SocketException => {}
      case e: IOException => {}
    }
  }

  def run() {
    val in = clientSocket.getInputStream
    while (in.available < 1) {} // wait for request
    val buffer = new Array[Byte](in.available)
    in.read(buffer)
    val request = new String(buffer)
    println("-------REQUEST--------\n" + request)
    val output = router(request)
    val header = output._1
    val fileStream = output._2
    println("-------RESPONSE-------\n" + header.toString)
    val out = clientSocket.getOutputStream
    out.write(header.getBytes)
    sendResponseBody(fileStream, out)
    clientSocket.close() // closes in and out streams
  }
}

class SimpleServer extends Activity {
  override protected def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_simple_server)
    (new Thread(new NetTask)).start()
  }
}