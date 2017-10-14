package pl.setblack.vide

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import play.api.libs.json.{JsString, JsValue, Json, Writes}

import scala.io.StdIn
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import pl.setblack.vide.walker.{JavaFile, FileScanner}

object WebServer {
  def main(args: Array[String]) {

    val allJavas = FileScanner.run

    val deployer  = new Deployer(allJavas)

    for( a <- 1 until 10000){
      deployer.update
    }

    /*implicit val javaFileWriter =  Json.writes[JavaFile]
    implicit val coordsWriter =  Json.writes[Coords]
    implicit val nodeWriter =  Json.writes[CodeNode]*/

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val route = cors() {
      path("code") {
        get {
          complete(HttpEntity(ContentTypes.`application/json`,Json.toJson(deployer.getNodes.toList).toString()))
        }
      } ~ path ("updated") {
         get {

           complete(HttpEntity(ContentTypes.`application/json`,Json.toJson(deployer.update.toList).toString()))
         }
      }
    }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
