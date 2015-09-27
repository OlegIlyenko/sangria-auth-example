import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpHeader, Uri, HttpRequest}
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer

import org.json4s.native.JsonMethods._
import org.json4s._

import de.heikoseeberger.akkahttpjson4s.Json4sSupport._

import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.integration.json4s._

import scala.concurrent.{Future, Await}
import scala.util.{Failure, Success}
import scala.concurrent.duration._

object Server extends App {
  implicit val system = ActorSystem("sangria")
  implicit val materializer = ActorMaterializer()
  implicit val serialization = native.Serialization
  implicit val formats = DefaultFormats

  import system.dispatcher

  def startServer() = {
    val userRepo = new Data.UserRepo
    val colorRepo = new Data.ColorRepo


    val schema = Schema.middlewareBased.schema

    // Alternative approach
    // val schema = Schema.resolveBased.schema

    val route: Route =
      (get & path("graphql")) {
        parameters('query, 'variables.?, 'operation.?) { (query, variables, operation) =>
          optionalHeaderValueByName("SecurityToken") { token =>
            QueryParser.parse(query) match {

              // query parsed successfully, time to execute it!
              case Success(queryAst) =>
                complete(Executor.execute(schema, queryAst,
                  userContext = new Data.SecureContext(token, userRepo, colorRepo),
                  exceptionHandler = Data.errorHandler,
                  middleware = Schema.middlewareBased.SecurityEnforcer :: Nil,
                  operationName = operation,
                  variables = variables map (parse(_, true)) getOrElse JObject()))

              // can't parse GraphQL query, return error
              case Failure(error) =>
                complete(BadRequest, JObject("error" -> JString(error.getMessage)))
            }

          }
        }
      }

    Http().bindAndHandle(route, "0.0.0.0", 8080)
  }

  def clientExampleRequests() = {
    def printResult(query: String, result: Future[JValue]) =
      println(s"Query:\n\n$query\n\nResult:\n\n${pretty(render(Await.result(result, 5 seconds)))}\n\n")

    // invalid token
    {
      val query =
        """
        query ShowEverything {
          me {userName, permissions}
          colors
        }
        """

      val result = Http().singleRequest(HttpRequest(uri = Uri("http://localhost:8080/graphql").withQuery("query" -> query))
        .withHeaders(RawHeader("SecurityToken", "some invalid token")))

      printResult(query, result.flatMap(Unmarshal(_).to[JValue]))

      // Prints:
      //
      // {
      //   "data":{
      //     "me":null,
      //     "colors":null
      //   },
      //   "errors":[{
      //     "message":"Invalid token",
      //     "field":"me",
      //     "locations":[{
      //       "line":3,
      //       "column":11
      //     }]
      //   },{
      //     "message":"Invalid token",
      //     "field":"colors",
      //     "locations":[{
      //       "line":4,
      //       "column":11
      //     }]
      //   }]
      // }
    }

    // admin login with subsequent fetch. Please note, that admin is allowed to view his permissions, so he sees them.
    {
      val loginQuery =
        """
        mutation JustLogin {
          login(userName: "admin", password: "secret")
        }
        """

      val loginResult = Http().singleRequest(HttpRequest(uri = Uri("http://localhost:8080/graphql").withQuery("query" -> loginQuery)))
        .flatMap(Unmarshal(_).to[JValue])

      printResult(loginQuery, loginResult)

      // Prints:
      //
      // {
      //   "data":{
      //     "login":"a4d7fc91-e490-446e-9d4c-90b5bb22e51d",
      //   }
      // }

      val JString(token) = Await.result(loginResult, 5 seconds) \ "data" \ "login"

      val query =
        """
        query ShowEverything {
          me {userName, permissions}
          colors
        }
        """

      val result = Http().singleRequest(HttpRequest(uri = Uri("http://localhost:8080/graphql").withQuery("query" -> query))
        .withHeaders(RawHeader("SecurityToken", token)))

      printResult(query, result.flatMap(Unmarshal(_).to[JValue]))

      // Prints:
      //
      // {
      //   "data":{
      //     "me":{
      //       "userName":"admin",
      //       "permissions":["VIEW_PERMISSIONS","EDIT_COLORS","VIEW_COLORS"]
      //     },
      //     "colors":["red","green","blue"]
      //   }
      // }
    }

    // normal user login with subsequent fetch. Please note, that normal users are not allowed to view their permissions.
    {
      val loginQuery =
        """
        mutation JustLogin {
          login(userName: "john", password: "apples")
        }
        """

      val loginResult = Http().singleRequest(HttpRequest(uri = Uri("http://localhost:8080/graphql").withQuery("query" -> loginQuery)))
          .flatMap(Unmarshal(_).to[JValue])

      printResult(loginQuery, loginResult)

      // Prints:
      //
      // {
      //   "data":{
      //     "login":"a4d7fc91-e490-446e-9d4c-90b5bb22e51d",
      //   }
      // }

      val JString(token) = Await.result(loginResult, 5 seconds) \ "data" \ "login"

      val query =
        """
        query ShowEverything {
          me {userName, permissions}
          colors
        }
        """

      val result = Http().singleRequest(HttpRequest(uri = Uri("http://localhost:8080/graphql").withQuery("query" -> query))
          .withHeaders(RawHeader("SecurityToken", token)))

      printResult(query, result.flatMap(Unmarshal(_).to[JValue]))

      // Prints:
      //
      // {
      //   "data":{
      //     "me":{
      //       "userName":"john",
      //       "permissions":null
      //     },
      //     "colors":["red","green","blue"]
      //   },
      //   "errors":[{
      //     "message":"You do not have permission to do this operation",
      //     "field":"me.permissions",
      //     "locations":[{
      //       "line":3,
      //       "column":25
      //     }]
      //   }]
      // }
    }

    // login and immediately add colors. It's possible because `UpdateCtx` action updated the context
    // for other mutation properties. Since mutation is strictly sequential, `addColor` gets updated context with the token
    {
      val query =
        """
        mutation LoginAndMutate {
          login(userName: "admin", password: "secret")

          withMagenta: addColor(color: "magenta")
          withOrange: addColor(color: "orange")
        }
        """

      val result = Http().singleRequest(HttpRequest(uri = Uri("http://localhost:8080/graphql").withQuery("query" -> query)))

      printResult(query, result.flatMap(Unmarshal(_).to[JValue]))

      // Prints:
      //
      // {
      //   "data":{
      //     "login":"a4d7fc91-e490-446e-9d4c-90b5bb22e51d",
      //     "withMagenta":["red","green","blue","magenta"],
      //     "withOrange":["red","green","blue","magenta","orange"]
      //   }
      // }
    }

    // failed login. Caused all fields to produce errors. Since fields are optional, they will be `null` in JSON and no updates will take place
    {
      val query =
        """
        mutation MutationWithWrongPassword {
          login(userName: "admin", password: "please let me in")

          withMagenta: addColor(color: "magenta")
          withOrange: addColor(color: "orange")
        }
        """

      val result = Http().singleRequest(HttpRequest(uri = Uri("http://localhost:8080/graphql").withQuery("query" -> query)))

      printResult(query, result.flatMap(Unmarshal(_).to[JValue]))

      // Prints:
      //
      // {
      //   "data":{
      //     "login":null,
      //     "withMagenta":null,
      //     "withOrange":null
      //   },
      //   "errors":[{
      //     "message":"UserName or password is incorrect",
      //     "field":"login",
      //     "locations":[{
      //       "line":3,
      //       "column":11
      //     }]
      //   },{
      //     "message":"Invalid token",
      //     "field":"withMagenta",
      //     "locations":[{
      //       "line":5,
      //       "column":11
      //     }]
      //   },{
      //     "message":"Invalid token",
      //     "field":"withOrange",
      //     "locations":[{
      //       "line":6,
      //       "column":11
      //     }]
      //   }]
      // }
    }

    // Login is successful, but user does not have permission to change the colors
    {
      val query =
        """
        mutation MutationWithWrongPermissions {
          login(userName: "john", password: "apples")

          withMagenta: addColor(color: "magenta")
          withOrange: addColor(color: "orange")
        }
        """

      val result = Http().singleRequest(HttpRequest(uri = Uri("http://localhost:8080/graphql").withQuery("query" -> query)))

      printResult(query, result.flatMap(Unmarshal(_).to[JValue]))

      // Prints:
      //
      // {
      //   "data":{
      //     "login":"f40ea6b0-4479-4552-9c76-51bb4c6f9d29",
      //     "withMagenta":null,
      //     "withOrange":null
      //   },
      //   "errors":[{
      //     "message":"You do not have permission to do this operation",
      //     "field":"withMagenta",
      //     "locations":[{
      //       "line":5,
      //       "column":11
      //     }]
      //   },{
      //     "message":"You do not have permission to do this operation",
      //     "field":"withOrange",
      //     "locations":[{
      //       "line":6,
      //       "column":11
      //     }]
      //   }]
      // }
    }

  }

  startServer()
  clientExampleRequests()
}
