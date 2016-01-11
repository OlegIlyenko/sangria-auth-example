import java.util.UUID

import sangria.execution.HandledException
import sangria.marshalling.ResultMarshaller

object Data {
  case class User(userName: String, permissions: List[String])

  class UserRepo {
    var tokens = Map.empty[String, User]

    /** Gives back a token or sessionId or anything else that identifies the user session  */
    def authenticate(userName: String, password: String): Option[String] =
      if (userName == "admin" && password == "secret") {
        val token = UUID.randomUUID().toString
        tokens = tokens + (token → User("admin", "VIEW_PERMISSIONS" :: "EDIT_COLORS" :: "VIEW_COLORS" :: Nil))
        Some(token)
      } else if (userName == "john" && password == "apples") {
        val token = UUID.randomUUID().toString
        tokens = tokens + (token → User("john", "VIEW_COLORS" :: Nil))
        Some(token)
      } else None

    /** Gives `User` object with his/her permissions */
    def authorise(token: String): Option[User] = tokens.get(token)
  }

  case class AuthenticationException(message: String) extends Exception(message)
  case class AuthorisationException(message: String) extends Exception(message)

  class ColorRepo {
    var colors = List("red", "green", "blue")

    def addColor(color: String) =
      colors = colors :+ color
  }

  case class SecureContext(token: Option[String], userRepo: UserRepo, colorRepo: ColorRepo) {
    def login(userName: String, password: String) = userRepo.authenticate(userName, password) getOrElse (
        throw new AuthenticationException("UserName or password is incorrect"))

    def authorised[T](permissions: String*)(fn: User ⇒ T) =
      token.flatMap(userRepo.authorise).fold(throw AuthorisationException("Invalid token")) { user ⇒
        if (permissions.forall(user.permissions.contains)) fn(user)
        else throw AuthorisationException("You do not have permission to do this operation")
      }

    def ensurePermissions(permissions: List[String]): Unit =
      token.flatMap(userRepo.authorise).fold(throw AuthorisationException("Invalid token")) { user ⇒
        if (!permissions.forall(user.permissions.contains))
          throw AuthorisationException("You do not have permission to do this operation")
      }

    def user = token.flatMap(userRepo.authorise).fold(throw AuthorisationException("Invalid token"))(identity)
  }

  val errorHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
    case (m, AuthenticationException(message)) ⇒ HandledException(message)
    case (m, AuthorisationException(message)) ⇒ HandledException(message)
  }
}