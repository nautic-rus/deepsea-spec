package deepsea.auth

import akka.actor.Actor
import deepsea.auth.AuthManager.{Login, User}
import deepsea.database.DatabaseManager.{GetConnection, ReleaseConnection}
import play.api.libs.json.{Json, OWrites}

import java.sql.Date
import java.util.UUID

object AuthManager{
  case class Login(token: Option[String], login: String = "", password: String = "")
  case class User(id: Int, login: String, password: String, name: String, surname: String, birthday: Date, email: String, phone: String, tcid: Int, avatar: String, var token: String)
  implicit val writesUser: OWrites[User] = Json.writes[User]
}
class AuthManager extends Actor{
  override def receive: Receive = {
    case Login(token, login, password) =>
      token match {
        case Some(token) =>
          getUserByToken(token) match {
            case Some(userLogin) =>
              getUser(userLogin) match {
                case Some(user) =>
                  user.token = token
                  sender() ! Json.toJson(user)
                case _ => None
              }
            case _ => sender() ! Json.toJson("wrong-token")
          }
        case _ =>
          getUserByLoginPassword(login, password) match {
            case Some(userLogin) =>
              addUserToken(userLogin) match {
                case Some(token) =>
                  getUser(userLogin) match {
                    case Some(user) =>
                      user.token = token
                      sender() ! Json.toJson(user)
                    case _ => None
                  }
                case _ => None
              }
            case _ => sender() ! Json.toJson("wrong-password")
          }
      }
    case _ => None
  }
  def addUserToken(user: String): Option[String] ={
    val token = UUID.randomUUID().toString
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"insert into sessions values ('$user', '$token')")
        s.close()
        c.close()
        Option(token)
      case _ => Option.empty[String]
    }
  }
  def getUserByToken(token: String): Option[String] ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select s.user from sessions s where token = '$token'")
        var res = Option.empty[String]
        while (rs.next()){
          res = Option(rs.getString("user"))
        }
        s.close()
        c.close()
        res
      case _ => Option.empty[String]
    }
  }
  def getUserByLoginPassword(login: String, password: String): Option[String] ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select login from users where login = '$login' and password = '$password'")
        var res = Option.empty[String]
        while (rs.next()){
          res = Option(rs.getString("login"))
        }
        s.close()
        c.close()
        res
      case _ => Option.empty[String]
    }
  }
  def getUser(login: String): Option[User] ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from users where login = '$login'")
        var res = Option.empty[User]
        while (rs.next()){
          res = Option(User(
            rs.getInt("id"),
            rs.getString("login"),
            rs.getString("password"),
            rs.getString("name"),
            rs.getString("surname"),
            rs.getDate("birthday"),
            rs.getString("email"),
            rs.getString("phone"),
            rs.getInt("tcid"),
            rs.getString("avatar"),
            ""))
        }
        s.close()
        c.close()
        res
      case _ => Option.empty[User]
    }
  }
}
