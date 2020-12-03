import sbt._
object Dependencies{
    object Http4s{
        val dsl = "org.http4s" %% "http4s-dsl" % 0.21.12
        val server= "org.http4s" %% "http4s-blaze-server" % 0.21.12
        val client= "org.http4s" %% "http4s-blaze-client" % 0.21.12
)
    }
}