import cats.*
import cats.derived.*
import cats.effect.*
import cats.syntax.all.*

import java.time.format.DateTimeFormatter
import java.time.{ZoneId, ZonedDateTime}

import scala.util.Try

def promptLine(msg: String): IO[String] = for {
  _ <- IO.println(msg)
  _ <- IO.print("> ")
  line <- IO.readLine
} yield line

def parseZoneId(tz: String): Option[ZoneId] = {
  Try(ZoneId.of(tz)).toOption
}

def getCurrentDateTimeInTimezone(zoneId: ZoneId): IO[ZonedDateTime] = IO {
  ZonedDateTime.now(zoneId)
}

def zonedDateTimeToString(datetime: ZonedDateTime)(formatter: DateTimeFormatter): Option[String] = {
  Try(formatter.format(datetime)).toOption
}

object Main extends IOApp.Simple {
  val run: IO[Unit] = for {
    /* Ask the user for a timezone */
    inp <- promptLine("Enter a timezone")
    _ <- IO.println(s"You entered `$inp`!")

    /* Try to parse it into a ZoneId */
    maybeZoneId = parseZoneId(inp)

    _ <- maybeZoneId match {
      /* User entered a valid ZoneId */
      case Some(zoneId) => for {
        /* If the ZoneId is valid, use it to get the current datetime in that timezone */
        currentDateTime <- getCurrentDateTimeInTimezone(zoneId)

        /* Format the datetime */
        formatted = zonedDateTimeToString(currentDateTime)(DateTimeFormatter.RFC_1123_DATE_TIME)

        /* Display it to the user */
        // Is there a better way to handle this such that I can still ignore `None`?
        _ <- IO { formatted.foreach { dt =>
          IO.println(s"The current time is `$dt`")
        } }
      } yield ()

      /* User entered an invalid ZoneId */
      case None => {
        IO.println("Hmm... That doesn't look like a timezone!")
      }
    }
  } yield ()
}
