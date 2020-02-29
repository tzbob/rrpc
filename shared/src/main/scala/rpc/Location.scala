package rpc

import io.circe.generic.JsonCodec

@JsonCodec
sealed trait Location { def superscript: String }

object Location {
  case object Server extends Location { val superscript = "ˢ" }
  case object Client extends Location { val superscript = "ᶜ" }
}
