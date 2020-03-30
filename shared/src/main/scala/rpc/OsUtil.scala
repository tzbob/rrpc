package rpc

object OsUtil {
  private lazy val os = System.getProperty("os.name").toLowerCase

  def windows: Boolean = os.startsWith("windows")
  def linux: Boolean   = os.startsWith("linux")
  def mac: Boolean     = os.startsWith("mac")

  def simpleOs(): String =
    if (windows) "windows"
    else if (linux) "linux"
    else if (mac) "mac"
    else throw new RuntimeException(s"Unknown OS: $os")
}
