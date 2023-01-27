package intersection

class Exceptions(text: String) extends Exception(text)

case class CarWreckException(text: String) extends Exceptions(text)

case class FileNotFoundException(text: String) extends Exceptions(text)

case class IOException(text: String) extends Exceptions(text)
