package intersection

import java.io.{BufferedWriter, FileNotFoundException, FileWriter, IOException, FileReader, BufferedReader}
import scala.collection.mutable.Buffer


object Writer {

def writeToFile(fileName: String, carCrash: Boolean) = {

  val duration = SimulationApp.seconds.toString
  val vehicleCount = Intersection.carsThroughIntersection

  val data: Array[String] = {
            if(carCrash) {
             Array(
                "Summary:\n",
                "Simulation ran for " + duration + " " + "second(s).\n",
                "A total of " + vehicleCount + " vehicles drove through the intersection.\n",
                "Car crash happened at position (" + Intersection.wreck.head.place.x.toInt + ", " + Intersection.wreck.head.place.y.toInt +  ").\n",
                "Two vehicles crashed into each other.\n"
              )
          } else {
             Array(
               "Summary:\n",
               "Simulation ran for " + duration + "second(s).\n",
               "A total of " + vehicleCount + " vehicles drove through the intersection.\n",
               "Simulation ended successfully."
             )
           }
  }

  try {
      val fw = new FileWriter(fileName)
      val buffWriter = new BufferedWriter(fw)

      try {
        for(str <- data) {
          buffWriter.write(str)
          buffWriter.newLine()
        }
      } finally {
        buffWriter.close()
      }
    } catch {
      case e: FileNotFoundException =>
        println("File not found");
      case ioe: IOException => println("Issue with IO.");
      case _: Throwable => println("Unexpected Exception");
    }
  }

 val parametersFromFile: Buffer[String] = Buffer()

 def readMyFile(sourceFile: String): Unit = {
   val myFileReader = try {
      new FileReader(sourceFile);
   } catch {
     case e: FileNotFoundException =>
        println("File not found")
      return
   }

   val lineReader = new BufferedReader(myFileReader)

   try {
      var inputLine = lineReader.readLine()

      while (inputLine != null) {
        parametersFromFile += inputLine
        inputLine = lineReader.readLine()
      }
    } catch {
      case e: IOException =>
        println("Reading finished with error")
    }
  }

 def parseParameters: Buffer[Int] = {
   val parameters: Buffer[Int] = Buffer()
    if(parametersFromFile.size == 2) {
        for(oneLine <- parametersFromFile) {
            val temporary = oneLine.split(":")
            if(temporary.size == 2) {
               val parameter = temporary(1).trim
               if(parameter == "On")
                   parameters += 1
               if(parameter == "Off")
                   parameters += 0
               if(parameter(0).isDigit) {
                   parameters += parameter.toInt
               }
            } else {
                println("Incorrect parameter type in data")
              }
        }
      parameters
    } else {
     println("Incorrect parameter type in data")
     Buffer()
    }
 }
}
