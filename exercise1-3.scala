// Class to store the measurements
case class Measurement(id : String, length : Double, width : Double)

// create empty sequence which will store measurements
var ourMeasurements = Seq[Measurement]()

// load the landmarks, calculate distances, add them to the sequence
(0 until 47).foreach{i :Int =>
  val landmarks = LandmarkIO.readLandmarksJson[_3D](new File("datasets/project-data/aligned-landmarks/" + i + ".json")).get
  val length : Vector[_3D] = landmarks(2).point - landmarks(5).point
  val width : Vector [_3D] = landmarks(3).point - landmarks(4).point
  var ourMeasurement = new Measurement(i.toString, length.norm, width.norm)
  ourMeasurements = ourMeasurements :+ ourMeasurement
}

// method to save the sequence of measurements
def writeCSV(csvFile : java.io.File, measurements :Seq[Measurement]) : Unit = {
  import java.io.PrintWriter
  val printWriter = new PrintWriter(csvFile)
  printWriter.write("id, length, width\n")
  for (measurement <- measurements) {
    printWriter.write(s"${measurement.id},${measurement.length},${measurement.width}\n")
  }
  printWriter.close()
}

// call to aforementioned method
writeCSV(new File("datasets/project-data/distances.csv"), ourMeasurements)
