//Load the model
val model = StatismoIO.readStatismoMeshModel(new File("datasets/project-data/Model4-2.h5")).get

// point IDs of the landmarks on the model
val pointIDs = IndexedSeq(new PointId(872), new PointId(1307), new PointId(1527), new PointId(3328), new PointId(3785), new PointId(2509))

//marginal model over landmarks only
val marginalModel = model.marginal(pointIDs)

// Class to store the measurements
case class Measurement(id : String, length : Double, width : Double)

// create empty sequence which will store measurements
var ourMeasurements = Seq[Measurement]()

// sample new landmarks, calculate distances, add them to the sequence
(0 until 50).foreach{i :Int =>
  val sample = marginalModel.sample
  val iterator = sample.points
  val samplePoints = (0 until 6).map{i => iterator.next}
  val length : Vector[_3D] = samplePoints(2) - samplePoints(5)
  val width : Vector [_3D] = samplePoints(3) - samplePoints(4)
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
writeCSV(new File("datasets/project-data/distancesModel4-2.csv"), ourMeasurements)
