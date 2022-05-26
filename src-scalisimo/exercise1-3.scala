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

/////////////////////////////

//Load the reference mesh
val reference = MeshIO.readMesh(new File("datasets/project-data/reference-mesh/reference.stl")).get
show(reference,"reference")

val referenceLandmarks = LandmarkIO.readLandmarksJson[_3D](new File("datasets/project-data/reference-landmarks/reference.json")).get
addLandmarksTo(referenceLandmarks, "reference")



//Initialize mean of GP as zero mean
val zeroMean = VectorField(RealSpace[_3D], (pt:Point[_3D]) => Vector(0,0,0))

//Build the covariance matrix using a finite rank representation since the resolution of the given mesh is too high (OutOfMemoryError)

//TODO: play around with kernel for better results
case class XmirroredKernel(ker : PDKernel[_3D]) extends PDKernel[_3D] {
  override def domain = RealSpace[_3D]
  override def k(x: Point[_3D], y: Point[_3D]) = ker(Point(x(0) * -1f ,x(1), x(2)), y)
}

case class MyKernel(sigma: Double) extends PDKernel[_3D] {
  override def domain = RealSpace[_3D]
  override def k(x: Point[_3D], y: Point[_3D]) = {
  exp(- (x.toVector dot y.toVector) / (2 * sigma * sigma))
  }
}

case class ElongingKernel(ker : PDKernel[_3D]) extends PDKernel[_3D] {
  override def domain = RealSpace[_3D]
  override def k(x: Point[_3D], y: Point[_3D]) = ker(Point(x(0) * 0f ,x(1) * 10f, x(2) * 0f), y)
}

def MyKernel(ker : PDKernel[_3D]) : MatrixValuedPDKernel[_3D,_3D] = {
   val xmirrored = XmirroredKernel(ker)
   val elonging = ElongingKernel(ker)
   val k1 = DiagonalKernel(ker)
   val k2 = DiagonalKernel(xmirrored * -1f, xmirrored, xmirrored)
   val k3 = DiagonalKernel(elonging * 0f, elonging * 1f, elonging * 0f)
   k1 + k2// + (k3 * 20)
}

val sigma :Double = 100.0
val s :Double = 10.0

val myKernel = new MyKernel(sigma)
val simGauss = MyKernel(myKernel * s)

val gp = GaussianProcess(zeroMean, simGauss)

val sampler = RandomMeshSampler3D(
  reference,
  numberOfPoints = 300,
  seed = 42)

val lowRankGP = LowRankGaussianProcess.approximateGP(gp, sampler, numBasisFunctions = 10)

//creates mesh from deformationfield
def transformFromDefField(defField : DiscreteVectorField[_3D, _3D]) =   (pt : Point[_3D]) => {
  pt+defField(reference.findClosestPoint(pt).id)
}

//Take and Visualize samples
remove("sampleField")
remove("sampleMesh")
val sample = lowRankGP.sampleAtPoints(reference)

//show(sample, "sampleField")
show(reference.transform(transformFromDefField(sample)), "sampleMesh")
addLandmarksTo(sampleLandmarks, "sampleMesh")