//Load the reference mesh
val reference = MeshIO.readMesh(new File("datasets/project-data/reference-mesh/reference.stl")).get
show(reference,"reference")

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
   k1 + k2 + (k3 * 20)
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