//Load the reference mesh
val reference = MeshIO.readMesh(new File("datasets/project-data/reference-mesh/reference.stl")).get
show(reference,"reference")

//Initialize mean of GP as zero mean
val zeroMean = VectorField(RealSpace[_3D], (pt:Point[_3D]) => Vector(0,0,0))

//Build the covariance matrix using a finite rank representation since the resolution of the given mesh is too high (OutOfMemoryError)

//TODO: play around with kernel for better results
def MyKernel(ker : PDKernel[_3D]) : MatrixValuedPDKernel[_3D,_3D] = {
   val k1 = DiagonalKernel(ker) 
   k1
}

val s :Double = 10.0
val l :Double = 10.0 

val simGauss = MyKernel(GaussianKernel[_3D](l) * s)

val gp = GaussianProcess(zeroMean, simGauss)

val sampler = RandomMeshSampler3D(
  reference, 
  numberOfPoints = 300, 
  seed = 42)

val lowRankGP = LowRankGaussianProcess.approximateGP(gp, sampler, numBasisFunctions = 100)

//creates mesh from deformationfield
def transformFromDefField(defField : DiscreteVectorField[_3D, _3D]) =   (pt : Point[_3D]) => {
  pt+defField(reference.findClosestPoint(pt).id)
}

//Take and Visualize samples
remove("sampleField")
remove("sampleMesh")
val sample = lowRankGP.sampleAtPoints(reference)
show(sample, "sampleField")
show(reference.transform(transformFromDefField(sample)), "sampleMesh")
