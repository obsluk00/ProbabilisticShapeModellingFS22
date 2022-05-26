val model = StatismoIO.readStatismoMeshModel(new File("datasets/project-data/LowRankGPModel.h5")).get
show(model, "model")

val landmarks = LandmarkIO.readLandmarksJson[_3D](new File("datasets/project-data/reference-landmarks/reference.json")).get
addLandmarksTo(landmarks, "model")

def attributeCorrespondences(target : TriangleMesh, pts : Seq[Point[_3D]]) : Seq[Point[_3D]] = {
  pts.map{pt => target.findClosestPoint(pt).point}
}

val pointSamples = UniformMeshSampler3D(model.mean, 5000, 42).sample.map(s => s._1)
val pointIds = pointSamples.map{s => model.mean.findClosestPoint(s).id}
val initialPositions = pointIds.map(id => model.mean.point(id))

val littleNoise = NDimensionalNormalDistribution(Vector(0,0,0), SquareMatrix((1f,0,0), (0,1f,0), (0,0,1f)))

def fitModel(pointIds: IndexedSeq[PointId],candidateCorresp: Seq[Point[_3D]]) :TriangleMesh = {
  val trainingData = (pointIds zip candidateCorresp).map{ case (mId, pPt) =>
    (mId, pPt, littleNoise)
  }
  val posterior = model.posterior(trainingData.toIndexedSeq)
  posterior.mean
}

def recursion(target : TriangleMesh, currentPoints : Seq[Point[_3D]], nbIterations : Int) : TriangleMesh= {

  val candidates = attributeCorrespondences(target, currentPoints)
  val fit = fitModel(pointIds, candidates)

  val newPoints= pointIds.map(id => fit.point(id))
  if(nbIterations> 0) {
    recursion(target, newPoints, nbIterations - 1)
  } else {
    fit
  }
}

//TODO: calculate fitted/registered landmarks
(0 until 47).foreach{i :Int =>
  val mesh : TriangleMesh = MeshIO.readMesh(new File("datasets/project-data/aligned-meshes/" + i + ".stl")).get
  //val landmarks = LandmarkIO.readLandmarksJson[_3D](new File("datasets/project-data/aligned-landmarks/" + i + ".json")).get
  val initialPositions = pointIds.map(id => model.mean.point(id))

  val alignedMesh = recursion(mesh, initialPositions, 20)

  //val alignedLandmarks = landmarkIds.map{id => Landmark("L_"+id, alignedMesh.point(PointId(id))) }
  //LandmarkIO.writeLandmarksJson(landmarks, new java.io.File("datasets/project-data/mega-aligned-landmarks/" + i + ".json")).get
  MeshIO.writeMesh(alignedMesh, new File("datasets/project-data/mega-aligned-meshes/" + i + ".stl")).get
}
