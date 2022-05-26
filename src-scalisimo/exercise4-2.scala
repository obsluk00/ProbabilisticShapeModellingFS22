val files = new File("../datasets/project-data/mega-aligned-meshes/").listFiles
val dataset = files.map{f => MeshIO.readMesh(f).get}


val reference = dataset.head

show(reference, "reference")

val defFields :IndexedSeq[DiscreteVectorField[_3D,_3D]] = dataset.tail.map{ m =>
  val deformationVectors = reference.pointIds.map{ id : PointId =>
    m.point(id) - reference.point(id)
  }.toIndexedSeq

  DiscreteVectorField(reference, deformationVectors)
}

val continousFields = defFields.map(f => f.interpolateNearestNeighbor)
val gp = DiscreteLowRankGaussianProcess.createUsingPCA(reference, continousFields)
val lowRankGP = gp.interpolateNearestNeighbor

def transformFromDefField(defField : DiscreteVectorField[_3D, _3D]) =   (pt : Point[_3D]) => {
  pt+defField(reference.findClosestPoint(pt).id)
}

val sample = lowRankGP.sampleAtPoints(reference)
show(sample, "sample")
show(reference.transform(transformFromDefField(sample)), "sampleMesh")
show(gp.mean, "GPMean")

val model = StatisticalMeshModel(reference, lowRankGP)
show(model, "model")

StatismoIO.writeStatismoMeshModel(model, new File("datasets/project-data/Model4-2.h5"))