// Type Scala code here.
// Press 'Shift + Enter' to execute selected text
// or current line.// Type Scala code here.
// Press 'Shift + Enter' to execute selected text

val files = new File("datasets/nonAlignedFaces/").listFiles
val dataset = files.map{f => MeshIO.readMesh(f).get}


val reference = dataset.head

show(reference, "reference")

val toAlign : IndexedSeq[TriangleMesh] = dataset.tail

val pIds = IndexedSeq(2214, 6341, 10008, 14129, 8156, 47775)
val refLandmarks = pIds.map{id => Landmark("L_"+id, reference.point(PointId(id))) }

val alignedSet = toAlign.map { mesh => 
  val landmarks = pIds.map{ id => Landmark("L_"+id, mesh.point(PointId(id))) }
  val rigidTrans = LandmarkRegistration.rigid3DLandmarkRegistration(landmarks, refLandmarks)
  mesh.transform(rigidTrans)
}
//(0 until alignedSet.size).foreach{i => show(alignedSet(i), "face_"+i)}

val defFields :IndexedSeq[DiscreteVectorField[_3D,_3D]] = alignedSet.map{ m => 
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