// Crossvalidation script

// load ICP aligned meshes
val files = new File("datasets/project-data/mega-aligned-meshes/").listFiles
val data = files.map{f => MeshIO.readMesh(f).get}

// define data object containing training data and test data
case class dataset(trainingData: Array[scalismo.mesh.TriangleMesh], testData: Array[scalismo.mesh.TriangleMesh])

// Since we have 47 data points we can not perform k-fold crossvalidation. instead we use 9 different sets leaving out 5 bones each
val datasets = (0 until 9).map{i => dataset(data.diff(data.slice(i * 5, (i + 1) * 5)), data.slice(i * 5, (i + 1) * 5))}

// function training a new model given trainingdata as in exercise 4.2
def trainModel(dataset: Array[scalismo.mesh.TriangleMesh]): StatisticalMeshModel = {
  val reference = dataset.head
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
  val model = StatisticalMeshModel(reference, lowRankGP)
  model
}

// function validating given model using given testdata
case class testResult(avgDistance: Double, hausdorffDistance: Double)

def testModel(model: StatisticalMeshModel, testData: Array[scalismo.mesh.TriangleMesh]): Array[testResult] = {
    testData.map(mesh => testResult(scalismo.mesh.MeshMetrics.avgDistance(mesh, model.project(mesh)), scalismo.mesh.MeshMetrics.hausdorffDistance(mesh, model.project(mesh))))
}

// train models then validate them
def testResults = datasets.map(set => testModel(trainModel(set.trainingData), set.testData))

(0 until 9).foreach{i : Int =>
  println("Results of fold " + i)
  val result = testResults(i)
  println("Average Distances: ")
  result.foreach{i : testResult => println(i.avgDistance)}
    println("Hausdorff Distances: ")
  result.foreach{i : testResult => println(i.avgDistance)}
}