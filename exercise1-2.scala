// Load the reference shape and its landmarks

val referenceMesh : TriangleMesh = MeshIO.readMesh(new File("datasets/project-data/reference-mesh/reference.stl")).get
show(referenceMesh, "reference")

val referenceLandmarks = LandmarkIO.readLandmarksJson[_3D](new File("datasets/project-data/reference-landmarks/reference.json")).get
addLandmarksTo(referenceLandmarks, "reference")

// for every sample we take the mesh and landmarks, find the best transformation for the landmarks, 
// transform the mesh and landmarks then save the results

(0 until 47).foreach{i :Int =>
  val mesh : TriangleMesh = MeshIO.readMesh(new File("datasets/project-data/meshes/" + i + ".stl")).get
  val landmarks = LandmarkIO.readLandmarksJson[_3D](new File("datasets/project-data/landmarks/" + i + ".json")).get
  val bestTransform = LandmarkRegistration.rigid3DLandmarkRegistration(landmarks, referenceLandmarks)
  val alignedLandmarks = landmarks.map{l => bestTransform(l.point)}
  val alignedMesh = mesh.transform(bestTransform)
  LandmarkIO.writeLandmarksJson(landmarks, new java.io.File("datasets/project-data/aligned-landmarks/" + i + ".json")).get
  MeshIO.writeMesh(alignedMesh, new File("datasets/project-data/aligned-meshes/" + i + ".stl")).get
}