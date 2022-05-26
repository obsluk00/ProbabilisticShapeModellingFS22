import scalismo.sampling.MHSample
import scalismo.sampling.MHDistributionEvaluator
import scalismo.sampling.evaluators.ProductEvaluator
import scalismo.sampling.MHProposalGenerator
import scalismo.sampling.proposals.GaussianRandomWalkProposal
import scalismo.sampling.proposals.MHProductProposal
import scalismo.sampling.ParameterConversion
import scalismo.sampling.algorithms.MetropolisHastings
import scalismo.sampling.loggers.MHSampleLogger
import scalismo.sampling.proposals.MHMixtureProposal
import scalismo.sampling.proposals.MHIdentityProposal
import breeze.stats.meanAndVariance
import java.io.File
import scalismo.io.StatisticalModelIO
import scalismo.geometry._
import scalismo.common._
import scalismo.common.interpolation.TriangleMeshInterpolator3D
import scalismo.mesh._
import scalismo.io.{StatisticalModelIO, MeshIO}
import scalismo.statisticalmodel._
import scalismo.numerics.UniformMeshSampler3D
import scalismo.kernels._

import scalismo.ui.api._
 
object Exercise64 extends App {

  scalismo.initialize()
  implicit val rng: scalismo.utils.Random = scalismo.utils.Random(42)

  // We need this line to seed breeze's random number generator
  implicit val randBasisBreeze: breeze.stats.distributions.RandBasis = rng.breezeRandBasis

  // read out data
  case class Measurements(
    id: String,
    sex: String,
    stature: Double,
    lengthOfBone: Double,
    trochanterDistance: Double
  )

  def readData(csvFile: java.io.File): Seq[Measurements] = {
    val lines = scala.io.Source.fromFile(csvFile).getLines.toList
    val header = lines.head.split(",")
    lines.tail.map(line => {
      val values = line.split(",")
      Measurements(
        values(0),
        values(1),
        values(2).toDouble,
        values(3).toDouble,
        values(4).toDouble
      )
    })
  }

  // truncate data to only include
  val fullData = readData(new File("Resources/statureAndSex.csv"))

  val data = fullData.map{i => if(i.sex.equals(" f")){(i.trochanterDistance, true)}else{(i.trochanterDistance, false)}}


  case class Parameters(a: Double, b: Double)

  implicit object tuple2ParameterConversion
    extends ParameterConversion[Tuple2[Double, Double], Parameters] {
    def from(p: Parameters): Tuple2[Double, Double] = (p.a, p.b)
    def to(t: Tuple2[Double, Double]): Parameters =
      Parameters(a = t._1, b = t._2)
  }
  case class LikelihoodEvaluator(data: Seq[(Double, Boolean)])
    extends MHDistributionEvaluator[Parameters] {

    override def logValue(theta: MHSample[Parameters]): Double = {

      val likelihoods = for ((x, y) <- data) yield {
        val likelihood = breeze.stats.distributions.Bernoulli(
          1 / (1 + math.exp(-(theta.parameters.a * x + theta.parameters.b)))
        )

        likelihood.logProbabilityOf(y)
      }
      likelihoods.sum
    }
  }

  object PriorEvaluator extends MHDistributionEvaluator[Parameters] {

    val priorDistA = breeze.stats.distributions.Gaussian(0, 10)
    val priorDistB = breeze.stats.distributions.Gaussian(0, 15)

    override def logValue(theta: MHSample[Parameters]): Double = {
      priorDistA.logPdf(theta.parameters.a)
      +priorDistB.logPdf(theta.parameters.b)
    }
  }
  val posteriorEvaluator = ProductEvaluator(PriorEvaluator, LikelihoodEvaluator(data))

  val genA = GaussianRandomWalkProposal(0.08, "rw-a-0.05").forType[Double]
  val genB = GaussianRandomWalkProposal(0.9, "rw-b-0.05").forType[Double]

  val parameterGenerator = MHProductProposal(genA, genB).forType[Parameters]

  val identProposal = MHIdentityProposal.forType[Double]
  val noiseOnlyGenerator =
    MHProductProposal(identProposal, identProposal).forType[Parameters]

  val mixtureGenerator = MHMixtureProposal((0.1, noiseOnlyGenerator), (0.9, parameterGenerator))
  val chain = MetropolisHastings(mixtureGenerator, posteriorEvaluator)

  val logger = MHSampleLogger[Parameters]()
  val initialSample = MHSample(Parameters(0.0, 0.0), generatedBy = "initial")

  val mhIterator = chain.iterator(initialSample, logger)
  val samples = mhIterator.drop(1000).take(5000).toIndexedSeq
  val meanAndVarianceA = meanAndVariance(samples.map(_.parameters.a))
  println(
    s"Estimates for parameter a: mean = ${meanAndVarianceA.mean}, var = ${meanAndVarianceA.variance}"
  )
  val meanAndVarianceB = meanAndVariance(samples.map(_.parameters.b))
  println(
    s"Estimates for parameter b: mean = ${meanAndVarianceB.mean}, var = ${meanAndVarianceB.variance}"
  )

  case class SampleMeasurements(
        id: String,
        length: Double,
        width: Double
    )

  val models = StatisticalModelIO.readStatisticalTriangleMeshModel3D(new File("Resources/Models/Model4-2.h5")).get

  val pointIDs = IndexedSeq(new PointId(872), new PointId(1307), new PointId(1527), new PointId(3328), new PointId(3785), new PointId(2509))

  val marginalModel = models.marginal(pointIDs)

  var ourMeasurements = Seq[SampleMeasurements]()

  (0 until 100).foreach{i : Int => 
    val sample = marginalModel.sample
    val iterator = sample.pointSet.points
    val samplePoints = (0 until 6).map{i => iterator.next}
    val length : EuclideanVector[_3D] = samplePoints(2) - samplePoints(5)
    val width : EuclideanVector [_3D] = samplePoints(3) - samplePoints(4)
    var ourMeasurement = new SampleMeasurements(i.toString, length.norm, width.norm)
    ourMeasurements = ourMeasurements :+ ourMeasurement
  }

  def writeCSV(csvFile : java.io.File, measurements :Seq[SampleMeasurements]) : Unit = {
  import java.io.PrintWriter
  val printWriter = new PrintWriter(csvFile)
  printWriter.write("id, length, width, stature, sex\n")
  for (measurement <- measurements) {
    var sex : String = ""
    if (meanAndVarianceA.mean * measurement.width + meanAndVarianceB.mean > 0) {
      sex = "f"
    } else {
      sex = "m"
    }
    var stature : Double = 3.9899466543375772 * measurement.length + 1.312283185407452
    printWriter.write(s"${measurement.id},${measurement.length},${measurement.width}, ${stature}, ${sex}\n")
  }
  printWriter.close()
}

  writeCSV(new File("results/classifiedSamples.csv"), ourMeasurements)
}
