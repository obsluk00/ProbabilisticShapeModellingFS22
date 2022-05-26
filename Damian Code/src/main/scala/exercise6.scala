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
 
object Exercise6 extends App {

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

  scalismo.initialize()

  val measurements : Seq[Measurements] = readData(new File("Resources/statureAndSex.csv"))
  val data : Seq[(Double, Boolean)] = measurements.map(measurement => if(measurement.sex.equals(" f")){(measurement.trochanterDistance, true)}else{(measurement.trochanterDistance, false)})

  implicit val rng: scalismo.utils.Random = scalismo.utils.Random(42)

// We need this line to seed breeze's random number generator
  implicit val randBasisBreeze: breeze.stats.distributions.RandBasis = rng.breezeRandBasis

  val a = 0.2
  val b = 4
  val sigma2 = 0.5
  val errorDist = breeze.stats.distributions.Gaussian(0, sigma2)(rng.breezeRandBasis)

  case class Parameters(a: Double, b: Double, sigma2: Double)

  implicit object tuple3ParameterConversion
      extends ParameterConversion[Tuple3[Double, Double, Double], Parameters] {
    def from(p: Parameters): Tuple3[Double, Double, Double] = (p.a, p.b, p.sigma2)
    def to(t: Tuple3[Double, Double, Double]): Parameters =
      Parameters(a = t._1, b = t._2, sigma2 = t._3)
  }
  case class LikelihoodEvaluator(data: Seq[(Double, Boolean)])
      extends MHDistributionEvaluator[Parameters] {

    override def logValue(theta: MHSample[Parameters]): Double = {

      val likelihoods = for ((x, y) <- data) yield {
        var tmp : Double = theta.parameters.a * x + theta.parameters.b
        tmp = 1 / (1 + math.exp(-tmp))
        val likelihood = breeze.stats.distributions.Bernoulli(tmp)
        likelihood.probabilityOf(y) 
      }
      likelihoods.product
    }
  }

  object PriorEvaluator extends MHDistributionEvaluator[Parameters] {

    val priorDistA = breeze.stats.distributions.Gaussian(0, 1)
    val priorDistB = breeze.stats.distributions.Gaussian(0, 3)
    val priorDistSigma = breeze.stats.distributions.LogNormal(0, 0.25)

    override def logValue(theta: MHSample[Parameters]): Double = {
      priorDistA.logPdf(theta.parameters.a)
      +priorDistB.logPdf(theta.parameters.b)
      +priorDistSigma.logPdf(theta.parameters.sigma2)
    }
  }
  
  val posteriorEvaluator = ProductEvaluator(PriorEvaluator, LikelihoodEvaluator(data))

  val genA = GaussianRandomWalkProposal(0.001, "rw-a-0.1").forType[Double] 
  val genB = GaussianRandomWalkProposal(0.01007, "rw-b-0.5").forType[Double]
  val genSigma = GaussianRandomWalkProposal(0.01, "rw-sigma-0.01").forType[Double]

  val parameterGenerator = MHProductProposal(genA, genB, genSigma).forType[Parameters]

  val identProposal = MHIdentityProposal.forType[Double]
  val noiseOnlyGenerator =
    MHProductProposal(identProposal, identProposal, genSigma).forType[Parameters]

  val mixtureGenerator = MHMixtureProposal((0.1, noiseOnlyGenerator), (0.9, parameterGenerator))
  val chain = MetropolisHastings(mixtureGenerator, posteriorEvaluator)

  val logger = MHSampleLogger[Parameters]()
  val initialSample = MHSample(Parameters(0, 0, 1.0), generatedBy = "initial")

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
  val meanAndVarianceSigma2 = meanAndVariance(samples.map(_.parameters.sigma2))
  println(
    s"Estimates for parameter sigma2: mean = ${meanAndVarianceSigma2.mean}, var = ${meanAndVarianceSigma2.variance}"
  )
  println("Acceptance ratios: " + logger.samples.acceptanceRatios)
  println(
    "acceptance ratios over the last 100 samples: " + logger.samples.takeLast(100).acceptanceRatios
  )
}
