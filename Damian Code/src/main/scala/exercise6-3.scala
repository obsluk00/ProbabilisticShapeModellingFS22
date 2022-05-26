//> using scala "2.13"
//> using lib "ch.unibas.cs.gravis::scalismo-ui:0.91-RC3"

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

object exercise63 extends App {

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

    val priorDistA = breeze.stats.distributions.Gaussian(0.1, 0.1)
    val priorDistB = breeze.stats.distributions.Gaussian(1.5, 0.2)

    override def logValue(theta: MHSample[Parameters]): Double = {
      priorDistA.logPdf(theta.parameters.a)
      +priorDistB.logPdf(theta.parameters.b)
    }
  }
  val posteriorEvaluator = ProductEvaluator(PriorEvaluator, LikelihoodEvaluator(data))

  val genA = GaussianRandomWalkProposal(0.001, "rw-a-0.05").forType[Double]
  val genB = GaussianRandomWalkProposal(0.01, "rw-b-0.05").forType[Double]

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

  println("Acceptance ratios: " + logger.samples.acceptanceRatios)
  println(
    "acceptance ratios over the last 100 samples: " + logger.samples.takeLast(100).acceptanceRatios
  )
}
