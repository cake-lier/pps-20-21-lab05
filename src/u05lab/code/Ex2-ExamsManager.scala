package u05lab.code

import u05lab.code.ExamResult.Kind.Kind

sealed trait ExamResult {
    def kind: Kind

    def evaluation: Option[Int]

    def cumLaude: Boolean
}
object ExamResult {
    object Kind extends Enumeration {
        type Kind = Value
        val RETIRED, FAILED, SUCCEEDED = Value
    }

    private case class ExamResultImpl(kind: Kind, evaluation: Option[Int], cumLaude: Boolean) extends ExamResult {
        override def toString: String = {
            kind.toString + (
                if (kind == Kind.SUCCEEDED) {
                    "(" + evaluation.get + (
                        if (cumLaude) {
                            "L"
                        } else {
                            ""
                        }
                    ) + ")"
                } else {
                    ""
                }
            )
        }
    }

    def failed: ExamResult = ExamResultImpl(Kind.FAILED, Option.empty, cumLaude = false)

    def retired: ExamResult = ExamResultImpl(Kind.RETIRED, Option.empty, cumLaude = false)

    def succeededCumLaude: ExamResult = ExamResultImpl(Kind.SUCCEEDED, Option(30), cumLaude = true)

    def succeeded(evaluation: Int): ExamResult = evaluation match {
        case v if v < 18 || v > 30 => throw new IllegalArgumentException
        case _ => ExamResultImpl(Kind.SUCCEEDED, Option(evaluation), cumLaude = false)
    }
}

sealed trait ExamsManager {
    def createNewCall(call: String): Unit

    def addStudentResult(call: String, student: String, result: ExamResult): Unit

    def getAllStudentsFromCall(call: String): Set[String]

    def getEvaluationsMapFromCall(call: String): Map[String, Int]

    def getResultsMapFromStudent(student: String): Map[String, String]

    def getBestResultFromStudent(student: String): Option[Int]
}
object ExamsManager {
    private case class ExamsManagerImpl() extends ExamsManager {
        private var calls: Map[String, Map[String, ExamResult]] = Map()

        override def createNewCall(call: String): Unit
            = if (this.calls.contains(call)) {
                throw new IllegalArgumentException
            } else {
                this.calls += call -> Map()
            }

        override def addStudentResult(call: String, student: String, result: ExamResult): Unit
            = if (this.calls(call).contains(student)) {
                throw new IllegalArgumentException
            } else {
                this.calls += call -> (this.calls(call) + (student -> result))
            }

        override def getAllStudentsFromCall(call: String): Set[String] = this.calls(call).keySet

        override def getEvaluationsMapFromCall(call: String): Map[String, Int]
            = this.calls(call).filter(_._2.evaluation.isDefined).map(e => (e._1, e._2.evaluation.get))

        override def getResultsMapFromStudent(student: String): Map[String, String]
            = this.calls.flatMap(e => e._2.filter(_._1 == student).map(c => (e._1, c._2.toString)))

        override def getBestResultFromStudent(student: String): Option[Int]
            = this.calls.values.flatten.filter(_._1 == student).map(_._2).map(_.evaluation).max
    }

    def apply(): ExamsManager = ExamsManagerImpl()
}