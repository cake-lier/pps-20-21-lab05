package u05lab

import org.junit.jupiter.api.{Assertions, Test}
import u05lab.code.{ExamResult, ExamsManager}

class ExamsManagerTest {
    private val em = ExamsManager()

    // verifica base di ExamResultFactory
    @Test
    def testExamResultsBasicBehaviour(): Unit = { // esame fallito, non c'è voto
        Assertions.assertEquals(ExamResult.failed.kind, ExamResult.Kind.FAILED)
        Assertions.assertFalse(ExamResult.failed.evaluation.isDefined)
        Assertions.assertFalse(ExamResult.failed.cumLaude)
        Assertions.assertEquals(ExamResult.failed.toString, "FAILED")
        // lo studente si è ritirato, non c'è voto
        Assertions.assertEquals(ExamResult.retired.kind, ExamResult.Kind.RETIRED)
        Assertions.assertFalse(ExamResult.retired.evaluation.isDefined)
        Assertions.assertFalse(ExamResult.retired.cumLaude)
        Assertions.assertEquals(ExamResult.retired.toString, "RETIRED")
        // 30L
        Assertions.assertEquals(ExamResult.succeededCumLaude.kind, ExamResult.Kind.SUCCEEDED)
        Assertions.assertEquals(ExamResult.succeededCumLaude.evaluation, Option(30))
        Assertions.assertTrue(ExamResult.succeededCumLaude.cumLaude)
        Assertions.assertEquals(ExamResult.succeededCumLaude.toString, "SUCCEEDED(30L)")
        // esame superato, ma non con lode
        Assertions.assertEquals(ExamResult.succeeded(28).kind, ExamResult.Kind.SUCCEEDED)
        Assertions.assertEquals(ExamResult.succeeded(28).evaluation, Option(28))
        Assertions.assertFalse(ExamResult.succeeded(28).cumLaude)
        Assertions.assertEquals(ExamResult.succeeded(28).toString, "SUCCEEDED(28)")
    }

    // verifica eccezione in ExamResultFactory
    @Test
    def optionalTestEvaluationCantBeGreaterThan30(): Unit = {
        Assertions.assertThrows(classOf[IllegalArgumentException], () => ExamResult.succeeded(32))
    }

    @Test
    def optionalTestEvaluationCantBeSmallerThan18(): Unit = {
        Assertions.assertThrows(classOf[IllegalArgumentException], () => ExamResult.succeeded(17))
    }

    // metodo di creazione di una situazione di risultati in 3 appelli
    private def prepareExams(): Unit = {
        this.em.createNewCall("gennaio")
        this.em.createNewCall("febbraio")
        this.em.createNewCall("marzo")
        this.em.addStudentResult("gennaio", "rossi", ExamResult.failed) // rossi -> fallito
        this.em.addStudentResult("gennaio", "bianchi", ExamResult.retired) // bianchi -> ritirato
        this.em.addStudentResult("gennaio", "verdi", ExamResult.succeeded(28)) // verdi -> 28
        this.em.addStudentResult("gennaio", "neri", ExamResult.succeededCumLaude) // neri -> 30L
        this.em.addStudentResult("febbraio", "rossi", ExamResult.failed) // etc..
        this.em.addStudentResult("febbraio", "bianchi", ExamResult.succeeded(20))
        this.em.addStudentResult("febbraio", "verdi", ExamResult.succeeded(30))
        this.em.addStudentResult("marzo", "rossi", ExamResult.succeeded(25))
        this.em.addStudentResult("marzo", "bianchi", ExamResult.succeeded(25))
        this.em.addStudentResult("marzo", "viola", ExamResult.failed)
    }

    // verifica base della parte obbligatoria di ExamManager
    @Test
    def testExamsManagement(): Unit = {
        this.prepareExams()
        // partecipanti agli appelli di gennaio e marzo
        Assertions.assertEquals(this.em.getAllStudentsFromCall("gennaio"), Set("rossi", "bianchi", "verdi", "neri"))
        Assertions.assertEquals(this.em.getAllStudentsFromCall("marzo"), Set("rossi", "bianchi", "viola"))
        // promossi di gennaio con voto
        Assertions.assertEquals(this.em.getEvaluationsMapFromCall("gennaio").size, 2)
        Assertions.assertEquals(this.em.getEvaluationsMapFromCall("gennaio")("verdi"), 28)
        Assertions.assertEquals(this.em.getEvaluationsMapFromCall("gennaio")("neri"), 30)
        // promossi di febbraio con voto
        Assertions.assertEquals(this.em.getEvaluationsMapFromCall("febbraio").size, 2)
        Assertions.assertEquals(this.em.getEvaluationsMapFromCall("febbraio")("bianchi"), 20)
        Assertions.assertEquals(this.em.getEvaluationsMapFromCall("febbraio")("verdi"), 30)
        // tutti i risultati di rossi (attenzione ai toString!!)
        Assertions.assertEquals(this.em.getResultsMapFromStudent("rossi").size, 3)
        Assertions.assertEquals(this.em.getResultsMapFromStudent("rossi")("gennaio"), "FAILED")
        Assertions.assertEquals(this.em.getResultsMapFromStudent("rossi")("febbraio"), "FAILED")
        Assertions.assertEquals(this.em.getResultsMapFromStudent("rossi")("marzo"), "SUCCEEDED(25)")
        // tutti i risultati di bianchi
        Assertions.assertEquals(this.em.getResultsMapFromStudent("bianchi").size, 3)
        Assertions.assertEquals(this.em.getResultsMapFromStudent("bianchi")("gennaio"), "RETIRED")
        Assertions.assertEquals(this.em.getResultsMapFromStudent("bianchi")("febbraio"), "SUCCEEDED(20)")
        Assertions.assertEquals(this.em.getResultsMapFromStudent("bianchi")("marzo"), "SUCCEEDED(25)")
        // tutti i risultati di neri
        Assertions.assertEquals(this.em.getResultsMapFromStudent("neri").size, 1)
        Assertions.assertEquals(this.em.getResultsMapFromStudent("neri")("gennaio"), "SUCCEEDED(30L)")
    }

    // verifica del metodo ExamManager.getBestResultFromStudent
    @Test
    def optionalTestExamsManagement(): Unit = {
        this.prepareExams()
        // miglior voto acquisito da ogni studente, o vuoto..
        Assertions.assertEquals(this.em.getBestResultFromStudent("rossi"), Option(25))
        Assertions.assertEquals(this.em.getBestResultFromStudent("bianchi"), Option(25))
        Assertions.assertEquals(this.em.getBestResultFromStudent("neri"), Option(30))
        Assertions.assertEquals(this.em.getBestResultFromStudent("viola"), Option.empty)
    }


    @Test
    def optionalTestCantCreateACallTwice(): Unit = {
        this.prepareExams()
        Assertions.assertThrows(classOf[IllegalArgumentException], () => this.em.createNewCall("marzo"))
    }

    @Test
    def optionalTestCantRegisterAnEvaluationTwice(): Unit = {
        this.prepareExams()
        Assertions.assertThrows(classOf[IllegalArgumentException],
                                () => this.em.addStudentResult("gennaio", "verdi", ExamResult.failed))
    }
}
