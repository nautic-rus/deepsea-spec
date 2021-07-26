package deepsea.camunda

import org.camunda.bpm.engine.delegate.{DelegateExecution, JavaDelegate}

class TestExecute extends JavaDelegate{
  override def execute(execution: DelegateExecution): Unit = {
    val qwe = 0
    val jk = 0
    println("ITS HERE EXECUTED //////////////*********************************////////////")
  }
}
