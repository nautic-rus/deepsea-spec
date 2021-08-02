package deepsea.camunda

import akka.actor.Actor
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.CamundaManagerStarted
import deepsea.camunda.CamundaManager.{CreateProcessInstance, UploadModel}
import org.apache.ibatis.logging.slf4j.Slf4jImpl
import org.camunda.bpm.engine._
import org.camunda.bpm.model.bpmn.Bpmn
import org.slf4j.LoggerFactory

import java.io.File
import scala.jdk.CollectionConverters.CollectionHasAsScala


object CamundaManager{
  case class Start()
  case class UploadModel()
  case class CreateProcessInstance()
  case class ChangeProcessInstance()
  case class GetProcessExecution()
}
class CamundaManager extends Actor {
  private val log = LoggerFactory.getLogger(classOf[Slf4jImpl])
  private var engine: ProcessEngine = _
  private var repositoryService: RepositoryService = _
  private var runtimeService: RuntimeService = _
  private var taskService: TaskService = _
  private var identityService: IdentityService = _
  private var formService: FormService = _
  private var historyService: HistoryService = _
  
  override def preStart(): Unit = {
    engine = ProcessEngineConfiguration.createStandaloneProcessEngineConfiguration()
      .setJdbcDriver("org.postgresql.Driver")
      .setDatabaseSchemaUpdate(ProcessEngineConfiguration.DB_SCHEMA_UPDATE_TRUE)
      .setJdbcUrl("jdbc:postgresql://localhost/camunda?user=camunda&password=Ship1234")
      .setJobExecutorActivate(false)
      .buildProcessEngine()
    repositoryService = engine.getRepositoryService
    runtimeService = engine.getRuntimeService
    taskService = engine.getTaskService
    identityService = engine.getIdentityService
    formService = engine.getFormService
    historyService = engine.getHistoryService
    ActorManager.startup ! CamundaManagerStarted()
  }
  override def receive: Receive = {
    case UploadModel() =>
      repositoryService.createDeployment()
        .addModelInstance("diag.bpmn", Bpmn.readModelFromFile(new File("C:\\Users\\isaev\\Desktop\\diag.bpmn")))
        .deploy()
      //self ! CreateProcessInstance()
    case CreateProcessInstance() =>
      runtimeService.startProcessInstanceByKey("create-task")
      println("imported")
//    case ChangeProcessInstance() =>
//      val history = historyService.createHistoricProcessInstanceQuery().processDefinitionKey("create-task").list()
//      val processInstance = runtimeService.createProcessInstanceQuery().processDefinitionKey("create-task").list()
//      runtimeService.createProcessInstanceModification(processInstance.getId).execute()
//      val exec = runtimeService.createExecutionQuery().processDefinitionKey("create-task").list().asScala
//      if (exec.nonEmpty){
//        val lastExec = exec.last
//        val variables: util.Map[String, AnyRef] = runtimeService.getVariables(lastExec.getId)
//        //runtimeService.setVariable(lastExec.getId, "InputTest", "SomeValue")
//        variables.replace("InputTest", "QWEAwesome")
//
//        runtimeService.signal(lastExec.getId, variables)
//      }
//      val q2 = exec

    case "get-process-status" =>
      val exec = runtimeService.createExecutionQuery().processDefinitionKey("create-task").list().asScala
      if (exec.nonEmpty){
        val lastActivity = exec.last
        val activeActivities = runtimeService.getActiveActivityIds(lastActivity.getId)
        var res = ""
        activeActivities.forEach(ac => res += ", " + ac)
        if (res.nonEmpty){
          res = res.substring(2)
        }
        sender() ! res
      }
      else{
        sender() ! "error"
      }
    case ("set-task-number", taskNumber: String) =>
      val tasks = taskService.createTaskQuery().active().taskName("Enter Task Number").list().asScala
      if (tasks.nonEmpty){
        taskService.setVariable(tasks.last.getId, "_taskNumber", taskNumber)
        taskService.complete(tasks.last.getId)
        sender() ! "success"
      }
      else{
        sender() ! "error"
      }
    case ("set-task-name", taskName: String) =>
      val tasks = taskService.createTaskQuery().active().taskName("Enter Task Name").list().asScala
      if (tasks.nonEmpty){
        taskService.setVariable(tasks.last.getId, "_taskName", taskName)
        taskService.complete(tasks.last.getId)
        sender() ! "success"
      }
      else{
        sender() ! "error"
      }
    case "executionVariable" =>
      println("**************************************************************************")
    case _ => None
  }
}
