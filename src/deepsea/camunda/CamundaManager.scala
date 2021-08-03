package deepsea.camunda

import akka.actor.Actor
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.CamundaManagerStarted
import deepsea.auth.AuthManager.User
import deepsea.camunda.CamundaManager.{GetIssuesForUser, InitIssueInstance, ProcessIssueInstance, UploadModel}
import deepsea.issues.IssueManager.IssueDef
import deepsea.issues.classes.Issue
import org.apache.ibatis.logging.slf4j.Slf4jImpl
import org.camunda.bpm.engine._
import org.camunda.bpm.engine.runtime.ProcessInstance
import org.camunda.bpm.model.bpmn.Bpmn
import org.slf4j.LoggerFactory

import java.io.File
import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer


object CamundaManager{
  case class Start()
  case class UploadModel()
  case class CreateProcessInstance()
  case class ChangeProcessInstance()
  case class GetProcessExecution()
  case class ProcessIssueInstance(issue: Issue)
  case class GetIssuesForUser(user: User)
  case class InitIssueInstance(user: String)
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
        .addModelInstance("diag.bpmn", Bpmn.readModelFromFile(new File("C:\\Users\\isaev\\Desktop\\diag-1627558039520.bpmn")))
        .deploy()

    case InitIssueInstance(user) =>
      val instance = runtimeService.createProcessInstanceByKey("task").setVariable("startedBy", user).execute()
      val task = taskService.createTaskQuery().processInstanceId(instance.getId).taskName("Specify Task Project and Type").singleResult()
      if (task != null){
        val variables = taskService.getVariables(task.getId)
        val taskProjects = variables.get("_taskProjects").asInstanceOf[util.ArrayList[String]].asScala.toList
        val taskTypes = variables.get("_taskTypes").asInstanceOf[util.ArrayList[String]].asScala.toList
        sender() ! IssueDef(instance.getId, taskTypes, taskProjects)
      }
    case ProcessIssueInstance(issue) =>
      var task = taskService.createTaskQuery().processInstanceId(issue.id).taskName("Specify Task Project and Type").singleResult()
      if (task != null){
        taskService.setVariable(task.getId, "_taskType", issue.taskType)
        taskService.setVariable(task.getId, "_taskProject", issue.project)
        taskService.complete(task.getId)
      }
      task = taskService.createTaskQuery().processInstanceId(issue.id).taskName("Fill Task Info").singleResult()
      if (task != null){
        issue.taskType match {
          case "IT" =>
            taskService.setVariable(task.getId, "_taskDetails", issue.details)
            taskService.setVariable(task.getId, "_taskName", issue.name)
            taskService.complete(task.getId)
          case _ => None
        }
      }
    case GetIssuesForUser(user) =>
      val issues = ListBuffer.empty[Issue]
      if (user.permissions.contains("view_all_tasks")){

      }
      else if (user.permissions.contains("view_department_tasks")){
        val proc = ListBuffer.empty[ProcessInstance]
        user.group.foreach(group => {
          proc ++= runtimeService.createProcessInstanceQuery().variableValueEquals("taskDepartment", group).list().asScala
        })
        proc.foreach(p => {
          val variables = runtimeService.getVariables(p.getId)
          issues += new Issue(p.getId, getVariable(variables,"taskStatus"), getVariable(variables, "taskProject"),
            getVariable(variables, "taskDepartment"), getVariable(variables, "startedBy"),
            getVariable(variables, "taskType"), getVariable(variables, "taskName"),
            getVariable(variables, "taskDetails"), getVariable(variables, "taskAssignedTo"))
        })
      }
      else{

      }
      sender() ! issues
    case _ => None
  }
  def getVariable(variables: util.Map[String, AnyRef], name: String): String ={
    if (!variables.isEmpty && variables.get(name) != null){
      variables.get(name).toString
    }
    else {
      ""
    }
  }
}
