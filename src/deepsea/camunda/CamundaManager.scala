package deepsea.camunda

import akka.actor.Actor
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.CamundaManagerStarted
import deepsea.auth.AuthManager.User
import deepsea.camunda.CamundaManager._
import deepsea.issues.IssueManager.IssueDef
import deepsea.issues.classes.{Issue, IssueMessage, VarMap}
import org.apache.ibatis.logging.slf4j.Slf4jImpl
import org.camunda.bpm.engine._
import org.camunda.bpm.engine.runtime.ProcessInstance
import org.camunda.bpm.model.bpmn.Bpmn
import org.slf4j.LoggerFactory
import play.api.libs.json.Json

import java.io.File
import java.util
import java.util.Date
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer


object CamundaManager{
  case class Start()
  case class UploadModel()
  case class CreateProcessInstance()
  case class ChangeProcessInstance()
  case class GetProcessExecution()
  case class ProcessIssueInstance(issue: Issue)
  case class RemoveIssueInstance(id: String)
  case class GetIssuesForUser(user: User)
  case class InitIssueInstance(user: String)
  case class GetIssueInstanceDetails(id: String, user: User, userMessages: ListBuffer[IssueMessage])
  case class SetIssueInstanceStatus(id: String, user: String, status: String)
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
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)

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
      val instance = runtimeService.createProcessInstanceByKey("task")
        .setVariable("startedBy", user)
        .setVariable("startedDate", new Date().getTime.toString)
        .execute()
      val task = taskService.createTaskQuery().processInstanceId(instance.getId).taskName("Specify Task Project and Type").singleResult()
      if (task != null){
        val variables = taskService.getVariables(task.getId)
        val taskProjects = variables.get("$taskProjects").asInstanceOf[util.ArrayList[String]].asScala.toList
        val taskTypes = variables.get("$taskTypes").asInstanceOf[util.ArrayList[String]].asScala.toList
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
            taskService.setVariable(task.getId, "fileAttachments", Json.toJson(issue.fileAttachments).toString())
            taskService.complete(task.getId)
          case _ => None
        }
      }
      sender() ! "success"
    case GetIssuesForUser(user) =>
      val issues = ListBuffer.empty[Issue]

      val process =
        runtimeService.createProcessInstanceQuery().variableValueEquals("assignedTo", user.login).list().asScala ++
        runtimeService.createProcessInstanceQuery().variableValueEquals("startedBy", user.login).list().asScala

      process.foreach(p => {
        val variables = runtimeService.getVariables(p.getId)
        issues += new Issue(p.getId, getVariable(variables,"taskStatus"), getVariable(variables, "taskProject"),
          getVariable(variables, "taskDepartment"), getVariable(variables, "startedBy"), getVariableLong(variables, "startedDate"),
          getVariable(variables, "taskType"), getVariable(variables, "taskName"),
          getVariable(variables, "taskDetails"), getVariable(variables, "taskAssignedTo"))
      })

      if (user.permissions.contains("view_all_tasks")){
        val process = runtimeService.createProcessInstanceQuery().list().asScala
        process.filter(x => !issues.map(issue => issue.id).contains(x.getId)).foreach(p => {
          val variables = runtimeService.getVariables(p.getId)
          issues += new Issue(p.getId, getVariable(variables,"taskStatus"), getVariable(variables, "taskProject"),
            getVariable(variables, "taskDepartment"), getVariable(variables, "startedBy"), getVariableLong(variables, "startedDate"),
            getVariable(variables, "taskType"), getVariable(variables, "taskName"),
            getVariable(variables, "taskDetails"), getVariable(variables, "taskAssignedTo"))
        })
      }
      else if (user.permissions.contains("view_department_tasks")){
        val process = ListBuffer.empty[ProcessInstance]
        user.group.foreach(group => {
          process ++= runtimeService.createProcessInstanceQuery().variableValueEquals("taskDepartment", group).list().asScala
        })
        process.filter(x => !issues.map(issue => issue.id).contains(x.getId)).foreach(p => {
          val variables = runtimeService.getVariables(p.getId)
          issues += new Issue(p.getId, getVariable(variables,"taskStatus"), getVariable(variables, "taskProject"),
            getVariable(variables, "taskDepartment"), getVariable(variables, "startedBy"), getVariableLong(variables, "startedDate"),
            getVariable(variables, "taskType"), getVariable(variables, "taskName"),
            getVariable(variables, "taskDetails"), getVariable(variables, "taskAssignedTo"))
        })
      }
      sender() ! issues
    case GetIssueInstanceDetails(id, user, userMessages) =>
      var issue: Issue = null
      val process = runtimeService.createProcessInstanceQuery().processInstanceId(id).list().asScala
      if (process.nonEmpty){
        val p = process.head
        val variables = runtimeService.getVariables(p.getId)
        issue = new Issue(p.getId, getVariable(variables,"taskStatus"), getVariable(variables, "taskProject"),
          getVariable(variables, "taskDepartment"), getVariable(variables, "startedBy"), getVariableLong(variables, "startedDate"),
          getVariable(variables, "taskType"), getVariable(variables, "taskName"),
          getVariable(variables, "taskDetails"), getVariable(variables, "taskAssignedTo"))
        val history = historyService.createHistoricActivityInstanceQuery().processInstanceId(id).finished().list().asScala
        history.foreach(h => {
          h.getActivityType match {
            case "userTask" =>
              val varMap = ListBuffer.empty[VarMap]
              historyService.createHistoricVariableInstanceQuery().activityInstanceIdIn(h.getId).list().asScala.filterNot(_.getName.head == '$').foreach(h => {
                val varMapValue = new VarMap(h.getName, h.getValue.toString)

                val hBefore = historyService.createHistoricActivityInstanceQuery().finishedBefore(h.getCreateTime).list().asScala
                if (hBefore.nonEmpty){
                  val vBefore = historyService.createHistoricVariableInstanceQuery().variableName(h.getName).activityInstanceIdIn(hBefore.map(x => x.getId).toList:_*).list().asScala
                  if (vBefore.nonEmpty){
                    varMapValue.prevValue = vBefore.head.getValue.toString
                  }
                }
                varMap += varMapValue
              })
              issue.messages += new IssueMessage("camunda", h.getAssignee, h.getActivityName, h.getStartTime.getTime){
                variables ++= varMap
              }
            case _ => None
          }
        })
        val tasks = taskService.createTaskQuery().processInstanceId(p.getId).active().list().asScala
        if (tasks.nonEmpty){
          val task = tasks.head
          var allow = false
          if (task.getAssignee == user.login){
            allow = true
          }
          else{
            val candidates = taskService.getIdentityLinksForTask(task.getId).asScala.toList
            if (candidates.nonEmpty){
              candidates.foreach(x => {
                if (user.group.contains(x.getGroupId) || x.getUserId == user.login){
                  allow = true
                }
              })
            }
          }
          if (allow){
            issue.availableStatuses ++= getVariableList(taskService.getVariables(task.getId), "$taskStatuses")
          }
        }
        issue.messages ++= userMessages
      }
      sender() ! issue
    case SetIssueInstanceStatus(id, user, status) =>
      val tasks = taskService.createTaskQuery().processInstanceId(id).active().list().asScala
      if (tasks.nonEmpty){
        val task = tasks.head
        taskService.setVariable(task.getId, "_taskStatus", status)
        if (user != ""){
          taskService.setVariable(task.getId, "_assignedTo", user)
        }
        taskService.complete(task.getId)
      }
    case RemoveIssueInstance(id) =>
      runtimeService.deleteProcessInstance(id, "purge")
      sender() ! "success"
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
  def getVariableLong(variables: util.Map[String, AnyRef], name: String): Long ={
    if (!variables.isEmpty && variables.get(name) != null){
      try{
        variables.get(name).toString.toLong
      }
      catch {
        case e: Throwable => 0
      }
    }
    else {
      0
    }
  }
  def getVariableList(variables: util.Map[String, AnyRef], name: String): List[String] ={
    if (!variables.isEmpty && variables.get(name) != null){
      try{
        variables.get(name).asInstanceOf[util.ArrayList[String]].asScala.toList
      }
      catch {
        case e: Throwable => List.empty[String]
      }
    }
    else {
      List.empty[String]
    }
  }
}
