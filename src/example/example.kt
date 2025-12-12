import workspace.WorkspaceStatus

fun main() {
  println("SHA: ${WorkspaceStatus.gitCommitSha}")
  println("Branch: ${WorkspaceStatus.gitBranch}")
  println("Tags: ${WorkspaceStatus.gitTags}")
  println("Body: ${WorkspaceStatus.gitCommitBody}")
}
