// See https://developer.hashicorp.com/terraform/tutorials/it-saas/github-user-teams for support
resource "github_repository" "spremuta" {
  name        = "spremuta"
  // full_name   = "smelc/spremuta"
  description = "Automation for GitHub"

  visibility = "public"

  // github_branch_default = "main"
}
