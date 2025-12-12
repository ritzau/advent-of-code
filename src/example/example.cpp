#include <iostream>
#include <workspace_status.h>

int main() {
    std::cout << "SHA: " << workspace::git_commit_sha << std::endl;
    std::cout << "Branch: " << workspace::git_branch << std::endl;
    std::cout << "Title: " << workspace::git_commit_title << std::endl;
    std::cout << "Body: " << workspace::git_commit_body << std::endl;
    std::cout << "Time: " << workspace::git_commit_time << std::endl;
    std::cout << "Tags: " << workspace::git_tags << std::endl;
    std::cout << "Status: " << workspace::git_is_dirty << std::endl;

    std::cout << "Label: " << workspace::build_embed_label << std::endl;
    std::cout << "Host: " << workspace::build_host << std::endl;
    std::cout << "Timestamp: " << workspace::build_timestamp << std::endl;
    std::cout << "User: " << workspace::build_user << std::endl;
    std::cout << "Date: " << workspace::formatted_date << std::endl;
}
