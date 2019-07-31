# ----------------------------------------------------------------------------
rstudioapi::executeCommand("activateFiles")

rstudioapi::executeCommand("newTextDoc")
rstudioapi::executeCommand("newCppDoc")
rstudioapi::executeCommand("newRNotebook")
rstudioapi::executeCommand("newRMarkdownDoc")

rstudioapi::executeCommand("openSourceDoc")
rstudioapi::executeCommand("reopenSourceDocWithEncoding")

rstudioapi::executeCommand("saveSourceDoc")
rstudioapi::executeCommand("renameSourceDoc")
rstudioapi::executeCommand("saveSourceDocAs")
rstudioapi::executeCommand("saveAllSourceDocs")


rstudioapi::executeCommand("newRNotebook")
rstudioapi::executeCommand("knitDocument")
rstudioapi::executeCommand("notebookClearAllOutput")
rstudioapi::executeCommand("notebookClearOutput")


rstudioapi::executeCommand("editRmdFormatOptions")

rstudioapi::executeCommand("showFolder")

show_folder_in_files_pane <- function(.dir) {
    old_wd <- setwd(.dir)
    rstudioapi::executeCommand("goToWorkingDir")
    setwd(old_wd)
}
