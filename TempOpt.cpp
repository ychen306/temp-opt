#include "clang/AST/AST.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;
using namespace clang;
using namespace clang::tooling;

// Option category for this tool
static cl::OptionCategory ToolCategory("temp-opt options");

// ========== The visitor (no-op) ==========
class TemplateVisitor : public RecursiveASTVisitor<TemplateVisitor> {
public:
  explicit TemplateVisitor(ASTContext &Ctx) : Context(Ctx) {}

  // Example of a no-op Visit method (you can add your own)
  // Return true to continue traversal, false to stop.
  bool VisitFunctionDecl(FunctionDecl *FD) {
    errs()  << "Found function: " << FD->getNameAsString() << "\n";
    return true;
  }

  // You can add more Visit*/Traverse* methods here later.

  // An entrypoint to start traversal on a declaration
  void run() {
    TranslationUnitDecl *TU = Context.getTranslationUnitDecl();
    // Start recursive traversal from the translation unit.
    TraverseDecl(TU);
  }

private:
  ASTContext &Context;
};

// ========== ASTConsumer that runs the visitor ==========
class MyASTConsumer : public ASTConsumer {
public:
  explicit MyASTConsumer(ASTContext &Ctx) : Visitor(Ctx) {}

  // Called by Clang when the AST for a translation unit is ready.
  void HandleTranslationUnit(ASTContext &Ctx) override {
    // Kick off the traversal. Visitor is currently a no-op.
    Visitor.run();
  }

private:
  TemplateVisitor Visitor;
};

// ========== FrontendAction to create the consumer ==========
class MyFrontendAction : public ASTFrontendAction {
public:
  std::unique_ptr<ASTConsumer>
  CreateASTConsumer(CompilerInstance &CI, StringRef /*InFile*/) override {
    return std::make_unique<MyASTConsumer>(CI.getASTContext());
  }
};

// ========== main: Set up CommonOptionsParser + ClangTool ==========
int main(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, ToolCategory);
  if (!ExpectedParser) {
    errs() << "Error creating CommonOptionsParser: "
                 << toString(ExpectedParser.takeError()) << "\n";
    return 1;
  }
  CommonOptionsParser &OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  // Run the tool with our frontend action
  int result = Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
  if (result != 0) {
    errs() << "Tool execution failed with code " << result << "\n";
  }
  return result;
}
