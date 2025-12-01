#include "clang/AST/AST.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"

#define DEBUG_TYPE "temp-opt"

using namespace llvm;
using namespace clang;
using namespace clang::tooling;

// Option category for this tool
static cl::OptionCategory ToolCategory("temp-opt options");

static cl::opt<std::string>
    OutputFile("o", cl::desc("Output file"), cl::value_desc("filename"),
               cl::init(""), cl::cat(ToolCategory));

// ========== The visitor (no-op) ==========
class TemplateVisitor : public RecursiveASTVisitor<TemplateVisitor> {
public:
  explicit TemplateVisitor(ASTContext &Ctx) : Context(Ctx) {}

  // Collect all primary function templates that appear in non-system code.
  bool VisitFunctionTemplateDecl(FunctionTemplateDecl *FTD) {
    SourceLocation Loc = FTD->getLocation();
    const SourceManager &SM = Context.getSourceManager();
    if (SM.isInSystemHeader(Loc) || SM.isInSystemMacro(Loc))
      return true;

    const FunctionTemplateDecl *Canon = FTD->getCanonicalDecl();
    AllFunctionTemplates.insert(Canon);
    return true;
  }

  // Collect all primary class templates that appear in non-system code.
  bool VisitClassTemplateDecl(ClassTemplateDecl *CTD) {
    SourceLocation Loc = CTD->getLocation();
    const SourceManager &SM = Context.getSourceManager();
    if (SM.isInSystemHeader(Loc) || SM.isInSystemMacro(Loc))
      return true;

    const ClassTemplateDecl *Canon = CTD->getCanonicalDecl();
    AllClassTemplates.insert(Canon);
    return true;
  }

  // Record when a function template is actually specialized/instantiated.
  bool VisitFunctionDecl(FunctionDecl *FD) {
    return true;
  }

  // Record when a class template is specialized/instantiated.
  bool
  VisitClassTemplateSpecializationDecl(ClassTemplateSpecializationDecl *CTSD) {
    return true;
  }

  // You can add more Visit*/Traverse* methods here later.

  // An entrypoint to start traversal on a declaration
  void run() {
    TranslationUnitDecl *TU = Context.getTranslationUnitDecl();
    // Start recursive traversal from the translation unit.
    TraverseDecl(TU);
  }

  void collectUninstantiated(
      SmallVectorImpl<const FunctionTemplateDecl *> &OutFuncs,
      SmallVectorImpl<const ClassTemplateDecl *> &OutClasses) {
    for (const FunctionTemplateDecl *FTD : AllFunctionTemplates) {
      bool HasInstantiation = false;
      for (const auto *Spec : FTD->specializations()) {
        TemplateSpecializationKind K = Spec->getTemplateSpecializationKind();
        switch (K) {
        case TSK_ImplicitInstantiation:
        case TSK_ExplicitInstantiationDeclaration:
        case TSK_ExplicitInstantiationDefinition:
        case TSK_ExplicitSpecialization:
          HasInstantiation = true;
          break;
        case TSK_Undeclared:
          break;
        }
        if (HasInstantiation)
          break;
      }
      if (!HasInstantiation)
        OutFuncs.push_back(FTD);
    }

    for (const ClassTemplateDecl *CTD : AllClassTemplates) {
      bool HasInstantiation = false;
      for (auto *Spec : CTD->specializations()) {
        TemplateSpecializationKind K = Spec->getSpecializationKind();
        switch (K) {
        case TSK_ImplicitInstantiation:
        case TSK_ExplicitInstantiationDeclaration:
        case TSK_ExplicitInstantiationDefinition:
        case TSK_ExplicitSpecialization:
          HasInstantiation = true;
          break;
        case TSK_Undeclared:
          break;
        }
        if (HasInstantiation)
          break;
      }
      if (!HasInstantiation)
        OutClasses.push_back(CTD);
    }
  }

private:

  ASTContext &Context;
  llvm::SmallPtrSet<const FunctionTemplateDecl *, 32> AllFunctionTemplates;
  llvm::SmallPtrSet<const ClassTemplateDecl *, 32> AllClassTemplates;
};

// ========== ASTConsumer that runs the visitor ==========
class MyASTConsumer : public ASTConsumer {
public:
  MyASTConsumer(ASTContext &Ctx, Rewriter &R) : Visitor(Ctx), TheRewriter(R) {}

  // Called by Clang when the AST for a translation unit is ready.
  void HandleTranslationUnit(ASTContext &Ctx) override {
    // Kick off the traversal.
    Visitor.run();

    SmallVector<const FunctionTemplateDecl *, 32> UninstFuncs;
    SmallVector<const ClassTemplateDecl *, 32> UninstClasses;
    Visitor.collectUninstantiated(UninstFuncs, UninstClasses);

    const SourceManager &SM = Ctx.getSourceManager();

    LLVM_DEBUG(dbgs() << "=== Uninstantiated function templates ===\n");
    for (const FunctionTemplateDecl *FTD : UninstFuncs) {
      SourceLocation Loc = FTD->getLocation();
      FullSourceLoc FullLoc(Loc, SM);
      StringRef FileName = SM.getFilename(Loc);
      if (FileName.empty())
        FileName = "<unknown>";

      LLVM_DEBUG(dbgs() << "  " << FTD->getNameAsString() << " at "
                        << FileName << ":"
                        << FullLoc.getSpellingLineNumber() << ":"
                        << FullLoc.getSpellingColumnNumber() << "\n");
    }

    LLVM_DEBUG(dbgs() << "=== Uninstantiated class templates ===\n");
    for (const ClassTemplateDecl *CTD : UninstClasses) {
      SourceLocation Loc = CTD->getLocation();
      FullSourceLoc FullLoc(Loc, SM);
      StringRef FileName = SM.getFilename(Loc);
      if (FileName.empty())
        FileName = "<unknown>";

      LLVM_DEBUG(dbgs() << "  " << CTD->getNameAsString() << " at "
                        << FileName << ":"
                        << FullLoc.getSpellingLineNumber() << ":"
                        << FullLoc.getSpellingColumnNumber() << "\n");
    }

    if (OutputFile.empty())
      return;

    FileID MainFileID = SM.getMainFileID();

    auto removeIfInMainFile = [&](const Decl *D) {
      SourceLocation Begin = D->getBeginLoc();
      if (Begin.isInvalid())
        return;
      FileID FID = SM.getFileID(SM.getSpellingLoc(Begin));
      if (FID != MainFileID)
        return;

      SourceRange Range = D->getSourceRange();
      if (Range.isInvalid())
        return;

      TheRewriter.RemoveText(Range);
    };

    for (const FunctionTemplateDecl *FTD : UninstFuncs)
      removeIfInMainFile(FTD);
    for (const ClassTemplateDecl *CTD : UninstClasses)
      removeIfInMainFile(CTD);

    raw_ostream *OS = nullptr;
    std::unique_ptr<raw_fd_ostream> OwnedStream;

    if (OutputFile == "-") {
      OS = &outs();
    } else {
      std::error_code EC;
      auto Stream =
          std::make_unique<raw_fd_ostream>(OutputFile, EC,
                                           llvm::sys::fs::OF_None);
      if (EC) {
        errs() << "Error opening output file '" << OutputFile
               << "': " << EC.message() << "\n";
        return;
      }
      OS = Stream.get();
      OwnedStream = std::move(Stream);
    }

    if (const RewriteBuffer *RewriteBuf =
            TheRewriter.getRewriteBufferFor(MainFileID)) {
      RewriteBuf->write(*OS);
    } else {
      *OS << StringRef(SM.getBufferData(MainFileID));
    }
  }

private:
  TemplateVisitor Visitor;
  Rewriter &TheRewriter;
};

// ========== FrontendAction to create the consumer ==========
class MyFrontendAction : public ASTFrontendAction {
public:
  std::unique_ptr<ASTConsumer>
  CreateASTConsumer(CompilerInstance &CI, StringRef /*InFile*/) override {
    RewriterForFile.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<MyASTConsumer>(CI.getASTContext(), RewriterForFile);
  }

private:
  Rewriter RewriterForFile;
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
