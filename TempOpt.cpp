#include "clang/AST/AST.h"
#include "clang/AST/Attr.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Lex/Lexer.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSet.h"
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

static bool isInStdNamespace(const DeclContext *DC) {
  const DeclContext *Cur = DC;
  while (Cur && !Cur->isTranslationUnit()) {
    if (const auto *NS = dyn_cast<NamespaceDecl>(Cur)) {
      if (!NS->isAnonymousNamespace() && NS->getName() == "std")
        return true;
    }
    Cur = Cur->getParent();
  }
  return false;
}

// ========== The visitor (no-op) ==========
class TemplateVisitor : public RecursiveASTVisitor<TemplateVisitor> {
public:
  explicit TemplateVisitor(ASTContext &Ctx) : Context(Ctx) {}

  // Collect all primary function templates that appear in non-system code.
  bool VisitFunctionTemplateDecl(FunctionTemplateDecl *FTD) {
    SourceLocation Loc = FTD->getLocation();
    const SourceManager &SM = Context.getSourceManager();
    const FunctionTemplateDecl *Canon = FTD->getCanonicalDecl();
    AllFunctionTemplates.insert(Canon);
    return true;
  }

  // Collect all primary class templates that appear in non-system code.
  bool VisitClassTemplateDecl(ClassTemplateDecl *CTD) {
    const ClassTemplateDecl *Canon = CTD->getCanonicalDecl();
    AllClassTemplates.insert(Canon);
    return true;
  }

  // You can add more Visit*/Traverse* methods here later.

  // An entrypoint to start traversal on a declaration
  void run() {
    TranslationUnitDecl *TU = Context.getTranslationUnitDecl();
    // Start recursive traversal from the translation unit.
    TraverseDecl(TU);
  }

  const llvm::SmallPtrSet<const FunctionTemplateDecl *, 32> &
  getFunctionTemplates() const {
    return AllFunctionTemplates;
  }

  const llvm::SmallPtrSet<const ClassTemplateDecl *, 32> &
  getClassTemplates() const {
    return AllClassTemplates;
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
    Visitor.run();

    const SourceManager &SM = Ctx.getSourceManager();
    FileID MainFileID = SM.getMainFileID();

    PrintingPolicy Policy(Ctx.getLangOpts());
    Policy.adjustForCPlusPlus();

    auto buildTemplateArgString = [&](const TemplateArgumentList &Args) {
      std::string S;
      raw_string_ostream OS(S);
      OS << "<";
      for (unsigned I = 0, E = Args.size(); I != E; ++I) {
        if (I)
          OS << ", ";
        Args[I].print(Policy, OS, true);
      }
      OS << ">";
      OS.flush();
      return S;
    };

    const auto &FuncTemplates = Visitor.getFunctionTemplates();
    const auto &ClassTemplates = Visitor.getClassTemplates();

    // === Explicit instantiation declarations for templates defined in this file ===
    for (const FunctionTemplateDecl *FTD : FuncTemplates) {
      const FunctionDecl *TemplateFD = FTD->getTemplatedDecl();
      SourceLocation Loc = TemplateFD->getLocation();
      FileID FID = SM.getFileID(SM.getSpellingLoc(Loc));
      if (FID != MainFileID)
        continue;

      llvm::StringSet<> SeenArgs;
      std::string InsertText;

      for (auto *SpecFD : FTD->specializations()) {
        auto *Info = SpecFD->getTemplateSpecializationInfo();
        if (!Info)
          continue;

        TemplateSpecializationKind K = Info->getTemplateSpecializationKind();
        if (K != TSK_ImplicitInstantiation)
          continue;

        // Only consider instantiations whose point of instantiation is
        // in the main file (i.e., triggered by user code in this TU).
        SourceLocation POI = Info->getPointOfInstantiation();
        if (POI.isInvalid())
          continue;
        FileID InstFID = SM.getFileID(SM.getSpellingLoc(POI));
        if (InstFID != MainFileID)
          continue;

        const TemplateArgumentList *Args = Info->TemplateArguments;
        if (!Args)
          continue;

        std::string ArgStr = buildTemplateArgString(*Args);
        if (!SeenArgs.insert(ArgStr).second)
          continue;

        QualType RetTy = SpecFD->getReturnType();
        std::string RetStr = RetTy.getAsString(Policy);
        std::string QualName = SpecFD->getQualifiedNameAsString();

        std::string AttrPrefix;
        if (SpecFD->hasAttr<CUDAGlobalAttr>())
          AttrPrefix += "__global__ ";
        if (SpecFD->hasAttr<CUDADeviceAttr>())
          AttrPrefix += "__device__ ";

        std::string ParamStr;
        for (unsigned I = 0, E = SpecFD->getNumParams(); I != E; ++I) {
          if (I)
            ParamStr += ", ";
          QualType PTy = SpecFD->getParamDecl(I)->getType();
          ParamStr += PTy.getAsString(Policy);
        }

        InsertText += "\n";
        InsertText += "template ";
        InsertText += AttrPrefix;
        InsertText += RetStr;
        InsertText += " ";
        InsertText += QualName;
        InsertText += ArgStr;
        InsertText += "(";
        InsertText += ParamStr;
        InsertText += ")";
        InsertText += ";";
      }

      if (InsertText.empty())
        continue;

      const FunctionDecl *InsertFD = TemplateFD;
      const FunctionDecl *DefFD = nullptr;
      if (TemplateFD->hasBody(DefFD) && DefFD)
        InsertFD = DefFD;

      SourceLocation End = InsertFD->getSourceRange().getEnd();
      End = Lexer::getLocForEndOfToken(End, 0, SM, Ctx.getLangOpts());
      TheRewriter.InsertTextAfter(End, InsertText);
    }

    for (const ClassTemplateDecl *CTD : ClassTemplates) {
      const CXXRecordDecl *RD = CTD->getTemplatedDecl();
      SourceLocation Loc = RD->getLocation();
      FileID FID = SM.getFileID(SM.getSpellingLoc(Loc));
      if (FID != MainFileID)
        continue;

      const CXXRecordDecl *DefRD = RD->getDefinition();
      const CXXRecordDecl *InsertRD = DefRD ? DefRD : RD;

      llvm::StringSet<> SeenArgs;
      std::string InsertText;

      for (auto *CTSD : CTD->specializations()) {
        TemplateSpecializationKind K = CTSD->getSpecializationKind();
        if (K != TSK_ImplicitInstantiation)
          continue;

        SourceLocation POI = CTSD->getPointOfInstantiation();
        if (POI.isInvalid())
          continue;
        FileID InstFID = SM.getFileID(SM.getSpellingLoc(POI));
        if (InstFID != MainFileID)
          continue;

        const TemplateArgumentList &Args = CTSD->getTemplateArgs();
        std::string ArgStr = buildTemplateArgString(Args);
        if (!SeenArgs.insert(ArgStr).second)
          continue;

        std::string QualName = RD->getQualifiedNameAsString();
        const char *Kw = "class";
        if (RD->isStruct())
          Kw = "struct";
        else if (RD->isUnion())
          Kw = "union";

        InsertText += "\n";
        InsertText += "template ";
        InsertText += Kw;
        InsertText += " ";
        InsertText += QualName;
        InsertText += ArgStr;
        InsertText += ";";
      }

      if (InsertText.empty())
        continue;

      SourceLocation End = InsertRD->getBraceRange().getEnd();
      if (End.isInvalid())
        End = InsertRD->getSourceRange().getEnd();

      SourceLocation AfterSemi =
          Lexer::findLocationAfterToken(End, tok::semi, SM, Ctx.getLangOpts(),
                                        /*SkipTrailingWhitespaceAndNewLine=*/true);
      if (AfterSemi.isInvalid())
        AfterSemi = Lexer::getLocForEndOfToken(End, 0, SM, Ctx.getLangOpts());

      TheRewriter.InsertTextAfter(AfterSemi, InsertText);
    }

    // === Explicit specialization definitions for templates defined in headers ===
    std::string ExternalInsertText;
    llvm::StringSet<> ExternalFuncSeen;
    llvm::StringSet<> ExternalClassSeen;

    auto appendSpecializationInNamespace =
        [&](const DeclContext *DC, StringRef DeclText,
            std::string &Out, bool IsFunction) {
          SmallVector<const NamespaceDecl *, 4> Namespaces;
          const DeclContext *Cur = DC;
          while (Cur && !Cur->isTranslationUnit()) {
            if (const auto *NS = dyn_cast<NamespaceDecl>(Cur))
              Namespaces.push_back(NS);
            Cur = Cur->getParent();
          }
          std::reverse(Namespaces.begin(), Namespaces.end());

          Out += "\n";
          for (const NamespaceDecl *NS : Namespaces) {
            if (NS->isAnonymousNamespace())
              continue;
            if (NS->isInline())
              Out += "inline ";
            Out += "namespace ";
            Out += NS->getNameAsString();
            Out += " {\n";
          }

          StringRef Printed = DeclText.rtrim();
          Out += Printed.str();
          if (!IsFunction && Printed.ends_with("}"))
            Out += ";";
          Out += "\n";
          for (auto It = Namespaces.rbegin(); It != Namespaces.rend(); ++It) {
            const NamespaceDecl *NS = *It;
            if (NS->isAnonymousNamespace())
              continue;
            Out += "} // namespace ";
            Out += NS->getNameAsString();
            Out += "\n";
          }
        };

    auto appendInNamespace = [&](const DeclContext *DC, StringRef DeclText,
                                 std::string &Out) {
      SmallVector<const NamespaceDecl *, 4> Namespaces;
      const DeclContext *Cur = DC;
      while (Cur && !Cur->isTranslationUnit()) {
        if (const auto *NS = dyn_cast<NamespaceDecl>(Cur))
          Namespaces.push_back(NS);
        Cur = Cur->getParent();
      }
      std::reverse(Namespaces.begin(), Namespaces.end());

      Out += "\n";
      for (const NamespaceDecl *NS : Namespaces) {
        if (NS->isAnonymousNamespace())
          continue;
        if (NS->isInline())
          Out += "inline ";
        Out += "namespace ";
        Out += NS->getNameAsString();
        Out += " {\n";
      }

      StringRef Printed = DeclText.rtrim();
      Out += Printed.str();
      if (!Printed.ends_with("\n"))
        Out += "\n";

      for (auto It = Namespaces.rbegin(); It != Namespaces.rend(); ++It) {
        const NamespaceDecl *NS = *It;
        if (NS->isAnonymousNamespace())
          continue;
        Out += "} // namespace ";
        Out += NS->getNameAsString();
        Out += "\n";
      }
    };

    // Simple sanitizer to turn arbitrary strings into valid identifiers.
    auto makeSafeIdentifier = [](StringRef S) {
      std::string R;
      R.reserve(S.size() + 8);
      for (char C : S) {
        if ((C >= 'a' && C <= 'z') || (C >= 'A' && C <= 'Z') ||
            (C >= '0' && C <= '9') || C == '_') {
          R.push_back(C);
        } else {
          R.push_back('_');
        }
      }
      if (!R.empty() && (R[0] >= '0' && R[0] <= '9'))
        R.insert(R.begin(), '_');
      return R;
    };

    // Functions from headers (user, non-std) -> emit monomorphic functions
    for (const FunctionTemplateDecl *FTD : FuncTemplates) {
      const FunctionDecl *TemplateFD = FTD->getTemplatedDecl();
      SourceLocation Loc = TemplateFD->getLocation();
      if (SM.isInSystemHeader(Loc) || SM.isInSystemMacro(Loc))
        continue;
      FileID FID = SM.getFileID(SM.getSpellingLoc(Loc));
      if (FID == MainFileID)
        continue;

      if (isInStdNamespace(TemplateFD->getDeclContext()))
        continue;

      const IdentifierInfo *FuncII = TemplateFD->getIdentifier();
      if (!FuncII)
        continue;
      StringRef FuncName = FuncII->getName();
      if (FuncName.size() >= 2 && FuncName[0] == '_' && FuncName[1] == '_')
        continue;

      for (auto *SpecFD : FTD->specializations()) {
        auto *Info = SpecFD->getTemplateSpecializationInfo();
        if (!Info)
          continue;

        TemplateSpecializationKind K = Info->getTemplateSpecializationKind();
        if (K != TSK_ImplicitInstantiation)
          continue;

        const TemplateArgumentList *Args = Info->TemplateArguments;
        if (!Args)
          continue;
        std::string ArgStr = buildTemplateArgString(*Args);

        std::string QualName = SpecFD->getQualifiedNameAsString();
        std::string Key = QualName + ArgStr;
        if (!ExternalFuncSeen.insert(Key).second)
          continue;

        // Only handle if we have a body to duplicate.
        const Stmt *Body = SpecFD->getBody();
        if (!Body)
          continue;

        bool HasLambdaParam = false;
        for (unsigned I = 0, E = SpecFD->getNumParams(); I != E; ++I) {
          QualType PTy = SpecFD->getParamDecl(I)->getType();
          const clang::Type *Ty = PTy.getTypePtrOrNull();
          if (!Ty)
            continue;
          if (const auto *RT = Ty->getAs<RecordType>()) {
            if (const auto *RD = dyn_cast<CXXRecordDecl>(RT->getDecl())) {
              if (RD->isLambda()) {
                HasLambdaParam = true;
                break;
              }
            }
          }
        }
        if (HasLambdaParam)
          continue;

        std::string Mangled = SpecFD->getQualifiedNameAsString() + ArgStr;
        std::string NewName = "__tempopt_fn_" + makeSafeIdentifier(Mangled);

        std::string QualPrefix;
        size_t PosNS = QualName.rfind("::");
        if (PosNS != std::string::npos)
          QualPrefix = QualName.substr(0, PosNS + 2);

        ExternalFuncNames[QualName] = QualPrefix + NewName;

        std::string FuncText;
        {
          raw_string_ostream OS(FuncText);

          std::string AttrPrefix;
          if (SpecFD->hasAttr<CUDAGlobalAttr>())
            AttrPrefix += "__global__ ";
          if (SpecFD->hasAttr<CUDADeviceAttr>())
            AttrPrefix += "__device__ ";

          QualType RetTy = SpecFD->getReturnType();
          OS << AttrPrefix << RetTy.getAsString(Policy) << " " << NewName
             << "(";
          for (unsigned I = 0, E = SpecFD->getNumParams(); I != E; ++I) {
            if (I)
              OS << ", ";
            const ParmVarDecl *P = SpecFD->getParamDecl(I);
            OS << P->getType().getAsString(Policy) << " ";
            if (!P->getName().empty())
              OS << P->getName();
            else
              OS << "param" << I;
          }
          OS << ") ";
          SpecFD->getBody()->printPretty(OS, nullptr, Policy);
          OS << "\n";
        }

        // Heuristic lowering for sizeof...(Is): replace it with the
        // concrete pack size for this specialization. This assumes the
        // pattern is exactly 'sizeof...(Is)' in the instantiated body.
        {
          std::string Pattern = "sizeof...(Is)";
          if (!Pattern.empty() && !Pattern.compare(0, Pattern.size(), "sizeof...(Is)")) {
            std::string CountStr = std::to_string(Args->size());
            size_t Pos = 0;
            while ((Pos = FuncText.find(Pattern, Pos)) != std::string::npos) {
              FuncText.replace(Pos, Pattern.size(), CountStr);
              Pos += CountStr.size();
            }
          }
        }

        appendInNamespace(SpecFD->getDeclContext(), FuncText,
                          ExternalInsertText);
      }
    }

    // Classes from headers (user, non-std)
    for (const ClassTemplateDecl *CTD : ClassTemplates) {
      const CXXRecordDecl *RD = CTD->getTemplatedDecl();
      SourceLocation Loc = RD->getLocation();
      if (SM.isInSystemHeader(Loc) || SM.isInSystemMacro(Loc))
        continue;
      FileID FID = SM.getFileID(SM.getSpellingLoc(Loc));
      if (FID == MainFileID)
        continue;

      // Skip variadic class templates (parameter packs) for the same
      // reason as functions.
      if (const TemplateParameterList *TPL = CTD->getTemplateParameters()) {
        if (TPL->hasParameterPack())
          continue;
      }

      if (isInStdNamespace(RD->getDeclContext()))
        continue;

      const IdentifierInfo *ClassII = RD->getIdentifier();
      if (!ClassII)
        continue;
      StringRef ClassName = ClassII->getName();
      if (ClassName.size() >= 2 && ClassName[0] == '_' && ClassName[1] == '_')
        continue;

      for (auto *CTSD : CTD->specializations()) {
        TemplateSpecializationKind K = CTSD->getSpecializationKind();
        if (K != TSK_ImplicitInstantiation)
          continue;

        const TemplateArgumentList &Args = CTSD->getTemplateArgs();
        std::string ArgStr = buildTemplateArgString(Args);

        std::string QualName = RD->getQualifiedNameAsString();
        std::string Key = QualName + ArgStr;
        if (!ExternalClassSeen.insert(Key).second)
          continue;

        std::string S;
        raw_string_ostream OS(S);
        CTSD->print(OS, Policy, 0);
        OS.flush();

        appendSpecializationInNamespace(CTSD->getDeclContext(), S,
                                        ExternalInsertText, /*IsFunction=*/false);
      }
    }

    // Rewrite calls in the main file to use monomorphic header functions.
    if (!ExternalFuncSeen.empty()) {
      class CallUseRewriter : public RecursiveASTVisitor<CallUseRewriter> {
      public:
        CallUseRewriter(ASTContext &Ctx, Rewriter &R,
                        const llvm::StringMap<std::string> &Map)
            : Ctx(Ctx), R(R), MonoNames(Map) {}

        bool VisitCallExpr(CallExpr *CE) {
          const FunctionDecl *FD = CE->getDirectCallee();
          if (!FD)
            return true;

          const FunctionDecl *Canon = FD->getCanonicalDecl();
          auto It = MonoNames.find(Canon->getQualifiedNameAsString());
          if (It == MonoNames.end())
            return true;

          Expr *Callee = CE->getCallee();
          SourceRange CalleeRange = Callee->getSourceRange();
          CharSourceRange Range =
              CharSourceRange::getTokenRange(CalleeRange);
          R.ReplaceText(Range, It->second);
          return true;
        }

      private:
        ASTContext &Ctx;
        Rewriter &R;
        const llvm::StringMap<std::string> &MonoNames;
      };

      CallUseRewriter RewriterVisitor(Ctx, TheRewriter, ExternalFuncNames);
      RewriterVisitor.TraverseDecl(Ctx.getTranslationUnitDecl());
    }

    if (!ExternalInsertText.empty()) {
      StringRef Buffer = SM.getBufferData(MainFileID);
      size_t InsertOffset = 0;
      size_t Pos = 0;

      while (Pos < Buffer.size()) {
        size_t Found = Buffer.find("#include", Pos);
        if (Found == StringRef::npos)
          break;
        if (Found == 0 || Buffer[Found - 1] == '\n') {
          size_t LineEnd = Buffer.find('\n', Found);
          if (LineEnd == StringRef::npos)
            InsertOffset = Buffer.size();
          else
            InsertOffset = LineEnd + 1;
        }
        Pos = Found + 8;
      }

      SourceLocation InsertLoc =
          SM.getLocForStartOfFile(MainFileID).getLocWithOffset(InsertOffset);
      TheRewriter.InsertTextAfter(InsertLoc, ExternalInsertText);
    }

    if (OutputFile.empty())
      return;

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
  llvm::StringMap<std::string> ExternalFuncNames;
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
