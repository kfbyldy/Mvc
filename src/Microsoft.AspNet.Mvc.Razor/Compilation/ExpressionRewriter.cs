// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Linq.Expressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.AspNet.Mvc.Razor.Compilation
{
    /// <summary>
    /// An expression rewriter which can hoist a simple expression lambda into a private field.
    /// </summary>
    public class ExpressionRewriter : CSharpSyntaxRewriter
    {
        private static readonly string FieldNameTemplate = "__h{0}";

        public ExpressionRewriter(SemanticModel semanticModel)
        {
            SemanticModel = semanticModel;

            Expressions = new List<KeyValuePair<SimpleLambdaExpressionSyntax, IdentifierNameSyntax>>();
        }

        private SemanticModel SemanticModel { get; }

        private List<KeyValuePair<SimpleLambdaExpressionSyntax, IdentifierNameSyntax>> Expressions { get; }

        public override SyntaxNode VisitClassDeclaration(ClassDeclarationSyntax node)
        {
            Expressions.Clear();

            // Call base first to visit all the children and populate Expressions.
            var classDeclaration = (ClassDeclarationSyntax)base.VisitClassDeclaration(node);

            var memberDeclarations = new List<MemberDeclarationSyntax>();
            foreach (var kvp in Expressions)
            {
                var expression = kvp.Key;
                var memberName = kvp.Value.GetFirstToken();

                var expressionType = SemanticModel.GetTypeInfo(expression);
                var declaration = SyntaxFactory.FieldDeclaration(
                    SyntaxFactory.List<AttributeListSyntax>(),
                    SyntaxFactory.TokenList(
                        SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                        SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                        SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)),
                    SyntaxFactory.VariableDeclaration(
                        SyntaxFactory.ParseTypeName(expressionType.ConvertedType.ToDisplayString()),
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.VariableDeclarator(
                                memberName,
                                SyntaxFactory.BracketedArgumentList(),
                                SyntaxFactory.EqualsValueClause(expression)))))
                    .WithTriviaFrom(expression);
                memberDeclarations.Add(declaration);
            }
            
            return classDeclaration.AddMembers(memberDeclarations.ToArray());
        }

        public override SyntaxNode VisitSimpleLambdaExpression(SimpleLambdaExpressionSyntax node)
        {
            var type = SemanticModel.GetTypeInfo(node);
            if (type.ConvertedType.Name!= typeof(Expression).Name && 
                type.ConvertedType.ContainingNamespace.Name != typeof(Expression).Namespace)
            {
                return node;
            }

            if (!node.Parent.IsKind(SyntaxKind.Argument))
            {
                return node;
            }

            var parameter = node.Parameter;
            if (IsValidForHoisting(parameter, node.Body))
            {
                // Replace with a MemberAccess
                var memberName = string.Format(FieldNameTemplate, Expressions.Count);
                var memberAccess = PadMemberAccess(node, SyntaxFactory.IdentifierName(memberName));
                Expressions.Add(new KeyValuePair<SimpleLambdaExpressionSyntax, IdentifierNameSyntax>(node, memberAccess));
                return memberAccess;
            }

            return node;
        }

        private IdentifierNameSyntax PadMemberAccess(SimpleLambdaExpressionSyntax node, IdentifierNameSyntax memberAccess)
        {
            // We want to make the new span 
            var originalSpan = node.GetLocation().GetMappedLineSpan();

            // Start by collecting all the trivia 'inside' the expression - we need to tack that on the end, but
            // if it ends with a newline, don't include that.
            var innerTrivia = SyntaxFactory.TriviaList(node.DescendantTrivia(descendIntoChildren: n => true));
            if (innerTrivia.Count > 0 && innerTrivia[innerTrivia.Count - 1].IsKind(SyntaxKind.EndOfLineTrivia))
            {
                innerTrivia = innerTrivia.RemoveAt(innerTrivia.Count - 1);
            }

            memberAccess = memberAccess.WithTrailingTrivia(innerTrivia);

            // If everything is all on one line, then make sure the spans are the same, to compensate
            // for the expression potentially being longer than the variable name.
            var lineSpan = originalSpan.EndLinePosition.Line - originalSpan.StartLinePosition.Line;
            if (lineSpan == 0)
            {
                var padding = node.Span.Length - memberAccess.FullSpan.Length;
                var trailingTrivia = 
                    SyntaxFactory.TriviaList(memberAccess.GetTrailingTrivia())
                    .Add(SyntaxFactory.Whitespace(new string(' ', padding)))
                    .AddRange(node.GetTrailingTrivia());

                return 
                    memberAccess
                    .WithLeadingTrivia(node.GetLeadingTrivia())
                    .WithTrailingTrivia(trailingTrivia);
            }
            else
            {
                // If everything isn't on the same line, we need to pad out the last line.
                var padding =
                    originalSpan.EndLinePosition.Character -
                    originalSpan.StartLinePosition.Character;

                var trailingTrivia =
                    SyntaxFactory.TriviaList(memberAccess.GetTrailingTrivia())
                    .Add(SyntaxFactory.Whitespace(new string(' ', padding)))
                    .AddRange(node.GetTrailingTrivia());

                return 
                    memberAccess
                    .WithLeadingTrivia(node.GetLeadingTrivia())
                    .WithTrailingTrivia(trailingTrivia);
            }
        }

        private bool IsValidForHoisting(ParameterSyntax parameter, CSharpSyntaxNode node)
        {
            if (node.IsKind(SyntaxKind.IdentifierName))
            {
                var identifier = (IdentifierNameSyntax)node;
                if (identifier.Identifier.Text == parameter.Identifier.Text)
                {
                    return true;
                }
            }
            else if (node.IsKind(SyntaxKind.SimpleMemberAccessExpression))
            {
                var memberAccess = (MemberAccessExpressionSyntax)node;
                var lhs = memberAccess.Expression;
                return IsValidForHoisting(parameter, lhs);
            }

            return false;
        }
    }
}
