#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <stdexcept>
#include <fstream>
#include <vector>

using namespace std;

enum TokenType
{
    T_INT,
    T_STRING,
    T_BOOL,
    T_ID,
    T_NUM,
    T_IF,
    T_ELSE,
    T_RETURN,
    T_ASSIGN,
    T_PLUS,
    T_MINUS,
    T_MUL,
    T_DIV,
    T_LPAREN,
    T_RPAREN,
    T_LBRACE,
    T_RBRACE,
    T_SEMICOLON,
    T_GT,
    T_WHILE,
    T_FUNC,
    T_SWITCH,
    T_CASE,
    T_DEFAULT,
    T_BREAK,
    T_TRUE,
    T_FALSE,
    T_COMMENT,
    T_COLON,
    T_EOF
};

struct Token
{
    TokenType type;
    string value;
    int lineNumber;
};

class Lexer
{
private:
    string src;
    size_t pos;
    int lineNumber;

public:
    Lexer(const string &src) : src(src), pos(0), lineNumber(1) {}

    vector<Token> tokenize()
    {
        vector<Token> tokens;
        while (pos < src.size())
        {
            char current = src[pos];

            if (current == '\n')
            {
                lineNumber++;
                pos++;
                continue;
            }
            if (isspace(current))
            {
                pos++;
                continue;
            }
            if (current == '/' && pos + 1 < src.size() && src[pos + 1] == '/')
            {
                while (pos < src.size() && src[pos] != '\n')
                    pos++;
                continue;
            }
            if (isdigit(current))
            {
                tokens.push_back(Token{T_NUM, consumeNumber(), lineNumber});
                continue;
            }
            if (isalpha(current))
            {
                string word = consumeWord();
                TokenType type = T_ID;

                if (word == "int")
                    type = T_INT;
                else if (word == "if")
                    type = T_IF;
                else if (word == "else")
                    type = T_ELSE;
                else if (word == "return")
                    type = T_RETURN;
                else if (word == "while")
                    type = T_WHILE;
                else if (word == "func")
                    type = T_FUNC;
                else if (word == "switch")
                    type = T_SWITCH;
                else if (word == "case")
                    type = T_CASE;
                else if (word == "default")
                    type = T_DEFAULT;
                else if (word == "break")
                    type = T_BREAK;
                else if (word == "bool")
                    type = T_BOOL;
                else if (word == "true")
                    type = T_TRUE;
                else if (word == "false")
                    type = T_FALSE;

                tokens.push_back(Token{type, word, lineNumber});
                continue;
            }
            if (current == '"')
            {
                tokens.push_back(Token{T_STRING, consumeString(), lineNumber});
                continue;
            }

            switch (current)
            {
            case '=':
                tokens.push_back(Token{T_ASSIGN, "=", lineNumber});
                break;
            case '+':
                tokens.push_back(Token{T_PLUS, "+", lineNumber});
                break;
            case '-':
                tokens.push_back(Token{T_MINUS, "-", lineNumber});
                break;
            case '*':
                tokens.push_back(Token{T_MUL, "*", lineNumber});
                break;
            case '/':
                tokens.push_back(Token{T_DIV, "/", lineNumber});
                break;
            case '(':
                tokens.push_back(Token{T_LPAREN, "(", lineNumber});
                break;
            case ')':
                tokens.push_back(Token{T_RPAREN, ")", lineNumber});
                break;
            case '{':
                tokens.push_back(Token{T_LBRACE, "{", lineNumber});
                break;
            case '}':
                tokens.push_back(Token{T_RBRACE, "}", lineNumber});
                break;
            case ';':
                tokens.push_back(Token{T_SEMICOLON, ";", lineNumber});
                break;
            case '>':
                tokens.push_back(Token{T_GT, ">", lineNumber});
                break;
            case ':':
                tokens.push_back(Token{T_COLON, ":", lineNumber});
                break;
            default:
                cout << "Unexpected character: " << current << " at line " << lineNumber << endl;
                exit(1);
            }
            pos++;
        }
        tokens.push_back(Token{T_EOF, "", lineNumber});
        return tokens;
    }

    string consumeNumber()
    {
        size_t start = pos;
        while (pos < src.size() && isdigit(src[pos]))
            pos++;
        return src.substr(start, pos - start);
    }

    string consumeWord()
    {
        size_t start = pos;
        while (pos < src.size() && isalnum(src[pos]))
            pos++;
        return src.substr(start, pos - start);
    }

    string consumeString()
    {
        pos++; // Skip the opening quote
        size_t start = pos;
        while (pos < src.size() && src[pos] != '"')
            pos++;
        string str = src.substr(start, pos - start);
        pos++; // Skip the closing quote
        return str;
    }
};

class SymbolTable
{
public:
    void declareVariable(const string &name, const string &type)
    {
        if (symbolTable.find(name) != symbolTable.end())
        {
            throw runtime_error("Semantic error: Variable '" + name + "' is already declared.");
        }
        symbolTable[name] = type;
    }

    string getVariableType(const string &name)
    {
        if (symbolTable.find(name) == symbolTable.end())
        {
            throw runtime_error("Semantic error: Variable '" + name + "' is not declared.");
        }
        return symbolTable[name];
    }

    bool isDeclared(const string &name) const
    {
        return symbolTable.find(name) != symbolTable.end();
    }

private:
    map<string, string> symbolTable;
};

class IntermediateCodeGenerator
{
public:
    vector<string> instructions;
    int tempCount = 0;

    string newTemp()
    {
        return "t" + to_string(tempCount++);
    }

    void addInstruction(const string &instr)
    {
        instructions.push_back(instr);
    }

    // Returns all instructions as a single string (joined by newlines)
    string getInstructionsAsString() const
    {
        string result;
        for (const auto &instr : instructions)
        {
            result += instr + "\n";
        }
        return result;
    }

    // Returns all instructions as a vector of strings
    vector<string> getInstructionsAsVector() const
    {
        return instructions;
    }

    void printInstructions()
    {
        for (const auto &instr : instructions)
        {
            cout << instr << endl;
        }
    }
};

class Parser
{
public:
    // Constructor
    Parser(const vector<Token> &tokens, SymbolTable &symTable, IntermediateCodeGenerator &icg)
        : tokens(tokens), pos(0), symTable(symTable), icg(icg) {}

    // here the private member of this class are being initalized with the arguments passed to this constructor

    void parseProgram()
    {
        while (tokens[pos].type != T_EOF)
        {
            parseStatement();
        }
    }

private:
    vector<Token> tokens;
    size_t pos;
    SymbolTable &symTable;
    IntermediateCodeGenerator &icg;

    void parseStatement()
    {
        if (tokens[pos].type == T_INT)
        {
            parseDeclaration();
        }
        else if (tokens[pos].type == T_ID)
        {
            parseAssignment();
        }
        else if (tokens[pos].type == T_IF)
        {
            parseIfStatement();
        }
        else if (tokens[pos].type == T_RETURN)
        {
            parseReturnStatement();
        }
        else if (tokens[pos].type == T_LBRACE)
        {
            parseBlock();
        }
        else if (tokens[pos].type == T_WHILE)
        {
            parseWhileStatement();
        }
        else if (tokens[pos].type == T_FUNC)
        {
            parseFunction();
        }
        else if (tokens[pos].type == T_SWITCH)
        {
            parseSwitchStatement();
        }
        else if (tokens[pos].type == T_BOOL)
        {
            parseBooleanDeclaration();
        }
        else
        {
            cout << "Syntax error: unexpected token '" << tokens[pos].value << "' at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
    }

    void parseDeclaration()
    {
        expect(T_INT);                               // Expect and consume the int keyword.
        string varName = expectAndReturnValue(T_ID); // Expect and return the variable name (identifier).
        symTable.declareVariable(varName, "int");    // Register the variable in the symbol table with type "int".
        expect(T_SEMICOLON);                         // Expect the semicolon to end the statement.
    }

    void parseAssignment()
    {
        string varName = expectAndReturnValue(T_ID);
        symTable.getVariableType(varName); // Ensure the variable is declared in the symbol table.
        expect(T_ASSIGN);
        string expr = parseExpression();
        icg.addInstruction(varName + " = " + expr); // Generate intermediate code for the assignment.
        expect(T_SEMICOLON);
    }

    void parseIfStatement()
    {
        expect(T_IF);
        expect(T_LPAREN);                // Expect and consume the opening parenthesis for the condition.
        string cond = parseExpression(); // Parse the condition expression inside the parentheses.
        expect(T_RPAREN);

        string temp = icg.newTemp();             // Generate a new temporary variable for the condition result.
        icg.addInstruction(temp + " = " + cond); // Generate intermediate code for storing the condition result.

        icg.addInstruction("if " + temp + " goto L1"); // Jump to label L1 if condition is true.
        icg.addInstruction("goto L2");                 // Otherwise, jump to label L2.
        icg.addInstruction("L1:");                     // Otherwise, jump to label L2.

        parseStatement();

        if (tokens[pos].type == T_ELSE)
        { // If an `else` part exists, handle it.
            icg.addInstruction("goto L3");
            icg.addInstruction("L2:");
            expect(T_ELSE);
            parseStatement(); // Parse the statement inside the else block.
            icg.addInstruction("L3:");
        }
        else
        {
            icg.addInstruction("L2:");
        }
    }

    void parseReturnStatement()
    {
        expect(T_RETURN);
        string expr = parseExpression();
        icg.addInstruction("return " + expr); // Generate intermediate code for the return statement.
        expect(T_SEMICOLON);
    }

    void parseBlock()
    {
        expect(T_LBRACE); // Expect and consume the opening brace `{`.
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF)
        {
            parseStatement(); // Parse the statements inside the block.
        }
        expect(T_RBRACE);
    }

    string parseExpression()
    {
        string term = parseTerm();
        while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS)
        {
            TokenType op = tokens[pos++].type;
            string nextTerm = parseTerm();                                                       // Parse the next term in the expression.
            string temp = icg.newTemp();                                                         // Generate a temporary variable for the result
            icg.addInstruction(temp + " = " + term + (op == T_PLUS ? " + " : " - ") + nextTerm); // Intermediate code for operation
            term = temp;
        }
        if (tokens[pos].type == T_GT)
        {
            pos++;
            string nextExpr = parseExpression();                        // Parse the next expression for the comparison.
            string temp = icg.newTemp();                                // Generate a temporary variable for the result.
            icg.addInstruction(temp + " = " + term + " > " + nextExpr); // Intermediate code for the comparison.
            term = temp;
        }
        return term;
    }

    string parseTerm()
    {
        string factor = parseFactor();
        while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV)
        {
            TokenType op = tokens[pos++].type;
            string nextFactor = parseFactor();
            string temp = icg.newTemp();                                                            // Generate a temporary variable for the result.
            icg.addInstruction(temp + " = " + factor + (op == T_MUL ? " * " : " / ") + nextFactor); // Intermediate code for operation.
            factor = temp;                                                                          // Update the factor to be the temporary result.
        }
        return factor;
    }

    string parseFactor()
    {
        if (tokens[pos].type == T_NUM)
        {
            return tokens[pos++].value;
        }
        else if (tokens[pos].type == T_ID)
        {
            return tokens[pos++].value;
        }
        else if (tokens[pos].type == T_LPAREN)
        {
            expect(T_LPAREN);
            string expr = parseExpression();
            expect(T_RPAREN);
            return expr;
        }
        else if (tokens[pos].type == T_STRING)
        {
            return "\"" + tokens[pos++].value + "\"";
        }
        else if (tokens[pos].type == T_TRUE || tokens[pos].type == T_FALSE)
        {
            return tokens[pos++].value;
        }
        else
        {
            cout << "Syntax error: unexpected token '" << tokens[pos].value << "' at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
    }

    void expect(TokenType type)
    {
        if (tokens[pos].type != type)
        {
            cout << "Syntax error: expected '" << type << "' at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
        pos++;
    }

    string expectAndReturnValue(TokenType type)
    {
        string value = tokens[pos].value;
        expect(type);
        return value;
    }

    void parseWhileStatement()
    {
        expect(T_WHILE);
        expect(T_LPAREN);
        string cond = parseExpression();
        expect(T_RPAREN);

        string temp = icg.newTemp();
        icg.addInstruction(temp + " = " + cond);

        icg.addInstruction("L1:");
        icg.addInstruction("if " + temp + " goto L2");
        icg.addInstruction("goto L3");
        icg.addInstruction("L2:");

        parseStatement();

        icg.addInstruction(temp + " = " + cond);
        icg.addInstruction("goto L1");
        icg.addInstruction("L3:");
    }

    void parseFunction()
    {
        expect(T_FUNC);
        string funcName = expectAndReturnValue(T_ID);
        expect(T_LPAREN);
        expect(T_RPAREN);
        expect(T_LBRACE);

        icg.addInstruction("FUNC " + funcName + ":");

        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF)
        {
            parseStatement();
        }

        expect(T_RBRACE);
        icg.addInstruction("END FUNC " + funcName);
    }

    void parseSwitchStatement()
    {
        expect(T_SWITCH);
        expect(T_LPAREN);
        string expr = parseExpression();
        expect(T_RPAREN);
        expect(T_LBRACE);

        string temp = icg.newTemp();
        icg.addInstruction(temp + " = " + expr);

        int switchEndLabel = icg.tempCount++;

        while (tokens[pos].type == T_CASE || tokens[pos].type == T_DEFAULT)
        {
            if (tokens[pos].type == T_CASE)
            {
                expect(T_CASE);
                string caseValue = expectAndReturnValue(T_NUM);
                expect(T_COLON);
                icg.addInstruction("if " + temp + " == " + caseValue + " goto L" + caseValue);
                icg.addInstruction("goto L" + to_string(switchEndLabel));
                icg.addInstruction("L" + caseValue + ":");
                parseStatement();
                icg.addInstruction("goto L" + to_string(switchEndLabel));
            }
            else if (tokens[pos].type == T_DEFAULT)
            {
                expect(T_DEFAULT);
                expect(T_COLON);
                icg.addInstruction("L" + to_string(switchEndLabel) + ":");
                parseStatement();
            }
        }

        expect(T_RBRACE);
        icg.addInstruction("L" + to_string(switchEndLabel) + ":");
    }

    void parseBooleanDeclaration()
    {
        expect(T_BOOL);
        string varName = expectAndReturnValue(T_ID);
        symTable.declareVariable(varName, "bool");
        expect(T_SEMICOLON);
    }
};

class MachineCodeGenerator
{
public:
    // Store the generated machine instructions
    vector<string> machineInstructions;

    // Generate machine code from intermediate code and store it
    void generateMachineCode(const vector<string> &intermediateCode)
    {
        for (const string &instr : intermediateCode)
        {
            machineInstructions.push_back(translateToMachineCode(instr));
        }
    }

    // Print the stored machine instructions
    void printMachineInstructions() const
    {
        for (const string &instr : machineInstructions)
        {
            cout << instr << endl;
        }
    }

    // Add this method to access the instructions
    std::vector<std::string> getInstructionsAsVector() const
    {
        return machineInstructions;
    }

private:
    // Translates an intermediate code instruction to machine code
    string translateToMachineCode(const string &intermediateInstr)
    {
        vector<string> tokens = split(intermediateInstr, ' ');

        if (tokens.empty())
        {
            return ""; // Return an empty line for invalid or empty input
        }

        if (tokens[0] == "if")
        {
            // Handle conditional instructions like: if t0 goto L1
            return "CMP " + tokens[1] + ", 0\nJNE " + tokens[3];
        }
        else if (tokens[0] == "goto")
        {
            // Handle unconditional jumps
            return "JMP " + tokens[1];
        }
        else if (tokens[0].back() == ':')
        {
            // Handle labels
            return tokens[0];
        }
        else if (tokens[1] == "=" && tokens.size() == 3)
        {
            // Handle simple assignments: x = y
            return "MOV " + tokens[0] + ", " + tokens[2];
        }
        else if (tokens[1] == "=" && tokens.size() == 5)
        {
            // Handle arithmetic operations: x = y + z
            string opCode;
            if (tokens[3] == "+")
                opCode = "ADD";
            else if (tokens[3] == "-")
                opCode = "SUB";
            else if (tokens[3] == "*")
                opCode = "MUL";
            else if (tokens[3] == "/")
                opCode = "DIV";
            else if (tokens[3] == ">")
                return "CMP " + tokens[2] + ", " + tokens[4]; // Directly handle comparison
            else
                throw runtime_error("Unsupported operation: " + tokens[3]);
            return opCode + " " + tokens[0] + ", " + tokens[2] + ", " + tokens[4];
        }
        else if (tokens[0] == "return")
        {
            // Handle return instructions
            return "MOV R0, " + tokens[1] + "\nRET";
        }
        else if (tokens[0] == "FUNC")
        {
            // Handle function labels
            return tokens[0] + " " + tokens[1];
        }
        else if (tokens[0] == "END" && tokens[1] == "FUNC")
        {
            // Handle function end labels
            return tokens[0] + " " + tokens[2];
        }
        else if (tokens[0] == "switch")
        {
            // Handle switch statements
            return "SWITCH " + tokens[1];
        }
        else if (tokens[0] == "Case")
        {
            // Handle case statements
            return "CASE " + tokens[1];
        }
        else if (tokens[0] == "default")
        {
            // Handle default case
            return "DEFAULT";
        }
        else if (tokens[0] == "break")
        {
            // Handle break statements
            return "BREAK";
        }
        else
        {
            throw runtime_error("Unsupported operation: " + intermediateInstr);
        }
    }

    // Splits a string into tokens by a delimiter
    vector<string> split(const string &str, char delimiter)
    {
        vector<string> tokens;
        size_t start = 0, end = 0;

        while ((end = str.find(delimiter, start)) != string::npos)
        {
            if (end != start)
            {
                tokens.push_back(str.substr(start, end - start));
            }
            start = end + 1;
        }

        if (start < str.size())
        {
            tokens.push_back(str.substr(start));
        }

        return tokens;
    }
};

int main()
{
    // Open the source file
    ifstream file("InputCode.txt");
    if (!file.is_open())
    {
        cerr << "Error: Could not open the file 'myCode.txt'." << endl;
        return 1; // Exit with an error code if file cannot be opened
    }

    // Read the entire file content into a string
    string src((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
    file.close(); // Close the file

    // Create Lexer and tokenize the source code
    Lexer lexer(src);
    vector<Token> tokens = lexer.tokenize();

    // Create Symbol Table and Intermediate Code Generator
    SymbolTable symTable;
    IntermediateCodeGenerator icg;

    // Create Parser and parse the tokens
    Parser parser(tokens, symTable, icg);
    cout << "Intermediate Code:" << endl;
    parser.parseProgram();
    icg.printInstructions();

    // Machine Code Generation
    MachineCodeGenerator mcg;
    mcg.generateMachineCode(icg.getInstructionsAsVector());

    // Print Machine Code to console
    cout << "\nMachine Code:\n";
    mcg.printMachineInstructions();

    // Write Machine Code to a file
    ofstream machineCodeFile("machineCode.txt");
    if (!machineCodeFile.is_open())
    {
        cerr << "Error: Could not create or write to the file 'machineCode.txt'." << endl;
        return 1; // Exit with an error code if file cannot be created
    }

    // Get machine code instructions and write them to the file
    vector<string> machineInstructions = mcg.getInstructionsAsVector();
    for (const string &instruction : machineInstructions)
    {
        machineCodeFile << instruction << endl;
    }

    machineCodeFile.close(); // Close the file
    cout << "\nMachine code successfully written to 'machineCode.txt'." << endl;

    return 0; // Exit successfully
}