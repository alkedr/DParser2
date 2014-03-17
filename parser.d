import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;
import std.ascii;



//struct ParserGenerator {
//	string action;  // used if not empty

//	string[dstring] onCharSequence;
//	string onIdentifier;
//	string onNoMatch;

//	string[dchar] generateSwitchRules() const {
//		string[dchar] result;

//	}

//	string generateSwitch(string[dchar] onCharSequence, string onIdentifier, string onNoMatch) const {

//	}

//	string code() const {
//		return (action.length > 0) ? action : generateSwitch(generateSwitchRules, onIdentifier, onNoMatch);
//	}
//}





class Parser {

	this(dstring text) {
		this.text = text ~ 0;
	}


private:

	dstring text;


	struct Cursor {
		uint index;
		uint line = 1;
		uint column = 1;
	}

	void advanceCursorNoSkip(ref Cursor currentCursor) {
		auto current = text[currentCursor.index];
		if ((current == '\u0000') || (current == '\u001A')) return;
		auto next = text[currentCursor.index+1];

		if (((current == '\u000D') && (next != '\u000A')) ||
		    (current == '\u000A') || (current == '\u2028') || (current == '\u2028')) {
			currentCursor.line++;
			currentCursor.column = 0;
		}
		currentCursor.column++;
		currentCursor.index++;
	}

	void skipLineComment() {
	}

	void skipBlockComment() {
	}

	void skipNestingBlockComment() {
	}

	void skipCrapForCursor(ref Cursor currentCursor) {
		while (true) {
			switch (text[currentCursor.index]) {
				case '\u000D':
				case '\u000A':
				case '\u2028':
				case '\u2029':
				case '\u0020':
				case '\u0009':
				case '\u000B':
				case '\u000C':
					break;

				case '/':
					switch (text[currentCursor.index+1]) {
						case '/': skipLineComment();
						case '*': skipBlockComment();
						case '+': skipNestingBlockComment();
						default: return;
					}

				default: return;
			}
			advanceCursorNoSkip(currentCursor);
		}
	}

	void advanceCursor(ref Cursor currentCursor) {
		advanceCursorNoSkip(currentCursor);
		skipCrapForCursor(currentCursor);
	}

	Cursor currentCursor;

	dchar currentChar() { return text[currentCursor.index]; }

	bool isEOF() { return (currentChar == '\u0000') || (currentChar == '\u001A'); }

	void advance() {
		advanceCursor(currentCursor);
	}

	void advanceNoSkip() {
		advanceCursorNoSkip(currentCursor);
	}

	void skipCrap() {
		skipCrapForCursor(currentCursor);
	}


	/*private struct ParserGenerator {
		ParserGenerator[dchar] rules;
		string action = "";
		string actionOnIdentifier = "";

		this(string actionOnIdentifier) {
			this.actionOnIdentifier = actionOnIdentifier;
		}

		ParserGenerator onCharSequence(const dchar[] chars, string action) {
			if (chars.length > 0) {
				if (chars[0] !in rules) rules[chars[0]] = ParserGenerator();
				rules[chars[0]].onCharSequence(chars[1..$], action);
			} else {
				this.action ~= action;
			}
			return this;
		}

		ParserGenerator onKeyword(const dchar[] chars, string action) {
			return onCharSequence(chars, "if(!isAlphaNum(currentChar)&&(currentChar!='_')){" ~ action ~ ";}else{" ~ actionOnIdentifier ~ ";}");
		}

		ParserGenerator onIdentifier(string action) {
			return this;
			//return onCharSequence(chars, "if(!isAlphaNum(currentChar)&&(currentChar!='_')){" ~ action ~ "}");
		}

		ParserGenerator onNoMatch(string action) {
			this.action ~= action;
			return this;
		}

		string code() const {
			return "skipCrap();auto begin = currentPosition;" ~ codeImpl();
		}

		private string codeImpl() const {
			if (rules.length == 0) return action ~ ";";
			auto result = "{switch(currentChar()){";
			foreach (key, value; rules) {
				result ~= format(`case'\U%08X':{advanceNoSkip();%s}break;`, key, value.codeImpl);
			}
			return result ~ format(`default:{advanceNoSkip();%s;}break;}}`, action.length > 0 ? action : actionOnIdentifier);
		}
	}*/

	void on_import() { writeln(__FUNCTION__); }
	void on_immutable() { writeln(__FUNCTION__); }
	void on_identifier() { writeln(__FUNCTION__); }
	void on_noMatch() { writeln(__FUNCTION__); }

	void parseTopLevelDeclaration() {
		mixin(
			generateParser([
				"import": "on_import",
				"immutable": "on_immutable",
			],
			"on_identifier",
			"on_noMatch")
		);
	}


	private static string generateParser(string[dstring] keywordActions,
		string identifierAction, string noMatchAction)
	{
		class ParserGenerator {
			int nestingLevel;
			ParserGenerator[dchar] rules;
			string action = "";

			this(int nestingLevel = 0) {
				this.nestingLevel = nestingLevel;
			}

			void onCharSequence(const dchar[] chars, string action) {
				if (chars.length > 0) {
					if (chars[0] !in rules) rules[chars[0]] = new ParserGenerator(nestingLevel+1);
					rules[chars[0]].onCharSequence(chars[1..$], action);
				} else {
					assert(this.action.length == 0);
					this.action ~= action;
				}
			}

			void onKeyword(const dchar[] chars, string actionOnMatch, string actionOnMismatch) {
				onCharSequence(chars, "if(!isAlphaNum(currentChar)&&(currentChar!='_')){" ~ actionOnMatch ~ ";}else{" ~ actionOnMismatch ~ ";}");
			}

			void onNoMatch(string action) {
				assert(this.action.length == 0);
				this.action ~= action;
			}

			private string code() const {
				if (rules.length == 0) return action;
				auto result = format("{switch(text.ptr[currentCursor.index + %d]){", nestingLevel);
				foreach (key, value; rules) {
					result ~= format(`case'\U%08X':{advanceNoSkip();%s;}break;`, key, value.code);
				}
				return result ~ format(`default:{advanceNoSkip();%s;}break;}}`, action);
			}
		}

		auto pg = new ParserGenerator;
		foreach (keyword, action; keywordActions) {
			pg.onKeyword(keyword, action, noMatchAction);
		}
		return "skipCrap();/*auto begin = currentPosition;*/" ~ pg.code;
	}


	private



unittest {
	auto p = new Parser("impor");
	p.parseTopLevelDeclaration;

	writeln(generateParser([
				"import": "on_import",
				"immutable": "on_immutable",
			],
			"on_identifier",
			"on_noMatch"));
}


}


void main() {

}







interface Gen {
	string code() const;
}

class




class Generator {
	Generator[string] rules;  // condition => generator
	string action;  // used if rules.empty
	bool optional;


	string code(string charIndexVarName, int offset) {

	}
}
